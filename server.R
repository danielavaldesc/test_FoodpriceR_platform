# -----------------------------------------------
# Cargamos las librerías necesarias
# -----------------------------------------------
library(shiny)
library(FoodpriceR)     # Para DataCol() y CoCA()
library(rhandsontable)  # Para edición de EER
library(DT)             # Para mostrar tabla de resultados
library(leaflet)        # Para el mapa interactivo
library(sf)             # Para manejar shapefiles
library(dplyr)          # Para filtros y transformaciones
library(scales)         # Para formatear los KPIs
library(ggplot2)        # Para gráficos personalizados
library(gridExtra)      # Para mostrar múltiples gráficos juntos

# -----------------------------------------------
# 1. Cargar shapefiles para el mapa
# -----------------------------------------------
# Directorio 
setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\test_FoodpriceR_platform\\")

# Cargar archivos geográficos (ajusta rutas si es necesario)
departamentos <- st_read("shape_files/dptos_col/departamentos.shp")
municipios <- st_read("shape_files/DANE_geodata/MGN_ANM_MPIOS_WGS84.shp")

# Lista de capitales reconocidas por FoodpriceR
capitales <- c(
  "Bogotá", "Medellín", "Cali", "Barranquilla", "Cartagena", "Cúcuta",
  "Bucaramanga", "Pereira", "Ibagué", "Pasto", "Manizales", "Neiva",
  "Villavicencio", "Valledupar", "Armenia", "Sincelejo", "Popayán",
  "Palmira", "Buenaventura", "Tuluá", "Cartago", "Tunja", "Ipiales", "Montería"
)

# Obtener centroides de las capitales para mostrarlas como puntos
capitales_sf <- municipios %>%
  filter(toupper(MPIO_CNMBR) %in% toupper(capitales)) %>%
  st_centroid()

# Relación Departamento → Ciudad base
dpto_a_ciudad <- c(
  "ANTIOQUIA" = "Medellín",
  "ATLANTICO" = "Barranquilla",
  "BOGOTÁ" = "Bogotá",
  "BOLÍVAR" = "Cartagena",
  "BOYACÁ" = "Tunja",
  "CALDAS" = "Manizales",
  "CAUCA" = "Popayán",
  "CESAR" = "Valledupar",
  "CÓRDOBA" = "Montería",
  "CUNDINAMARCA" = "Bogotá",
  "HUILA" = "Neiva",
  "LA GUAJIRA" = "Riohacha",
  "MAGDALENA" = "Santa Marta",
  "META" = "Villavicencio",
  "NARIÑO" = "Pasto",
  "NORTE DE SANTANDER" = "Cúcuta",
  "QUINDIO" = "Armenia",
  "RISARALDA" = "Pereira",
  "SANTANDER" = "Bucaramanga",
  "SUCRE" = "Sincelejo",
  "TOLIMA" = "Ibagué",
  "VALLE DEL CAUCA" = "Cali",
  "ARAUCA" = "Arauca",
  "CASANARE" = "Yopal",
  "PUTUMAYO" = "Mocoa",
  "AMAZONAS" = "Leticia"
)

# -----------------------------------------------
# 2. Lógica reactiva principal
# -----------------------------------------------

server <- function(input, output, session) {
  
  # ------------------- Selección de ciudad -------------------
  ciudad_seleccionada <- reactiveVal("Bogotá")  # valor inicial
  
  output$mapa_ciudades <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = departamentos,
        color = "#444444", weight = 1, fillOpacity = 0.2,
        layerId = ~NOMBRE_DPT, label = ~NOMBRE_DPT
      ) %>%
      addCircleMarkers(
        data = capitales_sf,
        radius = 6, color = "red", fillOpacity = 0.8,
        label = ~MPIO_CNMBR, layerId = ~MPIO_CNMBR
      )
  })
  
  observeEvent(input$mapa_ciudades_shape_click, {
    click_id <- input$mapa_ciudades_shape_click$id
    ciudad <- dpto_a_ciudad[[click_id]]
    if (!is.null(ciudad)) ciudad_seleccionada(ciudad)
  })
  
  observeEvent(input$mapa_ciudades_marker_click, {
    ciudad_click <- input$mapa_ciudades_marker_click$id
    if (!is.null(ciudad_click)) ciudad_seleccionada(ciudad_click)
  })
  
  output$ciudad_activa <- renderText({
    paste("Ciudad activa:", ciudad_seleccionada())
  })
  
  # ------------------- Entrada EER manual -------------------
  output$hot_table_col_manual_eer <- renderRHandsontable({
    rhandsontable(FoodpriceR::EER, useTypes = TRUE, stretchH = "all")
  })
  
  eer_manual_col <- reactive({
    if (!is.null(input$hot_table_col_manual_eer)) {
      hot_to_r(input$hot_table_col_manual_eer)
    } else {
      FoodpriceR::EER
    }
  })
  
  # ------------------- Carga de datos y cálculo -------------------
  data_input_col <- reactive({
    DataCol(
      Month = as.numeric(input$month),
      Year = as.numeric(input$year),
      City = ciudad_seleccionada()
    )
  })
  
  coca_result_col <- eventReactive(input$goButton_col, {
    if (input$eer == "EER nacionales") {
      CoCA(data = data_input_col(), EER = FoodpriceR::EER)$cost
    } else {
      eer_data <- eer_manual_col()
      validate(need(all(complete.cases(eer_data)), "Completa todos los campos en la tabla de EER."))
      CoCA(data = data_input_col(), EER = eer_data)$cost
    }
  })
  
  # ------------------- Tabla de resultados -------------------
  output$coca_table_col <- renderDT({
    req(input$show_table_col)
    data <- coca_result_col()
    data$cost_day <- round(data$cost_day, 1)  # reducir decimales
    datatable(data, options = list(pageLength = 10))
  })
  
  # ------------------- Botón de descarga -------------------
  output$download_col_results <- downloadHandler(
    filename = function() {
      paste0("CoCA_", ciudad_seleccionada(), "_", input$year, "_", input$month, ".csv")
    },
    content = function(file) {
      data <- coca_result_col()
      data$cost_day <- round(data$cost_day, 1)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # ------------------- KPIs visuales -------------------
  output$kpi_ui <- renderUI({
    req(coca_result_col())
    data <- coca_result_col()
    kpi_1 <- round(mean(data$cost_day), 1)
    kpi_2 <- round(min(data$cost_day), 1)
    kpi_3 <- round(max(data$cost_day), 1)
    
    fluidRow(
      column(4, h4("Costo Promedio"), h3(comma(kpi_1))),
      column(4, h4("Costo Mínimo"), h3(comma(kpi_2))),
      column(4, h4("Costo Máximo"), h3(comma(kpi_3)))
    )
  })
  
  output$titulo_resultado_coca <- renderText({
    paste("Resultados: CoCA -", ciudad_seleccionada())
  })
  
  # ------------------- Gráfico: Comparativo y pirámide -------------------
  output$plot_col <- renderPlot({
    req(coca_result_col())
    data <- coca_result_col()
    data$Sex <- recode(as.character(data$Sex), `0` = "Hombre", `1` = "Mujer")
    if (!"Age" %in% names(data)) {
      data$Age <- "Total"
    }
    data$cost_day <- round(data$cost_day, 1)
    
    # 1. Barras comparativas
    p1 <- ggplot(data, aes(x = Age, y = cost_day, fill = Sex)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
      scale_fill_manual(values = c("Hombre" = "#1f77b4", "Mujer" = "#ff7f0e")) +
      labs(title = "Costo diario estimado por grupo de edad y sexo",
           x = "Grupo de Edad", y = "Costo Diario (COP)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    # 2. Pirámide si aplica
    plot_list <- list(p1)
    if (length(unique(data$Sex)) > 1 && length(unique(data$Age)) > 1) {
      data_pyr <- data
      data_pyr$cost_plot <- ifelse(data_pyr$Sex == "Mujer", -data_pyr$cost_day, data_pyr$cost_day)
      
      p2 <- ggplot(data_pyr, aes(x = Age, y = cost_plot, fill = Sex)) +
        geom_bar(stat = "identity", width = 0.6) +
        coord_flip() +
        scale_y_continuous(labels = abs) +
        scale_fill_manual(values = c("Hombre" = "#1f77b4", "Mujer" = "#ff7f0e")) +
        labs(title = "Pirámide comparativa de costo diario",
             x = "Grupo de Edad", y = "Costo Diario (COP)") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
      
      plot_list[[2]] <- p2
    }
    
    # Mostrar uno o ambos gráficos
    gridExtra::grid.arrange(grobs = plot_list, ncol = 1)
  })
}
