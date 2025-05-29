# -----------------------------------------------
# Cargamos librerías necesarias para la lógica
# -----------------------------------------------
library(shiny)
library(FoodpriceR)
library(rhandsontable)
library(DT)
library(leaflet)
library(sf)
library(dplyr)
library(scales)
library(ggplot2)
library(stringr)

# -----------------------------------------------
# 1. Cargar shapefiles y definir capitales
# -----------------------------------------------
departamentos <- st_read("shape_files/dptos_col/departamentos.shp")
municipios <- st_read("shape_files/DANE_geodata/MGN_ANM_MPIOS_WGS84.shp")

capitales <- c(
  "Bogotá", "Medellín", "Cali", "Barranquilla", "Cartagena", "Cúcuta",
  "Bucaramanga", "Pereira", "Ibagué", "Pasto", "Manizales", "Neiva",
  "Villavicencio", "Valledupar", "Armenia", "Sincelejo", "Popayán",
  "Palmira", "Buenaventura", "Tuluá", "Cartago", "Tunja", "Ipiales", "Montería"
)

ciudades_disponibles <- unique(capitales)

dpto_a_ciudad <- c(
  "ANTIOQUIA" = "Medellín", "ATLANTICO" = "Barranquilla", "BOGOTÁ" = "Bogotá",
  "BOLÍVAR" = "Cartagena", "BOYACÁ" = "Tunja", "CALDAS" = "Manizales",
  "CAUCA" = "Popayán", "CESAR" = "Valledupar", "CÓRDOBA" = "Montería",
  "CUNDINAMARCA" = "Bogotá", "HUILA" = "Neiva", "LA GUAJIRA" = "Riohacha",
  "MAGDALENA" = "Santa Marta", "META" = "Villavicencio", "NARIÑO" = "Pasto",
  "NORTE DE SANTANDER" = "Cúcuta", "QUINDIO" = "Armenia", "RISARALDA" = "Pereira",
  "SANTANDER" = "Bucaramanga", "SUCRE" = "Sincelejo", "TOLIMA" = "Ibagué",
  "VALLE DEL CAUCA" = "Cali", "ARAUCA" = "Arauca", "CASANARE" = "Yopal",
  "PUTUMAYO" = "Mocoa", "AMAZONAS" = "Leticia"
)

colores_personalizados <- c(
  "Hombre" = "#2c582b",
  "Mujer"  = "#f6bc2d"
)

# -----------------------------------------------
# 2. Servidor principal
# -----------------------------------------------
server <- function(input, output, session) {
  
  ciudad_seleccionada <- reactiveVal("Bogotá")
  
  # ---------- Mapa con resaltado y diferenciación ----------
  output$mapa_ciudades <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = departamentos,
        fillColor = ~ifelse(NOMBRE_DPT %in% names(dpto_a_ciudad), "#8fc751", "#e0e0e0"),
        fillOpacity = 0.5,
        weight = 1,
        color = "#444444",
        label = ~NOMBRE_DPT,
        layerId = ~NOMBRE_DPT
      )
  })
  
  observeEvent(input$mapa_ciudades_shape_click, {
    click_id <- input$mapa_ciudades_shape_click$id
    ciudad <- dpto_a_ciudad[[click_id]]
    if (!is.null(ciudad)) {
      ciudad_seleccionada(ciudad)
      
      leafletProxy("mapa_ciudades") %>%
        clearGroup("selected") %>%
        addPolylines(
          data = departamentos %>% filter(NOMBRE_DPT == click_id),
          color = "#2c582b",
          weight = 4,
          opacity = 1,
          group = "selected"
        )
    }
  })
  
  output$ciudad_activa <- renderText({
    paste("Ciudad activa:", ciudad_seleccionada())
  })
  
  # ---------- Tabla editable de EER ----------
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
  
  # ---------- Datos y cálculo ----------
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
  
  # ---------- Tabla de resultados ----------
  output$coca_table_col <- renderDT({
    req(input$show_table_col)
    data <- coca_result_col()
    num_cols <- sapply(data, is.numeric)
    data[num_cols] <- lapply(data[num_cols], function(x) round(x, 2))
    
    datatable(
      data,
      options = list(pageLength = 10, dom = 'tip'),
      rownames = FALSE,
      class = 'stripe hover cell-border order-column',
      style = "bootstrap5"
    )
  })
  
  output$download_col_results <- downloadHandler(
    filename = function() {
      paste0("CoCA_", ciudad_seleccionada(), "_", input$year, "_", input$month, ".csv")
    },
    content = function(file) {
      data <- coca_result_col()
      num_cols <- sapply(data, is.numeric)
      data[num_cols] <- lapply(data[num_cols], function(x) round(x, 2))
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # ---------- KPI dinámicos ----------
  output$kpi_ui <- renderUI({
    req(coca_result_col())
    data <- coca_result_col()
    
    mayor <- data[which.max(data$cost_day), ]
    menor <- data[which.min(data$cost_day), ]
    promedio <- round(mean(data$cost_day), 1)
    
    fluidRow(
      column(4,
             h4("Grupo con mayor costo"),
             tags$b(paste0(mayor$Age, " - ", mayor$Sex)),
             h3(comma(round(mayor$cost_day, 1)))
      ),
      column(4,
             h4("Grupo con menor costo"),
             tags$b(paste0(menor$Age, " - ", menor$Sex)),
             h3(comma(round(menor$cost_day, 1)))
      ),
      column(4,
             h4("Promedio general"),
             h3(comma(promedio))
      )
    )
  })
  
  output$titulo_resultado_coca <- renderText({
    paste("Resultados: CoCA -", ciudad_seleccionada())
  })
  
  # ---------- Gráfico diferenciado ----------
  output$plot_col <- renderPlot({
    req(coca_result_col())
    data <- coca_result_col()
    data$Sex <- recode(as.character(data$Sex), `0` = "Hombre", `1` = "Mujer")
    if (!"Age" %in% names(data)) data$Age <- "Total"
    data$cost_day <- round(data$cost_day, 1)
    data$Grupo <- paste(data$Age, "-", data$Sex)
    data$Grupo <- factor(data$Grupo, levels = unique(data$Grupo))
    
    ggplot(data, aes(x = Grupo, y = cost_day, fill = Sex)) +
      geom_bar(stat = "identity", width = 0.7) +
      scale_fill_manual(values = colores_personalizados) +
      labs(
        title = "Costo diario estimado por grupo etario y sexo",
        x = "Grupo Demográfico", y = "Costo Diario (COP)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
}


