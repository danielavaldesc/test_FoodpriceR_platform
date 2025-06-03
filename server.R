# ----------------------
# Cargamos librerías 
# ----------------------
library(shiny)
library(bslib)
library(shinyBS)
library(plotly)
library(DT)
library(FoodpriceR)
library(readxl)
library(rhandsontable)
library(ggplot2)
library(dplyr)
library(sf)
library(leaflet)

# Datos geográficos y reactivo de ciudad
shapefile_path <- "shape_files/dptos_col/departamentos.shp"
departamentos <- st_read(shapefile_path)
municipios <- st_read("shape_files/DANE_geodata/MGN_ANM_MPIOS_WGS84.shp")

capitales <- c("Bogotá", "Medellín", "Cali", "Barranquilla", "Cartagena", "Cúcuta",
               "Bucaramanga", "Pereira", "Ibagué", "Pasto", "Manizales", "Neiva",
               "Villavicencio", "Valledupar", "Armenia", "Sincelejo", "Popayán",
               "Palmira", "Buenaventura", "Tuluá", "Cartago", "Tunja", "Ipiales", "Montería")

capitales_sf <- municipios %>%
  filter(toupper(MPIO_CNMBR) %in% toupper(capitales)) %>%
  st_centroid()

ciudad_seleccionada <- reactiveVal("Bogotá")

dpto_a_ciudad <- c(
  "ANTIOQUIA" = "Medellín", "ATLANTICO" = "Barranquilla", "BOGOTÁ" = "Bogotá",
  "BOLÍVAR" = "Cartagena", "BOYACÁ" = "Tunja", "CALDAS" = "Manizales",
  "CAQUETÁ" = "Florencia", "CAUCA" = "Popayán", "CESAR" = "Valledupar",
  "CÓRDOBA" = "Montería", "CUNDINAMARCA" = "Bogotá", "HUILA" = "Neiva",
  "MAGDALENA" = "Santa Marta", "META" = "Villavicencio", "NARIÑO" = "Pasto",
  "NORTE DE SANTANDER" = "Cúcuta", "QUINDIO" = "Armenia", "RISARALDA" = "Pereira",
  "SANTANDER" = "Bucaramanga", "SUCRE" = "Sincelejo", "TOLIMA" = "Ibagué",
  "VALLE DEL CAUCA" = "Cali"
)

server <- function(input, output, session) {
  
  output$mapa_ciudades <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = departamentos,
        fillColor = ~ifelse(NOMBRE_DPT %in% names(dpto_a_ciudad), "#8fc751", "#e0e0e0"),
        fillOpacity = 0.5,
        weight = 1.5,
        color = "#2c582b",
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
  
  data_input_col <- reactive({
    DataCol(
      Month = as.numeric(input$month),
      Year = as.numeric(input$year),
      City = ciudad_seleccionada()
    )
  })
  
  coca_result_col <- eventReactive(input$goButton_col, {
    data <- isolate(data_input_col())
    eer_data <- if (input$eer == "EER nacionales") {
      FoodpriceR::EER
    } else {
      eer_manual <- isolate(eer_manual_col())
      validate(need(all(complete.cases(eer_manual)), "Completa todos los campos en la tabla de EER."))
      eer_manual
    }
    CoCA(data = data, EER = eer_data)$cost
  })
  
  output$titulo_resultado_coca <- renderText({
    paste("Resultados: CoCA -", ciudad_seleccionada())
  })
  
  output$kpi_ui <- renderUI({
    req(coca_result_col())
    data <- coca_result_col()
    prom <- round(mean(data$cost_day), 0)
    minval <- round(min(data$cost_day), 0)
    maxval <- round(max(data$cost_day), 0)
    grupo_max <- data$Demo_Group[which.max(data$cost_day)]
    grupo_min <- data$Demo_Group[which.min(data$cost_day)]
    
    fluidRow(
      column(4, h4("Grupo con mayor costo"), h3(grupo_max), h5(scales::comma(maxval)), p("COP por día")),
      column(4, h4("Grupo con menor costo"), h3(grupo_min), h5(scales::comma(minval)), p("COP por día")),
      column(4, h4("Promedio general"), h3(scales::comma(prom)), p("COP por día (estimado por 1000 kcal)"))
    )
  })
  
  output$coca_table_col <- renderDT({
    req(input$show_table_col)
    data <- coca_result_col()
    data$cost_day <- round(data$cost_day, 2)
    data$quantity <- round(data$quantity, 2)
    datatable(data, options = list(pageLength = 10))
  })
  
  output$plot_col <- renderPlotly({
    req(coca_result_col())
    data <- coca_result_col()
    data$Sex <- recode(as.character(data$Sex), `0` = "Hombre", `1` = "Mujer")
    data <- data %>% mutate(grupo = paste(Demo_Group, Sex, sep = " - "))
    
    p <- ggplot(data, aes(x = grupo, y = cost_day, fill = Sex,
                          text = paste("Grupo:", grupo, "<br>Costo:", round(cost_day, 1), "COP"))) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Hombre" = "#2c582b", "Mujer" = "#f6bc2d")) +
      labs(x = "Grupo Demográfico", y = "Costo Diario (COP)",
           title = "Costo diario estimado por grupo etario y sexo") +
      theme_minimal(base_family = "DM Sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$download_col_results <- downloadHandler(
    filename = function() {
      paste0("CoCA_resultados_", ciudad_seleccionada(), ".csv")
    },
    content = function(file) {
      data <- coca_result_col()
      write.csv(data, file, row.names = FALSE)
    }
  )
} 


