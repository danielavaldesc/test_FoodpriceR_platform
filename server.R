# ----------------------
# Cargamos librerías 
# ----------------------
library(shiny)
library(bslib)
library(shinyBS)
library(plotly)
library(leaflet)
library(DT)
library(sf)
library(dplyr)
library(rhandsontable)
library(openai)       
library(shiny)
library(readxl)
library(stringr)
library(FoodpriceR)

# ----------------------
# Datos geográficos
# ----------------------
departamentos <- st_read("shape_files/dptos_col/departamentos.shp")
municipios <- st_read("shape_files/DANE_geodata/MGN_ANM_MPIOS_WGS84.shp")

# Asociar departamento a capital principal
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

ciudad_seleccionada <- reactiveVal("Bogotá")

capitales_sf <- municipios %>%
  filter(toupper(MPIO_CNMBR) %in% toupper(dpto_a_ciudad)) %>%
  group_by(MPIO_CNMBR) %>%
  slice(1) %>%
  st_centroid()

# ----------------------
# Server
# ----------------------
server <- function(input, output, session) {
  
  ################
  #      CoCA    #  
  ################

  output$mapa_coca <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = departamentos,
        fillColor = ~ifelse(NOMBRE_DPT %in% names(dpto_a_ciudad), "#8fc751", "#e0e0e0"),
        fillOpacity = 0.5,
        weight = 1.5,
        color = "black",
        label = ~NOMBRE_DPT,
        layerId = ~NOMBRE_DPT
      ) %>%
      addLabelOnlyMarkers(
        data = capitales_sf,
        lng = ~st_coordinates(geometry)[,1],
        lat = ~st_coordinates(geometry)[,2],
        label = ~paste("●", MPIO_CNMBR),
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "top",
                                    style = list("font-weight" = "bold", "font-family" = "DM Sans", "color" = "#2c582b"))
      )
  })
  
  observeEvent(input$mapa_coca_shape_click, {
    click_id <- input$mapa_coca_shape_click$id
    ciudad <- dpto_a_ciudad[[click_id]]
    if (!is.null(ciudad)) {
      ciudad_seleccionada(ciudad)
      leafletProxy("mapa_coca") %>%
        clearGroup("selected") %>%
        addPolylines(
          data = departamentos %>% filter(NOMBRE_DPT == click_id),
          color = "#2c582b",
          weight = 4,
          opacity = 1,
          group = "selected"
        )
    } else {
      showModal(modalDialog(
        title = "Sin estimaciones disponibles",
        "No hay estimaciones de CoCA para esta ciudad.",
        easyClose = TRUE
      ))
    }
  })
  
  output$ciudad_activa_coca <- renderText({
    paste("Ciudad activa:", ciudad_seleccionada())
  })
  
  
  # CoCA cálculo
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
      eer_manual <- isolate(hot_to_r(input$hot_table_col_manual_eer))
      validate(need(all(complete.cases(eer_manual)), "Completa todos los campos en la tabla de EER."))
      eer_manual
    }
    CoCA(data = data, EER = eer_data)$cost
  })
  
  # CoCA kpi 
  
  output$kpi_ui <- renderUI({
    req(coca_result_col())
    data <- coca_result_col()
    prom <- round(mean(data$Cost_1000kcal), 4)
    minval <- round(min(data$cost_day), 0)
    maxval <- round(max(data$cost_day), 0)
    grupo_max <- paste(data$Demo_Group[which.max(data$cost_day)], ifelse(data$Sex[which.max(data$cost_day)] == 0, "Hombre", "Mujer"), sep = " - ")
    grupo_min <- paste(data$Demo_Group[which.min(data$cost_day)], ifelse(data$Sex[which.min(data$cost_day)] == 0, "Hombre", "Mujer"), sep = " - ")
    
    tagList(
      fluidRow(
        column(4, div(style = "background-color:#e6f3e6; padding:15px; border-radius:10px; border:1px solid #2c582b",
                      h4("Grupo con mayor costo"), h3(grupo_max), h5(scales::comma(maxval)), p("COP por día calculado"))),
        column(4, div(style = "background-color:#e6f3e6; padding:15px; border-radius:10px; border:1px solid #2c582b",
                      h4("Grupo con menor costo"), h3(grupo_min), h5(scales::comma(minval)), p("COP por día calculado"))),
        column(4, div(style = "background-color:#e6f3e6; padding:15px; border-radius:10px; border:1px solid #2c582b",
                      h4("Promedio general"), h3(scales::comma(round(mean(data$cost_day)))), p("COP por día (ajustado por 1000 kcal)")))
      ),
      div(style = "background-color:#f9f9f9; padding:10px; margin-top:10px; border-left:4px solid #2c582b",
          p(paste0("CoCA: Costo promedio diario por 1000 kilocalorías calculado es ", prom, " COP.")))
    )
  })
  
  # CoCA tabla 
  
  output$coca_table_col <- renderDT({
    req(input$show_table_col)
    data <- coca_result_col()
    data$cost_day <- round(data$cost_day, 2)
    data$quantity <- round(data$quantity, 2)
    data$Cost_1000kcal <- round(data$Cost_1000kcal, 2)
    data$Sexo <- ifelse(data$Sex == 0, "Hombre", "Mujer")
    data <- data[, c("Food", "quantity", "Demo_Group", "Sexo", "Group", "cost_day", "Cost_1000kcal")]
    colnames(data) <- c("Alimento", "Cantidad", "Grupo demográfico", "Sexo", "Grupo de alimentos", "Costo diario (COP)", "Costo por 1000 kcal")
    datatable(data, options = list(pageLength = 10))
  })
  
  # CoCA gráfica 
  
  output$plot_col <- renderPlotly({
    req(coca_result_col())
    data <- coca_result_col()
    data$Sexo <- recode(as.character(data$Sex), `0` = "Hombre", `1` = "Mujer")
    data <- data %>% mutate(grupo = Demo_Group)
    
    p <- ggplot(data, aes(x = grupo, y = cost_day, fill = Sexo,
                          text = paste("Grupo:", Demo_Group, "<br>Sexo:", Sexo, "<br>Costo:", round(cost_day, 2), "COP"))) +
      geom_col(position = position_dodge()) +
      scale_fill_manual(values = c("Hombre" = "#2c582b", "Mujer" = "#f6bc2d")) +
      labs(x = "Grupo Demográfico", y = "Costo Diario (COP)", fill = "Sexo",
           title = "Costo diario estimado por grupo etario y sexo") +
      theme_minimal(base_family = "DM Sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # CoCA resultados 
  
  output$download_col_results <- downloadHandler(
    filename = function() {
      paste0("CoCA_resultados_", ciudad_seleccionada(), ".csv")
    },
    content = function(file) {
      data <- coca_result_col()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  ################
  #      CoNA    #  
  ################
  
  # Mapa CoNA
  output$mapa_ciudades <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = departamentos,
        fillColor = ~ifelse(NOMBRE_DPT %in% names(dpto_a_ciudad), "#8fc751", "#e0e0e0"),
        fillOpacity = 0.5,
        weight = 1.5,
        color = "black",
        label = ~NOMBRE_DPT,
        layerId = ~NOMBRE_DPT
      ) %>%
      addLabelOnlyMarkers(
        data = capitales_sf,
        lng = ~st_coordinates(geometry)[,1],
        lat = ~st_coordinates(geometry)[,2],
        label = ~paste("●", MPIO_CNMBR),
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "top",
                                    style = list("font-weight" = "bold", "font-family" = "DM Sans", "color" = "#2c582b"))
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
    } else {
      showModal(modalDialog(
        title = "Sin estimaciones disponibles",
        "No hay estimaciones de CoNA disponibles para esta ciudad.",
        easyClose = TRUE
      ))
    }
  })
  
  output$ciudad_activa_cona <- renderText({
    paste("Ciudad activa:", ciudad_seleccionada())
  })
  
  # Entrada de datos
  data_input_cona <- reactive({
    DataCol(
      Month = as.numeric(input$month_cona),
      Year = as.numeric(input$year_cona),
      City = ciudad_seleccionada()
    )
  })
  
  eerll_cona <- reactive({
    if (input$eer_cona == "EER nacionales") {
      FoodpriceR::EER_LL
    } else {
      hot_to_r(input$hot_table_cona_manual_eerll)
    }
  })
  
  ul_cona <- reactive({
    if (input$eer_cona == "EER nacionales") {
      FoodpriceR::UL
    } else {
      hot_to_r(input$hot_table_cona_manual_ul)
    }
  })
  
  # Modelo CoNA directo
  Mod <- eventReactive(input$goButton_cona, {
    data <- isolate(data_input_cona())
    eer <- isolate(eerll_cona())
    ul <- isolate(ul_cona())
    CoNA(data = data, EER_LL = eer, UL = ul)
  })
  
  # KPI
  output$kpi_cona_ui <- renderUI({
    req(Mod()$cost)
    modelo <- Mod()
    
    costo_promedio <- round(mean(modelo$cost$Cost_1000kcal, na.rm = TRUE), 0)
    print(paste("Costo promedio diario por 1000 kilocalorías ajustado:", costo_promedio, "COP"))
    
    data <- modelo$cost
    data$Sexo <- ifelse("Sex" %in% colnames(data),
                        ifelse(data$Sex == 0, "Hombre", "Mujer"),
                        "No especificado")
    data$Grupo <- ifelse("Demo_Group" %in% colnames(data),
                         data$Demo_Group,
                         data[[1]])
    
    grupo_mas_caro <- paste(data$Grupo[which.max(data$cost_day)], "-", data$Sexo[which.max(data$cost_day)])
    grupo_mas_barato <- paste(data$Grupo[which.min(data$cost_day)], "-", data$Sexo[which.min(data$cost_day)])
    max_valor <- round(max(data$cost_day, na.rm = TRUE), 0)
    min_valor <- round(min(data$cost_day, na.rm = TRUE), 0)
    
    tagList(
      fluidRow(
        column(4, div(style = "background-color:#e6f3e6; padding:15px; border-radius:10px; border:1px solid #2c582b",
                      h4("Grupo con mayor costo"), h3(grupo_mas_caro), h5(paste0("$", format(max_valor, big.mark = "."))), p("COP por día calculado"))),
        column(4, div(style = "background-color:#e6f3e6; padding:15px; border-radius:10px; border:1px solid #2c582b",
                      h4("Grupo con menor costo"), h3(grupo_mas_barato), h5(paste0("$", format(min_valor, big.mark = "."))), p("COP por día calculado"))),
        column(4, div(style = "background-color:#e6f3e6; padding:15px; border-radius:10px; border:1px solid #2c582b",
                      h4("Promedio general"), h3(paste0("$", format(costo_promedio, big.mark = "."))), p("COP por día (calculado por 1000 kcal)")))
      ),
      div(style = "background-color:#f9f9f9; padding:10px; margin-top:10px; border-left:4px solid #2c582b",
          p("El costo promedio ajustado corresponde a una dieta adecuada en nutrientes para cada grupo demográfico."))
    )
  })

  
  
  # Tabla CoNA
  output$cona_table <- DT::renderDataTable({
    req(input$show_table_cona)
    tabla <- Mod()$cost
    
    if ("Sex" %in% colnames(tabla)) {
      tabla$Sexo <- ifelse(tabla$Sex == 0, "Hombre", "Mujer")
    } else {
      tabla$Sexo <- "No especificado"
    }
    
    if (!"Demo_Group" %in% colnames(tabla)) {
      tabla$Demo_Group <- tabla[[1]]
    }
    
    tabla <- tabla[, c("Demo_Group", "Sexo", "cost_day", "Cost_1000kcal")]
    
    colnames(tabla) <- c("Grupo demográfico", "Sexo", "Costo diario (COP)", "Costo por 1000 kcal (COP)")
    
    tabla$`Costo diario (COP)` <- round(tabla$`Costo diario (COP)`, 2)
    tabla$`Costo por 1000 kcal (COP)` <- round(tabla$`Costo por 1000 kcal (COP)`, 2)
    
    DT::datatable(tabla,
                  options = list(pageLength = 10, scrollX = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/es-ES.json')),
                  rownames = FALSE)
  })
  
  
  # Gráfico CoNA
  output$plot_cona <- renderPlotly({
    req(Mod()$cost)
    data <- Mod()$cost
    
    if ("Sex" %in% colnames(data)) {
      data$Sexo <- ifelse(data$Sex == 0, "Hombre", "Mujer")
    } else {
      data$Sexo <- "No especificado"
    }
    
    if (!"Demo_Group" %in% colnames(data)) {
      data$Demo_Group <- data[[1]]
    }
    
    p <- ggplot(data, aes(x = Demo_Group, y = cost_day, fill = Sexo,
                          text = paste("Grupo:", Demo_Group,
                                       "<br>Sexo:", Sexo,
                                       "<br>Costo:", round(cost_day, 2), "COP"))) +
      geom_col(position = position_dodge()) +
      scale_fill_manual(values = c("Hombre" = "#2c582b", "Mujer" = "#f6bc2d", "No especificado" = "#999999")) +
      labs(x = "Grupo Demográfico", y = "Costo Diario (COP)", fill = "Sexo",
           title = "Costo diario estimado por grupo etario y sexo (CoNA)") +
      theme_minimal(base_family = "DM Sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Descarga
  output$download_cona_results <- downloadHandler(
    filename = function() {
      paste0("resultados_CoNA_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(Mod()$cost, file, row.names = FALSE)
    }
  )
  
  ################
  #     CoRD     #
  ################
  
  output$mapa_cord <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = departamentos,
        fillColor = ~ifelse(NOMBRE_DPT %in% names(dpto_a_ciudad), "#8fc751", "#e0e0e0"),
        fillOpacity = 0.5,
        weight = 1.5,
        color = "black",
        label = ~NOMBRE_DPT,
        layerId = ~NOMBRE_DPT
      ) %>%
      addLabelOnlyMarkers(
        data = capitales_sf,
        lng = ~st_coordinates(geometry)[,1],
        lat = ~st_coordinates(geometry)[,2],
        label = ~paste("●", MPIO_CNMBR),
        labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE, direction = "top",
                                    style = list("font-weight" = "bold", "font-family" = "DM Sans", "color" = "#2c582b"))
      )
  })
  
  observeEvent(input$mapa_cord_shape_click, {
    click_id <- input$mapa_cord_shape_click$id
    ciudad <- dpto_a_ciudad[[click_id]]
    if (!is.null(ciudad)) {
      ciudad_seleccionada(ciudad)
      leafletProxy("mapa_cord") %>%
        clearGroup("selected") %>%
        addPolylines(
          data = departamentos %>% filter(NOMBRE_DPT == click_id),
          color = "#2c582b",
          weight = 4,
          opacity = 1,
          group = "selected"
        )
    } else {
      showModal(modalDialog(
        title = "Sin estimaciones disponibles",
        "No hay estimaciones de CoRD disponibles para esta ciudad.",
        easyClose = TRUE
      ))
    }
  })
  
  output$ciudad_activa_cord <- renderText({
    paste("Ciudad activa:", ciudad_seleccionada())
  })
  
  # Datos de entrada
  
  data_input_cord <- reactive({
    DataCol(
      Month = as.numeric(input$month_cord),
      Year = as.numeric(input$year_cord),
      City = ciudad_seleccionada()
    )
  })
  
  serv_input <- reactive({
    FoodpriceR::serv_example
  })
  
  diverse_input <- reactive({
    FoodpriceR::diverse_example
  })
  
  Mod_CoRD <- eventReactive(input$goButton_cord, {
    tryCatch({
      CoRD(
        data = isolate(data_input_cord()),
        serv = serv_input(),
        diverse = diverse_input()
      )
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en ejecución",
        paste("No se pudo calcular CoRD:", e$message),
        easyClose = TRUE
      ))
      return(NULL)
    })
  })
  
  # KPI CoRD
  
  output$kpi_cord_ui <- renderUI({
    req(Mod_CoRD())
    resultados <- Mod_CoRD()$cost
    
    resultados$Sexo <- ifelse(resultados$Sex == 0, "Hombre", "Mujer")
    resultados$Grupo <- resultados$Demo_Group
    
    costo_promedio <- round(mean(resultados$Cost_1000kcal, na.rm = TRUE), 2)
    grupo_mas_caro <- paste(resultados$Grupo[which.max(resultados$cost_day)], "-", resultados$Sexo[which.max(resultados$cost_day)])
    grupo_mas_barato <- paste(resultados$Grupo[which.min(resultados$cost_day)], "-", resultados$Sexo[which.min(resultados$cost_day)])
    max_valor <- round(max(resultados$cost_day, na.rm = TRUE), 2)
    min_valor <- round(min(resultados$cost_day, na.rm = TRUE), 2)
    
    tagList(
      fluidRow(
        column(4, div(style = "background-color:#e6f3e6; padding:15px; border-radius:10px; border:1px solid #2c582b",
                      h4("Grupo con mayor costo"), h3(grupo_mas_caro), h5(paste0("$", format(max_valor, big.mark = "."))), p("COP por día calculado"))),
        column(4, div(style = "background-color:#e6f3e6; padding:15px; border-radius:10px; border:1px solid #2c582b",
                      h4("Grupo con menor costo"), h3(grupo_mas_barato), h5(paste0("$", format(min_valor, big.mark = "."))), p("COP por día calculado"))),
        column(4, div(style = "background-color:#e6f3e6; padding:15px; border-radius:10px; border:1px solid #2c582b",
                      h4("Promedio general"), h3(paste0("$", format(costo_promedio, big.mark = "."))), p("COP por día (calculado por 1000 kcal)")))
      ),
      div(style = "background-color:#f9f9f9; padding:10px; margin-top:10px; border-left:4px solid #2c582b",
          p("El costo promedio estimado corresponde a una dieta saludable para cada grupo demográfico."))
    )
  })
  
  # Tabla CoRD
  
  output$cord_table <- DT::renderDataTable({
    req(input$show_table_cord)
    resultados <- Mod_CoRD()$cost
    
    resultados$Sexo <- ifelse(resultados$Sex == 0, "Hombre", "Mujer")
    resultados$`Grupo demográfico` <- resultados$Demo_Group
    resultados$`Costo diario (COP)` <- round(resultados$cost_day, 2)
    resultados$`Costo por 1000 kcal (COP)` <- round(resultados$Cost_1000kcal, 2)
    
    tabla <- resultados[, c("Grupo demográfico", "Sexo", "Costo diario (COP)", "Costo por 1000 kcal (COP)")]
    
    DT::datatable(
      tabla,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.13.4/i18n/es-ES.json')
      ),
      rownames = FALSE
    )
  })
  
  # Gráfico CoRD
  
  output$plot_cord <- renderPlotly({
    req(Mod_CoRD())
    resultados <- Mod_CoRD()$cost
    
    resultados$Sexo <- ifelse(resultados$Sex == 0, "Hombre", "Mujer")
    resultados$Grupo <- resultados$Demo_Group
    
    p <- ggplot(resultados, aes(x = Grupo, y = cost_day, fill = Sexo,
                                text = paste("Grupo:", Grupo,
                                             "<br>Sexo:", Sexo,
                                             "<br>Costo:", round(cost_day, 2), "COP"))) +
      geom_col(position = position_dodge()) +
      scale_fill_manual(values = c("Hombre" = "#2c582b", "Mujer" = "#f6bc2d")) +
      labs(x = "Grupo Demográfico", y = "Costo Diario (COP)", fill = "Sexo",
           title = "Costo diario estimado por grupo etario y sexo (CoRD)") +
      theme_minimal(base_family = "DM Sans") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
 # Descargar CoRD
  
  output$download_cord_results <- downloadHandler(
    filename = function() {
      paste0("resultados_CoRD_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(Mod_CoRD()$cost, file, row.names = FALSE)
    }
  )
  
  # --------------------- CoCA - Manual -----------------------------
  
  output$hot_table_coca_manual_alimentos <- renderRHandsontable({
    rhandsontable(FoodpriceR::data_example, useTypes = TRUE, stretchH = "all")
  })
  
  output$hot_table_coca_manual_eer <- renderRHandsontable({
    rhandsontable(FoodpriceR::eer_example, useTypes = TRUE, stretchH = "all")
  })
  
  user_data_coca_manual <- reactive({
    req(input$hot_table_coca_manual_alimentos)
    hot_to_r(input$hot_table_coca_manual_alimentos)
  })
  
  eer_data_coca_manual <- reactive({
    req(input$hot_table_coca_manual_eer)
    eer <- hot_to_r(input$hot_table_coca_manual_eer)
    eer$Sex <- recode(eer$Sex, `0` = "Hombre", `1` = "Mujer")
    eer
  })
  
  coca_result_manual <- eventReactive(input$runButton_coca_manual, {
    validate(
      need(all(complete.cases(user_data_coca_manual())), "Completa todos los campos en la tabla de alimentos."),
      need(all(complete.cases(eer_data_coca_manual())), "Completa todos los campos en la tabla EER.")
    )
    CoCA(data = user_data_coca_manual(), EER = eer_data_coca_manual())$cost
  })
  
  output$coca_manual_table <- renderDT({
    req(input$show_table_coca_manual)
    datatable(coca_result_manual(), options = list(pageLength = 10))
  })
  
  output$plot_coca_manual <- renderPlot({
    req(coca_result_manual())
    plot_diet_cost(data = coca_result_manual(), diet_type = "CoCA", plot_type = "bar")
  })
  
  output$download_coca_manual_results <- downloadHandler(
    filename = function() {
      paste0("CoCA_manual_result_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(coca_result_manual(), file, row.names = FALSE)
    }
)
  
  # --------------------- CoNA - Manual -----------------------------
  
  output$hot_table_cona_manual_alimentos <- renderRHandsontable({
    rhandsontable(FoodpriceR::data_example, useTypes = TRUE, stretchH = "all")
  })
  
  output$hot_table_cona_manual_eer <- renderRHandsontable({
    rhandsontable(FoodpriceR::eer_example_cona, useTypes = TRUE, stretchH = "all")
  })
  
  user_data_cona_manual <- reactive({
    req(input$hot_table_cona_manual_alimentos)
    hot_to_r(input$hot_table_cona_manual_alimentos)
  })
  
  eer_data_cona_manual <- reactive({
    req(input$hot_table_cona_manual_eer)
    eer <- hot_to_r(input$hot_table_cona_manual_eer)
    eer$Sex <- recode(eer$Sex, `0` = "Hombre", `1` = "Mujer")
    eer
  })
  
  cona_result_manual <- eventReactive(input$runButton_cona_manual, {
    validate(
      need(all(complete.cases(user_data_cona_manual())), "Completa todos los campos de alimentos."),
      need(all(complete.cases(eer_data_cona_manual())), "Completa los requerimientos nutricionales.")
    )
    CoNA(data = user_data_cona_manual(), EER_LL = eer_data_cona_manual(), UL = eer_data_cona_manual())$cost
  })
  
  output$cona_manual_table <- renderDT({
    req(input$show_table_cona_manual)
    datatable(cona_result_manual(), options = list(pageLength = 10))
  })
  
  output$plot_cona_manual <- renderPlot({
    req(cona_result_manual())
    plot_diet_cost(data = cona_result_manual(), diet_type = "CoNA", plot_type = "bar")
  })
  
  output$download_cona_manual_results <- downloadHandler(
    filename = function() {
      paste0("CoNA_manual_result_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(cona_result_manual(), file, row.names = FALSE)
    }
)
  
  # --------------------- CoRD - Manual -----------------------------
  
  output$hot_table_cord_manual_serv <- renderRHandsontable({
    rhandsontable(FoodpriceR::serv_example, useTypes = TRUE, stretchH = "all")
  })
  
  output$hot_table_cord_manual_diverse <- renderRHandsontable({
    rhandsontable(FoodpriceR::diverse_example, useTypes = TRUE, stretchH = "all")
  })
  
  user_serv_cord_manual <- reactive({
    req(input$hot_table_cord_manual_serv)
    hot_to_r(input$hot_table_cord_manual_serv)
  })
  
  user_diverse_cord_manual <- reactive({
    req(input$hot_table_cord_manual_diverse)
    hot_to_r(input$hot_table_cord_manual_diverse)
  })
  
  cord_result_manual <- eventReactive(input$runButton_cord_manual, {
    tryCatch({
      CoRD(
        data = DataCol(
          Month = as.numeric(input$month_cord_manual),
          Year = as.numeric(input$year_cord_manual),
          City = "Internacional"
        ),
        serv = user_serv_cord_manual(),
        diverse = user_diverse_cord_manual()
      )
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error en ejecución",
        paste("No se pudo calcular CoRD:", e$message),
        easyClose = TRUE
      ))
      return(NULL)
    })
  })
  
  output$cord_manual_table <- renderDT({
    req(input$show_table_cord_manual)
    datatable(cord_result_manual()$cost, options = list(pageLength = 10))
  })
  
  output$plot_cord_manual <- renderPlotly({
    req(cord_result_manual())
    resultados <- cord_result_manual()$cost
    resultados$Sexo <- ifelse(resultados$Sex == 0, "Hombre", "Mujer")
    resultados$Grupo <- resultados$Demo_Group
    
    p <- ggplot(resultados, aes(x = Grupo, y = cost_day, fill = Sexo,
                                text = paste("Grupo:", Grupo,
                                             "<br>Sexo:", Sexo,
                                             "<br>Costo:", round(cost_day, 2), "COP"))) +
      geom_col(position = position_dodge()) +
      scale_fill_manual(values = c("Hombre" = "#2c582b", "Mujer" = "#f6bc2d")) +
      labs(x = "Grupo Demográfico", y = "Costo Diario (COP)", fill = "Sexo",
           title = "Costo diario estimado por grupo etario y sexo (CoRD)") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$download_cord_manual_results <- downloadHandler(
    filename = function() {
      paste0("CoRD_manual_result_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(cord_result_manual()$cost, file, row.names = FALSE)
    }
  )
  
  # --- Plantillas de Excel para descarga ---
  
  # Plantilla CoCA
  output$plantilla_coca <- downloadHandler(
    filename = function() { "Plantilla_CoCA.xlsx" },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Alimentos")
      openxlsx::writeData(wb, "Alimentos", data.frame(
        Alimento = "Ej: Arroz",
        Precio = "Ej: 3000",
        Cantidad = "Ej: 100",
        Grupo = "Ej: Cereales"
      ))
      openxlsx::addWorksheet(wb, "Requerimientos")
      openxlsx::writeData(wb, "Requerimientos", data.frame(
        Grupo_edad = "Ej: 6-11 años",
        Sexo = "Ej: masculino",
        Energia_kcal = "Ej: 1800"
      ))
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Plantilla CoNA
  output$plantilla_cona <- downloadHandler(
    filename = function() { "Plantilla_CoNA.xlsx" },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Alimentos")
      openxlsx::writeData(wb, "Alimentos", data.frame(
        Alimento = "Ej: Lentejas",
        Precio = "Ej: 2800",
        Cantidad = "Ej: 100",
        Grupo = "Ej: Leguminosas"
      ))
      openxlsx::addWorksheet(wb, "EER_LL")
      openxlsx::writeData(wb, "EER_LL", data.frame(
        Grupo_edad = "Ej: 6-11 años",
        Sexo = "Ej: femenino",
        Energia_kcal = "Ej: 1600"
      ))
      openxlsx::addWorksheet(wb, "UL")
      openxlsx::writeData(wb, "UL", data.frame(
        Nutriente = "Ej: Sodio",
        Valor = "Ej: 2000"
      ))
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Plantilla CoRD
  output$plantilla_cord <- downloadHandler(
    filename = function() { "Plantilla_CoRD.xlsx" },
    content = function(file) {
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Alimentos")
      openxlsx::writeData(wb, "Alimentos", data.frame(
        Alimento = "Ej: Zanahoria",
        Precio = "Ej: 1500",
        Cantidad = "Ej: 100",
        Group = "Ej: Verduras"
      ))
      openxlsx::addWorksheet(wb, "Servings")
      openxlsx::writeData(wb, "Servings", data.frame(
        Subgroup = "Ej: Verduras",
        Serving = "Ej: 2",
        Age = "Ej: 6-11 años"
      ))
      openxlsx::addWorksheet(wb, "Diversidad")
      openxlsx::writeData(wb, "Diversidad", data.frame(
        Subgroup = "Ej: Verduras",
        Number = "Ej: 3"
      ))
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ---- Asistente Inteligente FoodPrice ----
  
  # Historial de conversación IA
  conversation_history <- reactiveVal(character())
  
  # -----------------------------
  # Función de interpretación básica por texto
  interpret_user_input <- function(pregunta) {
    pregunta <- tolower(pregunta)
    if (grepl("coca", pregunta)) {
      return("CoCA estima el costo mínimo diario para cubrir necesidades calóricas. Sube tu archivo con alimentos y requerimientos energéticos.")
    } else if (grepl("cona", pregunta)) {
      return("CoNA evalúa si una dieta cumple con nutrientes mínimos y máximos. Usa archivos con energía mínima (EER_LL) y límites superiores (UL).")
    } else if (grepl("cord", pregunta)) {
      return("CoRD usa porciones recomendadas y diversidad alimentaria. Carga los archivos serv y diverse.")
    } else if (grepl("cómo subir", pregunta)) {
      return("Usa los botones 'Elegir archivo' en la sección correspondiente. También puedes descargar una plantilla.")
    } else {
      return("No entendí completamente tu pregunta. Puedes preguntarme sobre CoCA, CoNA, CoRD o cómo subir los archivos.")
    }
  }
  
  # -----------------------------
  # Chatbot: Entrada y respuesta IA
  observeEvent(input$ask_ia, {
    req(input$user_question)
    user_input <- input$user_question
    history <- conversation_history()
    response <- interpret_user_input(user_input)
    new_history <- c(history, paste0("Usuario: ", user_input), paste0("FoodBot: ", response))
    conversation_history(new_history)
  })
  
  output$conversation_box <- renderUI({
    req(conversation_history())
    HTML(paste(
      lapply(conversation_history(), function(x) {
        if (startsWith(x, "Usuario:")) {
          paste0("<div style='background:#f0f0f0;padding:8px;border-radius:5px;margin-bottom:5px'><strong>", x, "</strong></div>")
        } else {
          paste0("<div style='background:#e6f3e6;padding:8px;border-radius:5px;margin-bottom:5px'>", x, "</div>")
        }
      }), collapse = "")
    )
  })
  
  # -----------------------------
  # Validación automática de archivos (estructura mínima)
  check_uploaded_file <- function(data, dieta) {
    if (dieta == "CoCA") {
      req_cols <- c("Alimento", "Precio", "Cantidad")
      if (!all(req_cols %in% names(data))) return(paste("❌ Faltan columnas: ", paste(setdiff(req_cols, names(data)), collapse=", ")))
      if (any(data$Cantidad < 50, na.rm = TRUE)) return("⚠️ Cantidades menores a 50g podrían ser insuficientes para cubrir necesidades calóricas.")
      return("✅ Archivo de CoCA cargado correctamente.")
    }
    if (dieta == "CoNA") {
      req_cols <- c("Grupo_edad", "Sexo", "EER_LL")
      if (!all(req_cols %in% names(data))) return("❌ Faltan columnas requeridas en EER_LL.")
      return("✅ Archivo de EER_LL válido.")
    }
    if (dieta == "CoRD") {
      req_cols <- c("Subgroup", "Serving")
      if (!all(req_cols %in% names(data))) return("❌ La tabla serv debe tener 'Subgroup' y 'Serving'.")
      return("✅ Tabla serv estructurada correctamente.")
    }
    return("⚠️ Dieta no reconocida para validación.")
  }
  
  # Trigger validación al subir archivo CoCA manual
  observeEvent(input$runButton_coca_manual, {
    validacion <- check_uploaded_file(user_data_coca_manual(), "CoCA")
    conversation_history(c(conversation_history(), paste0("FoodBot: ", validacion)))
  })
  
  # Trigger validación al subir archivo CoNA manual
  observeEvent(input$runButton_cona_manual, {
    validacion <- check_uploaded_file(eer_data_cona_manual(), "CoNA")
    conversation_history(c(conversation_history(), paste0("FoodBot: ", validacion)))
  })
  
  # Trigger validación al subir archivo CoRD manual
  observeEvent(input$runButton_cord_manual, {
    validacion <- check_uploaded_file(user_serv_cord_manual(), "CoRD")
    conversation_history(c(conversation_history(), paste0("FoodBot: ", validacion)))
  })
  
  # -----------------------------
  # Explicación de resultados CoCA, CoNA, CoRD
  explain_results_generic <- function(resultados, dieta) {
    if (is.null(resultados)) return("No hay resultados para analizar.")
    tryCatch({
      grupo_caro <- resultados$Demo_Group[which.max(resultados$cost_day)]
      costo_max <- round(max(resultados$cost_day, na.rm = TRUE), 0)
      return(paste0("En ", dieta, ", el grupo más costoso es ", grupo_caro, " con $", costo_max, ". Esto puede deberse a alimentos de alto precio en esa categoría."))
    }, error = function(e) {
      return("No se pudo analizar el resultado. Verifica que la tabla tenga 'Demo_Group' y 'cost_day'.")
    })
  }
  
  observeEvent(input$show_table_coca_manual, {
    msg <- explain_results_generic(coca_result_manual(), "CoCA")
    conversation_history(c(conversation_history(), paste0("FoodBot: ", msg)))
  })
  
  observeEvent(input$show_table_cona_manual, {
    msg <- explain_results_generic(cona_result_manual(), "CoNA")
    conversation_history(c(conversation_history(), paste0("FoodBot: ", msg)))
  })
  
  observeEvent(input$show_table_cord_manual, {
    msg <- explain_results_generic(cord_result_manual()$cost, "CoRD")
    conversation_history(c(conversation_history(), paste0("FoodBot: ", msg)))
  })
  
  
}
