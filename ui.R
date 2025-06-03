# ----------------------
# Cargamos librerías 
# ----------------------
library(shiny)
library(bslib)
library(shinyBS)
library(plotly)

ui <- navbarPage(
  title = div(
    tags$img(src = "logo_FoodPriceR.png", height = "40px", style = "margin-right:10px;"),
    strong("FoodPrice")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c582b",
    base_font = font_google("DM Sans")
  ),
  id = "main_navbar",
  
  tabPanel("Inicio",
           fluidPage(
             fluidRow(
               column(12,
                      h1("Bienvenido a la Plataforma FoodPrice"),
                      p("Aquí puedes estimar el Costo Diario de una Dieta Asequible (CoCA) usando distintas fuentes de datos. Usa el menú para navegar por las diferentes secciones."),
                      br(),
                      tags$img(src = "logo_FoodPriceR.png", height = "150px"),
                      br(),
                      h3("¿Qué puedes hacer en esta plataforma?"),
                      tags$ul(
                        tags$li("Visualizar datos por ciudad a través del mapa"),
                        tags$li("Calcular CoCA por archivo, manualmente o usando bases nacionales"),
                        tags$li("Consultar definiciones clave en el glosario"),
                        tags$li("Descargar resultados en formato CSV")
                      )
               )
             )
           )
  ),
  
  navbarMenu("FoodPrice Colombia",
             tabPanel("CoCA",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("month", "Seleccionar Mes:", choices = 1:12),
                          selectInput("year", "Seleccionar Año:", choices = 2013:2023),
                          selectInput("eer", "Seleccionar EER:", choices = c("EER nacionales", "Definir EER")),
                          conditionalPanel(
                            condition = "input.eer == 'Definir EER'",
                            h4("Definir requerimientos energéticos"),
                            rHandsontableOutput("hot_table_col_manual_eer"),
                            br()
                          ),
                          actionButton("goButton_col", "Estimar"),
                          checkboxInput("show_table_col", "Ver tabla de resultados", value = FALSE),
                          downloadButton("download_col_results", "Descargar resultados (.csv)")
                        ),
                        mainPanel(
                          textOutput("ciudad_activa"),
                          leafletOutput("mapa_ciudades", height = "400px"),
                          br(),
                          h4(textOutput("titulo_resultado_coca")),
                          uiOutput("kpi_ui"),
                          plotlyOutput("plot_col"),
                          DT::dataTableOutput("coca_table_col")
                        )
                      )
             ),
             tabPanel("CoNA",
                      fluidPage(
                        h4("(Próximamente implementación del cálculo de CoNA)")
                      )
             ),
             tabPanel("CoRD",
                      fluidPage(
                        h4("(Próximamente implementación del cálculo de CoRD)")
                      )
             )
  ),
  
  tabPanel("FoodPrice",
           fluidPage(
             h3("(Próximamente más información sobre el paquete y sus funciones)")
           )
  ),
  
  tabPanel("Glosario",
           fluidPage(
             h3("Glosario CoCA"),
             tags$ul(
               tags$li(strong("CoCA"), ": Costo Diario de la Dieta Asequible calculado con base en precios de alimentos locales y requerimientos energéticos."),
               tags$li(strong("EER"), ": Estimated Energy Requirements. Requerimiento energético promedio diario según edad y sexo."),
               tags$li(strong("Grupo demográfico"), ": Subdivisión por edad y sexo usada para estimar el costo de la dieta."),
               tags$li(strong("COP"), ": Pesos colombianos."),
               tags$li(strong("1000 kcal"), ": Estimación del costo con base en mil kilocalorías de energía diaria.")
             )
           )
  ),
  
  tabPanel("Datos de contacto",
           fluidPage(
             h3("Contacto"),
             p("Si tienes dudas o deseas más información, puedes escribir a: foodprice@javeriana.edu.co")
           )
  )
)
