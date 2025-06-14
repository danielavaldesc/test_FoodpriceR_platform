# ----------------------
# Cargamos librerías 
# ----------------------
library(shiny)
library(bslib)
library(shinyBS)
library(plotly)
library(rhandsontable)


ui <- navbarPage(
  title = div(
    tags$img(src = "logo_FoodPriceR2.png",
             height = "120px", 
             style = "margin-right:10px;")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c582b",
    secondary = "#f6bc2d",
    base_font = font_google("DM Sans")
  ),
  id = "main_navbar",
  
  # Estilo para separar ítems del navbar
  header = tags$head(
    tags$link(rel = "stylesheet", 
              type = "text/css",
              href = "custom.css")
  ),
  
  panel_inicio,
  
  tabPanel("Metodología",
           fluidPage(
             withMathJax(),
             
             tags$head(
               tags$style(HTML(".dieta-verde { color: #2c582b; font-weight: bold; }"))
             ),
             
             h2("Metodología"),
             
             p("La plataforma FoodPrice utiliza métricas estandarizadas para estimar el costo mínimo de dietas con tres niveles crecientes de calidad nutricional. Estas métricas son:"),
             
             tags$ul(
               tags$li(tags$span(class = "dieta-verde", "CoCA"), ": Costo de una Dieta Suficiente en Energía. Estima el costo mínimo de una dieta que cumple con los requerimientos energéticos diarios (EER)."),
               tags$li(tags$span(class = "dieta-verde", "CoNA"), ": Costo de una Dieta Adecuada en Nutrientes. Además de cumplir con la energía, incluye restricciones mínimas y máximas de macronutrientes y micronutrientes esenciales."),
               tags$li(tags$span(class = "dieta-verde", "CoRD"), ": Costo de una Dieta Recomendada o Saludable. Calcula el costo mínimo para una dieta que respeta las recomendaciones de las Guías Alimentarias Basadas en Alimentos (FBDGs), diferenciadas por grupos alimentarios y con criterios de diversidad inter e intragrupal.")
             ),
             
             h3("Estimación de las métricas"),
             
             p("Las métricas ", tags$span(class = "dieta-verde", "CoCA"), " y ", tags$span(class = "dieta-verde", "CoNA"), " se estiman mediante problemas de programación lineal (MPL). En el caso de ", tags$span(class = "dieta-verde", "CoCA"), ", se minimiza el costo total de alimentos sujetos a una única restricción de energía. La formulación matemática es la siguiente:"),
             
             helpText("$$\\text{CoCA} = \\min_{x_1, ..., x_n} \\sum_{j=1}^J p_j x_j$$"),
             helpText("$$\\text{sujeto a: } \\sum_{j=1}^J e_j x_j = EER \\quad ; \\quad x_j \\geq 0$$"),
             
             p("La solución óptima selecciona el alimento con menor relación precio/energía:"),
             helpText("$$x^*_j = \\begin{cases} \\frac{EER}{e_k}, & \\text{si } \\frac{p_j}{e_j} = \\min_j \\left\\{ \\frac{p_j}{e_j} \\right\\} \\\\ 0, & \\text{en otro caso} \\end{cases}$$"),
             
             br(),
             
             p("Para ", tags$span(class = "dieta-verde", "CoNA"), ", se considera una formulación más compleja que incluye restricciones adicionales para asegurar niveles adecuados de nutrientes específicos. Se utilizan límites inferiores y superiores para cada nutriente:"),
             
             helpText("$$\\text{CoNA} = \\min_{x_1, ..., x_n} \\sum_{j=1}^J p_j x_j$$"),
             helpText("$$\\text{sujeto a: } \\sum_{j=1}^J e_j x_j = EER$$"),
             helpText("$$L_k \\leq \\sum_{j=1}^J a_{jk} x_j \\leq U_k \\quad \\text{para todo } k = 1, ..., K$$"),
             helpText("$$x_j \\geq 0$$"),
             
             br(),
             
             p("Finalmente, ", tags$span(class = "dieta-verde", "CoRD"), " se basa en estudios previos (Herforth et al., 2019; Raghunathan et al., 2021) y se construye como una dieta que, al mínimo costo, cumple con las recomendaciones alimentarias de las FBDGs, incorporando condiciones de diversidad entre y dentro de los grupos de alimentos."),
             h3("Análisis de sensibilidad"),
             
             p("El modelo de programación lineal también permite identificar nutrientes limitantes, es decir, aquellos que más restringen la solución óptima del modelo por estar cercanos a sus niveles mínimos requeridos. Un nutriente \\( p \\) se considera limitante si:"),
             
             helpText("$$\\sum_{j=1}^J a_{jp} x_j^* = L_p$$"),
             
             p("Aunque hay correlación, los nutrientes limitantes no implican necesariamente una deficiencia nutricional en la población (Masters et al., 2018; Dizon & Herforth, 2018; Headey & Alderman, 2019)."),
             
             br(),
             
             p("Además, se calcula la elasticidad de precio sombra \\( \\eta_k \\) para cada nutriente en CoNA, la cual mide el cambio porcentual en el costo mínimo óptimo de la dieta (CoNA*) ante un incremento del 1% en el requerimiento del nutriente \\( k \\):"),
             
             helpText("$$\\eta_k = \\frac{\\Delta \\% \\text{CoNA}^*}{\\Delta DRI_k}$$"),
             helpText("$$DRI_k \\in [L_k, U_k]$$"),
             
             p("Esto permite evaluar la sensibilidad del modelo frente a variaciones en las recomendaciones nutricionales.")
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
                          actionButton("goButton_col", "Estimar CoCA"),
                          checkboxInput("show_table_col", "Ver tabla de resultados", value = FALSE),
                          downloadButton("download_col_results", "Descargar resultados (.csv)")
                        ),
                        mainPanel(
                          textOutput("ciudad_activa_coca"),
                          leafletOutput("mapa_coca", height = "400px"),
                          br(),
                          h4(textOutput("titulo_resultado_coca")),
                          uiOutput("kpi_ui"),
                          plotlyOutput("plot_col"),
                          DT::dataTableOutput("coca_table_col")
                        )
                      )
             ),
             
             tabPanel("CoNA",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("month_cona", "Seleccionar Mes:", choices = 1:12),
                          selectInput("year_cona", "Seleccionar Año:", choices = 2013:2023),
                          selectInput("eer_cona", "Seleccionar requerimientos:", choices = c("EER nacionales", "Definir manualmente")),
                          conditionalPanel(
                            condition = "input.eer_cona == 'Definir manualmente'",
                            h5("Definir requerimientos mínimos (EER_LL):"),
                            rHandsontableOutput("hot_table_cona_manual_eerll"),
                            br(),
                            h5("Definir requerimientos máximos (UL):"),
                            rHandsontableOutput("hot_table_cona_manual_ul"),
                            br()
                          ),
                          actionButton("goButton_cona", "Estimar CoNA"),
                          checkboxInput("show_table_cona", "Ver tabla de resultados", value = FALSE),
                          downloadButton("download_cona_results", "Descargar resultados (.csv)")
                        ),
                        mainPanel(
                          h4(textOutput("ciudad_activa_cona")),
                          leafletOutput("mapa_ciudades", height = "400px"),
                          br(),
                          uiOutput("kpi_cona_ui"),
                          DT::dataTableOutput("cona_table"),
                          plotlyOutput("plot_cona")
                        )
                      )
             ),
             
             tabPanel("CoRD",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("month_cord", "Seleccionar Mes:", choices = 1:12),
                          selectInput("year_cord", "Seleccionar Año:", choices = 2013:2023),
                          actionButton("goButton_cord", "Estimar CoRD"),
                          checkboxInput("show_table_cord", "Ver tabla de resultados", value = FALSE),
                          downloadButton("download_cord_results", "Descargar resultados (.csv)"),
                          br(),
                        ),
                        mainPanel(
                          h4(textOutput("ciudad_activa_cord")),
                          leafletOutput("mapa_cord", height = "400px"),
                          br(),
                          uiOutput("kpi_cord_ui"),
                          plotlyOutput("plot_cord"),
                          conditionalPanel(
                            condition = "input.show_table_cord == true",
                            DT::dataTableOutput("cord_table")
                          )
                        )
                      )
             )
             
          ),
  
  tabPanel("FoodPrice",
           tabsetPanel(
             
             # CoCA
             tabPanel("CoCA - Manual",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Paso a paso para cargar datos"),
                          helpText("1. Descarga la plantilla de Excel."),
                          downloadButton("plantilla_coca", "Descargar Plantilla CoCA"),
                          br(), br(),
                          fileInput("archivo_coca", "2. Sube tu archivo de alimentos (.xlsx):", accept = ".xlsx"),
                          fileInput("eer_coca", "3. Sube tu archivo de requerimientos energéticos (.xlsx):", accept = ".xlsx"),
                          actionButton("runButton_coca_manual", "Ejecutar CoCA"),
                          checkboxInput("show_table_coca_manual", "Ver resultados", value = TRUE)
                        ),
                        mainPanel(
                          h4("Resultados CoCA - Manual"),
                          DTOutput("tabla_coca_manual"),
                          plotOutput("grafico_coca_manual")
                        )
                      )
             ),
             
             # CoNA
             tabPanel("CoNA - Manual",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Paso a paso para cargar datos"),
                          helpText("1. Descarga la plantilla de Excel."),
                          downloadButton("plantilla_cona", "Descargar Plantilla CoNA"),
                          br(), br(),
                          fileInput("archivo_cona", "2. Sube tu archivo de alimentos (.xlsx):", accept = ".xlsx"),
                          fileInput("eerll_cona", "3. Sube el archivo de EER Inferior (.xlsx):", accept = ".xlsx"),
                          fileInput("ul_cona", "4. Sube el archivo de UL (.xlsx):", accept = ".xlsx"),
                          actionButton("runButton_cona_manual", "Ejecutar CoNA"),
                          checkboxInput("show_table_cona_manual", "Ver resultados", value = TRUE)
                        ),
                        mainPanel(
                          h4("Resultados CoNA - Manual"),
                          DTOutput("tabla_cona_manual"),
                          plotOutput("grafico_cona_manual")
                        )
                      )
             ),
             
             # CoRD
             tabPanel("CoRD - Manual",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Paso a paso para cargar datos"),
                          helpText("1. Descarga la plantilla de Excel."),
                          downloadButton("plantilla_cord", "Descargar Plantilla CoRD"),
                          br(), br(),
                          fileInput("archivo_cord", "2. Sube tu archivo de alimentos (.xlsx):", accept = ".xlsx"),
                          fileInput("serv_cord", "3. Sube el archivo de porciones (serv).xlsx:", accept = ".xlsx"),
                          fileInput("diverse_cord", "4. Sube el archivo de diversidad de alimentos (.xlsx):", accept = ".xlsx"),
                          actionButton("runButton_cord_manual", "Ejecutar CoRD"),
                          checkboxInput("show_table_cord_manual", "Ver resultados", value = TRUE)
                        ),
                        mainPanel(
                          h4("Resultados CoRD - Manual"),
                          DTOutput("tabla_cord_manual"),
                          plotOutput("grafico_cord_manual")
                        )
                      )
             ),
             
             # Asistente IA
             tabPanel("Asistente IA",
                      sidebarLayout(
                        sidebarPanel(
                          textInput("user_question", "Escribe una pregunta para el asistente:"),
                          actionButton("ask_ia", "Preguntar a FoodBot")
                        ),
                        mainPanel(
                          h4("Conversación con FoodBot"),
                          uiOutput("conversation_box")
                        )
                      )
             )
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