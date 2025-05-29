# -----------------------------------------------
# Cargamos librerías necesarias para la interfaz
# -----------------------------------------------
library(shiny)
library(bslib)
library(DT)
library(rhandsontable)
library(leaflet)
library(FoodpriceR)

# -----------------------------------------------
# Interfaz principal con menú personalizado
# -----------------------------------------------
ui <- navbarPage(
  title = "Plataforma CoCA",
  
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",         # Fondo blanco
    fg = "#2c582b",         # Texto verde institucional
    primary = "#bd8d1a",    # Botones dorados
    secondary = "#2c582b",  # Verde activo
    success = "#8fc751",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    navbar_bg = "#2c582b",  # Fondo navbar
    navbar_fg = "#ffffff",  # Texto navbar
    nav_bg = "#2c582b",     # Hover activo
    nav_fg = "#ffffff"
  ),
  
  # ---------------- INICIO ----------------
  tabPanel("Inicio",
           fluidPage(
             h2("Bienvenido a la Plataforma CoCA"),
             p("Esta plataforma permite estimar el Costo Diario de la Dieta Asequible (CoCA) en Colombia."),
             p("Podrás visualizar, descargar resultados y realizar análisis usando tus propios datos."),
             br(),
             h4("¿Qué es CoCA?"),
             p("CoCA es un indicador desarrollado por la FAO y adaptado con datos locales para evaluar cuánto costaría consumir una dieta energética adecuada diariamente."),
             h4("¿Cómo funciona?"),
             p("Selecciona una ciudad desde el mapa, un mes y un año; define el requerimiento energético promedio y obtendrás el costo estimado de la dieta más asequible.")
           )
  ),
  
  # ---------------- FOODPRICE COLOMBIA (submenu) ----------------
  navbarMenu("FoodPrice Colombia",
             
             tabPanel("CoCA",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("month", "Mes:", choices = 1:12),
                          selectInput("year", "Año:", choices = 2013:2023),
                          selectInput("eer", "EER:", choices = c("EER nacionales", "Definir EER")),
                          
                          conditionalPanel(
                            condition = "input.eer == 'Definir EER'",
                            h4("Requerimientos Energéticos Manuales"),
                            helpText("• Edad (ej. '6-11 años')"),
                            helpText("• Sexo (Hombre/Mujer)"),
                            helpText("• Energía (en kcal)"),
                            rHandsontableOutput("hot_table_col_manual_eer")
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
                          DTOutput("coca_table_col"),
                          plotOutput("plot_col")
                        )
                      )
             ),
             
             tabPanel("CoNA", h3("Próximamente: CoNA")),
             tabPanel("CoRD", h3("Próximamente: CoRD")),
             tabPanel("Asequibilidad", h3("Próximamente: Asequibilidad"))
  ),
  
  # ---------------- FOODPRICE (general) ----------------
  tabPanel("FoodPrice", h3("Sección en construcción...")),
  
  # ---------------- GLOSARIO ----------------
  navbarMenu("Glosario",
             tabPanel("¿Qué es CoCA?", 
                      fluidPage(
                        h3("¿Qué es CoCA?"),
                        p("CoCA significa Costo Diario de la Dieta Asequible. Es una medida que estima el valor mínimo requerido para adquirir una dieta que satisfaga los requerimientos energéticos promedio de una población, con base en los precios y disponibilidad local de alimentos.")
                      )
             )
  ),
  
  # ---------------- DATOS DE CONTACTO ----------------
  tabPanel("Datos de contacto",
           fluidPage(
             h3("Datos de contacto"),
             p("Para más información, sugerencias o soporte técnico, puedes escribirnos a:"),
             tags$ul(
               tags$li("📧 contacto@foodprice.co"),
               tags$li("📍 Pontificia Universidad Javeriana Cali"),
               tags$li("📞 +57 2 555 1234")
             )
           )
  )
)
