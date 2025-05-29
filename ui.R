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
  title = "FoodPrice",
  id = "main_navbar",   # ayuda para referenciar si quieres luego
  
  # Forzamos CSS de navbar visible y horizontal
  header = tags$head(
    tags$style(HTML("
      /* Fondo verde y diseño horizontal */
      .navbar, .navbar-default {
        background-color: #2c582b !important;
        border: none;
        border-radius: 0;
        margin-bottom: 0;
        min-height: 56px;
      }

      /* Ítems del menú alineados horizontalmente */
      .navbar-nav {
        float: left !important;
        margin: 0;
      }

      /* Marca (título) */
      .navbar-brand {
        color: #ffffff !important;
        font-weight: bold;
        padding-top: 14px;
      }

      /* Ítems del menú */
      .navbar-default .navbar-nav > li > a {
        color: #ffffff !important;
        padding-top: 15px;
        padding-bottom: 15px;
      }

      /* Hover sobre ítems */
      .navbar-default .navbar-nav > li > a:hover {
        color: #f6bc2d !important;
      }

      /* Activo (pestaña seleccionada) */
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:focus,
      .navbar-default .navbar-nav > .active > a:hover {
        color: #bd8d1a !important;
        background-color: #2c582b !important;
      }
    "))
  ),
  
  # Tema base para botones y fuentes
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#2c582b",
    primary = "#bd8d1a",
    secondary = "#2c582b",
    success = "#8fc751",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    navbar_bg = "#2c582b",     # <-- Solo esta línea cambia el fondo
    navbar_fg = "#ffffff",     # Texto blanco
    nav_fg = "#ffffff",
    nav_bg = "#2c582b"
  ),
  
  # ---------------- INICIO ----------------
  tabPanel("Inicio",
           fluidPage(
             h2("FoodPrice – Plataforma de Análisis de Costos de Dietas Asequibles"),
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

