# Cargamos las librerías necesarias
library(shiny)
library(shinythemes)         # Para estilos visuales
library(DT)                  # Para mostrar tablas interactivas
library(rhandsontable)       # Para tablas editables
library(leaflet)             # Para el mapa interactivo
library(FoodpriceR)          # ✅ Carga funciones y datos como EER y CoCA

# Precargamos los valores nacionales de EER desde el paquete
initial_eer2 <- FoodpriceR::EER

# Definimos la interfaz de la app con un menú superior
ui <- navbarPage(
  title = "Plataforma CoCA",                # Título visible en la parte superior
  theme = shinytheme("flatly"),             # Estilo profesional y limpio
  
  # ---------------- PESTAÑA 1: INICIO ----------------
  tabPanel("Inicio",
           fluidPage(
             h2("Bienvenido a la Plataforma CoCA"),
             p("Esta plataforma permite estimar el Costo Diario de la Dieta Asequible (CoCA) en Colombia."),
             p("Podrás visualizar, descargar resultados y realizar análisis usando tus propios datos."),
             
             br(),
             h4("¿Qué es CoCA?"),
             p("CoCA es un indicador desarrollado por la FAO y adaptado con datos locales para evaluar cuánto costaría consumir una dieta energética adecuada diariamente."),
             
             h4("¿Cómo funciona?"),
             p("Selecciona una ciudad, un mes y un año; define el requerimiento energético promedio y obtendrás el costo estimado de la dieta más asequible.")
           )
  ),
  
  # ---------------- PESTAÑA 2: VISUALIZAR COCA ----------------
  tabPanel("Visualizar CoCA",
           sidebarLayout(
             sidebarPanel(
               helpText("Selecciona los parámetros para estimar el Costo Diario de la Dieta Asequible (CoCA)."),
               
               selectInput("month", "Mes:", choices = 1:12),                 # Mes a consultar
               selectInput("year", "Año:", choices = 2013:2023),             # Año a consultar
               
               selectInput("eer", "EER:", choices = c("EER nacionales", "Definir EER")),  # Selector tipo EER
               
               # Si el usuario elige "Definir EER", aparece esta tabla editable
               conditionalPanel(
                 condition = "input.eer == 'Definir EER'",
                 h4("Requerimientos Energéticos Manuales"),
                 helpText("• Edad (ej. '6-11 años')"),
                 helpText("• Sexo (Hombre/Mujer)"),
                 helpText("• Energía (en kcal)"),
                 rHandsontableOutput("hot_table_col_manual_eer")
               ),
               
               actionButton("goButton_col", "Estimar"),                      # Ejecutar cálculo
               checkboxInput("show_table_col", "Ver tabla de resultados", value = FALSE),
               downloadButton("download_col_results", "Descargar resultados (.csv)")
             ),
             
             mainPanel(
               textOutput("ciudad_activa"),                                  # Texto: ciudad seleccionada
               leafletOutput("mapa_ciudades", height = "400px"),             # Mapa interactivo
               br(),
               
               h4(textOutput("titulo_resultado_coca")),                      # Título dinámico
               uiOutput("kpi_ui"),                                           # Indicadores clave
               DTOutput("coca_table_col"),                                   # Tabla de resultados
               plotOutput("plot_col")                                        # Gráfico de barras
             )
           )
  )
)
