# app.R
# Este archivo carga y ejecuta la aplicación principal de CoCA

# Requiere tener en el mismo directorio:
# - ui.R
# - server.R

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\test_FoodpriceR_platform\\")

# Carga shiny y ejecuta la app uniendo UI y servidor
library(shiny)

# Llama a los archivos de interfaz y lógica del servidor
source("ui.R")       # Archivo de la interfaz gráfica
source("server.R")   # Archivo con la lógica reactiva

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
