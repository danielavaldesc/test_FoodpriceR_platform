
library(shiny)

setwd("C:\\Users\\danie\\OneDrive\\Escritorio\\test_FoodpriceR_platform\\")

source("ui.R")       
source("server.R")   

# Ejecuta la aplicación
shinyApp(ui = ui, server = server)
