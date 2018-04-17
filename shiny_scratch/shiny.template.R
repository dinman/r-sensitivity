rm(list=ls())
R_LIBS= ("/home/R/library")

library (shiny)

ui <- fluidPage()

server <- function (input, output) {}

shinyApp (ui = ui, server = server)
