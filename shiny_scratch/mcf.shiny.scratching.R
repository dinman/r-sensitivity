rm(list=ls())
R_LIBS= ("/home/R/library")

library (data.table)
library (dplyr)
library (kSamples)
library (shiny)

setwd ("~/GitHub/r-sensitivity/shiny_scratch/")

#load the anderson-darling functions
source ("get.ad.R")

#load the study design
load ("foo.vbsa.design.RDA")

#load the study results
load ("foo.data.RDA")

#shiny
ui <- fluidPage(
  titlePanel ("Local Sensitivity Analysis of a Dataset Using Anderson-Darling"),
  
  actionButton("ad", "Run AD"),
  
  p("Select data from the plot using the X min and Y min inputs. The selection will be highlighted."),
  
  numericInput (inputId = "num", label = "X min", value = 4,
                min = 3, max = 5, step = 1),
  
  numericInput (inputId = "num2", label = "Y min", value = 10,
                min = 8, max = 10, step = 0.01),
  
  plotOutput ("results"),
  
  tableOutput ("ad.results")
)

server <- function (input, output) {
  
  output$results <- renderPlot ({
    
    plot (out$vbl, out$result, xlab = "X variable", ylab = "Y variable", main = "Some Data", col = "light grey") #plot study results
    
    B <- subset (out, out$vbl > input$num & out$result > input$num2) #make some selection
    
    points (B$vbl, B$result, col = "red") #display above selection
    
    
  })
  
  # B <- eventReactive(input$ad, {
  #  subset (out, out$vbl > input$num & out$result > input$num2)
  # })
  
  
  output$ad.results <- renderTable ({
    
    B <- eventReactive(input$ad, {
      subset (out, out$vbl > input$num & out$result > input$num2)})
    
    
    #B <- subset (out, out$vbl > input$num & out$result > input$num2)#make some selection
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    
    progress$set(message = "Running Anderson-Darling Test", value = 0)
    
    
    get.ad (selection = B(), design = vbsa.design, boots = 5)
    
    
  })
}

shinyApp (ui = ui, server = server)

