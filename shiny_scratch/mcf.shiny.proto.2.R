rm(list=ls())
R_LIBS= ("/home/R/library")

library (data.table)
library (dplyr)
library (kSamples)
library (ggplot2)
library (shiny)

setwd ("~/GitHub/r-sensitivity/shiny_scratch/")

#load the anderson-darling functions
source ("get.ad.R")

#load the study design
load ("foo.vbsa.design.RDA")

#load the study results
load ("foo.data.RDA")
setnames (out, c("run_id", "result", "vbl"))

#shiny

ui <- fluidPage(
  titlePanel ("Local Sensitivity Analysis of a Dataset Using Anderson-Darling"),
  fluidRow(
    column(width = 4,
           plotOutput("plot1", height = 400, width = 600,
                      brush = brushOpts(
                        id = "plot1_brush",
                        fill = "pink",
                        stroke = "black",
                        clip = TRUE,
                        delay = 100
                      )
           )
    )
  ),
  
    fluidRow (
       column(width = 6,
          h4("Anderson-Darling"),
          tableOutput("ad.results") 
    )
  ),

  column(width = 6,
         h4("Selected points"),
         verbatimTextOutput("brush_info"),
         
         tags$style(type="text/css",
                    ".shiny-output-error { visibility: hidden; }",
                    ".shiny-output-error:before { visibility: hidden; }"
         )

    )
  )

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(out, aes(vbl, result)) + geom_point(color = "light blue")
  })
  
  output$brush_info <- renderPrint({
    brushedPoints(out, input$plot1_brush)
  })  
  
  output$ad.results <- renderTable ({
    
    df <- as.data.table ( brushedPoints(out, input$plot1_brush))
    
    get.ad (selection = df, design = vbsa.design, boots = 5)
      
    
  })
}
  

shinyApp(ui, server)

