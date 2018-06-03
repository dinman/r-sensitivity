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
print("Loading design...")
load ("sampled_design.ca.sludge.fixedfactornew.RDA")
vbsa.design <- filtered_design
names(vbsa.design)[names(vbsa.design) == "run"] <- "run_id"
names(vbsa.design)[names(vbsa.design) == "variable"] <- "factor"


#load the study results
print("Loading results...")
load ("sampled_results.ca.sludge.fixedfactornew.RDA")
results.2040 <- energy.wtot[which(energy.wtot$time == 2040), ]
total.results.2040 <- results.2040[which(results.2040$type_energy == "sludge.total energy"), ]
ordered <- total.results.2040[order(total.results.2040$value), ]
ordered$order_id <- seq.int(nrow(ordered))
out <- ordered[, c("run_id", "value", "order_id")]
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
    if (is.null (input$plot1_brush)) {
      print ("B not selected")
      return(NULL)
    }
    
    withProgress(message = "Calculation in progress \n", value = 0, {
      df <- as.data.table ( brushedPoints(out, input$plot1_brush))
      selected <- out[out$run_id %in% df$run_id, ]
      results <- get.ad (selection = df, design = vbsa.design, boots = 5, design.melted = TRUE)
      results_round <- results 
      results_round$B.med <-round(results$B.med, 3)
      results_round$B.bar.med <-round(results$B.bar.med, 3)
      print(head(results_round))
      return(results_round)
    })
  }, digits=3)
}
  

shinyApp(ui, server)

