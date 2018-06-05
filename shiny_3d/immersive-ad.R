require(data.table)
require(ggplot2)
require(GGally)
require(kSamples)
require(shiny)
require(shinyjs)


source("get.ad.R")


iplotAvailable <- require(iplot)


load("../shiny_scratch/sampled_design.rotus.sludge.ratereturn.31may.RDA")
load("../shiny_scratch/sampled_results.rotus.sludge.ratereturn.31may.RDA")
vbsa.design <- filtered_design
rm(filtered_design)


variableChoices <- c(unique(vbsa.design$factor), as.character(unique(energy.wtot$type_energy)))

timeChoices <- unique(energy.wtot$time)

dataset <- merge(
             dcast(data.table(energy.wtot), run_id + time ~ type_energy, value.var="value"),
             dcast(data.table(vbsa.design), run_id        ~ factor     , value.var="value.factor")
           )


shiftSelection <- function(session, inputId, value, values, delta) {
    i <- match(value, values)
    j <- max(1, i + delta)
    if (i != j)
      updateRadioButtons(session, inputId, selected=values[[j]])
}


ui <- {

  fluidPage(

    useShinyjs(),

    titlePanel("Local Sensitivity Analysis of a Dataset Using Anderson-Darling"),

    fluidRow(
      column(3, column(12, column(6, radioButtons("iplot", label="Output", choices=list(Browser=FALSE, PlottyR=TRUE), selected=FALSE)),
                           column(6, actionButton("test", label="Run Test"))
                      ),
                selectInput("x", label=h5("X Axis"), choices=variableChoices, selected=variableChoices[length(variableChoices) - 2]),
                selectInput("y", label=h5("Y Axis"), choices=variableChoices, selected=variableChoices[length(variableChoices) - 1]),
                selectInput("z", label=h5("Z Axis"), choices=variableChoices, selected=variableChoices[length(variableChoices) - 0]),
                selectInput("t", label=h5("Year"  ), choices=timeChoices    , selected=timeChoices    [length(timeChoices    ) - 0])
                
            ),
      column(9, plotOutput(outputId="scatter"))
    ),

    fluidRow(
      HTML("&nbsp;&nbsp;"),
      tags$b("Keyboard Shortcuts:"),
      "x = next variable for x-axis; X = previous variable for x-axis; y = next variable for y-axis; Y = previous variable for y-axis; z = next variable for z-axis; Z = previous variable for z-axis; t = next time; T = previous time."
    ),
    tags$script('
      $(document).on("keypress", function(e) {
        Shiny.onInputChange("keydata", e.which + "_" + new Date());
      });
    '),

    fluidRow (
      column(width = 6,
        h4("Anderson-Darling"),
        tableOutput("ad.results") 
      )
    )

  )

}


server <- function(input, output, session) {

  useShinyjs()

  if (!iplotAvailable)
    disable("iplot")

  observeEvent(input$keydata, {
    k <- sub("_.*$", "", input$keydata)
    if (k == "120")
      shiftSelection(session, "x"  , input$x, variableChoices,  1)
    else if (k == "88")
      shiftSelection(session, "x"  , input$x, variableChoices, -1)
    else if (k == "121")
      shiftSelection(session, "y"  , input$y, variableChoices,  1)
    else if (k == "89")
      shiftSelection(session, "y"  , input$y, variableChoices, -1)
    else if (k == "122")
      shiftSelection(session, "z"  , input$z, variableChoices,  1)
    else if (k == "90")
      shiftSelection(session, "z"  , input$z, variableChoices, -1)
    else if (k == "116")
      shiftSelection(session, "t"  , input$t, timeChoices    ,  1)
    else if (k == "84")
      shiftSelection(session, "t"  , input$t, timeChoices    , -1)
  })

  project <- reactive({
    dataset[time == input$t, c("run_id", input$x, input$y, input$z), with=FALSE]
  })

  values <- reactiveValues(ids=data.table(run_id=numeric()), last=list(x="", y="", z="", t=""))

  output$scatter <- renderPlot({
    if (iplotAvailable && as.logical(input$iplot) && !plotty_connected())
      plotty_init()
    projection <- project()
    if (iplotAvailable && as.logical(input$iplot) && plotty_connected()) {
      if (input$x != values$last$x || input$y != values$last$y || input$z != values$last$z || input$t != values$last$t) {
        iplot(
            projection[[input$x]],
            projection[[input$y]],
            projection[[input$z]],
            id=1,
            xlab=input$x,
            ylab=input$y,
            zlab=input$z,
            col="orange",
            wx_lim=c(-1.25, 1.25),
            wy_lim=c( 0.25, 1.75),
            wz_lim=c( 0.00,-1.50),
            w_size=0.004
        )
        itooltips(as.character(projection$run_id), id=1)
        values$last <- list(x=input$x, y=input$y, z=input$z, t=input$t)
      }
    }
    if (FALSE)
      # TODO: Should there be an option to turn off the plots in the browser?
      plot.new()
    else {
      colnames(projection) <- gsub("\\.", "_", colnames(projection))
      colnames(projection) <- gsub("\\[", "_", colnames(projection))
      colnames(projection) <- gsub("]"  , "_", colnames(projection))
      colnames(projection) <- gsub(" "  , "_", colnames(projection))
      n <- dim(projection)
      cs <- rep("black", dim(projection)[1])
      if (!is.null(values$ids))
        cs[projection$run_id %in% values$ids$run_id] <- "orange"
      ggpairs(
        projection,
        columns=2:4,
        upper=list(continuous="density"),
        lower=list(continuous=wrap("points", alpha=max(0.05, min(1, 5000 / n[1])), size=0.1, color=cs)),
      )
    }
    # TODO: Consider adding a plotly option.
  })

  observeEvent(input$test, {
    projection <- isolate(project())
    if (iplotAvailable && as.logical(input$iplot) && plotty_connected()) {
      values$ids <- projection[iselected(1), "run_id", with=FALSE]
    } else
      # FIXME: Add brushing to the plots in the browser.
      values$ids <- data.table(run_id=sample(unique(vbsa.design$run_id), 1000))
  })

  output$ad.results <- renderTable({
    if (is.null(values$ids) || dim(values$ids)[1] < 10)
      return(NULL)
    withProgress(message = "Calculation in progress \n", value = 0, {
      results <- get.ad (selection=values$ids, design=vbsa.design, boots=5, design.melted=TRUE)
      results_round <- results 
      results_round$B.med <-round(results$B.med, 3)
      results_round$B.bar.med <-round(results$B.bar.med, 3)
      return(results_round)
    })
  }, digits=3)

}


options(browser="echo")


args <- commandArgs(TRUE)

if (length(args) == 2)
  runApp(
    list(ui=ui, server=server),
    host=args[1], port=as.numeric(args[2])
  )
