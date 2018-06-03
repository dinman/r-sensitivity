require(data.table)
require(memo)
require(ggplot2)
require(GGally)
require(plotrix)
require(shiny)
require(shinyjs)

iplotAvailable <- FALSE #require(iplot)


load("../shiny_scratch/sampled_design.ca.sludge.fixedfactornew.RDA")
load("../shiny_scratch/sampled_results.ca.sludge.fixedfactornew.RDA")
vbsa.design <- filtered_design
rm(filtered_design)


variableChoices <- c(unique(vbsa.design$factor), as.character(unique(energy.wtot$type_energy)))

timeChoices <- unique(energy.wtot$time)

dataset <- merge(
             dcast(data.table(energy.wtot), run_id + time ~ type_energy, value.var="value"),
             dcast(data.table(vbsa.design), run_id        ~ factor     , value.var="value")
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
      column(3, radioButtons("iplot", label="Output", choices=list(Browser=FALSE, PlottyR=TRUE), selected=FALSE),
                selectInput("x", label=h5("X Axis"), choices=variableChoices, selected=variableChoices[length(variableChoices) - 2]),
                selectInput("y", label=h5("Y Axis"), choices=variableChoices, selected=variableChoices[length(variableChoices) - 1]),
                selectInput("z", label=h5("Z Axis"), choices=variableChoices, selected=variableChoices[length(variableChoices) - 0]),
                selectInput("t", label=h5("Year"  ), choices=timeChoices    , selected=timeChoices    [length(timeChoices    ) - 0])
                
            ),
      column(9, plotOutput(outputId="scatter",
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

    fluidRow(
      HTML("&nbsp;&nbsp;"),
      tags$b("Keyboard Shortcuts:"),
      "x = next variable for x-axis; X = previous variable for x-axis; y = next variable for y-axis; Y = previous variable for y-axis; z = next variable for z-axis; Z = previous variable for z-axis; t = next time; T = previous time."
    ),
    tags$script('
      $(document).on("keypress", function(e) {
        Shiny.onInputChange("keydata", e.which + "_" + new Date());
      });
    ')

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

  output$scatter <- renderPlot({
    label <- input$variable
    if (iplotAvailable && as.logical(input$iplot) && !plotty_connected())
      plotty_init()
    if (iplotAvailable && as.logical(input$iplot) && plotty_connected()) {
      iplot(
          dataset[, input$x],
          dataset[, input$y],
          dataset[, input$z],
          w_size=0.005,
          id=1,
          xlab=input$x,
          ylab=input$y,
          zlim=input$z
      )
      itooltips(as.character(dataset$run_id))
      plot.new()
    } else {
      projection <- dataset[time == input$t, c(input$x, input$y, input$z), with=FALSE]
      colnames(projection) <- gsub("\\.", "_", colnames(projection))
      colnames(projection) <- gsub("\\[", "_", colnames(projection))
      colnames(projection) <- gsub("]", "_"  , colnames(projection))
      colnames(projection) <- gsub(" ", "_"  , colnames(projection))
      n <- dim(projection)
      ggpairs(
        projection,
        columns=1:3,
        upper=list(continuous="density"),
        lower=list(continuous=wrap("points", alpha=max(0.05, min(1, 5000 / n[1])), size=0.1)),
      )
    }
  })
}


options(browser="echo")


args <- commandArgs(TRUE)

if (length(args) == 2)
  runApp(
    list(ui=ui, server=server),
    host=args[1], port=as.numeric(args[2])
  )
