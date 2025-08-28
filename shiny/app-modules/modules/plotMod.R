# UI part of plot module
plotModuleUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("hist"))
}

# Server part of plot module
plotModuleServer <- function(id, number) {
  moduleServer(id, function(input, output, session) {

    # Render histogram based on number
    output$hist <- renderPlot({
      x <- rnorm(1000, mean = number(), sd = 5)
      hist(x,
           main = paste("Histogram with mean =", number()),
           col = "skyblue", border = "white")
    })
  })
}
