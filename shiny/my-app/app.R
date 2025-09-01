library(shiny)

# Load modules
source("R/numericInputMod.R")
source("R/plotMod.R")

ui <- fluidPage(
  titlePanel("Shiny App med Modules"),

  sidebarLayout(
    sidebarPanel(
      numericInputModuleUI("num_input", "Skrev et tall:")
    ),
    mainPanel(
      plotModuleUI("plot_display")
    )
  )
)

server <- function(input, output, session) {
  # Call numeric input module
  number <- numericInputModuleServer("num_input")

  # Call plot module using number from numeric module
  plotModuleServer("plot_display", number)
}

shinyApp(ui, server)
