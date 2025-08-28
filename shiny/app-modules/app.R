library(shiny)

# Load modules
source("modules/numericInputMod.R")
source("modules/plotMod.R")

ui <- fluidPage(
  titlePanel("Shiny App with Modules"),

  sidebarLayout(
    sidebarPanel(
      numericInputModuleUI("num_input", "Enter a number:")
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
