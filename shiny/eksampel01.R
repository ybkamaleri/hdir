
library(shiny)

ui = fluidPage(
  textOutput(outputId = "app_info")
)

server = function(input, output) {
  output$app_info = renderText("Kodemøte")
}

shinyApp(ui = ui, server = server)

## print(as.character(ui))
