library(shiny)

ui <- fluidPage(
  textInput("navn", "Skriv inn navnet ditt:", ""),
  textOutput("hilsen")
)

server <- function(input, output) {
  output$hilsen <- renderText({
    paste("Hei,", input$navn)
  })
}

shinyApp(ui = ui, server = server)
