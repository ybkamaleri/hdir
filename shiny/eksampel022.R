
library(shiny)
library(bslib)

data <- data.frame(col1 = 1:3,
                   col2 = 4:6)

ui = page_fluid(
  textOutput(outputId = "app_txt"),
  br(),
  tags$b("Fet tekst av UI objekt"),
  br(),
  tableOutput(outputId = "table_in")
)

server = function(input, output, session) {
  output$app_txt <- renderText("Behandlet i server!")
  output$table_in <- renderTable(data)

  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)
