# UI part of numeric input module
numericInputModuleUI <- function(id, label) {
  ns <- NS(id)
  tagList(
    numericInput(ns("num"), label, value = 10, min = 1, max = 100),
    textOutput(ns("sum_text"))
  )
}

# Server part of numeric input module
numericInputModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive value for the number
    number <- reactive({
      input$num
    })

    # Display sum of 1 to number
    output$sum_text <- renderText({
      paste("Sum from 1 to", number(), "=", sum(1:number()))
    })

    return(number)
  })
}
