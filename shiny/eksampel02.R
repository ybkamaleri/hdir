
library(shiny)

ui <- fluidPage(
  titlePanel("Hei Shiny!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Antall observasjoner:", min = 10, max = 1000, value = 500)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
  })

  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)
