library(shiny)
library(bslib)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  titlePanel("App med tema"),
  sidebarLayout(
    sidebarPanel("Sidepanel"),
    mainPanel("Hovedinnhold")
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
