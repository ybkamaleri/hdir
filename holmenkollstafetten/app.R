
library(shiny)

# Function to calculate exchange times
calculate_exchange_times <- function(start_time, distances, speeds, standard_speed) {
  # Use standard speed for relays where speed is not specified
  speeds <- ifelse(is.na(speeds), standard_speed, speeds)

  if (length(distances) != length(speeds)) {
    stop("Antall distanser m?? v??re lik antall hastighet.")
  }

  start_time <- as.POSIXct(start_time, format = "%H:%M", tz = "UTC")
  exchange_times <- c(start_time)

  for (i in seq_along(distances)) {
    time_taken <- (distances[i] / 1000) * speeds[i]
    next_time <- tail(exchange_times, 1) + as.difftime(time_taken, units = "mins")
    exchange_times <- c(exchange_times, next_time)
  }

  formatted_times <- format(exchange_times, "%H:%M")
  return(formatted_times[-(length(exchange_times))])
}

# Define UI
ui <- fluidPage(
  titlePanel("Holmenkollstafetten: Estimert vekslingstiden"),
  sidebarLayout(
    sidebarPanel(
      textInput("start_time", "Start Tid (HH:MM):", value = "15:35"),
      numericInput("standard_speed", "Gjennomsnitt hastighet (min per km):", value = 5, min = 1, step = 0.1),
      textInput("speeds", "Spesifikk hastighet per vekslinger (kommaseparert eks 4,6,5,5,4,...) Ellers, la denne tom hvis gennomsnitt hastighet skal brukes:", value = ""),
      actionButton("calculate", "Beregn")
    ),
    mainPanel(
      tableOutput("results")
    )
  )
)


# Define Server
server <- function(input, output) {

  distances <- c(1100,
                 1070,
                 595,
                 1920,
                 1210,
                 1250,
                 1770,
                 1780,
                 625,
                 2860,
                 1520,
                 350,
                 1080,
                 710,
                 535)

  results <- eventReactive(input$calculate, {
                                        # Parse speeds input
    speeds <- if (input$speeds == "") {
      rep(NA, length(distances)) # Use NA to indicate standard speed
    } else {
      as.numeric(unlist(strsplit(input$speeds, ",")))
    }

    # Ensure speeds length matches distances
    if (length(speeds) != length(distances)) {
      return("Error: The number of speeds must match the number of relays or be left blank.")
    }

    tryCatch({
      calculate_exchange_times(input$start_time, distances, speeds, input$standard_speed)
    }, error = function(e) {
      return(paste("Error:", e$message))
    })
  })

  output$results <- renderTable({
    times <- results()
    # if (is.character(times)) {
    #   return(data.frame(Message = times))
    # } else {
      data.frame(Etappe = paste0("Etappe ", seq_along(times)), Tid = times)
    # }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
