library(data.table)
library(highcharter)

# Create example dataset
dt <- data.table(

  indicator = paste("Indicator", 1:20),

  # Lower confidence interval
  lower = c(
    -0.8, -0.5, 0.1, 0.2, -0.7,
    -0.4, 0.3, 0.5, -0.2, 0.1,
    0.2, -0.6, 0.4, 0.3, -0.1,
    0.2, -0.3, 0.1, 0.4, -0.5
  ),

  # Upper confidence interval
  upper = c(
    0.5, 0.8, 1.0, 1.2, 0.3,
    0.6, 1.1, 1.3, 0.7, 0.9,
    1.0, 0.4, 1.2, 1.1, 0.5,
    0.8, 0.6, 0.9, 1.2, 0.3
  ),

  # Observed estimate
  estimate = c(
    -0.2, -0.1, 0.6, 0.7, -0.3,
    0.1, 0.8, 0.9, 0.2, 0.5,
    0.6, -0.2, 0.9, 0.8, 0.1,
    0.5, 0.1, 0.6, 0.9, -0.1
  ),

  # Statistical classification
  status = c(
    "Not significant",
    "No change",
    "Significant",
    "Significant",
    "Not significant",
    "Not significant",
    "Significant",
    "Significant",
    "Significant",
    "Significant",
    "Significant",
    "Not significant",
    "Significant",
    "Significant",
    "Other",
    "No change",
    "Not significant",
    "Significant",
    "Significant",
    "Other"
  ),

  # Group classification
  group = c(
    rep("Group 1", 5),
    rep("Group 2", 5),
    rep("Group 3", 5),
    rep("Group 4", 5)
  )
)

# Add row number
dt[, row := .I]

# Colors
my_colours <- c(
  "Not significant" = "#FF0000",
  "Significant" = "#00DD00",
  "No change" = "#FFFFFF",
  "Other" = "#FFFF00"
)

library(data.table)
library(highcharter)

# ---- 1. Data for the grey CI bars -------------------------------------
# xrange needs x (start), x2 (end) and y (row position, 0-based).
ci_data <- dt[, .(
  x  = as.numeric(lower),
  x2 = as.numeric(upper),
  y  = as.numeric(row - 1)
)]

stopifnot(
  !anyNA(ci_data),
  all(ci_data$x2 > ci_data$x)
)

# ---- 2. Data for the diamonds -----------------------------------------
# Keep only x and y. An extra "status" column would be sent to
# Highcharts as an unexpected point property.
point_data <- dt[, .(
  x      = estimate,
  y      = row - 1,
  status = status
)]

# ---- 3. Group separator positions -------------------------------------
# Compare each row with the next one: TRUE where the group changes.
# i is the 1-based index of the LAST row of a group. Rows are 0-based on
# the axis, so the line sits half a step after that row: i - 1 + 0.5.
# This replaces shift() + [-1], which dropped a real boundary.
last_in_group   <- which(dt$group[-1] != dt$group[-nrow(dt)])
group_positions <- last_in_group - 0.5

group_lines <- lapply(group_positions, function(pos) {
  list(value = pos, color = "#BDBDBD", width = 1, zIndex = 4)
})

# ---- 4. Base chart + CI bars ------------------------------------------
hc <- highchart() |>
  # Loads xrange.js, which ships with highcharter. Without it the series
  # type is unknown and nothing is drawn.
  hc_add_dependency("modules/xrange.js") |>
  hc_add_series(
    # list_parse() keeps names -> {x, x2, y}; list_parse2() would drop them
    data = list_parse(as.data.frame(ci_data)),
    type = "xrange",
    name = "95% Confidence interval",
    colorByPoint = FALSE,     # KEY FIX: otherwise "color" below is ignored
    color = "#C8CACA",
    borderWidth = 0,          # no outline, so the bar stays a flat grey
    borderRadius = 0,         # square ends like in your target figure
    pointWidth = 10,          # bar thickness in pixels
    grouping = FALSE,
    zIndex = 1,               # draw behind the diamonds
    showInLegend = FALSE,
    enableMouseTracking = TRUE,
    # Series-specific tooltip: show the interval, not just x
    tooltip = list(
      pointFormat = "<b>95% CI</b><br>{point.x} to {point.x2}"
    )
  )

# ---- 5. Diamond markers, one series per status ------------------
# One series per status gives a legend entry per status and lets each
# diamond have its own fill and border colour.
add_status <- function(chart, status_name, colour, border_colour = colour) {
  selected <- point_data[status == status_name, .(x, y)]

  chart |>
    hc_add_series(
      data = list_parse(selected),
      type = "scatter",
      name = status_name,
      color = colour,
      zIndex = 5,             # above bars (1) and separators (4)
      marker = list(
        symbol    = "diamond",
        radius    = 6,
        fillColor = colour,
        lineColor = border_colour,
        lineWidth = 1.2
      ),
      tooltip = list(pointFormat = "<b>{series.name}</b><br>Estimate: {point.x}")
    )
}

hc <- hc |>
  add_status("Not significant", "#FF0000", "#990000") |>
  add_status("Significant",     "#00DD00", "#008800") |>
  add_status("No change",       "#FFFFFF", "#888888") |>
  add_status("Other",           "#FFFF00", "#999900")

# ---- 6. Axes, title, styling ------------------------------------------
hc <- hc |>
  hc_xAxis(
    title = list(text = "Observed change"),
    min = min(dt$lower) - 0.2,        # padding so end bars aren't cut off
    max = max(dt$upper) + 0.2,
    gridLineWidth = 0,
    lineWidth = 1,
    # Reference line at 0, in the same call as the other xAxis options
    plotLines = list(list(
      value = 0, color = "#CC0033", width = 2.5, zIndex = 5
    ))
  ) |>
  hc_yAxis(
    categories = dt$indicator,
    reversed = TRUE,                  # row 0 at the top, like a table
    title = list(text = NULL),
    labels = list(enabled = TRUE, style = list(fontSize = "11px")),
    gridLineWidth = 0,
    lineWidth = 0,
    tickLength = 0,
    plotLines = group_lines
  ) |>
  hc_title(
    text = "Folkehelsebarometer for Oslo",
    style = list(fontSize = "16px", fontWeight = "bold")
  ) |>
  hc_chart(
    backgroundColor = "#FFFFFF",
    height = 750,
    spacing = c(10, 10, 10, 10)
  ) |>
  hc_legend(
    enabled = TRUE, align = "center",
    verticalAlign = "bottom", layout = "horizontal"
  ) |>
  hc_credits(enabled = FALSE)

hc




# ------------------------------------------------------------------------------
# Check modules exist
# ------------------------------------------------------------------------------
xrange_file <- system.file(
  "htmlwidgets/lib/highcharts/modules/xrange.js",
  package = "highcharter"
)

print(xrange_file)

file.exists(xrange_file)
