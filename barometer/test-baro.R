library(data.table)
library(highcharter)

# =========================================================================
# 0. Example data
# =========================================================================
dt <- data.table(
  indicator = paste("Indicator", 1:20),

  # Lower and upper limits of the 95% confidence interval
  lower = c(
    -0.8, -0.5, 0.1, 0.2, -0.7,
    -0.4, 0.3, 0.5, -0.2, 0.1,
    0.2, -0.6, 0.4, 0.3, -0.1,
    0.2, -0.3, 0.1, 0.4, -0.5
  ),
  upper = c(
    0.5, 0.8, 1.0, 1.2, 0.3,
    0.6, 1.1, 1.3, 0.7, 0.9,
    1.0, 0.4, 1.2, 1.1, 0.5,
    0.8, 0.6, 0.9, 1.2, 0.3
  ),

  # Observed estimate (the diamond)
  estimate = c(
    -0.2, -0.1, 0.6, 0.7, -0.3,
    0.1, 0.8, 0.9, 0.2, 0.5,
    0.6, -0.2, 0.9, 0.8, 0.1,
    0.5, 0.1, 0.6, 0.9, -0.1
  ),

  # Statistical classification (decides the diamond colour)
  status = c(
    "Not significant", "No change", "Significant", "Significant", "Not significant",
    "Not significant", "Significant", "Significant", "Significant", "Significant",
    "Significant", "Not significant", "Significant", "Significant", "Other",
    "No change", "Not significant", "Significant", "Significant", "Other"
  ),

  group = rep(paste("Group", 1:4), each = 5)
)

# =========================================================================
# 1. Vertical position of each row (the only place where we "subtract 1")
# =========================================================================
# .I counts rows from 1 (R convention), but Highcharts places the categories
# of a category axis at 0, 1, 2, ... So we convert ONCE here and reuse y_pos
# everywhere (bars, diamonds, separators). This avoids repeating `row - 1`
# and makes an off-by-one mistake much less likely.
dt[, y_pos := .I - 1L]

# =========================================================================
# 2. Data for the grey confidence-interval bars
# =========================================================================
# An xrange point needs: x (start), x2 (end) and y (row position).
# as.numeric() makes sure the values are serialised as plain JSON numbers.
ci_data <- dt[, .(
  x  = as.numeric(lower),
  x2 = as.numeric(upper),
  y  = as.numeric(y_pos)
)]

# Fail early and loudly: bad input gives a silently empty/broken chart in JS,
# where the error is much harder to find than here in R.
stopifnot(
  !anyNA(ci_data),
  all(ci_data$x2 > ci_data$x)
)

# =========================================================================
# 3. Data for the diamonds
# =========================================================================
# We keep `status` for now because we filter on it in add_status().
# The helper below drops it again before sending data to Highcharts,
# so that no unexpected property ends up on the points.
point_data <- dt[, .(
  x = as.numeric(estimate),
  y = as.numeric(y_pos),
  status
)]

# =========================================================================
# 4. Group separator lines
# =========================================================================
# We want a line between the last row of one group and the first row of the
# next. Comparing each row with the NEXT row (type = "lead") finds the last
# row of every group but the final one:
#   - the final row is compared with NA, giving NA, and data.table treats an
#     NA in `i` as FALSE, so the bottom of the chart gets no separator.
# Whole numbers on the axis are the centres of the rows, so + 0.5 puts the
# line on the border between this row and the next.
group_positions <- dt[group != shift(group, type = "lead"), y_pos + 0.5]

group_lines <- lapply(group_positions, function(pos) {
  list(value = pos, color = "#BDBDBD", width = 1, zIndex = 4)
})

# =========================================================================
# 5. Base chart with the grey CI bars
# =========================================================================
hc <- highchart() |>
  # xrange is an add-on module, not part of core Highcharts. Without it the
  # series type is unknown and nothing is drawn.
  hc_add_dependency("modules/xrange.js") |>
  hc_add_series(
    # list_parse() keeps the column names, giving {x, x2, y} for each point.
    # list_parse2() would drop them and force Highcharts to guess the meaning.
    data = list_parse(as.data.frame(ci_data)),
    type = "xrange",
    name = "95% Confidence interval",
    colorByPoint = FALSE,   # KEY FIX: xrange defaults to a palette colour per
                            # point, which silently ignores `color` below
    color = "#C8CACA",
    borderWidth = 0,        # no outline, so the bar is a flat grey
    borderRadius = 0,       # square ends as in the target figure
    pointWidth = 10,        # bar thickness in pixels
    grouping = FALSE,       # do not dodge bars from other series
    zIndex = 1,             # draw behind the diamonds
    showInLegend = FALSE,   # the diamonds are what the legend explains
    enableMouseTracking = TRUE,
    tooltip = list(
      pointFormat = "<b>95% CI</b><br>{point.x} to {point.x2}"
    )
  )

# =========================================================================
# 6. Diamonds: one scatter series per status
# =========================================================================
# One series per status gives (a) one legend entry per status and (b) a
# separate fill and border colour for each status.
status_style <- data.table(
  status = c("Not significant", "Significant", "No change", "Other"),
  fill   = c("#FF0000",         "#00DD00",     "#FFFFFF",   "#FFFF00"),
  border = c("#990000",         "#008800",     "#888888",   "#999900")
)

add_status <- function(chart, status_name, fill, border) {
  # Keep only x and y; `status` was just needed for the filter
  selected <- point_data[status == status_name, .(x, y)]

  chart |>
    hc_add_series(
      data = list_parse(selected),
      type = "scatter",
      name = status_name,
      color = fill,
      zIndex = 5,           # above bars (1) and separator lines (4)
      marker = list(
        symbol    = "diamond",
        radius    = 6,
        fillColor = fill,
        lineColor = border, # a border keeps the white diamonds visible
        lineWidth = 1.2
      ),
      tooltip = list(
        pointFormat = "<b>{series.name}</b><br>Estimate: {point.x}"
      )
    )
}

# Add the series in the order of status_style (this is also the legend order)
for (i in seq_len(nrow(status_style))) {
  hc <- add_status(
    hc,
    status_name = status_style$status[i],
    fill        = status_style$fill[i],
    border      = status_style$border[i]
  )
}

# =========================================================================
# 7. Axes, title and styling
# =========================================================================
hc <- hc |>
  hc_xAxis(
    title = list(text = "Observed change"),
    # Padding so bars at the extremes are not clipped by the plot edge
    min = min(dt$lower) - 0.2,
    max = max(dt$upper) + 0.2,
    gridLineWidth = 0,
    lineWidth = 1,
    # Reference line at 0, set in the same call as the other xAxis options
    plotLines = list(list(
      value = 0, color = "#CC0033", width = 2.5, zIndex = 5
    ))
  ) |>
  hc_yAxis(
    # categories[0] is drawn at y = 0, categories[1] at y = 1, and so on,
    # which is why y_pos starts at 0
    categories = dt$indicator,
    reversed = TRUE,        # y = 0 at the top, like a table
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
    enabled = TRUE,
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal"
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
