library(data.table)

path <- "c:/Users/ykama/OneDrive - Helsedirektoratet/dataset/barometer"
fcsv <- "InndataBarometer_kommune2026.csv"

DT <- data.table::fread(file.path(path, fcsv))
data.table::setnames(DT, tolower(names(DT)))

DT[stedskode_string != 1103, .ROW := NULL]

library(data.table)
library(highcharter)

# Assumes `DT` is already loaded: one row per indicator, for ONE place
# (here stedskode 301).

# =========================================================================
# 0. Settings
# =========================================================================
# Reference value: the red vertical line. Taken from the data, not typed in.
ref_value <- unique(DT$ref)
stopifnot("`ref` must have exactly one value" = length(ref_value) == 1)

# x-axis range. A log axis cannot show 0 or negative values, so min > 0.
# The data is capped at 200 and its lowest value is 50. On a log scale
# 50 to 200 is symmetric around 100 (100 / 2 and 100 * 2).
x_min <- 50
x_max <- 200
stopifnot(x_min > 0, x_max > x_min)

# =========================================================================
# 1. Row order, groups and vertical position
# =========================================================================
# Sort explicitly so that y_pos follows lpnr, rather than relying on the
# order the rows happen to arrive in.
setorder(DT, lpnr)

# Non-overlapping blocks of rows. Your original DT[1:10], DT[10:15], ... had
# overlapping ranges (rows 10, 15 and 20 were each assigned twice, and the
# later assignment won). Here the sizes are 10 + 5 + 5 + 14 = 34 rows.
DT[, group := rep(c("grp1", "grp2", "grp3", "grp4"), times = c(10, 5, 5, 14))]

# 0-based row position for Highcharts (R counts from 1, Highcharts from 0).
# No `keyby`: .I is the row number in the WHOLE table even inside a group,
# so grouping adds nothing here and only re-sorts the table.
DT[, y_pos := .I - 1L]

# =========================================================================
# 2.1 Grey confidence-interval / range bars
# =========================================================================
# An xrange point needs x (start), x2 (end) and y (row position).
ci_grey <- DT[, .(
  x  = as.numeric(soylemin),
  x2 = as.numeric(soylemax),
  y  = as.numeric(y_pos)
)]

stopifnot(
  !anyNA(ci_grey),
  all(ci_grey$x > 0)           # log axis needs strictly positive values
)

# A bar with x == x2 has zero width and is invisible. Warn instead of
# stopping, so the symbols are still drawn while you investigate the data.
n_flat <- sum(ci_grey$x2 <= ci_grey$x)
if (n_flat > 0) {
  warning(sprintf(
    "%d of %d bars have soylemax <= soylemin and will not be visible.",
    n_flat, nrow(ci_grey)
  ))
}

# =========================================================================
# 2.2 White 10 best-interval / range bars
# =========================================================================
# An xrange point needs x (start), x2 (end) and y (row position).
ci_white <- DT[, .(
  x  = as.numeric(beste10pct_min),
  x2 = as.numeric(beste10stk_max),
  y  = as.numeric(y_pos)
)]

# Ignore missing values when checking positivity
stopifnot(
  all(ci_white$x > 0, na.rm = TRUE)
)

# A bar with x == x2 has zero width and is invisible. Warn instead of
# stopping, so the symbols are still drawn while you investigate the data.
white_flat <- sum(ci_white$x2 <= ci_white$x, na.rm = TRUE)
if (white_flat > 0) {
  warning(sprintf(
    "%d of %d bars have soylemax <= soylemin and will not be visible.",
    white_flat, nrow(ci_white)
  ))
}

# =========================================================================
# 3. Symbols: from wide to long format
# =========================================================================
# Each indicator has its value in exactly ONE of the colour columns
# (roede, groenne, gule, hvite, hvitmprikk).
# `column` links a data column to its legend text and colours.
status_style <- data.table(
  column = c("roede", "groenne", "gule", "hvite", "hvitmprikk"),
  # Use for legende text
  status = c(
    "Signifikant dårlig",
    "Signifikant bedre",
    "Ikke signifikant fra landet",
    "Ikke testet",
    "Signifikant forskjell fra landet"
  ),
  fill = c("#FF0000","#00DD00","#FFD700","#FFFFFF","#00BFFF"),
  border = c("#CC0000","#009900","#CC9900","#808080","#0077AA"),
  symbol = c("circle", "circle", "diamond", "triangle-down", "square")
)


#   "#CC79A7", "#00BFFF", "#005555", "#0000CD"
#   "#993366", "#0077AA", "#003333", "#000080"
# status_style

# na.rm = TRUE drops all the NA cells, so every indicator keeps one row.
# figurverdi_graa is NOT included: it holds a value on every row (the same
# value as the coloured column), so it would add a duplicate point
# on top of each symbol.
symbols <- melt(
  DT,
  id.vars       = c("lpnr", "y_pos"),
  measure.vars  = na.omit(status_style$column),
  variable.name = "column",
  value.name    = "x",
  na.rm         = TRUE
)
symbols[, column := as.character(column)]
setnames(symbols, "y_pos", "y")

stopifnot(
  "each indicator should have exactly one symbol" = nrow(symbols) == nrow(DT),
  all(symbols$x > 0)
)

# =========================================================================
# 4. Group separator lines (horizontal, on the y axis)
# =========================================================================
# Compare each row with the NEXT row: TRUE on the last row of every group
# (the final row compares with NA and is dropped). + 0.5 moves the line from
# the centre of that row to the border with the next row.
group_positions <- DT[group != shift(group, type = "lead"), y_pos + 0.5]

group_lines <- lapply(group_positions, function(pos) {
  list(value = pos, color = "#BDBDBD", width = 1, zIndex = 3)
})

# =========================================================================
# 5.1 Base chart with the grey bars
# =========================================================================
hc <- highchart() |>
  # xrange is an add-on module; without it the series type is unknown
  hc_add_dependency("modules/xrange.js") |>
  hc_add_series(
    data = list_parse(as.data.frame(ci_grey)),  # keeps {x, x2, y} names
    type = "xrange",
    name = "Range",
    colorByPoint = FALSE,   # otherwise xrange ignores `color` below
    color = "#C8CACA",
    borderWidth = 0,
    borderRadius = 0,
    pointWidth = 12,
    grouping = FALSE,
    zIndex = 1,             # behind the symbols
    showInLegend = FALSE,
    enableMouseTracking = TRUE,
    tooltip = list(
      pointFormat = "<b>Range</b><br>{point.x:.1f} to {point.x2:.1f}"
    )
  )


# =========================================================================
# 5.2 Base chart with the white bars
# =========================================================================
hc <- hc |>
  hc_add_series(
    data = list_parse(as.data.frame(ci_white)),  # keeps {x, x2, y} names
    type = "xrange",
    name = "Range",
    colorByPoint = FALSE,   # otherwise xrange ignores `color` below
    color = "#80A6D6",
    borderWidth = 0,
    borderRadius = 0,
    pointWidth = 4,
    grouping = FALSE,
    zIndex = 1,             # behind the symbols
    showInLegend = FALSE,
    enableMouseTracking = TRUE,
    tooltip = list(
      pointFormat = "<b>Range</b><br>{point.x:.1f} to {point.x2:.1f}"
    )
  )

# -------------------------------------------------------------------------
# Grå prikk
# -------------------------------------------------------------------------
dt_gra <- data.frame(x = DT$figurverdi_graa, y = DT$y_pos)

hc <- hc |>
    hc_add_series(
      data = list_parse(dt_gra),
      type = "scatter",
      name = "Fylke verdi",
      color = "#808080",
      zIndex = 3,           # above bars (1) and red line (3)
      marker = list(
        symbol    = "diamond",
        radius    = 5,
        fillColor = "#808080",
        lineColor = "#000000",
        lineWidth = 1.2
      ),
      tooltip = list(
        pointFormat = "<b>{series.name}</b><br>Value: {point.x:.1f}"
      )
    )

# =========================================================================
# 6. Symbols: one scatter series per status
# =========================================================================
# One series per status gives one legend entry per status and its own
# fill/border colour.
add_status <- function(chart, col_name, status_name, fill, border, symbol) {
  selected <- symbols[column == col_name, .(x, y)]

  # Nothing to draw: skip, so no empty legend entry appears
  if (nrow(selected) == 0) return(chart)

  chart <- chart |>
    hc_add_series(
      data = list_parse(selected),
      type = "scatter",
      name = status_name,
      color = fill,
      zIndex = 4,           # above bars (1) and red line (3)
      marker = list(
        symbol    = symbol,
        radius    = 5,
        fillColor = fill,
        lineColor = border,
        lineWidth = 1.2
      ),
      tooltip = list(
        pointFormat = "<b>{series.name}</b><br>Value: {point.x:.1f}"
      )
    )

  chart
}

for (i in seq_len(nrow(status_style))) {
  hc <- add_status(
    hc,
    col_name    = status_style$column[i],
    status_name = status_style$status[i],
    fill        = status_style$fill[i],
    border      = status_style$border[i],
    symbol      = status_style$symbol[i]
  )
}

# =========================================================================
# 7. Axes, title and styling
# =========================================================================
# Row labels: replace with real indicator names if you have them
DT[, indikator_navn := paste("Indikator ", lpnr, sep = " ")]
y_labels <- as.character(DT$indikator_navn)

hc <- hc |>
  hc_xAxis(
    # Log axis: the DATA STAYS in original units (50, 100, 200). Highcharts
    # does the log10 scaling itself, and min, max, tick positions and
    # plot lines are all given in original units too.
    type = "logarithmic",
    min = x_min,
    max = x_max,
    # Keep the axis exactly at min/max instead of rounding outwards to the
    # next "nice" tick, which on a log axis would be 10 and 1000
    startOnTick = FALSE,
    endOnTick = FALSE,
    tickPositions = c(50, 70, 100, 140, 200),  # readable labels, even spacing
    title = list(text = "Norge"),
    gridLineWidth = 0,
    lineWidth = 1,
    plotLines = list(list(
      value = ref_value,    # 100, in original units
      color = "#CC0033",
      width = 2.5,
      zIndex = 6            # above the bars, below the symbols
    ))
  ) |>
  hc_yAxis(
    categories = y_labels,  # category i is drawn at y = i - 1, i.e. y_pos
    reversed = TRUE,        # y_pos 0 at the top, like a table
    title = list(text = NULL),
    labels = list(enabled = TRUE, style = list(fontSize = "11px")),
    gridLineWidth = 0,
    lineWidth = 0,
    tickLength = 0,
    plotLines = group_lines
  ) |>
  hc_title(
    text = "Folkehelsebarometer",
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
