library(data.table)
library(highcharter)
library(data.table)

path <- "c:/Users/ykama/OneDrive - Helsedirektoratet/dataset/barometer"
fcsv <- "InndataBarometer_kommune2026.csv"

DT <- data.table::fread(file.path(path, fcsv))
data.table::setnames(DT, tolower(names(DT)))

DT[stedskode_string != 1103, .ROW := NULL]

# =========================================================================
# 0. Settings
# =========================================================================
ref_value <- unique(DT$ref)                      # red reference line (100)
stopifnot("`ref` must have exactly one value" = length(ref_value) == 1)

x_min <- 50                                      # log axis: must be > 0
x_max <- 200
stopifnot(x_min > 0, x_max > x_min)

# Layout. The chart is split horizontally: the left `table_frac` of the plot
# width holds the table text, the rest holds the log10 chart.
table_frac <- 0.50
row_px     <- 24                                 # height of one row in pixels
min_bar_px <- 3                                  # width of zero-width bars; 0 = off

# =========================================================================
# 1. Row order, groups and vertical position
# =========================================================================
setorder(DT, lpnr)

# Non-overlapping blocks: 10 + 5 + 5 + 14 = 34 rows
DT[, group := rep(c("grp1", "grp2", "grp3", "grp4"), times = c(10, 5, 5, 14))]

# 0-based row position (R counts from 1, Highcharts from 0). Used for the
# bars, the symbols AND the table text, so everything lines up by construction.
DT[, y_pos := .I - 1L]
n_rows <- nrow(DT)

# =========================================================================
# 2. Grey range bars
# =========================================================================
bar_min_col <- "soylemin"                        # change here if the range
bar_max_col <- "soylemax"                        # comes from other columns

ci_data <- data.table(
  x  = as.numeric(DT[[bar_min_col]]),
  x2 = as.numeric(DT[[bar_max_col]]),
  y  = as.numeric(DT$y_pos)
)

# Show the widths straight away: a width of 0 means an invisible bar
cat("Bar widths (x2 - x):\n")
print(summary(ci_data$x2 - ci_data$x))

# Keep only usable bars instead of stopping the whole script. Missing bounds
# or bounds <= 0 (impossible on a log axis) simply get no bar.
usable <- ci_data[, !is.na(x) & !is.na(x2) & x > 0 & x2 > 0]
if (any(!usable)) {
  message(sum(!usable), " row(s) without a usable range were dropped from the bars.")
}
ci_data <- ci_data[usable]

n_flat <- sum(ci_data$x2 <= ci_data$x)
if (n_flat > 0) {
  warning(sprintf(
    "%d of %d bars have %s <= %s: zero width, so they are only visible as thin stubs (min_bar_px).",
    n_flat, nrow(ci_data), bar_max_col, bar_min_col
  ))
}

# =========================================================================
# 3. Symbols (diamonds)
# =========================================================================
# `column` links a data column to a legend text and colours. Rows with
# column = NA have no data column in this dataset and are skipped.
# `z` is the drawing order: figurverdi_graa (4) is drawn UNDER the coloured
# symbols (5) so it never hides them; the dot (6) is drawn on top.
status_style <- data.table(
  column = c("roede", "groenne", "gule", "hvite", "hvitmprikk",
             "figurverdi_graa", NA, NA, NA, NA),
  status = c(
    "Signifikant dårlig", "Signifikant bedre", "Ikke signifikant",
    "Ikke testet", "Hvit med prikk", "Grå (figurverdi)",
    "Signifikant forskjell", "Fylke ikke testet",
    "Variasjonen mellom kommunene", "De ti beste kommune"
  ),
  fill = c("#FF0000", "#00DD00", "#FFD700", "#FFFFFF", "#FFFFFF", "#A6A6A6",
           "#CC79A7", "#00BFFF", "#005555", "#0000CD"),
  border = c("#CC0000", "#009900", "#CC9900", "#808080", "#808080", "#000000",
             "#993366", "#0077AA", "#003333", "#000080"),
  dot = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
  z   = c(5, 5, 5, 5, 5, 4, 5, 5, 5, 5)
)

symbol_cols  <- status_style$column[!is.na(status_style$column)]
colored_cols <- setdiff(symbol_cols, "figurverdi_graa")

# Each indicator may have at most ONE coloured value (figurverdi_graa is
# separate: it can be filled on every row).
stopifnot(
  "an indicator has more than one coloured value" =
    all(rowSums(!is.na(DT[, ..colored_cols])) <= 1)
)

symbols <- melt(
  DT,
  id.vars       = c("lpnr", "y_pos"),
  measure.vars  = symbol_cols,
  variable.name = "column",
  value.name    = "x",
  na.rm         = TRUE        # drop NA cells; the other values are kept
)
symbols[, column := as.character(column)]
setnames(symbols, "y_pos", "y")
stopifnot(all(symbols$x > 0))   # log axis

# =========================================================================
# 4. Table content (the text left of the chart)
# =========================================================================
# EDIT: one row per indicator, in the same order as DT. Replace the
# placeholders with your real columns (names, kommune/fylke/norge values,
# units, theme names).
tbl <- DT[, .(
  tema      = group,
  nr        = lpnr,
  indikator = paste("Indikator", indikator_kodet),
  kommune   = formatC(figurverdi_graa, format = "f", digits = 1, decimal.mark = ","),
  fylke     = "–",
  norge     = "–",
  enhet     = "prosent"
)]

# Column layout. x_rel is where the text is anchored, as a fraction (0-1) of
# the TABLE width. `align` says how the text sits on that anchor point.
table_cols <- data.table(
  field  = c("nr", "indikator", "kommune", "fylke", "norge", "enhet"),
  header = c("#", "Indikator", "Kommune", "Fylke", "Norge", "Enhet"),
  x_rel  = c(0.065, 0.095, 0.62, 0.71, 0.80, 0.86),
  align  = c("center", "left", "center", "center", "center", "left")
)

# The theme (Tema) column is special: one rotated label per group, centred
# vertically on the group's rows.
tema_x_rel <- 0.02
tema_pos   <- DT[, .(y = mean(y_pos)), by = group]

# =========================================================================
# 5. Rows: zebra stripes, header band, group separators (y axis)
# =========================================================================
# Plot bands/lines on the y axis span the WHOLE plot width, i.e. table and
# chart together, like the rows of a real table.
header_y <- -1                                   # the header is row -1

header_band <- list(list(from = header_y - 0.5, to = header_y + 0.5,
                         color = "#8BA9B5", zIndex = 0))

zebra <- lapply(DT[y_pos %% 2 == 1, y_pos], function(y) {
  list(from = y - 0.5, to = y + 0.5, color = "#F2F2F2", zIndex = 0)
})

# Border between the last row of each group and the next (see earlier notes)
group_positions <- DT[group != shift(group, type = "lead"), y_pos + 0.5]
group_lines <- lapply(group_positions, function(pos) {
  list(value = pos, color = "#BDBDBD", width = 1, zIndex = 4)
})

# =========================================================================
# 6. Helper: draw text at (x, y) positions as data labels
# =========================================================================
# A table cell is an invisible scatter point whose DATA LABEL is the text.
# xAxis = 1 puts these series on the hidden 0-1 "text axis" (see section 9),
# yAxis stays the shared y axis, so every cell sits exactly on its row.
add_text <- function(chart, values, x, y, align = "left",
                     bold = FALSE, color = "#333333", rotation = 0) {
  values <- as.character(values)
  values[is.na(values)] <- ""

  chart |>
    hc_add_series(
      data = list_parse(data.frame(x = x, y = y, name = values)),
      type = "scatter",
      xAxis = 1,                    # the text axis, not the chart axis
      showInLegend = FALSE,
      enableMouseTracking = FALSE,
      marker = list(enabled = FALSE),
      zIndex = 2,
      dataLabels = list(
        enabled = TRUE,
        format = "{point.name}",    # show the text stored in `name`
        align = align,              # left / center / right of the anchor x
        verticalAlign = "middle",   # centred on the row
        x = 0, y = 0, padding = 0,
        rotation = rotation,
        allowOverlap = TRUE,        # never hide cells that touch
        crop = FALSE, overflow = "allow",
        defer = FALSE,              # no fade-in animation
        style = list(
          fontSize = "11px",
          fontWeight = if (bold) "bold" else "normal",
          color = color,
          textOutline = "none"      # remove Highcharts' white text halo
        )
      )
    )
}

# =========================================================================
# 7. Base chart with the grey bars
# =========================================================================
hc <- highchart() |>
  hc_add_dependency("modules/xrange.js") |>
  hc_add_series(
    data = list_parse(as.data.frame(ci_data)),
    type = "xrange",
    name = "Variasjonen mellom kommunene",
    colorByPoint = FALSE,
    color = "#C8CACA",
    borderWidth = 0,
    borderRadius = 0,
    pointWidth = 12,
    minPointLength = min_bar_px,    # zero-width bars become thin stubs
    grouping = FALSE,
    zIndex = 1,
    showInLegend = TRUE,
    enableMouseTracking = TRUE,
    tooltip = list(
      pointFormat = "<b>Range</b><br>{point.x:.1f} to {point.x2:.1f}"
    )
  )

# =========================================================================
# 8. Symbols and table text
# =========================================================================
add_status <- function(chart, col_name, status_name, fill, border, dot, z) {
  selected <- symbols[column == col_name, .(x, y)]
  if (nrow(selected) == 0) return(chart)     # nothing to draw: no legend entry

  chart <- chart |>
    hc_add_series(
      data = list_parse(selected),
      type = "scatter",
      name = status_name,
      color = fill,
      zIndex = z,
      marker = list(
        symbol = "diamond", radius = 6,
        fillColor = fill, lineColor = border, lineWidth = 1.2
      ),
      tooltip = list(
        pointFormat = "<b>{series.name}</b><br>Value: {point.x:.1f}"
      )
    )

  if (dot) {            # white diamond with a small black dot on top
    chart <- chart |>
      hc_add_series(
        data = list_parse(selected),
        type = "scatter",
        name = paste(status_name, "(dot)"),
        linkedTo = ":previous",
        showInLegend = FALSE,
        enableMouseTracking = FALSE,
        zIndex = 6,
        marker = list(symbol = "circle", radius = 1.5,
                      fillColor = "#000000", lineWidth = 0)
      )
  }
  chart
}

for (i in seq_len(nrow(status_style))) {
  hc <- add_status(
    hc,
    col_name    = status_style$column[i],
    status_name = status_style$status[i],
    fill        = status_style$fill[i],
    border      = status_style$border[i],
    dot         = status_style$dot[i],
    z           = status_style$z[i]
  )
}

# --- Table text: header cells (row -1) and body cells (rows 0..n-1) ------
for (i in seq_len(nrow(table_cols))) {
  x_abs <- table_frac * table_cols$x_rel[i]    # fraction of the FULL width
  hc <- hc |>
    add_text(table_cols$header[i], x_abs, header_y,
             align = table_cols$align[i], bold = TRUE, color = "#FFFFFF") |>
    add_text(tbl[[table_cols$field[i]]], x_abs, DT$y_pos,
             align = table_cols$align[i])
}

# Theme column: rotated text, one label per group
x_tema <- table_frac * tema_x_rel
hc <- hc |>
  add_text("Tema", x_tema, header_y, align = "center",
           bold = TRUE, color = "#FFFFFF") |>
  add_text(tema_pos$group, x_tema, tema_pos$y, align = "center",
           bold = TRUE, rotation = 270)

# Header text above the chart part (this replaces hc_title)
hc <- add_text(
  hc, "Folkehelsebarometer for Oslo",
  x = table_frac + (1 - table_frac) / 2, y = header_y,
  align = "center", bold = TRUE, color = "#FFFFFF"
)

# =========================================================================
# 9. Axes and styling
# =========================================================================
# TWO x axes side by side:
#   [[1]] = the chart panel (log10), the RIGHT part. Series use it by default.
#   [[2]] = a hidden text axis from 0 to 1 over the FULL width, used only by
#           the table text (xAxis = 1 in add_text()). Because it spans the
#           whole width, text positions are plain fractions of the chart width.
# Percentages in left/width refer to the plot area width.
x_axes <- list(
  list(
    type = "logarithmic",
    left  = sprintf("%.1f%%", table_frac * 100),
    width = sprintf("%.1f%%", (1 - table_frac) * 100),
    min = x_min, max = x_max,
    startOnTick = FALSE, endOnTick = FALSE,
    tickPositions = c(50, 70, 100, 140, 200),
    title = list(text = paste0("Index (reference = ", ref_value, ")")),
    gridLineWidth = 0,
    lineWidth = 1,
    plotLines = list(list(value = ref_value, color = "#CC0033",
                          width = 2.5, zIndex = 3))
  ),
  list(
    min = 0, max = 1, visible = FALSE,
    startOnTick = FALSE, endOnTick = FALSE,
    minPadding = 0, maxPadding = 0
  )
)
hc$x$hc_opts$xAxis <- x_axes     # same effect as hc_xAxis_multiples()

hc <- hc |>
  hc_yAxis(
    # Numeric (no categories): the row labels are now drawn by the text
    # series, and the header needs position -1, which a category axis
    # would not place.
    reversed = TRUE,                          # row 0 at the top
    min = header_y - 0.5,                     # header row included
    max = n_rows - 0.5,
    startOnTick = FALSE, endOnTick = FALSE,
    tickInterval = 1,
    title = list(text = NULL),
    labels = list(enabled = FALSE),           # text comes from the table
    gridLineWidth = 0,
    lineWidth = 0,
    tickLength = 0,
    plotBands = c(header_band, zebra),
    plotLines = group_lines
  ) |>
  hc_chart(
    backgroundColor = "#FFFFFF",
    height = row_px * (n_rows + 1) + 100,     # rows + header + axis/legend
    spacing = c(10, 10, 10, 10)
  ) |>
  hc_legend(
    enabled = TRUE, align = "center",
    verticalAlign = "bottom", layout = "horizontal"
  ) |>
  hc_credits(enabled = FALSE)

hc
