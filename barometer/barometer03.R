library(data.table)
library(highcharter)
library(data.table)

path <- "c:/Users/ykama/OneDrive - Helsedirektoratet/dataset/barometer"
fcsv <- "InndataBarometer_kommune2026.csv"

DT <- data.table::fread(file.path(path, fcsv))
data.table::setnames(DT, tolower(names(DT)))

DT[stedskode_string != 1103, .ROW := NULL] #Stavanger
# DT[stedskode_string != 301, .ROW := NULL]

# folkehelse_dt tabell
source(here::here("barometer/tema-tbl.R"))

# =========================================================================
# 0. Settings
# =========================================================================
# Reference value: the red vertical line. Taken from the data, not typed in.
ref_value <- unique(DT$ref)
stopifnot("`ref` must have exactly one value" = length(ref_value) == 1)

# Maximum height of the red line
y_max <- max(DT$lpnr - 1, na.rm = TRUE)

# Two points with the same x value create a vertical line
ref_line <- data.frame(
  x = c(ref_value, ref_value),
  y = c(-0.5, y_max + 0.5)
)

# x-axis range. A log axis cannot show 0 or negative values, so min > 0.
# The data is capped at 200 and its lowest value is 50. On a log scale
# 50 to 200 is symmetric around 100 (100 / 2 and 100 * 2).
x_min <- 50
x_max <- 200
stopifnot(x_min > 0, x_max > x_min)

# Layout
table_frac  <- 0.60     # 60% of the width for the table, 40% for the chart
row_px      <- 24       # height of one row in pixels
chart_width <- 1200     # px. The text columns are placed as fractions of the
                        # width, so a fixed width keeps them from colliding.
                        # Use NULL to fill the container instead.
header_y    <- -1       # the header row sits one slot above row 0

# =========================================================================
# 1. Row order, groups and vertical position
# =========================================================================
# Sort explicitly so that y_pos follows lpnr, rather than relying on the
# order the rows happen to arrive in.
setorder(DT, lpnr)

# Add the table texts (incl. the theme, `tema`) to DT. Matching on the
# indicator number guarantees that every text lands on the right row.
n_before <- nrow(DT)
DT <- merge(DT, folkehelse_dt,
            by.x = "lpnr", by.y = "indikator_nr",
            all.x = TRUE, sort = TRUE)
stopifnot(
  "merge changed the number of rows (duplicate indikator_nr?)" = nrow(DT) == n_before,
  "an indicator has no match in folkehelse_dt" = !anyNA(DT$tema)
)

# The groups are the themes. Separator lines are drawn where `tema` changes,
# so each theme must form ONE block of consecutive rows.
DT[, group := tema]
stopifnot(
  "tema must be one consecutive block per theme" =
    uniqueN(rleid(DT$group)) == uniqueN(DT$group)
)

# 0-based row position for Highcharts (R counts from 1, Highcharts from 0).
# Used for the bars, the symbols AND the table text.
DT[, y_pos := .I - 1L]
n_rows <- nrow(DT)

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
# NB: the start comes from beste10pct_min but the end from beste10stk_max
# (pct vs stk). Check that this mix is intended.
ci_white <- DT[, .(
  x  = as.numeric(beste10pct_min),
  x2 = as.numeric(beste10stk_max),
  y  = as.numeric(y_pos)
)]

# Rows without a "10 best" range (NA) get no bar. They must be removed here:
# an NA would be sent to Highcharts as a null x value.
ci_white <- ci_white[!is.na(x) & !is.na(x2)]

stopifnot(all(ci_white$x > 0))

white_flat <- sum(ci_white$x2 <= ci_white$x)
if (white_flat > 0) {
  warning(sprintf(
    "%d of %d white bars have x2 <= x and will not be visible.",
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

# na.rm = TRUE drops all the NA cells, so every indicator keeps one row.
# figurverdi_graa is NOT included here: it holds a value on every row (the
# value for the county), so it is drawn as its own series in section 5.3.
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
# 4. Rows: header band, zebra stripes and group separator lines (y axis)
# =========================================================================
# Plot bands and lines on the y axis span the WHOLE plot width, i.e. the
# table and the chart together, like the rows of a real table.
header_band <- list(list(from = header_y - 0.5, to = header_y + 0.5,
                         color = "#8BA9B5", zIndex = 0))

zebra <- lapply(DT[y_pos %% 2 == 1, y_pos], function(y) {
  list(from = y - 0.5, to = y + 0.5, color = "#F2F2F2", zIndex = 0)
})

# Compare each row with the NEXT row: TRUE on the last row of every group
# (the final row compares with NA and is dropped). + 0.5 moves the line from
# the centre of that row to the border with the next row.
group_positions <- DT[group != shift(group, type = "lead"), y_pos + 0.5]

group_lines <- lapply(group_positions, function(pos) {
  list(value = pos, color = "#BDBDBD", width = 1, zIndex = 3)
})

# =========================================================================
# 4.5 Table content, and extra fields for the grey bars' tooltip
# =========================================================================
# Numbers use a decimal comma. An indicator is shown without decimals when
# its kommune, fylke and norge values are all whole numbers, otherwise with
# one decimal (this matches most of your screenshot; adjust if needed).
all_whole <- DT[, (kommune %% 1 == 0) & (fylke %% 1 == 0) & (norge %% 1 == 0)]
all_whole[is.na(all_whole)] <- FALSE

fmt_num <- function(x) {
  out <- ifelse(
    all_whole,
    formatC(x, format = "f", digits = 0, decimal.mark = ","),
    formatC(x, format = "f", digits = 1, decimal.mark = ",")
  )
  out[is.na(x)] <- ""
  out
}

tbl <- DT[, .(
  nr        = lpnr,
  indikator = indikator,
  kommune   = fmt_num(kommune),
  fylke     = fmt_num(fylke),
  norge     = fmt_num(norge),
  enhet     = enhet
)]

# Highcharts keeps any extra property you put on a point, so the tooltip can
# print it with {point.kommune} etc. ci_grey and tbl both come from DT in the
# same row order, so the texts belong to the right bar.
fmt_1 <- function(x) formatC(x, format = "f", digits = 1, decimal.mark = ",")

ci_grey[, `:=`(
  name      = tbl$indikator,     # used as the tooltip header
  kommune   = tbl$kommune,
  fylke     = tbl$fylke,
  norge     = tbl$norge,
  enhet     = tbl$enhet,
  range_txt = paste0(fmt_1(DT$soylemin), " – ", fmt_1(DT$soylemax))
)]

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
      # Header = indicator name, body = the three table values
      headerFormat = "<span style='font-size:11px'><b>{point.name}</b></span><br/>",
      pointFormat = paste0(
        "Kommune: <b>{point.kommune}</b><br/>",
        "Fylke: <b>{point.fylke}</b><br/>",
        "Norge: <b>{point.norge}</b><br/>",
        "Enhet: {point.enhet}<br/>",
        "Variasjon mellom kommunene: {point.range_txt}"
      )
    )
  )

# =========================================================================
# 5.2 White (10 best) bars
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
# 5.3 Grey dots for the county value
# -------------------------------------------------------------------------
dt_gra <- data.frame(x = DT$figurverdi_graa, y = DT$y_pos)

hc <- hc |>
  hc_add_series(
    data = list_parse(dt_gra),
    type = "scatter",
    name = "Fylke verdi",
    color = "#808080",
    zIndex = 3,
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
      zIndex = 4,           # above bars (1) and separators (3)
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
# 7. The table: text drawn at (x, y) positions as data labels
# =========================================================================
# A table cell is an invisible scatter point whose DATA LABEL is the text.
#   - xAxis = 1 puts the cell on the hidden 0-1 "text axis" (see section 8),
#     so x is a fraction of the full chart width.
#   - yAxis is the shared y axis, so a cell on row y_pos sits exactly on the
#     same line as the bar and symbols of that row.
add_text <- function(chart, values, x, y, align = "left",
                     bold = FALSE, color = "#333333",
                     rotation = 0, width = NULL, font_size = "11px") {
  values <- as.character(values)
  values[is.na(values)] <- ""

  style <- list(
    fontSize = font_size,
    fontWeight = if (bold) "bold" else "normal",
    color = color,
    textOutline = "none"        # remove Highcharts' white text halo
  )
  # A width makes long text wrap onto several lines instead of overflowing
  if (!is.null(width)) style$width <- paste0(round(width), "px")

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
        style = style
      )
    )
}

# --- Table content: `tbl` is built in section 4.5 -------------------------

# --- Column layout -------------------------------------------------------
# x_rel is where the text is anchored, as a fraction (0-1) of the TABLE
# width; `align` says how the text sits on that anchor point.
table_cols <- data.table(
  field  = c("nr", "indikator", "kommune", "fylke", "norge", "enhet"),
  header = c("#", "Indikator", "Kommune", "Fylke", "Norge", "Enhet"),
  x_rel  = c(0.07, 0.10, 0.56, 0.65, 0.74, 0.82),
  align  = c("center", "left", "center", "center", "center", "left")
)

# Header cells (row -1) and body cells (rows 0 .. n-1), one series per column
for (i in seq_len(nrow(table_cols))) {
  x_abs <- table_frac * table_cols$x_rel[i]   # fraction of the FULL width
  hc <- hc |>
    add_text(table_cols$header[i], x_abs, header_y,
             align = table_cols$align[i], bold = TRUE, color = "#FFFFFF") |>
    add_text(tbl[[table_cols$field[i]]], x_abs, DT$y_pos,
             align = table_cols$align[i])
}

# --- Theme (tema) column -------------------------------------------------
# One rotated label per theme, centred vertically on that theme's rows. The
# text may wrap, but only within the height of the theme's own rows.
tema_x   <- table_frac * 0.025
tema_pos <- DT[, .(y = mean(y_pos), n = .N), by = group]

hc <- add_text(hc, "Tema", tema_x, header_y, align = "center",
               bold = TRUE, color = "#FFFFFF")

for (i in seq_len(nrow(tema_pos))) {
  hc <- add_text(
    hc, tema_pos$group[i], tema_x, tema_pos$y[i],
    align = "center", bold = TRUE, rotation = 270,
    width = tema_pos$n[i] * row_px - 10,       # wrap inside the theme's rows
    font_size = "10px"
  )
}

# --- Header text above the chart part (this replaces hc_title) -----------
hc <- add_text(
  hc, "Folkehelsebarometer",
  x = table_frac + (1 - table_frac) / 2, y = header_y,
  align = "center", bold = TRUE, color = "#FFFFFF", font_size = "13px"
)

# =========================================================================
# 8. Axes, title and styling
# =========================================================================
# TWO x axes side by side:
#   [[1]] = the chart panel (log10), the RIGHT part. Series use it by default.
#   [[2]] = a hidden text axis from 0 to 1 over the FULL width, used only by
#           the table text (xAxis = 1 in add_text()).
# Percentages in left/width refer to the plot area width.
x_axes <- list(
  list(
    # Log axis: the DATA STAYS in original units (50, 100, 200). Highcharts
    # does the log10 scaling itself, and min, max, tick positions and
    # plot lines are all given in original units too.
    type = "logarithmic",
    left  = sprintf("%.1f%%", table_frac * 100),
    width = sprintf("%.1f%%", (1 - table_frac) * 100),
    min = x_min,
    max = x_max,
    # Keep the axis exactly at min/max instead of rounding outwards to the
    # next "nice" tick, which on a log axis would be 10 and 1000
    startOnTick = FALSE,
    endOnTick = FALSE,
    tickPositions = c(50, 70, 100, 140, 200),  # readable labels, even spacing
    title = list(text = "Norge"),
    gridLineWidth = 0,
    lineWidth = 1
#     plotLines = list(list(
#       value = ref_value,    # 100, in original units
#       color = "#CC0033",
#       width = 2.5,
#       zIndex = 6
#     ))
  ),
  list(
    min = 0, max = 1, visible = FALSE,
    startOnTick = FALSE, endOnTick = FALSE,
    minPadding = 0, maxPadding = 0
  )
)
hc$x$hc_opts$xAxis <- x_axes     # same effect as hc_xAxis_multiples()

hc <- hc |> 
  hc_add_series(
    data = highcharter::list_parse2(ref_line),
    type = "line",
    color = "#CC0033",
    lineWidth = 2.5,
    marker = list(enabled = FALSE),
    enableMouseTracking = FALSE,
    showInLegend = FALSE,
    zIndex = 6
  )

hc <- hc |>
  hc_yAxis(
    # Numeric (no categories): the row labels are now drawn by the table
    # text, and the header needs position -1, which a category axis would
    # not place.
    reversed = TRUE,                          # y_pos 0 at the top, like a table
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
  # By default, hovering a point fades ALL other series to 20% opacity (the
  # "inactive" state). The table text is made of series too, so it would
  # fade or vanish whenever you hover a bar. Turn that state off for all.
  hc_plotOptions(
    series = list(states = list(inactive = list(enabled = FALSE)))
  ) |>
  hc_chart(
    backgroundColor = "#FFFFFF",
    height = row_px * (n_rows + 1) + 100,     # rows + header + axis/legend
    width = chart_width,
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
