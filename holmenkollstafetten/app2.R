library(shiny)
library(bslib)

## ── Stage data (St.Hanshaugen start, 2026) ──────────────────────────────────
stages <- data.frame(
  etappe   = 1:15,
  distanse = c(1100, 1070, 595, 1920, 1210, 1250, 1770, 1780, 625, 2860,
               1520,  350, 1080,  710,  535),
  profil   = c("Kupert", "Bratt stigning", "Flat", "Kupert", "Kupert",
               "Bratt stigning", "Bratt stigning", "Nedover", "Nedover",
               "Nedover", "Kupert", "Flat", "Lett stigning", "Flat", "Flat"),
  start    = c(
    "Knud Knudsens plass", "Louises gate", "Wolffs gate",
    "Wilhelm Færdens vei", "Forskningsveien", "Holmenveien",
    "Slemdal skole", "Besserud", "Gressbanen", "Holmendammen",
    "Frognerparken", "Nordraaks gate", "Arno Bergs plass",
    "Camilla Collets vei", "Bislettgata"
  ),
  veksling = c(
    "Louises gate", "Wolffs gate", "Wilhelm Færdens vei",
    "Forskningsveien", "Holmenveien", "Slemdal skole",
    "Besserud", "Gressbanen", "Holmendammen", "Frognerparken",
    "Nordraaks gate", "Arno Bergs plass", "Camilla Collets vei",
    "Bislettgata", "Mål"
  ),
  stringsAsFactors = FALSE
)

## ── Helper: profil → emoji ───────────────────────────────────────────────────
profil_icon <- function(p) {
  icons <- c(
    "Kupert"         = "〰️",
    "Bratt stigning" = "⬆️",
    "Nedover"        = "⬇️",
    "Flat"           = "➡️",
    "Lett stigning"  = "↗️"
  )
  ifelse(p %in% names(icons), icons[p], "")
}

## ── Pace calculation ─────────────────────────────────────────────────────────
calc_exchange_times <- function(start_time, distances, paces, standard_pace) {
  paces <- ifelse(is.na(paces), standard_pace, paces)

  warn_msg <- NULL
  if (length(paces) < length(distances)) {
    missing_n  <- length(distances) - length(paces)
    warn_msg   <- paste0(
      "⚠️ Mangler fart for ", missing_n, " etappe(r) ",
      "(etappe ", length(paces) + 1, " til ", length(distances), "). ",
      "Gjennomsnittsfart ", standard_pace, " min/km brukes for disse."
    )
    paces <- c(paces, rep(standard_pace, missing_n))
  }

  t0    <- as.POSIXct(start_time, format = "%H:%M", tz = "UTC")
  times <- t0                          # times[1] = start = Etappe 1 tid
  for (i in seq_along(distances)) {
    mins_taken <- (distances[i] / 1000) * paces[i]
    times <- c(times, tail(times, 1) + as.difftime(mins_taken, units = "mins"))
  }

  list(
    times    = format(times[-length(times)], "%H:%M"),
    paces    = paces,
    warn_msg = warn_msg
  )
}

## ── UI ───────────────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title = div(
    style = "display:flex; align-items:center; gap:12px;",
    span("🏃", style = "font-size:1.6rem;"),
    div(
      style = "flex:1;",
      div("Holmenkollstafetten", style = "font-weight:700; font-size:1.15rem; line-height:1.1;"),
      div("Estimert vekslingstider", style = "font-size:0.78rem; opacity:.7;")
    ),
    # img(src = "hdir.png", height = "40px", style = "margin-left:auto;") #
  ),
  theme = bs_theme(
    version = 5,
    primary  = "#1a5c2e",
    base_font = font_google("Inter")
  ),

  ## ── Sidebar ──
  sidebar = sidebar(
    width = 290,
    bg = "#f4f8f5",

    h6("⏱ Start og tempo", class = "text-uppercase text-muted mb-3",
       style = "letter-spacing:.05em; font-size:.72rem;"),

    textInput("start_time", "Starttid (HH:MM)", value = "15:35",
              placeholder = "f.eks. 15:35"),

    numericInput("standard_pace",
                 "Gjennomsnittsfart (min/km)",
                 value = 5, min = 2, max = 20, step = 0.1),

    hr(style = "border-color:#cde0d4;"),
    h6("🎯 Individuell fart per etappe", class = "text-uppercase text-muted mb-1",
       style = "letter-spacing:.05em; font-size:.72rem;"),
    p("Fyll inn fart (min/km) per etappe, kommaseparert.",
      br(), "La stå tomt for å bruke gjennomsnittsfart.",
      style = "font-size:.78rem; color:#666;"),

    textAreaInput("speeds", NULL, value = "",
                  rows = 3,
                  placeholder = "f.eks. 4.5,5,5.5,..."),

    actionButton("calculate", "Beregn vekslingstider",
                 class = "btn-primary w-100 mt-2",
                 icon = icon("calculator")),

    hr(style = "border-color:#cde0d4;"),
    uiOutput("summary_box"),

    div(
      style = "margin-top:auto; padding-top:16px; text-align:center;",
      p("© YBK",
        style = "font-size:.7rem; color:#aaa; margin:0; letter-spacing:.05em;")
    )

  ),

  ## ── Main ──
  div(
    uiOutput("error_msg"),
    uiOutput("stage_table")
  )
)

## ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  results <- eventReactive(input$calculate, {
    n <- nrow(stages)
    raw <- trimws(input$speeds)
    paces <- if (raw == "") {
      rep(NA_real_, n)
    } else {
      vals <- suppressWarnings(as.numeric(unlist(strsplit(raw, ","))))
      ## Allow fewer values than etapper — missing ones use standard_pace
      if (length(vals) > n)
        return(list(error = paste0(
          "Feil: Du har oppgitt ", length(vals),
          " verdier, men det er kun ", n, " etapper."
        )))
      vals
    }

    res <- tryCatch(
      calc_exchange_times(input$start_time, stages$distanse, paces, input$standard_pace),
      error = function(e) list(error = e$message)
    )
    if (!is.null(res$error)) return(res)

    eff_pace <- res$paces
    times    <- res$times
    mins_vec  <- (stages$distanse / 1000) * eff_pace
    m_int     <- floor(mins_vec)
    s_int     <- round((mins_vec - m_int) * 60)

    list(
      data = data.frame(
        etappe   = stages$etappe,
        distanse = stages$distanse,
        profil   = stages$profil,
        start    = stages$start,
        veksling = stages$veksling,
        pace     = eff_pace,
        tid_leg  = sprintf("%d:%02d", m_int, s_int),
        tid_veks = times,
        stringsAsFactors = FALSE
      ),
      warn_msg = res$warn_msg
    )
  })

  ## Show popup warning if paces were padded with standard_pace
  observeEvent(input$calculate, {
    res <- results()
    if (!is.null(res$warn_msg)) {
      showModal(modalDialog(
        title = tagList(icon("triangle-exclamation", style = "color:#e67e00;"),
                        " Manglende fart"),
        p(res$warn_msg),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
    }
  })

  ## Error banner
  output$error_msg <- renderUI({
    r <- results()
    if (!is.null(r$error))
      div(class = "alert alert-danger mt-2", icon("triangle-exclamation"), " ", r$error)
  })

  ## Summary cards
  output$summary_box <- renderUI({
    r <- results()
    if (!is.null(r$error)) return(NULL)
    r <- r$data
    total_m   <- sum((r$distanse / 1000) * r$pace)
    total_min <- floor(total_m); total_sec <- round((total_m - total_min) * 60)
    total_km  <- round(sum(r$distanse) / 1000, 2)
    finish    <- tail(r$tid_veks, 1)

    tagList(
      h6("📊 Sammendrag", class = "text-uppercase text-muted mb-2",
         style = "letter-spacing:.05em; font-size:.72rem;"),
      div(style = "display:grid; gap:8px;",
        summary_card("Total distanse", paste0(total_km, " km"), "#1a5c2e"),
        summary_card("Estimert tid",   sprintf("%d:%02d", total_min, total_sec), "#2e7d52"),
        summary_card("Mål-tid",        finish, "#3a9e68")
      )
    )
  })

  ## Stage table
  output$stage_table <- renderUI({
    r <- results()
    if (!is.null(r$error)) return(NULL)
    r <- r$data

    rows <- lapply(seq_len(nrow(r)), function(i) {
      row <- r[i, ]
      tags$tr(
        tags$td(
          div(
            span(paste0("Etappe ", row$etappe),
                 style = "font-weight:700; font-size:.95rem;"),
            br(),
            span(paste0(profil_icon(row$profil), " ", row$profil),
                 style = "font-size:.78rem; color:#666;")
          )
        ),
        tags$td(
          span(paste0(format(row$distanse, big.mark = " "), " m"),
               style = "font-weight:600;")
        ),
        tags$td(
          div(
            div(span("Fra:", style = "color:#888; font-size:.72rem;"),
                span(row$start, style = "font-size:.85rem;")),
            div(span("Til:", style = "color:#888; font-size:.72rem;"),
                span(row$veksling, style = "font-size:.85rem; font-weight:600;"))
          )
        ),
        tags$td(
          div(
            div(span("Fart:", style = "color:#888; font-size:.72rem;"),
                span(paste0(row$pace, " min/km"), style = "font-size:.85rem;")),
            div(span("Etappetid:", style = "color:#888; font-size:.72rem;"),
                span(paste0(row$tid_leg, " min"), style = "font-size:.85rem;"))
          )
        ),
        tags$td(
          div(
            style = paste0(
              "background:", if (row$veksling == "Mål") "#1a5c2e" else "#e8f5ee", ";",
              "color:", if (row$veksling == "Mål") "#fff" else "#1a5c2e", ";",
              "border-radius:8px; padding:6px 12px; text-align:center;",
              "font-weight:700; font-size:1.05rem; min-width:64px;"
            ),
            if (row$veksling == "Mål") "🏁 " else "🔄 ",
            row$tid_veks
          )
        )
      )
    })

    div(
      style = "overflow-x:auto;",
      tags$table(
        class = "table table-hover align-middle",
        style = "font-size:.88rem; margin-top:4px;",
        tags$thead(
          style = "background:#1a5c2e; color:#fff;",
          tags$tr(
            lapply(
              c("Etappe", "Distanse", "Rute", "Tempo", "Veksling / Mål-tid"),
              function(h) tags$th(h, style = "padding:10px 14px; font-weight:600;")
            )
          )
        ),
        tags$tbody(rows)
      )
    )
  })

  session$onSessionEnded(stopApp)
}

## ── Helper widget ─────────────────────────────────────────────────────────────
summary_card <- function(label, value, color) {
  div(
    style = paste0(
      "background:#fff; border-left:4px solid ", color, ";",
      "border-radius:6px; padding:8px 12px;"
    ),
    div(label, style = "font-size:.7rem; color:#888; text-transform:uppercase; letter-spacing:.04em;"),
    div(value,  style = paste0("font-size:1.1rem; font-weight:700; color:", color, ";"))
  )
}

shinyApp(ui = ui, server = server)
