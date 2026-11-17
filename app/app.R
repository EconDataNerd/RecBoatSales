# Recreational Boating Dashboard 1: Monthly Sales (units & value) with YoY comparison
# Source: Kevin Decker | recreational boating dashboards

library(shiny)
library(readr)

# ---- Data ----------------------------------------------------------------
# Expects a CSV with columns: year, month (1-12), units, value
sales <- read_csv("boat_sales.csv", show_col_types = FALSE)

month_names <- c("January","February","March","April","May","June",
                 "July","August","September","October","November","December")

# The newest month(s) in the data are still filling in (weekly upload lag), so the
# dashboard opens on the latest COMPLETE month rather than the newest partial one.
# If the lag ever exceeds one month, bump this to 2, 3, ...
incomplete_trailing_months <- 1

sales_ord   <- sales[order(sales$year, sales$month), ]
default_idx <- max(1, nrow(sales_ord) - incomplete_trailing_months)
default_year  <- sales_ord$year[default_idx]
default_month <- sales_ord$month[default_idx]

# Rolling 12-month (R12 / trailing-twelve-months). Anchored to the latest complete
# month. Reindexed to a gap-free monthly series spanning ALL the data, so each window
# is a true 12 calendar months even if a month (or a source in a month) is missing.
all_month_dates <- seq(min(as.Date(sprintf("%d-%02d-01", sales_ord$year, sales_ord$month))),
                       max(as.Date(sprintf("%d-%02d-01", sales_ord$year, sales_ord$month))),
                       by = "month")
make_r12 <- function(df) {
  sdate <- as.Date(sprintf("%d-%02d-01", df$year, df$month))
  full  <- data.frame(date = all_month_dates)
  full$units <- df$units[match(full$date, sdate)]; full$units[is.na(full$units)] <- 0
  full$value <- df$value[match(full$date, sdate)]; full$value[is.na(full$value)] <- 0
  n  <- nrow(full)
  ru <- rv <- rep(NA_real_, n)
  if (n >= 12) for (i in 12:n) {
    ru[i] <- sum(full$units[(i - 11):i]); rv[i] <- sum(full$value[(i - 11):i])
  }
  end_i <- max(1, n - incomplete_trailing_months)   # latest complete month
  list(full = full, ru = ru, rv = rv, end_i = end_i, n = n, monthly = df)
}
R12_all <- make_r12(sales_ord)

# Monthly totals by source (written by prepare_data.R). Optional: the Trends tab
# shows a note if it hasn't been generated yet.
by_source <- if (file.exists("boat_sales_by_source.csv")) {
  read_csv("boat_sales_by_source.csv", show_col_types = FALSE)
} else NULL

# Plain-language definitions of each Source of Sales category.
# The order here also drives the dropdown order (New -> Used -> In-Migrant).
source_defs <- c(
  "New Import"          = "a new boat purchased from an out-of-state dealer.",
  "New Dealer"          = "a new boat purchased from a WA dealer.",
  "Used Dealer"         = "a used boat purchased from a WA dealer.",
  "Used Dealer Import"  = "a used boat purchased from an out-of-state dealer.",
  "Used Import"         = "a used boat purchased from an out-of-state private seller.",
  "Used Private"        = "a used boat purchased from a WA private seller.",
  "In-Migrant"          = "a new WA resident registers a boat brought into WA."
)

# Categories to offer, in the definition order above; any unexpected value in the
# data (e.g. a new or misspelled label) is appended so nothing is hidden.
source_levels <- if (is.null(by_source)) character(0) else {
  present <- unique(by_source$source)
  c(names(source_defs)[names(source_defs) %in% present],
    sort(setdiff(present, names(source_defs))))
}

# Quick picks for the Rolling 12-Mo source filter.
r12_presets <- list(
  all        = source_levels,
  new        = intersect(c("New Import", "New Dealer"), source_levels),
  used_dealer = intersect(c("Used Dealer", "Used Dealer Import"), source_levels)
)

# All-source monthly totals keyed "YYYY-MM" — used for a fixed axis ceiling and for
# looking up the same month a year earlier (the prior-year reference).
all_tot <- if (is.null(by_source)) NULL else {
  a <- aggregate(cbind(units, value) ~ year + month, data = by_source, FUN = sum)
  a$ym <- sprintf("%04d-%02d", a$year, a$month)
  a
}

# Month start-dates spanning the data, for the Transaction Date range slider.
month_dates       <- sort(as.Date(sprintf("%d-%02d-01", sales$year, sales$month)))
date_min          <- min(month_dates)
# Stop the slider at the latest COMPLETE month — the newest still-filling month(s)
# are excluded (same rule / setting as the Dashboard default) until they finish.
last_complete_idx <- max(1, length(month_dates) - incomplete_trailing_months)
date_max          <- month_dates[last_complete_idx]
# Default to roughly the most recent 13 complete months, like the reference view.
default_start     <- if (last_complete_idx >= 13) month_dates[last_complete_idx - 12] else date_min

# ---- Explore tab data ------------------------------------------------------
# One row per transaction, written by run_weekly.R. Contains no VIN, make, or ZIP.
# Optional: the Explore tab shows a note if the file hasn't been generated yet.
records <- if (file.exists("boat_sales_records.rds")) readRDS("boat_sales_records.rds") else NULL

# Filterable attributes: label shown to users = column in the records file.
explore_dims <- c(
  "Source of Sales" = "source",     "Boat Type"      = "boat_type",
  "Length Class"    = "length_class", "Hull Material" = "hull",
  "Engine Drive"    = "engine",     "Propulsion"     = "propulsion",
  "Fuel"            = "fuel",       "Moorage County" = "moor_county"
)

# The 15 coastal counties WSG serves (used by the "Coastal counties" quick pick).
coastal_counties <- c(
  "Clallam", "Grays Harbor", "Island", "Jefferson", "King", "Kitsap", "Mason",
  "Pacific", "Pierce", "San Juan", "Skagit", "Snohomish", "Thurston",
  "Wahkiakum", "Whatcom"
)

# Bars based on fewer boats than this are grayed out as less reliable.
small_n <- 10

explore_choices <- if (is.null(records)) NULL else lapply(explore_dims, function(col) {
  x <- records[[col]]
  lv <- if (is.factor(x)) levels(droplevels(x)) else sort(unique(x))
  if (col == "source") lv <- c(names(source_defs)[names(source_defs) %in% lv],
                               setdiff(lv, names(source_defs)))
  if ("Unknown" %in% lv) lv <- c(setdiff(lv, "Unknown"), "Unknown")
  lv
})

comma <- function(x) formatC(round(x), format = "f", digits = 0, big.mark = ",")
fmt_short_money <- function(x) if (is.na(x)) "—" else if (abs(x) >= 1e6)
  paste0("$", formatC(x / 1e6, format = "f", digits = 1), "M") else paste0("$", comma(x))

ym_to_date <- function(ym) as.Date(sprintf("%d-%02d-01", ym %/% 12L, ym %% 12L + 1L))
date_to_ym <- function(d) as.integer(format(d, "%Y")) * 12L + as.integer(format(d, "%m")) - 1L

# ---- Helpers -------------------------------------------------------------
get_row <- function(df, yr, mo) {
  r <- df[df$year == yr & df$month == mo, ]
  if (nrow(r) == 0) return(NULL)
  r[1, ]
}

pct_change <- function(now, last) {
  if (is.null(last) || is.na(last) || last == 0) return(NA_real_)
  (now - last) / last * 100
}

fmt_int   <- function(x) if (is.null(x) || is.na(x)) "—" else format(round(x), big.mark = ",")
fmt_money <- function(x) if (is.null(x) || is.na(x)) "—" else paste0("$", format(round(x), big.mark = ","))
fmt_pct   <- function(x) if (is.na(x)) "—" else sprintf("%+.1f%%", x)

# ---- Branding ------------------------------------------------------------
WSG_PURPLE <- "#4b2e83"   # Washington Sea Grant purple
# Header mark: use the logo if ANY image is in the www/ folder (Shiny serves www/),
# preferring a logo-ish name; otherwise fall back to a text badge. So just drop the
# Sea Grant Washington image into a "www" folder next to app.R — any name/format works.
logo_file <- local({
  if (!dir.exists("www")) return(NA_character_)
  imgs <- list.files("www", pattern = "(?i)\\.(png|jpe?g|svg|gif|webp)$")
  if (!length(imgs)) return(NA_character_)
  pref <- imgs[grepl("(?i)logo|wsg|sea.?grant", imgs)]
  if (length(pref)) pref[1] else imgs[1]
})
# Print what was found at launch so it's easy to diagnose from the Console.
if (is.na(logo_file)) {
  message("WSG logo: no usable image found in www/ (contents: ",
          paste(if (dir.exists("www")) list.files("www") else "<no www folder>", collapse = ", "),
          ") — using text badge.")
} else {
  message("WSG logo: using www/", logo_file)
}
header_brand <- if (!is.na(logo_file)) {
  tags$img(src = logo_file, height = "62px", alt = "Washington Sea Grant",
           style = "display:block;")
} else {
  tags$span("WSG",
    style = paste0("font-weight:800; font-size:1.5em; letter-spacing:1px; color:white; ",
                   "background:", WSG_PURPLE, "; padding:4px 12px; border-radius:4px;"))
}

# Light watermark drawn on every chart so screenshots carry the WSG attribution.
watermark <- function() {
  mtext("Washington Sea Grant (WSG)", side = 3, line = -1.1, adj = 0.99,
        col = "#c9c9c9", cex = 0.75, font = 2)
}

# ---- UI ------------------------------------------------------------------
ui <- fluidPage(
  title = "WSG Recreational Boating Dashboard",
  tags$div(
    style = paste0("display:flex; align-items:center; gap:16px; padding:10px 4px; ",
                   "margin-bottom:10px; border-bottom:3px solid ", WSG_PURPLE, ";"),
    header_brand,
    tags$div(
      tags$div("Recreational Boating Dashboard",
               style = paste0("font-weight:700; font-size:1.2em; color:", WSG_PURPLE, ";")),
      tags$div("Washington State — monthly and rolling 12-month sales",
               style = "color:#555; font-size:0.9em;"))
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel("input.tabs !== 'Explore'",
        selectInput("month", "Month",
                    choices = setNames(1:12, month_names),
                    selected = default_month),
        selectInput("year", "Year",
                    choices = sort(unique(sales$year), decreasing = TRUE),
                    selected = default_year)
      ),
      conditionalPanel("input.tabs === 'Explore'",
        tags$p("The Explore tab has its own filters above the charts.",
               style = "color:#666; font-size:0.9em;"))
    ),
    mainPanel(
      width = 9,
      tabsetPanel(id = "tabs",
        tabPanel("Monthly Summary",
          h4(textOutput("caption")),
          tableOutput("summary"),
          tags$p(textOutput("last_updated"),
                 style = "color:#888; font-size:0.85em; margin-top:8px;")
        ),
        tabPanel("Rolling 12-Mo",
          if (length(source_levels)) tagList(
            checkboxGroupInput("r12_src", "Source of Sales",
                               choices = source_levels, selected = source_levels,
                               inline = TRUE),
            tags$div(style = "margin:-6px 0 10px; font-size:0.9em;",
              "Quick picks: ",
              actionLink("r12_pick_all", "All sources"), " · ",
              actionLink("r12_pick_new", "New (Dealer + Import)"), " · ",
              actionLink("r12_pick_used_dealer", "Used Dealer + Used Dealer Import"))
          ),
          h4(textOutput("r12_caption")),
          tableOutput("r12_table"),
          tags$p("Rolling 12-month (R12) sums the trailing 12 months, so it removes ",
                 "seasonality and shows the underlying year-over-year trend. Use the ",
                 tags$b("Month"), " and ", tags$b("Year"), " selectors to move the window; ",
                 "it stops at the latest complete month.",
                 style = "color:#888; font-size:0.85em; margin:0 0 10px;"),
          h4("Rolling 12-month trend", style = "margin:6px 0;"),
          plotOutput("r12_units_plot", height = "250px"),
          plotOutput("r12_value_plot", height = "250px"),
          h4("Year-to-date", style = "margin:14px 0 6px;"),
          tableOutput("ytd_table")
        ),
        tabPanel("Trends",
          checkboxGroupInput("src", "Source of Sales",
                             choices = source_levels, selected = source_levels,
                             inline = TRUE),
          sliderInput("daterange", "Transaction Date",
                      min = date_min, max = date_max,
                      value = c(default_start, date_max),
                      timeFormat = "%b %Y", width = "100%"),
          div(style = "white-space:nowrap;",
              checkboxInput("show_prev", "Show prior-year reference", value = TRUE)),
          tags$p("Checked sources are summed. Prior-year reference = same month one ",
                 "year earlier (black tick). Definitions are on the ",
                 tags$b("Source & Definitions"), " tab.",
                 style = "color:#888; font-size:0.85em; margin:0 0 6px;"),
          h4("Boat Sales", style = "margin:6px 0;"),
          plotOutput("trend_units", height = "300px"),
          h4("Sales Value", style = "margin:6px 0;"),
          plotOutput("trend_value", height = "300px")
        ),
        tabPanel("Explore",
          if (is.null(records)) tags$p(
            "Transaction data not found. Run run_weekly.R to generate boat_sales_records.rds."
          ) else tagList(
            tags$style(HTML(".selectize-input { max-height: 84px; overflow-y: auto; }")),
            tags$p("Filter boat sales by any combination of attributes. Leave a box empty to ",
                   "include everything. Price statistics leave out $0 transactions ",
                   "(boats registered by new residents) unless you uncheck the box below.",
                   style = "color:#555; font-size:0.9em; margin:8px 0 10px;"),
            fluidRow(lapply(names(explore_dims)[1:4], function(lbl) column(3,
              selectizeInput(paste0("ex_", explore_dims[[lbl]]), lbl,
                             choices = explore_choices[[lbl]], multiple = TRUE,
                             options = list(placeholder = "All", plugins = list("remove_button")))))),
            fluidRow(lapply(names(explore_dims)[5:8], function(lbl) column(3,
              selectizeInput(paste0("ex_", explore_dims[[lbl]]), lbl,
                             choices = explore_choices[[lbl]], multiple = TRUE,
                             options = list(placeholder = "All", plugins = list("remove_button"))),
              if (lbl == "Moorage County") tags$div(style = "margin:-10px 0 8px; font-size:0.85em;",
                actionLink("ex_pick_coastal", "15 coastal counties"), " · ",
                actionLink("ex_pick_county_clear", "Clear"))))),
            fluidRow(
              column(6, sliderInput("ex_dates", "Transaction Date",
                                    min = date_min, max = date_max,
                                    value = c(default_start, date_max),
                                    timeFormat = "%b %Y", width = "100%")),
              column(2, numericInput("ex_pmin", "Price minimum ($)", value = NA, min = 0, step = 1000)),
              column(2, numericInput("ex_pmax", "Price maximum ($)", value = NA, min = 0, step = 1000)),
              column(2, tags$div(style = "margin-top:25px;",
                checkboxInput("ex_excl0", "Exclude $0 from price stats", value = TRUE)),
                actionLink("ex_reset", "Reset all filters"))
            ),
            uiOutput("ex_kpis"),
            fluidRow(
              column(6, selectInput("ex_group", "Compare by", choices = explore_dims,
                                    selected = "source")),
              column(6, selectInput("ex_metric", "Show",
                                    choices = c("Median price" = "median", "Average price" = "avg",
                                                "Boats sold" = "units", "Total sales value" = "value"),
                                    selected = "median"))
            ),
            plotOutput("ex_group_plot", height = "auto"),
            tags$p(textOutput("ex_small_note", inline = TRUE),
                   style = "color:#888; font-size:0.85em; margin:4px 0 12px;"),
            h4(textOutput("ex_trend_title"), style = "margin:10px 0 6px;"),
            plotOutput("ex_trend_plot", height = "260px"),
            h4("Summary table", style = "margin:14px 0 6px;"),
            tableOutput("ex_table"),
            downloadButton("ex_download", "Download table (CSV)", class = "btn-sm")
          )
        ),
        tabPanel("Source & Definitions",
          h4("Source of Sales — definitions"),
          tags$ul(
            lapply(names(source_defs), function(nm)
              tags$li(tags$b(nm), " — ", source_defs[[nm]]))
          ),
          tags$hr(),
          h4("Data source"),
          tags$p(
            "The results include data obtained from the Washington State ",
            "Department of Licensing (DOL). DOL did not verify the accuracy ",
            "of the data or how it was used."
          )
        )
      )
    )
  ),
  tags$hr(style = "margin-top:18px;"),
  tags$p(
    HTML(paste0(
      "Produced by <b>Washington Sea Grant (WSG)</b> · University of Washington. ",
      "Data source: WA Department of Licensing. ",
      "&copy; ", format(Sys.Date(), "%Y"),
      " Washington Sea Grant — not for redistribution or reuse without attribution.")),
    style = paste0("color:#777; font-size:0.8em; text-align:center; padding:4px 0 12px;")
  )
)

# ---- Server --------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Explore tab ---------------------------------------------------------
  observeEvent(input$ex_pick_coastal, updateSelectizeInput(session, "ex_moor_county",
    selected = intersect(coastal_counties, explore_choices[["Moorage County"]])))
  observeEvent(input$ex_pick_county_clear, updateSelectizeInput(session, "ex_moor_county", selected = character(0)))
  observeEvent(input$ex_reset, {
    for (col in explore_dims) updateSelectizeInput(session, paste0("ex_", col), selected = character(0))
    updateSliderInput(session, "ex_dates", value = c(default_start, date_max))
    updateNumericInput(session, "ex_pmin", value = NA)
    updateNumericInput(session, "ex_pmax", value = NA)
    updateCheckboxInput(session, "ex_excl0", value = TRUE)
  })

  ex_window <- reactive({
    rng <- input$ex_dates
    if (is.null(rng)) rng <- c(default_start, date_max)
    c(date_to_ym(rng[1]), date_to_ym(rng[2]))
  })

  # Records matching every filter.
  ex_data <- reactive({
    req(records)
    keep <- rep(TRUE, nrow(records))
    for (col in explore_dims) {
      sel <- input[[paste0("ex_", col)]]
      if (length(sel)) keep <- keep & as.character(records[[col]]) %in% sel
    }
    w <- ex_window()
    keep <- keep & records$ym >= w[1] & records$ym <= w[2]
    if (isTRUE(input$ex_pmin > 0)) keep <- keep & records$price >= input$ex_pmin
    if (isTRUE(!is.na(input$ex_pmax))) keep <- keep & records$price <= input$ex_pmax
    records[keep, ]
  })

  priced <- function(d) if (isTRUE(input$ex_excl0)) d$price > 0 else rep(TRUE, nrow(d))

  output$ex_kpis <- renderUI({
    d <- ex_data(); p <- d$price[priced(d)]
    card <- function(label, val, sub = NULL) column(3, tags$div(
      style = "text-align:center; padding:10px 4px; margin:6px 0 12px; border:1px solid #e3e3e3; border-radius:6px;",
      tags$div(label, style = "color:#555; font-size:0.9em;"),
      tags$div(val, style = paste0("font-size:1.6em; font-weight:700; color:", WSG_PURPLE, ";")),
      if (!is.null(sub)) tags$div(sub, style = "color:#999; font-size:0.75em;")))
    fluidRow(
      card("Boats Sold", fmt_int(nrow(d))),
      card("Total Sales Value", fmt_short_money(sum(d$price))),
      card("Average Price", if (length(p)) fmt_money(mean(p)) else "—",
           paste0("n = ", comma(length(p)))),
      card("Median Price", if (length(p)) fmt_money(median(p)) else "—",
           paste0("n = ", comma(length(p))))
    )
  })

  # One row per category of the "Compare by" attribute.
  ex_summary <- reactive({
    d <- ex_data(); col <- input$ex_group
    if (is.null(col) || nrow(d) == 0) return(NULL)
    lbl <- names(explore_dims)[explore_dims == col]
    lv <- explore_choices[[lbl]]
    g <- factor(as.character(d[[col]]), levels = lv)
    pr <- priced(d)
    out <- data.frame(
      group  = lv,
      units  = as.numeric(tapply(rep(1, nrow(d)), g, sum)),
      value  = as.numeric(tapply(d$price, g, sum)),
      n_priced = as.numeric(tapply(as.numeric(pr), g, sum)),
      avg    = as.numeric(tapply(d$price[pr], g[pr], mean)),
      median = as.numeric(tapply(d$price[pr], g[pr], median)),
      stringsAsFactors = FALSE
    )
    out$units[is.na(out$units)] <- 0
    out[out$units > 0, ]
  })

  ex_plot_height <- function() {
    s <- ex_summary(); max(220, 32 * (if (is.null(s)) 1 else nrow(s)) + 70)
  }

  output$ex_group_plot <- renderPlot({
    s <- ex_summary()
    if (is.null(s)) { blank("No sales match these filters."); return() }
    met <- input$ex_metric
    is_price <- met %in% c("median", "avg")
    y <- s[[met]]; y[is.na(y)] <- 0
    col <- if (is_price) "#6aa84f" else "#4a6fa5"
    cols <- ifelse(is_price & s$n_priced < small_n, "#cfcfcf", col)
    lab_val <- if (met == "units") comma(y) else
               if (met == "value") paste0("$", formatC(y / 1e6, format = "f", digits = 1), "M") else
               paste0("$", comma(y))
    n_lab <- if (is_price) s$n_priced else s$units
    labs <- paste0(lab_val, "   (n = ", comma(n_lab), ")")
    if (is_price) labs[s$n_priced == 0] <- "no priced sales"
    o <- rev(seq_len(nrow(s)))                     # first category on top
    par(mar = c(3, 12, 1, 1))
    xmax <- max(y, 1) * 1.45
    bp <- barplot(y[o], names.arg = s$group[o], horiz = TRUE, las = 1, col = cols[o],
                  border = NA, xlim = c(0, xmax), cex.names = 0.85, xaxt = "n")
    ticks <- pretty(c(0, max(y, 1)))
    axis(1, at = ticks, cex.axis = 0.8, labels = if (met == "units") comma(ticks) else
         if (met == "value") paste0("$", ticks / 1e6, "M") else paste0("$", comma(ticks)))
    text(y[o], bp, labs[o], pos = 4, cex = 0.8, xpd = TRUE)
    watermark()
  }, height = ex_plot_height)

  output$ex_small_note <- renderText({
    s <- ex_summary()
    if (is.null(s) || !(input$ex_metric %in% c("median", "avg"))) return("")
    if (any(s$n_priced > 0 & s$n_priced < small_n))
      paste0("Gray bars are based on fewer than ", small_n, " sales; treat them with caution.") else ""
  })

  output$ex_trend_title <- renderText({
    m <- c(median = "Median price", avg = "Average price", units = "Boats sold",
           value = "Total sales value")[[input$ex_metric]]
    paste0(m, " by month (current filters)")
  })

  output$ex_trend_plot <- renderPlot({
    d <- ex_data()
    if (nrow(d) == 0) { blank("No sales match these filters."); return() }
    w <- ex_window(); yms <- w[1]:w[2]
    met <- input$ex_metric
    f <- factor(d$ym, levels = yms); pr <- priced(d)
    y <- switch(met,
      units  = as.numeric(tapply(rep(1, nrow(d)), f, sum)),
      value  = as.numeric(tapply(d$price, f, sum)) / 1e6,
      avg    = as.numeric(tapply(d$price[pr], f[pr], mean)),
      median = as.numeric(tapply(d$price[pr], f[pr], median)))
    y[is.na(y)] <- 0
    par(mar = c(5, 6, 1, 1))
    bp <- barplot(y, names.arg = format(ym_to_date(yms), "%b %Y"), las = 2, border = NA,
                  col = if (met %in% c("median", "avg")) "#6aa84f" else "#4a6fa5",
                  cex.names = 0.75, yaxt = "n", ylim = c(0, max(y, 1) * 1.1))
    tk <- axTicks(2)
    axis(2, at = tk, las = 1, cex.axis = 0.8, labels = switch(met,
      units = comma(tk), value = paste0("$", tk, "M"),
      paste0("$", comma(tk))))
    watermark()
  })

  ex_table_df <- reactive({
    s <- ex_summary(); if (is.null(s)) return(NULL)
    lbl <- names(explore_dims)[explore_dims == input$ex_group]
    df <- data.frame(s$group, s$units, s$value, s$avg, s$median, s$n_priced,
                     check.names = FALSE, stringsAsFactors = FALSE)
    names(df) <- c(lbl, "Boats Sold", "Total Sales Value", "Average Price", "Median Price",
                   "Sales Used for Price Stats")
    df
  })

  output$ex_table <- renderTable({
    df <- ex_table_df(); if (is.null(df)) return(NULL)
    df[[2]] <- vapply(df[[2]], fmt_int, ""); df[[3]] <- vapply(df[[3]], fmt_money, "")
    df[[4]] <- vapply(df[[4]], fmt_money, ""); df[[5]] <- vapply(df[[5]], fmt_money, "")
    df[[6]] <- vapply(df[[6]], fmt_int, "")
    df
  }, striped = TRUE, bordered = TRUE, align = "lrrrrr", width = "100%")

  output$ex_download <- downloadHandler(
    filename = function() paste0("wa_boat_sales_", input$ex_group, "_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- ex_table_df()
      w <- ex_window()
      writeLines(c(
        paste0("# Washington Sea Grant recreational boat sales, ",
               format(ym_to_date(w[1]), "%b %Y"), " to ", format(ym_to_date(w[2]), "%b %Y")),
        "# Data source: WA Department of Licensing. Price statistics exclude $0 transactions unless noted."),
        file)
      if (!is.null(df)) suppressWarnings(write.table(df, file, sep = ",", row.names = FALSE,
                                                    append = TRUE, col.names = TRUE))
    }
  )

  vals <- reactive({
    yr <- as.integer(input$year)
    mo <- as.integer(input$month)

    cur  <- get_row(sales, yr, mo)
    prev <- get_row(sales, yr - 1, mo)

    units_now  <- if (is.null(cur))  NA else cur$units
    units_last <- if (is.null(prev)) NA else prev$units
    value_now  <- if (is.null(cur))  NA else cur$value
    value_last <- if (is.null(prev)) NA else prev$value

    list(
      units_now = units_now, units_last = units_last,
      units_chg = pct_change(units_now, units_last),
      value_now = value_now, value_last = value_last,
      value_chg = pct_change(value_now, value_last)
    )
  })

  output$caption <- renderText({
    paste0(month_names[as.integer(input$month)], " ", input$year,
           "  (vs. ", as.integer(input$year) - 1, ")")
  })

  # ---- Trends tab: monthly units & value (total of selected sources) over a range --
  # Snap the slider handles to month starts (shared by the series and the axis ceiling).
  date_window <- reactive({
    rng <- input$daterange
    c(as.Date(format(rng[1], "%Y-%m-01")), as.Date(format(rng[2], "%Y-%m-01")))
  })

  trend_labels <- function(dates) format(dates, "%b %Y")
  blank <- function(msg) { plot.new(); text(0.5, 0.5, msg, cex = 1.1) }

  # Build source x month matrices for the selected sources + prior-year totals.
  trend <- reactive({
    if (is.null(by_source)) return(NULL)
    sel <- input$src
    if (length(sel) == 0) return(list(empty = "none"))
    srcs <- source_levels[source_levels %in% sel]        # keep defined order/colors
    w <- date_window()

    d <- by_source[by_source$source %in% srcs, ]
    d$ym <- sprintf("%04d-%02d", d$year, d$month)
    d$date <- as.Date(paste0(d$ym, "-01"))
    dw <- d[d$date >= w[1] & d$date <= w[2], ]
    if (nrow(dw) == 0) return(list(empty = "nodata"))

    ym     <- sort(unique(dw$ym))
    months <- as.Date(paste0(ym, "-01"))
    umat <- tapply(dw$units, list(factor(dw$source, srcs), factor(dw$ym, ym)), sum)
    vmat <- tapply(dw$value, list(factor(dw$source, srcs), factor(dw$ym, ym)), sum)
    umat[is.na(umat)] <- 0; vmat[is.na(vmat)] <- 0

    # Prior-year total (selected sources), same month one year earlier.
    sel_u <- tapply(d$units, d$ym, sum); sel_v <- tapply(d$value, d$ym, sum)
    prev_key <- sprintf("%04d-%02d", as.integer(substr(ym, 1, 4)) - 1, as.integer(substr(ym, 6, 7)))
    list(empty = FALSE, months = months, srcs = srcs, umat = umat, vmat = vmat,
         prev_u = as.numeric(sel_u[prev_key]), prev_v = as.numeric(sel_v[prev_key]))
  })

  # Fixed y-axis ceilings from ALL-source totals (incl. prior year) in the window, so
  # deselecting a source shortens the stack instead of the panel rescaling to fill.
  axis_ceiling <- reactive({
    if (is.null(all_tot)) return(list(units = 1, value_m = 1))
    w <- date_window()
    d <- all_tot; d$date <- as.Date(paste0(d$ym, "-01"))
    win <- d[d$date >= w[1] & d$date <= w[2], ]
    if (nrow(win) == 0) return(list(units = 1, value_m = 1))
    pk <- sprintf("%04d-%02d", as.integer(substr(win$ym, 1, 4)) - 1, as.integer(substr(win$ym, 6, 7)))
    u_look <- setNames(d$units, d$ym); v_look <- setNames(d$value, d$ym)
    list(units   = max(c(win$units, u_look[pk]), 1, na.rm = TRUE),
         value_m = max(c(win$value, v_look[pk]), 1, na.rm = TRUE) / 1e6)
  })

  output$trend_units <- renderPlot({
    tr <- trend()
    if (is.null(tr)) { blank("No source data yet.\nRun prepare_data.R to generate boat_sales_by_source.csv."); return() }
    if (identical(tr$empty, "none"))   { blank("Select at least one source."); return() }
    if (identical(tr$empty, "nodata")) { blank("No sales for this selection."); return() }
    tot <- colSums(tr$umat)                         # total across selected sources
    par(mar = c(5, 5, 1, 1))
    bp <- barplot(tot, names.arg = trend_labels(tr$months), col = "#4a6fa5",
                  border = NA, las = 1, cex.names = 0.8,
                  ylab = "Units Sold", ylim = c(0, axis_ceiling()$units * 1.15))
    watermark()
    text(bp, tot, labels = format(tot, big.mark = ","), pos = 3, cex = 0.8, xpd = TRUE)
    if (isTRUE(input$show_prev))
      segments(bp - 0.45, tr$prev_u, bp + 0.45, tr$prev_u, col = "#111111", lwd = 2)
  })

  output$trend_value <- renderPlot({
    tr <- trend()
    if (is.null(tr)) { blank("No source data yet.\nRun prepare_data.R to generate boat_sales_by_source.csv."); return() }
    if (identical(tr$empty, "none"))   { blank("Select at least one source."); return() }
    if (identical(tr$empty, "nodata")) { blank("No sales for this selection."); return() }
    tot <- colSums(tr$vmat) / 1e6                   # total across selected sources, in $M
    par(mar = c(5, 6, 1, 1))
    bp <- barplot(tot, names.arg = trend_labels(tr$months), col = "#6aa84f",
                  border = NA, las = 1, cex.names = 0.8,
                  ylab = "Price", yaxt = "n", ylim = c(0, axis_ceiling()$value_m * 1.15))
    axis(2, at = axTicks(2), labels = paste0("$", axTicks(2), "M"), las = 1)
    watermark()
    text(bp, tot, labels = paste0("$", formatC(tot, format = "f", digits = 1), "M"),
         pos = 3, cex = 0.8, xpd = TRUE)
    if (isTRUE(input$show_prev))
      segments(bp - 0.45, tr$prev_v / 1e6, bp + 0.45, tr$prev_v / 1e6, col = "#111111", lwd = 2)
  })

  output$last_updated <- renderText({
    mtime <- file.info("boat_sales.csv")$mtime           # when prepare_data.R last wrote the file
    latest <- sales_ord[nrow(sales_ord), ]                 # newest month present in the data
    paste0("Data last updated ", format(mtime, "%b %d, %Y"),
           " · latest month in data: ", month_names[latest$month], " ", latest$year)
  })

  output$summary <- renderTable({
    v <- vals()
    data.frame(
      check.names = FALSE,
      " " = c("Boats Sold", "Sold Last Year", "Units Change (%)"),
      "Units" = c(fmt_int(v$units_now), fmt_int(v$units_last), fmt_pct(v$units_chg)),
      "  " = c("Sales Value", "Value Last Year", "Value Change (%)"),
      "Value" = c(fmt_money(v$value_now), fmt_money(v$value_last), fmt_pct(v$value_chg)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, align = "lrlr", width = "100%")

  # ---- Rolling 12-month tab ------------------------------------------------
  # End the R12 window on the month picked in the sidebar, but never past the
  # latest COMPLETE month (so it can't anchor on the still-filling one).
  # Quick pick links set the Rolling 12-Mo checkboxes.
  observeEvent(input$r12_pick_all, updateCheckboxGroupInput(session, "r12_src", selected = r12_presets$all))
  observeEvent(input$r12_pick_new, updateCheckboxGroupInput(session, "r12_src", selected = r12_presets$new))
  observeEvent(input$r12_pick_used_dealer,
               updateCheckboxGroupInput(session, "r12_src", selected = r12_presets$used_dealer))

  # R12 series for the selected sources (all sources uses the monthly totals file).
  r12 <- reactive({
    sel <- input$r12_src
    if (is.null(by_source) || is.null(sel) || setequal(sel, source_levels)) return(R12_all)
    d <- by_source[by_source$source %in% sel, ]
    if (nrow(d) == 0) d <- data.frame(year = sales_ord$year[1], month = sales_ord$month[1], units = 0, value = 0)
    m <- aggregate(cbind(units, value) ~ year + month, data = d, FUN = sum)
    make_r12(m[order(m$year, m$month), ])
  })

  r12_sources_label <- reactive({
    sel <- input$r12_src
    if (is.null(by_source) || is.null(sel) || setequal(sel, source_levels)) return("all sources")
    if (length(sel) == 0) return("no sources selected")
    paste(source_levels[source_levels %in% sel], collapse = ", ")
  })

  r12_end_i <- reactive({
    target <- as.Date(sprintf("%d-%02d-01", as.integer(input$year), as.integer(input$month)))
    idx <- match(target, R12_all$full$date)
    if (is.na(idx)) idx <- R12_all$end_i
    min(idx, R12_all$end_i)
  })

  output$r12_caption <- renderText({
    ed <- r12()$full$date[r12_end_i()]
    paste0("12 months ending ", format(ed, "%B %Y"), "  vs. the prior 12 months (",
           r12_sources_label(), ")")
  })

  output$r12_table <- renderTable({
    i <- r12_end_i()
    if (i < 12)
      return(data.frame(check.names = FALSE,
        "Note" = "Need 12 months of history ending on the selected month — pick a later month."))
    cu <- r12()$ru[i]; cv <- r12()$rv[i]
    has_prev <- i >= 24                       # need a full prior 12-month window
    pu <- if (has_prev) r12()$ru[i - 12] else NA
    pv <- if (has_prev) r12()$rv[i - 12] else NA
    df <- data.frame(
      check.names = FALSE, stringsAsFactors = FALSE,
      m  = c("Boats Sold", "Sales Value"),
      c  = c(fmt_int(cu),   fmt_money(cv)),
      p  = c(fmt_int(pu),   fmt_money(pv)),
      ch = c(fmt_pct(pct_change(cu, pu)), fmt_pct(pct_change(cv, pv)))
    )
    cur_lab <- paste0(format(r12()$full$date[i - 11], "%b %Y"), "–", format(r12()$full$date[i], "%b %Y"))
    prv_lab <- if (has_prev)
      paste0(format(r12()$full$date[i - 23], "%b %Y"), "–", format(r12()$full$date[i - 12], "%b %Y"))
    else "Prior 12 Mo"
    names(df) <- c(" ", cur_lab, prv_lab, "Change (%)")
    df
  }, striped = TRUE, bordered = TRUE, align = "lrrr", width = "100%")

  output$r12_units_plot <- renderPlot({
    if (r12()$end_i < 12) { blank("Not enough history for a 12-month rolling total."); return() }
    d <- r12()$full$date[12:r12()$end_i]; y <- r12()$ru[12:r12()$end_i]
    par(mar = c(4, 7, 1, 1))                          # wider left margin for the axis title
    plot(d, y, type = "l", lwd = 2, col = "#4a6fa5", xlab = "", ylab = "",
         las = 1, ylim = c(0, max(y) * 1.08))
    title(ylab = "R12 Units Sold", line = 5)          # push title clear of the numbers
    watermark()
    i <- r12_end_i()                                  # selected endpoint marker
    if (i >= 12) {
      md <- r12()$full$date[i]; my <- r12()$ru[i]
      abline(v = md, col = "#bbbbbb", lty = 2)
      points(md, my, pch = 19, col = "#4a6fa5")
      text(md, my, labels = format(round(my), big.mark = ","), pos = 2, cex = 0.85)
    }
  })

  output$r12_value_plot <- renderPlot({
    if (r12()$end_i < 12) { blank("Not enough history for a 12-month rolling total."); return() }
    d <- r12()$full$date[12:r12()$end_i]; y <- r12()$rv[12:r12()$end_i] / 1e6
    par(mar = c(4, 7, 1, 1))
    plot(d, y, type = "l", lwd = 2, col = "#6aa84f", xlab = "", ylab = "",
         las = 1, yaxt = "n", ylim = c(0, max(y) * 1.08))
    axis(2, at = axTicks(2), labels = paste0("$", axTicks(2), "M"), las = 1)
    title(ylab = "R12 Sales Value", line = 5)
    watermark()
    i <- r12_end_i()
    if (i >= 12) {
      md <- r12()$full$date[i]; my <- r12()$rv[i] / 1e6
      abline(v = md, col = "#bbbbbb", lty = 2)
      points(md, my, pch = 19, col = "#6aa84f")
      text(md, my, labels = paste0("$", formatC(my, format = "f", digits = 1), "M"),
           pos = 2, cex = 0.85)
    }
  })

  output$ytd_table <- renderTable({
    ed <- r12()$full$date[r12_end_i()]
    cy <- as.integer(format(ed, "%Y")); cm <- as.integer(format(ed, "%m"))
    mdat <- r12()$monthly
    ytd <- function(yr) {
      s <- mdat[mdat$year == yr & mdat$month <= cm, ]
      c(units = sum(s$units), value = sum(s$value))
    }
    cur <- ytd(cy); prv <- ytd(cy - 1)
    df <- data.frame(
      check.names = FALSE, stringsAsFactors = FALSE,
      m  = c("Boats Sold", "Sales Value"),
      c  = c(fmt_int(cur["units"]),  fmt_money(cur["value"])),
      p  = c(fmt_int(prv["units"]),  fmt_money(prv["value"])),
      ch = c(fmt_pct(pct_change(cur["units"], prv["units"])),
             fmt_pct(pct_change(cur["value"], prv["value"])))
    )
    names(df) <- c(paste0("Jan–", month_names[cm]), paste0("YTD ", cy),
                   paste0("YTD ", cy - 1), "Change (%)")
    df
  }, striped = TRUE, bordered = TRUE, align = "lrrr", width = "100%")
}

shinyApp(ui, server)
