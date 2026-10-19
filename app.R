# app.R
# Eliteserien title-race simulator.
#
# Reads the cached results and European schedule (from GitHub, local
# fallback), fits a time-weighted Dixon-Coles model with parameter
# uncertainty, ratings shrinkage and a European congestion term, runs a
# Monte Carlo over the remaining fixtures, and reports full finishing-position
# probabilities.
#
# Deploy to shinyapps.io with rsconnect::deployApp().

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(MASS)

source("R/fetch_data.R")
source("R/model.R")

# ----- Fixture derivation ----------------------------------------------------

# Remaining fixtures = current-season rows with no score yet. The date is
# kept so fixtures can be matched against the European schedule.
derive_remaining <- function(results, target_season = max(results$season)) {
  results %>%
    filter(season == target_season, !played) %>%
    dplyr::select(date, home, away)
}

# ----- Palette ---------------------------------------------------------------

COL_BG     <- "#0f1c2e"
COL_PANEL  <- "#16273d"
COL_LINE   <- "#23364f"
COL_INK    <- "#e8eef5"
COL_MUTED  <- "#8aa0b8"
COL_ACCENT <- "#f0a500"
COL_BAR    <- "#4a90c2"
COL_GOOD   <- "#3fb27f"
COL_BAD    <- "#d76b6b"

theme_pitch <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = COL_BG, color = NA),
      panel.background = element_rect(fill = COL_BG, color = NA),
      panel.grid.major = element_line(color = COL_LINE, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      text  = element_text(color = COL_INK),
      axis.text = element_text(color = COL_MUTED),
      legend.text = element_text(color = COL_MUTED),
      plot.title = element_text(face = "bold", size = 15)
    )
}

# Data older than this (hours since the last fetch) is flagged as stale.
STALE_HOURS <- 36

fmt_oslo <- function(t, fmt = "%d %b %Y, %H:%M") format(t, fmt, tz = "Europe/Oslo")

# ----- UI --------------------------------------------------------------------

css <- sprintf("
  body { background:%1$s; color:%2$s; font-family:'Inter','Helvetica Neue',sans-serif; }
  .title-bar { font-weight:800; font-size:26px; letter-spacing:-0.5px; padding:18px 0 2px; }
  .subtitle { color:%3$s; font-size:14px; margin-bottom:10px; }
  .status-row { display:flex; flex-wrap:wrap; gap:8px; margin-bottom:18px; }
  .pill { background:%4$s; border-radius:999px; padding:4px 12px; font-size:13px; color:%3$s; }
  .pill b { color:%2$s; font-weight:600; }
  .pill .dot { display:inline-block; width:8px; height:8px; border-radius:50%%; margin-right:6px; }
  .well { background:transparent; border:none; box-shadow:none; padding:0; }
  .control-card { background:%4$s; border-radius:10px; padding:16px 18px; margin-bottom:16px; }
  .card-title { font-size:12px; text-transform:uppercase; letter-spacing:0.8px; color:%3$s; margin-bottom:10px; }
  .stat-note, .help-block { color:%3$s; font-size:12px; }
  details summary { cursor:pointer; color:%3$s; font-size:13px; margin:4px 0 12px; list-style:none; }
  details summary::-webkit-details-marker { display:none; }
  details summary::before { content:'\\25B8'; display:inline-block; width:14px; }
  details[open] summary::before { content:'\\25BE'; }
  details[open] summary { margin-bottom:14px; }
  .model-line { font-size:13px; margin-bottom:6px; color:%3$s; }
  .model-line b { color:%2$s; font-weight:600; }
  .irs-bar,.irs-single { background:%5$s !important; border-color:%5$s !important; }
  .irs-min,.irs-max { background:%6$s !important; color:%3$s !important; }
  .nav-tabs { border-bottom:1px solid %6$s; }
  .nav-tabs > li > a { color:%3$s; border:none !important; background:transparent !important; }
  .nav-tabs > li > a:hover { color:%2$s; }
  .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
    color:%2$s; background:transparent !important; border-bottom:2px solid %5$s !important; }
  table.dataTable, table.dataTable th, table.dataTable td { color:%2$s; border-color:%6$s !important; }
  table.dataTable tbody tr { background-color:transparent !important; }
  table.dataTable.stripe tbody tr.odd, table.dataTable.display tbody tr.odd { background-color:%4$s !important; }
  table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover { background-color:%6$s !important; }
  table.dataTable tbody tr > .sorting_1 { background-color:transparent !important; }
  .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate,
  .dataTables_wrapper .dataTables_paginate .paginate_button { color:%3$s !important; }
  caption { color:%3$s; }
  .recalculating { opacity:0.45; transition:opacity 0.2s; }
", COL_BG, COL_INK, COL_MUTED, COL_PANEL, COL_ACCENT, COL_LINE)

ui <- fluidPage(
  tags$head(tags$style(HTML(css))),

  div(class = "title-bar", "Eliteserien season simulator"),
  div(class = "subtitle",
      "Time-weighted Dixon-Coles with parameter uncertainty and European congestion. ",
      "Monte Carlo over remaining fixtures."),
  uiOutput("status_pills"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "control-card",
        sliderInput("half_life", "Recency half-life (days)",
                    min = 30, max = 540, value = 180, step = 30, ticks = FALSE),
        helpText("Lower = recent form dominates. Higher leans on last season."),
        sliderInput("n_sims", "Simulations",
                    min = 2000, max = 20000, value = 4000, step = 1000, ticks = FALSE),
        tags$details(
          tags$summary("Advanced settings"),
          sliderInput("shrink", "Ratings shrinkage",
                      min = 0, max = 0.6, value = 0.1, step = 0.05, ticks = FALSE),
          helpText("Pulls team ratings toward the league average. Higher = more ",
                   "cautious, useful early in the season."),
          checkboxInput("param_unc", "Parameter uncertainty", value = TRUE),
          helpText("Accounts for ratings being estimated from limited data, so ",
                   "strong-but-unlucky teams don't show a false 0% chance."),
          checkboxInput("euro", "European congestion", value = TRUE),
          sliderInput("euro_window", "Days after a European match",
                      min = 2, max = 7, value = 5, step = 1, ticks = FALSE),
          helpText("League matches this many days after a UEFA match are ",
                   "flagged; the effect is estimated from past matches.")
        ),
        actionButton("run", "Run simulation",
                     style = sprintf("background:%s;color:%s;border:none;
                                      font-weight:700;width:100%%;margin-top:6px;",
                                     COL_ACCENT, COL_BG))
      ),
      div(class = "control-card",
        div(class = "card-title", "Model fit"),
        uiOutput("fit_status")
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Title & Europe", br(), plotOutput("multi_plot", height = "480px")),
        tabPanel("Position heatmap", br(), plotOutput("heatmap", height = "560px"),
                 div(class = "stat-note", style = "padding:8px;",
                     "Probability each team finishes in each position. ",
                     "The single most complete view of the simulation.")),
        tabPanel("Full table", br(), DTOutput("prob_table")),
        tabPanel("Team strengths", br(), plotOutput("strength_plot", height = "540px"),
                 div(class = "stat-note", style = "padding:8px;",
                     "Attack and defence ratings on the model's log scale, league-centered.")),
        tabPanel("European schedule", br(),
                 div(class = "stat-note", style = "padding:0 0 8px;",
                     "Upcoming UEFA matches for Eliteserien clubs, and the league ",
                     "fixtures the model treats as congested."),
                 DTOutput("euro_table"), br(), DTOutput("euro_fixtures"))
      )
    )
  )
)

# ----- Server ----------------------------------------------------------------

server <- function(input, output, session) {

  results <- reactive({ read_cache_remote() })
  europe  <- reactive({ read_europe_remote() })
  updated <- reactive({ read_last_updated() })

  # Header: when the data was last fetched, the latest result in it, and
  # where the season stands.
  output$status_pills <- renderUI({
    r <- results(); cur <- max(r$season)
    up <- updated()
    np <- sum(r$played & r$season == cur)
    nr <- sum(!r$played & r$season == cur)
    last <- max(r$date[r$played])

    age_h <- if (is.null(up)) Inf else as.numeric(difftime(Sys.time(), up, units = "hours"))
    fresh <- age_h <= STALE_HOURS
    dot <- span(class = "dot", style = sprintf("background:%s;", if (fresh) COL_GOOD else COL_ACCENT))
    upd_txt <- if (is.null(up)) "unknown" else fmt_oslo(up)

    div(class = "status-row",
      span(class = "pill", dot, "Data updated ", tags$b(upd_txt),
           if (!fresh && !is.null(up)) " (stale)"),
      span(class = "pill", "Latest result ", tags$b(format(last, "%d %b"))),
      span(class = "pill", tags$b(cur), " season: ", tags$b(np), " played, ",
           tags$b(nr), " remaining")
    )
  })

  sim <- eventReactive(input$run, {
    r <- results(); cur <- max(r$season)
    rem <- derive_remaining(r, cur)
    validate(need(nrow(rem) > 0,
                  sprintf("The %d season has no remaining fixtures to simulate.", cur)))
    eu <- if (isTRUE(input$euro)) europe() else NULL
    fit <- fit_dixon_coles(r, half_life_days = input$half_life,
                           euro = eu, euro_window = input$euro_window)
    res <- simulate_season(fit, r, rem, n_sims = input$n_sims, seed = 1,
                           param_uncertainty = input$param_unc,
                           shrink = input$shrink, current_season = cur,
                           euro = eu)
    list(fit = fit, res = res, n_rem = nrow(rem))
  }, ignoreNULL = FALSE)

  output$fit_status <- renderUI({
    s <- sim(); f <- s$fit
    line <- function(...) div(class = "model-line", ...)
    tagList(
      line("Home advantage ", tags$b(sprintf("x%.2f", exp(f$home_adv))), " goals"),
      line("Low-score correction (rho) ", tags$b(sprintf("%.3f", f$rho))),
      if (f$use_euro) tagList(
        line("After Europe, own goals ",
             tags$b(sprintf("x%.2f", exp(f$euro_att))),
             sprintf(" (SE %.2f)", f$euro_se[1])),
        line("After Europe, conceded ",
             tags$b(sprintf("x%.2f", exp(f$euro_def))),
             sprintf(" (SE %.2f)", f$euro_se[2])),
        line(tags$b(f$euro_n), " past and ", tags$b(nrow(s$res$euro_fixtures)),
             " remaining matches flagged")
      ) else line("European congestion off"),
      if (!s$res$sampled && input$param_unc)
        line("Uncertainty unavailable for this fit; using point estimates.")
    )
  })

  # Title / top-3 / Europe (top 4) grouped bars.
  output$multi_plot <- renderPlot({
    d <- sim()$res$summary %>%
      filter(top3 > 0.01) %>%
      transmute(team, Title = title, `Top 3` = top3, `Europe (top 4)` = europe) %>%
      pivot_longer(-team, names_to = "metric", values_to = "p") %>%
      mutate(metric = factor(metric, levels = c("Title","Top 3","Europe (top 4)")),
             team = reorder(team, p, max))
    ggplot(d, aes(team, p, fill = metric)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      coord_flip() +
      scale_y_continuous(labels = scales::percent,
                         expand = expansion(mult = c(0, 0.05))) +
      scale_fill_manual(values = c("Title" = COL_ACCENT, "Top 3" = COL_BAR,
                                   "Europe (top 4)" = COL_GOOD)) +
      labs(title = "Probability of title, top 3, and a European place",
           x = NULL, y = NULL, fill = NULL) +
      theme_pitch() + theme(legend.position = "top")
  })

  # Position heatmap, with the probability printed in cells of 5% or more.
  output$heatmap <- renderPlot({
    res <- sim()$res
    ord <- res$summary$team[order(res$summary$exp_pos)]
    m <- as.data.frame(res$position)
    m$team <- rownames(res$position)
    d <- m %>% pivot_longer(-team, names_to = "pos", values_to = "p") %>%
      mutate(pos = as.integer(gsub("pos", "", pos)),
             team = factor(team, levels = rev(ord)),
             lab = ifelse(p >= 0.05, sprintf("%.0f", 100 * p), ""))
    ggplot(d, aes(pos, team, fill = p)) +
      geom_tile(color = COL_BG) +
      geom_text(aes(label = lab, color = p > 0.5), size = 3.3, show.legend = FALSE) +
      scale_color_manual(values = c(`TRUE` = COL_BG, `FALSE` = COL_INK)) +
      scale_fill_gradient(low = COL_PANEL, high = COL_ACCENT,
                          labels = scales::percent) +
      scale_x_continuous(breaks = seq_len(length(ord)), expand = c(0, 0)) +
      labs(title = "Finishing-position probabilities (%)",
           x = "Final position", y = NULL, fill = NULL) +
      theme_pitch() + theme(panel.grid = element_blank())
  })

  output$prob_table <- renderDT({
    d <- sim()$res$summary %>%
      transmute(Team = team,
                Title = title, `Top 3` = top3, `Europe` = europe, `Top 6` = top6,
                `Rel. playoff` = rel_playoff, Relegation = relegation,
                `Exp. pts` = round(exp_pts, 1), `Exp. pos` = round(exp_pos, 1))
    datatable(d, rownames = FALSE, style = "default", class = "display compact",
              options = list(pageLength = 16, dom = "t")) %>%
      formatPercentage(c("Title","Top 3","Europe","Top 6","Rel. playoff","Relegation"), 1)
  })

  output$strength_plot <- renderPlot({
    f <- sim()$fit
    sim_teams <- sim()$res$teams
    d <- data.frame(team = f$teams, attack = f$attack, defence = f$defence) %>%
      filter(team %in% sim_teams)
    ggplot(d, aes(attack, defence, label = team)) +
      geom_hline(yintercept = 0, color = COL_LINE) +
      geom_vline(xintercept = 0, color = COL_LINE) +
      geom_point(color = COL_BAR, size = 3) +
      geom_text(vjust = -0.9, color = COL_INK, size = 3.5) +
      labs(title = "Attack vs defence",
           x = "Attack (higher = scores more)",
           y = "Defence (higher = concedes less)") +
      theme_pitch()
  })

  output$euro_table <- renderDT({
    e <- europe()
    validate(need(!is.null(e), "No European schedule in the cache."))
    d <- e %>% filter(!played) %>% arrange(date) %>%
      transmute(Date = date, Team = team,
                Opponent = paste0(opponent, ifelse(venue == "home", " (H)", " (A)")),
                Competition = competition)
    datatable(d, rownames = FALSE, style = "default", class = "display compact",
              caption = "Upcoming European matches",
              options = list(pageLength = 20, dom = "tp"))
  })

  output$euro_fixtures <- renderDT({
    s <- sim()
    validate(need(s$fit$use_euro, "European congestion is off."))
    d <- s$res$euro_fixtures %>% arrange(date) %>%
      transmute(Date = date, Home = home, Away = away,
                Congested = case_when(home_flag & away_flag ~ "Both",
                                      home_flag ~ home, TRUE ~ away))
    datatable(d, rownames = FALSE, style = "default", class = "display compact",
              caption = "Remaining league fixtures flagged as congested",
              options = list(pageLength = 20, dom = "tp"))
  })
}

shinyApp(ui, server)
