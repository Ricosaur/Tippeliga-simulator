# app.R
# Eliteserien title-race simulator.
#
# Reads the cached results CSV (from GitHub, local fallback), fits a time-
# weighted Dixon-Coles model with parameter uncertainty and optional ratings
# shrinkage, runs a Monte Carlo over the remaining fixtures, and reports full
# finishing-position probabilities.
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

# Remaining fixtures = current-season rows with no score yet.
derive_remaining <- function(results, target_season = max(results$season)) {
  results %>%
    filter(season == target_season, !played) %>%
    dplyr::select(date, home, away)
}

# ----- Palette ---------------------------------------------------------------

COL_BG     <- "#0f1c2e"
COL_PANEL  <- "#16273d"
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
      panel.grid.major = element_line(color = "#23364f", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      text  = element_text(color = COL_INK),
      axis.text = element_text(color = COL_MUTED),
      legend.text = element_text(color = COL_MUTED),
      plot.title = element_text(face = "bold", size = 15)
    )
}

# ----- UI --------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML(sprintf("
    body { background:%s; color:%s; font-family:'Inter','Helvetica Neue',sans-serif; }
    .title-bar { font-weight:800; font-size:26px; letter-spacing:-0.5px; padding:18px 0 2px; }
    .subtitle { color:%s; font-size:14px; margin-bottom:18px; }
    .control-card { background:%s; border-radius:10px; padding:16px 18px; margin-bottom:16px; }
    .stat-note { color:%s; font-size:12px; }
    .irs-bar,.irs-single { background:%s !important; border-color:%s !important; }
    table.dataTable { color:%s; }
  ", COL_BG, COL_INK, COL_MUTED, COL_PANEL, COL_MUTED, COL_ACCENT, COL_ACCENT, COL_INK)))),

  div(class = "title-bar", "Eliteserien season simulator"),
  div(class = "subtitle",
      "Time-weighted Dixon-Coles with parameter uncertainty. Monte Carlo over remaining fixtures."),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "control-card",
        sliderInput("half_life", "Recency half-life (days)",
                    min = 30, max = 540, value = 180, step = 30),
        helpText(class = "stat-note",
          "Lower = recent form dominates. Higher leans on last season."),
        br(),
        sliderInput("shrink", "Ratings shrinkage",
                    min = 0, max = 0.6, value = 0.1, step = 0.05),
        helpText(class = "stat-note",
          "Pulls team ratings toward the league average. Higher = more cautious, ",
          "useful early in the season."),
        br(),
        checkboxInput("param_unc", "Parameter uncertainty", value = TRUE),
        helpText(class = "stat-note",
          "Accounts for ratings being estimated from limited data. Keeps strong-",
          "but-unlucky teams from showing a false 0% chance."),
        br(),
        sliderInput("n_sims", "Simulations",
                    min = 2000, max = 20000, value = 4000, step = 1000),
        br(),
        sliderInput("fatigue_mult", "European fatigue penalty",
                    min = 0.75, max = 1.0, value = 0.92, step = 0.01),
        helpText(class = "stat-note",
          "Multiplies expected goals for a team that played a European match in the ",
          "4 days prior. 1.0 = no effect. Applies to fitting and simulation."),
        br(),
        actionButton("run", "Run simulation",
                     style = sprintf("background:%s;color:#0f1c2e;border:none;
                                      font-weight:700;width:100%%;", COL_ACCENT))
      ),
      div(class = "control-card",
        textOutput("data_status"),
        br(),
        textOutput("fit_status")
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Title & Europe", br(), plotOutput("multi_plot", height = "480px")),
        tabPanel("Position heatmap", br(), plotOutput("heatmap", height = "560px"),
                 div(class = "stat-note", style = "padding:8px;",
                     "Probability each team finishes in each position. Darker = more likely. ",
                     "The single most complete view of the simulation.")),
        tabPanel("Full table", br(), DTOutput("prob_table")),
        tabPanel("Team strengths", br(), plotOutput("strength_plot", height = "540px"),
                 div(class = "stat-note", style = "padding:8px;",
                     "Attack and defence ratings on the model's log scale, league-centered."))
      )
    )
  )
)

# ----- Server ----------------------------------------------------------------

server <- function(input, output, session) {

  results       <- reactive({ read_cache_remote() })
  euro_fixtures <- reactive({ read_european_fixtures() })

  output$data_status <- renderText({
    r <- results(); cur <- max(r$season)
    np <- sum(r$played & r$season == cur)
    nr <- sum(!r$played & r$season == cur)
    sprintf("Cache: %d matches. %d season: %d played, %d remaining.",
            nrow(r), cur, np, nr)
  })

  sim <- eventReactive(input$run, {
    r <- results(); cur <- max(r$season)
    euro <- euro_fixtures()
    fit <- fit_dixon_coles(r, half_life_days = input$half_life,
                           euro_dates = euro, fatigue_mult = input$fatigue_mult)
    rem <- derive_remaining(r, cur)
    res <- simulate_season(fit, r, rem, n_sims = input$n_sims, seed = 1,
                           param_uncertainty = input$param_unc,
                           shrink = input$shrink, current_season = cur,
                           euro_dates = euro, fatigue_mult = input$fatigue_mult)
    list(fit = fit, res = res, n_rem = nrow(rem))
  }, ignoreNULL = FALSE)

  output$fit_status <- renderText({
    s <- sim()
    sprintf("Home advantage %.2f (log goals). rho %.3f. %d fixtures remaining.%s",
            s$fit$home_adv, s$fit$rho, s$n_rem,
            if (!s$res$sampled && input$param_unc)
              " (uncertainty unavailable for this fit; using point estimates.)" else "")
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

  # Position heatmap.
  output$heatmap <- renderPlot({
    res <- sim()$res
    ord <- res$summary$team[order(res$summary$exp_pos)]
    m <- as.data.frame(res$position)
    m$team <- rownames(res$position)
    d <- m %>% pivot_longer(-team, names_to = "pos", values_to = "p") %>%
      mutate(pos = as.integer(gsub("pos", "", pos)),
             team = factor(team, levels = rev(ord)))
    ggplot(d, aes(pos, team, fill = p)) +
      geom_tile(color = COL_BG) +
      scale_fill_gradient(low = COL_PANEL, high = COL_ACCENT,
                          labels = scales::percent) +
      scale_x_continuous(breaks = seq_len(length(ord)), expand = c(0, 0)) +
      labs(title = "Finishing-position probabilities",
           x = "Final position", y = NULL, fill = NULL) +
      theme_pitch() + theme(panel.grid = element_blank())
  })

  output$prob_table <- renderDT({
    d <- sim()$res$summary %>%
      transmute(Team = team,
                Title = title, `Top 3` = top3, `Europe` = europe, `Top 6` = top6,
                `Rel. playoff` = rel_playoff, Relegation = relegation,
                `Exp. pts` = round(exp_pts, 1), `Exp. pos` = round(exp_pos, 1))
    datatable(d, rownames = FALSE, options = list(pageLength = 16, dom = "t")) %>%
      formatPercentage(c("Title","Top 3","Europe","Top 6","Rel. playoff","Relegation"), 1)
  })

  output$strength_plot <- renderPlot({
    f <- sim()$fit
    sim_teams <- sim()$res$teams
    d <- data.frame(team = f$teams, attack = f$attack, defence = f$defence) %>%
      filter(team %in% sim_teams)
    ggplot(d, aes(attack, defence, label = team)) +
      geom_hline(yintercept = 0, color = "#23364f") +
      geom_vline(xintercept = 0, color = "#23364f") +
      geom_point(color = COL_BAR, size = 3) +
      geom_text(vjust = -0.9, color = COL_INK, size = 3.5) +
      labs(title = "Attack vs defence",
           x = "Attack (higher = scores more)",
           y = "Defence (higher = concedes less)") +
      theme_pitch()
  })
}

shinyApp(ui, server)
