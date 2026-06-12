# model.R
# Time-weighted Dixon-Coles fit + Monte Carlo season simulation, with
# parameter uncertainty and ratings shrinkage.
#
# Model: each team has an attack and defence parameter. Home expected goals =
# exp(attack_home - defence_away + home_adv); away = exp(attack_away -
# defence_home). Goals are Poisson with the Dixon-Coles tau() correction for
# the four low-score outcomes. Matches are weighted by recency (exponential
# decay, tunable half-life).
#
# Two features that keep early-season probabilities honest:
#   1. Parameter uncertainty. Ratings are estimated from limited data, so each
#      simulation draws a fresh set of team ratings from the fit's sampling
#      distribution (multivariate normal via the inverse Hessian). This stops
#      the model treating noisy estimates as certain, and gives strong-but-
#      unlucky teams a realistic tail instead of a flat 0%.
#   2. Shrinkage. Attack/defence ratings are pulled toward the league average
#      by a tunable factor, which tempers extreme ratings when the sample is
#      small. shrink = 0 is the raw fit; shrink = 1 makes every team average.

suppressWarnings(suppressMessages({
  library(dplyr)
}))

# ----- European fatigue ------------------------------------------------------

# Returns a logical vector: TRUE where the team played a European game in the
# `window` days strictly before each match date. Precomputed outside negll()
# and the sim loop so it doesn't re-run on every optimizer call or iteration.
flag_fatigued <- function(teams, dates, euro_dates, window) {
  if (is.null(euro_dates) || nrow(euro_dates) == 0)
    return(rep(FALSE, length(teams)))
  vapply(seq_along(teams), function(k) {
    d <- dates[k]; t <- teams[k]
    any(euro_dates$team == t &
          euro_dates$date >= (d - window) &
          euro_dates$date <  d)
  }, logical(1))
}

# ----- Recency weights -------------------------------------------------------

decay_weights <- function(dates, ref_date, half_life_days) {
  age <- as.numeric(difftime(ref_date, dates, units = "days"))
  age[age < 0] <- 0
  0.5 ^ (age / half_life_days)
}

# ----- Dixon-Coles low-score correction --------------------------------------

dc_tau <- function(hg, ag, lambda, mu, rho) {
  out <- rep(1, length(hg))
  i <- hg == 0 & ag == 0; out[i] <- 1 - lambda[i] * mu[i] * rho
  i <- hg == 0 & ag == 1; out[i] <- 1 + lambda[i] * rho
  i <- hg == 1 & ag == 0; out[i] <- 1 + mu[i] * rho
  i <- hg == 1 & ag == 1; out[i] <- 1 - rho
  pmax(out, 1e-10)
}

# ----- Fit -------------------------------------------------------------------

# Returns attack/defence per team, home advantage, rho, and the covariance of
# the parameter estimates (used for uncertainty sampling). hessian = TRUE asks
# optim for the Hessian at the optimum; its inverse is the parameter covariance.
fit_dixon_coles <- function(results, half_life_days = 180, ref_date = Sys.Date(),
                            euro_dates = NULL, fatigue_mult = 0.92,
                            fatigue_window = 4L) {
  played <- results %>% filter(played)
  if (nrow(played) < 20) stop("Not enough played matches to fit the model.")

  teams <- sort(unique(c(played$home, played$away)))
  nt <- length(teams)
  hi <- match(played$home, teams)
  ai <- match(played$away, teams)
  w  <- decay_weights(played$date, ref_date, half_life_days)

  # Params: [attack_free (nt-1), defence (nt), home_adv, rho]. Attacks use a
  # sum-to-zero constraint, so only nt-1 are free.
  init <- c(rep(0.1, nt - 1), rep(-0.1, nt), 0.25, -0.05)

  unpack <- function(p) {
    att_free <- p[1:(nt - 1)]
    att <- c(att_free, -sum(att_free))
    def <- p[nt:(2 * nt - 1)]
    list(att = att, def = def, hfa = p[2 * nt], rho = p[2 * nt + 1])
  }

  # Fatigue multipliers: precomputed once, captured by the negll closure.
  fat_h <- ifelse(flag_fatigued(played$home, played$date, euro_dates, fatigue_window),
                  fatigue_mult, 1)
  fat_a <- ifelse(flag_fatigued(played$away, played$date, euro_dates, fatigue_window),
                  fatigue_mult, 1)

  negll <- function(p) {
    pr <- unpack(p)
    lambda <- exp(pr$att[hi] - pr$def[ai] + pr$hfa) * fat_h
    mu     <- exp(pr$att[ai] - pr$def[hi])            * fat_a
    tau <- dc_tau(played$hg, played$ag, lambda, mu, pr$rho)
    ll <- log(tau) + dpois(played$hg, lambda, log = TRUE) +
      dpois(played$ag, mu, log = TRUE)
    -sum(w * ll)
  }

  opt <- optim(init, negll, method = "BFGS",
               hessian = TRUE, control = list(maxit = 700, reltol = 1e-10))

  # Parameter covariance from the inverse Hessian. Guard against a non-
  # invertible or non-positive-definite Hessian (can happen with sparse data).
  cov <- tryCatch(solve(opt$hessian), error = function(e) NULL)
  if (!is.null(cov)) {
    # Symmetrise and floor tiny negative eigenvalues for a valid covariance.
    cov <- (cov + t(cov)) / 2
    ev <- eigen(cov, symmetric = TRUE)
    ev$values[ev$values < 1e-10] <- 1e-10
    cov <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)
  }

  pr <- unpack(opt$par)
  list(
    teams = teams,
    par = opt$par,             # raw parameter vector (for uncertainty draws)
    cov = cov,                 # parameter covariance, or NULL
    unpack = unpack,
    nt = nt,
    attack = setNames(pr$att, teams),
    defence = setNames(pr$def, teams),
    home_adv = pr$hfa,
    rho = pr$rho,
    converged = opt$convergence == 0,
    ref_date = ref_date,
    half_life_days = half_life_days
  )
}

# Apply shrinkage: pull attack and defence toward 0 (the league mean) by factor
# `shrink` in [0, 1]. Home advantage and rho are left alone.
apply_shrinkage <- function(att, def, shrink) {
  list(att = att * (1 - shrink), def = def * (1 - shrink))
}

# ----- Standings -------------------------------------------------------------

current_standings <- function(results, teams, season = NULL) {
  played <- results %>% filter(played)
  if (!is.null(season)) played <- played %>% filter(season == !!season)
  tab <- data.frame(team = teams, pts = 0L, gf = 0L, ga = 0L,
                    stringsAsFactors = FALSE)
  for (k in seq_len(nrow(played))) {
    h <- played$home[k]; a <- played$away[k]
    if (!(h %in% teams) || !(a %in% teams)) next
    hg <- played$hg[k]; ag <- played$ag[k]
    rh <- match(h, tab$team); ra <- match(a, tab$team)
    if (hg > ag) tab$pts[rh] <- tab$pts[rh] + 3
    else if (hg < ag) tab$pts[ra] <- tab$pts[ra] + 3
    else { tab$pts[rh] <- tab$pts[rh] + 1; tab$pts[ra] <- tab$pts[ra] + 1 }
    tab$gf[rh] <- tab$gf[rh] + hg; tab$ga[rh] <- tab$ga[rh] + ag
    tab$gf[ra] <- tab$gf[ra] + ag; tab$ga[ra] <- tab$ga[ra] + hg
  }
  tab
}

# ----- Simulation ------------------------------------------------------------

# Monte Carlo over remaining fixtures. Returns:
#   $summary  one row per team: title, top3, top6, europe (top4), rel_playoff
#             (14th), relegation (15-16th), exp_pts, exp_pos
#   $position team x finishing-position probability matrix (for the heatmap)
#
# param_uncertainty: if TRUE and the fit has a covariance, draw a fresh set of
# team ratings each simulation. shrink: ratings shrinkage in [0, 1].
simulate_season <- function(fit, results, remaining, n_sims = 10000,
                            max_goals = 8, seed = NULL,
                            param_uncertainty = TRUE, shrink = 0,
                            current_season = NULL, euro_dates = NULL,
                            fatigue_mult = 0.92, fatigue_window = 4L) {
  if (!is.null(seed)) set.seed(seed)

  # The league being simulated is the set of teams with remaining fixtures
  # (i.e. the current season's clubs), not every team that ever appears.
  sim_teams <- sort(unique(c(remaining$home, remaining$away)))
  nt <- length(sim_teams)
  base <- current_standings(results, sim_teams, season = current_season)

  gh <- 0:max_goals
  can_sample <- param_uncertainty && !is.null(fit$cov)

  # Position counts and per-team accumulators.
  pos_counts <- matrix(0, nrow = nt, ncol = nt,
                       dimnames = list(sim_teams, paste0("pos", seq_len(nt))))
  pts_sum <- setNames(numeric(nt), sim_teams)
  pos_sum <- setNames(numeric(nt), sim_teams)

  # Precompute fixture team indices and fatigue multipliers once (not per-sim).
  rem <- remaining %>% filter(home %in% sim_teams, away %in% sim_teams)
  fh <- match(rem$home, sim_teams)
  fa <- match(rem$away, sim_teams)
  nf <- nrow(rem)

  h_mult <- ifelse(flag_fatigued(rem$home, rem$date, euro_dates, fatigue_window),
                   fatigue_mult, 1)
  a_mult <- ifelse(flag_fatigued(rem$away, rem$date, euro_dates, fatigue_window),
                   fatigue_mult, 1)

  # Helper: given a parameter vector, return named attack/defence/hfa.
  ratings_from_par <- function(par) {
    pr <- fit$unpack(par)
    sh <- apply_shrinkage(pr$att, pr$def, shrink)
    list(att = setNames(sh$att, fit$teams),
         def = setNames(sh$def, fit$teams),
         hfa = pr$hfa)
  }

  # Base (point-estimate) ratings, used when not sampling.
  base_ratings <- ratings_from_par(fit$par)

  for (s in seq_len(n_sims)) {
    r <- if (can_sample) {
      par_draw <- MASS::mvrnorm(1, mu = fit$par, Sigma = fit$cov)
      ratings_from_par(par_draw)
    } else base_ratings

    # Expected goals per remaining fixture under this draw, with fatigue applied.
    lh <- exp(r$att[rem$home] - r$def[rem$away] + r$hfa) * h_mult
    la <- exp(r$att[rem$away] - r$def[rem$home])          * a_mult

    pts <- base$pts; gf <- base$gf; ga <- base$ga
    for (k in seq_len(nf)) {
      hg <- rpois(1, lh[k]); ag <- rpois(1, la[k])
      h <- fh[k]; a <- fa[k]
      if (hg > ag) pts[h] <- pts[h] + 3
      else if (hg < ag) pts[a] <- pts[a] + 3
      else { pts[h] <- pts[h] + 1; pts[a] <- pts[a] + 1 }
      gf[h] <- gf[h] + hg; ga[h] <- ga[h] + ag
      gf[a] <- gf[a] + ag; ga[a] <- ga[a] + hg
    }

    gd <- gf - ga
    ord <- order(-pts, -gd, -gf, runif(nt))   # pts, GD, GF, random tiebreak
    finish <- integer(nt); finish[ord] <- seq_len(nt)
    pos_counts[cbind(seq_len(nt), finish)] <-
      pos_counts[cbind(seq_len(nt), finish)] + 1
    pts_sum <- pts_sum + pts
    pos_sum <- pos_sum + finish
  }

  prob <- pos_counts / n_sims
  summary <- data.frame(
    team        = sim_teams,
    title       = prob[, 1],
    top3        = rowSums(prob[, 1:3, drop = FALSE]),
    europe      = rowSums(prob[, 1:4, drop = FALSE]),   # top 4
    top6        = rowSums(prob[, 1:6, drop = FALSE]),
    rel_playoff = prob[, nt - 2],                        # 14th
    relegation  = rowSums(prob[, (nt - 1):nt, drop = FALSE]), # 15-16th
    exp_pts     = as.numeric(pts_sum / n_sims),
    exp_pos     = as.numeric(pos_sum / n_sims),
    stringsAsFactors = FALSE
  )
  summary <- summary[order(-summary$title, summary$exp_pos), ]

  list(summary = summary, position = prob, teams = sim_teams,
       sampled = can_sample)
}
