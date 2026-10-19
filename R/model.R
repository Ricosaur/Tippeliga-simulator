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
#
# European congestion. A team that played a UEFA match within `euro_window`
# days before a league match is flagged for that match. Two extra parameters
# are estimated jointly with everything else:
#   euro_att  shift in the flagged team's own log expected goals
#   euro_def  shift in its opponent's log expected goals (goals conceded)
# The simulation applies them to remaining fixtures using the scheduled
# European dates, and they are part of the parameter-uncertainty draws.

suppressWarnings(suppressMessages({
  library(dplyr)
}))

# ----- European congestion flags ---------------------------------------------

# TRUE where `team` played a European match 1..window days before `date`.
euro_flags <- function(team, date, euro, window) {
  out <- logical(length(team))
  if (is.null(euro) || nrow(euro) == 0) return(out)
  for (t in intersect(unique(team), unique(euro$team))) {
    ed <- euro$date[euro$team == t]
    idx <- which(team == t)
    out[idx] <- vapply(date[idx], function(d) {
      gap <- as.numeric(d - ed)
      any(gap >= 1 & gap <= window)
    }, logical(1))
  }
  out
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
#
# euro: European schedule (date, team, ...) or NULL. If given and at least one
# played league match is flagged, the two congestion parameters are fitted.
fit_dixon_coles <- function(results, half_life_days = 180, ref_date = Sys.Date(),
                            euro = NULL, euro_window = 5) {
  played <- results %>% filter(played)
  if (nrow(played) < 20) stop("Not enough played matches to fit the model.")

  teams <- sort(unique(c(played$home, played$away)))
  nt <- length(teams)
  hi <- match(played$home, teams)
  ai <- match(played$away, teams)
  w  <- decay_weights(played$date, ref_date, half_life_days)

  eh <- as.numeric(euro_flags(played$home, played$date, euro, euro_window))
  ea <- as.numeric(euro_flags(played$away, played$date, euro, euro_window))
  use_euro <- sum(eh + ea) > 0

  # Params: [attack_free (nt-1), defence (nt), home_adv, rho, (euro_att,
  # euro_def)]. Attacks use a sum-to-zero constraint, so only nt-1 are free.
  init <- c(rep(0.1, nt - 1), rep(-0.1, nt), 0.25, -0.05,
            if (use_euro) c(0, 0))

  unpack <- function(p) {
    att_free <- p[1:(nt - 1)]
    att <- c(att_free, -sum(att_free))
    def <- p[nt:(2 * nt - 1)]
    list(att = att, def = def, hfa = p[2 * nt], rho = p[2 * nt + 1],
         euro_att = if (use_euro) p[2 * nt + 2] else 0,
         euro_def = if (use_euro) p[2 * nt + 3] else 0)
  }

  negll <- function(p) {
    pr <- unpack(p)
    lambda <- exp(pr$att[hi] - pr$def[ai] + pr$hfa +
                    pr$euro_att * eh + pr$euro_def * ea)
    mu     <- exp(pr$att[ai] - pr$def[hi] +
                    pr$euro_att * ea + pr$euro_def * eh)
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
    use_euro = use_euro,
    euro_att = pr$euro_att,
    euro_def = pr$euro_def,
    euro_se = if (use_euro && !is.null(cov))
      sqrt(diag(cov)[2 * nt + 2:3]) else c(NA_real_, NA_real_),
    euro_n = sum(eh + ea),     # flagged team-matches in the fit
    euro_window = euro_window,
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
# euro: European schedule; with a fit that estimated the congestion term,
# remaining fixtures (which need a date column) are flagged the same way.
simulate_season <- function(fit, results, remaining, n_sims = 10000,
                            max_goals = 8, seed = NULL,
                            param_uncertainty = TRUE, shrink = 0,
                            current_season = NULL, euro = NULL) {
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

  # Precompute fixture team indices once.
  rem <- remaining %>% filter(home %in% sim_teams, away %in% sim_teams)
  fh <- match(rem$home, sim_teams)
  fa <- match(rem$away, sim_teams)
  nf <- nrow(rem)

  # Congestion flags for remaining fixtures (0 if the term is not in the fit).
  use_euro <- isTRUE(fit$use_euro) && !is.null(euro) && "date" %in% names(rem)
  eh <- if (use_euro) as.numeric(euro_flags(rem$home, rem$date, euro, fit$euro_window)) else numeric(nf)
  ea <- if (use_euro) as.numeric(euro_flags(rem$away, rem$date, euro, fit$euro_window)) else numeric(nf)

  # Helper: given a parameter vector, return named attack/defence/hfa and
  # the congestion effects.
  ratings_from_par <- function(par) {
    pr <- fit$unpack(par)
    sh <- apply_shrinkage(pr$att, pr$def, shrink)
    list(att = setNames(sh$att, fit$teams),
         def = setNames(sh$def, fit$teams),
         hfa = pr$hfa, ea = pr$euro_att, ed = pr$euro_def)
  }

  # Base (point-estimate) ratings, used when not sampling.
  base_ratings <- ratings_from_par(fit$par)

  # All parameter draws up front (one row per simulation).
  par_draws <- if (can_sample) {
    MASS::mvrnorm(n_sims, mu = fit$par, Sigma = fit$cov)
  } else NULL

  # Team-by-fixture incidence matrices, so a whole round of results is
  # credited to the table with two matrix products instead of a loop.
  Mh <- matrix(0, nt, nf); Mh[cbind(fh, seq_len(nf))] <- 1
  Ma <- matrix(0, nt, nf); Ma[cbind(fa, seq_len(nf))] <- 1

  for (s in seq_len(n_sims)) {
    r <- if (can_sample) ratings_from_par(par_draws[s, ]) else base_ratings

    # Expected goals per remaining fixture under this draw.
    lh <- exp(r$att[rem$home] - r$def[rem$away] + r$hfa + r$ea * eh + r$ed * ea)
    la <- exp(r$att[rem$away] - r$def[rem$home] + r$ea * ea + r$ed * eh)

    hg <- rpois(nf, lh); ag <- rpois(nf, la)
    hp <- ifelse(hg > ag, 3, ifelse(hg == ag, 1, 0))
    ap <- ifelse(ag > hg, 3, ifelse(hg == ag, 1, 0))
    pts <- base$pts + as.vector(Mh %*% hp + Ma %*% ap)
    gf  <- base$gf  + as.vector(Mh %*% hg + Ma %*% ag)
    ga  <- base$ga  + as.vector(Mh %*% ag + Ma %*% hg)

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
       sampled = can_sample,
       euro_fixtures = rem[eh + ea > 0, , drop = FALSE] %>%
         mutate(home_flag = eh[eh + ea > 0] > 0, away_flag = ea[eh + ea > 0] > 0))
}
