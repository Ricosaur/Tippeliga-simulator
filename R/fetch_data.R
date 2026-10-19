# fetch_data.R
# Data layer for the Eliteserien simulator.
#
# Produces two tidy tables:
#   results_cache.csv  league matches: date, home, away, hg, ag, season
#                      (unplayed fixtures have NA goals)
#   europe_cache.csv   UEFA matches involving Eliteserien clubs: date, team,
#                      opponent, competition, venue, played
#
# Source: ESPN's public scoreboard API (no key, no browser session). One call
# per league per calendar year returns every match, played or scheduled, with
# real dates. The same API carries all six UEFA competitions (CL/EL/ECL and
# their qualifiers), so the European schedule comes for free.
#
# Workflow:
#   - The canonical store is the two CSVs in data/ (committed to GitHub).
#   - A scheduled GitHub Action runs update_cache.R, which calls
#     build_cache_from_espn() and commits the CSVs if anything changed.
#   - The Shiny app only ever reads the CSVs (from GitHub, with a local fallback).
#   - Fallback if ESPN ever breaks: build_cache_from_xlsx() still rebuilds the
#     league table from fotball.no "Kamper" exports.

suppressWarnings(suppressMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
}))

# ----- Configuration ---------------------------------------------------------

# Raw-GitHub folder the app reads from (repo name is case-sensitive):
GITHUB_RAW_BASE <- "https://raw.githubusercontent.com/ricosaur/Tippeliga-simulator/main/data/"
CACHE_PATH      <- "data/results_cache.csv"
EUROPE_PATH     <- "data/europe_cache.csv"
UPDATED_PATH    <- "data/last_updated.txt"
CACHE_URL       <- paste0(GITHUB_RAW_BASE, "results_cache.csv")
EUROPE_URL      <- paste0(GITHUB_RAW_BASE, "europe_cache.csv")
UPDATED_URL     <- paste0(GITHUB_RAW_BASE, "last_updated.txt")

# Seasons to fetch: last season (strength prior) and the current one.
# Eliteserien runs within a calendar year, so season = year.
default_seasons <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  c(y - 1L, y)
}

ESPN_BASE   <- "https://site.api.espn.com/apis/site/v2/sports/soccer/"
ESPN_LEAGUE <- "nor.1"
ESPN_EUROPE <- c(
  "uefa.champions"        = "Champions League",
  "uefa.champions_qual"   = "CL qualifying",
  "uefa.europa"           = "Europa League",
  "uefa.europa_qual"      = "EL qualifying",
  "uefa.europa.conf"      = "Conference League",
  "uefa.europa.conf_qual" = "ECL qualifying"
)

# Source spellings (ESPN and fotball.no) mapped to one canonical name. Add a
# line here if a newly promoted club shows up under an odd name.
NAME_MAP <- c(
  # ESPN
  "Bodo/Glimt"         = "Bodø/Glimt",
  "SK Brann"           = "Brann",
  "Hamarkameratene"    = "HamKam",
  "KFUM Oslo"          = "KFUM",
  "Kristiansund BK"    = "Kristiansund",
  "Sarpsborg FK"       = "Sarpsborg",
  "Stromsgodset"       = "Strømsgodset",
  "Tromso"             = "Tromsø",
  "Viking FK"          = "Viking",
  "Lillestrom"         = "Lillestrøm",
  "Valerenga"          = "Vålerenga",
  "IK Start"           = "Start",
  # fotball.no export
  "Sandefjord Fotball" = "Sandefjord",
  "Sarpsborg 08"       = "Sarpsborg"
)

# ----- Reading the caches (used by the app) ----------------------------------

read_remote_csv <- function(url, local) {
  out <- tryCatch(readr::read_csv(url, show_col_types = FALSE),
                  error = function(e) NULL)
  if (is.null(out) && file.exists(local)) {
    out <- readr::read_csv(local, show_col_types = FALSE)
  }
  out
}

read_cache_remote <- function(url = CACHE_URL, local = CACHE_PATH) {
  out <- read_remote_csv(url, local)
  if (is.null(out)) stop("Could not read results cache from URL or local file.")
  normalise_results(out)
}

# European schedule. Returns NULL if unavailable; the model then simply runs
# without the congestion term.
read_europe_remote <- function(url = EUROPE_URL, local = EUROPE_PATH) {
  out <- read_remote_csv(url, local)
  if (is.null(out)) return(NULL)
  out %>% mutate(date = as.Date(date), played = as.logical(played))
}

# When the data was last fetched from the source (POSIXct, UTC), or NULL.
# Written on every successful update, even if no results changed.
read_last_updated <- function(url = UPDATED_URL, local = UPDATED_PATH) {
  txt <- tryCatch(suppressWarnings(readLines(url, n = 1, warn = FALSE)),
                  error = function(e) NULL)
  if (is.null(txt) && file.exists(local)) txt <- readLines(local, n = 1, warn = FALSE)
  if (length(txt) == 0) return(NULL)
  out <- lubridate::ymd_hms(txt, tz = "UTC", quiet = TRUE)
  if (is.na(out)) NULL else out
}

read_cache_local <- function(local = CACHE_PATH) {
  if (!file.exists(local)) return(NULL)
  normalise_results(readr::read_csv(local, show_col_types = FALSE))
}

normalise_results <- function(df) {
  df %>%
    mutate(
      date   = as.Date(date),
      home   = as.character(home),
      away   = as.character(away),
      hg     = suppressWarnings(as.integer(hg)),
      ag     = suppressWarnings(as.integer(ag)),
      season = as.integer(season),
      played = !is.na(hg) & !is.na(ag)
    ) %>%
    arrange(date)
}

harmonise_names <- function(x) {
  x <- trimws(as.character(x))
  hit <- x %in% names(NAME_MAP)
  x[hit] <- NAME_MAP[x[hit]]
  x
}

# ----- Fetching from ESPN ----------------------------------------------------

# All events for one competition in one calendar year, one row per match.
# Dates are converted from UTC to the Oslo calendar day.
fetch_espn_events <- function(league, year) {
  url <- sprintf("%s%s/scoreboard?dates=%d&limit=1000", ESPN_BASE, league, year)
  h <- curl::new_handle(useragent = "eliteserien-sim (R)", timeout = 60)
  resp <- curl::curl_fetch_memory(url, handle = h)
  if (resp$status_code != 200) {
    stop(sprintf("ESPN returned HTTP %d for %s %d", resp$status_code, league, year))
  }
  txt <- rawToChar(resp$content); Encoding(txt) <- "UTF-8"
  js <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  ev <- js$events
  if (length(ev) == 0) return(NULL)

  rows <- lapply(ev, function(e) {
    cs <- e$competitions[[1]]$competitors
    side <- vapply(cs, function(c) c$homeAway, character(1))
    hc <- cs[[which(side == "home")]]; ac <- cs[[which(side == "away")]]
    done <- isTRUE(e$status$type$completed)
    data.frame(
      kickoff   = e$date,
      home_id   = hc$team$id, home_name = hc$team$displayName,
      away_id   = ac$team$id, away_name = ac$team$displayName,
      hg        = if (done) as.integer(hc$score) else NA_integer_,
      ag        = if (done) as.integer(ac$score) else NA_integer_,
      status    = e$status$type$name,
      stringsAsFactors = FALSE
    )
  })
  out <- bind_rows(rows)
  utc <- lubridate::ymd_hm(sub("Z$", "", out$kickoff), tz = "UTC", quiet = TRUE)
  out$date <- as.Date(format(utc, tz = "Europe/Oslo"))
  out
}

# League matches for the given seasons, in the cache schema. Also returns the
# ESPN ids of every Eliteserien club seen, used to pick out European matches.
fetch_league_espn <- function(seasons) {
  parts <- lapply(seasons, function(s) {
    ev <- fetch_espn_events(ESPN_LEAGUE, s)
    if (is.null(ev)) {
      warning(sprintf("ESPN has no Eliteserien matches for %d (skipped)", s))
      return(NULL)
    }
    ev$season <- as.integer(s)
    ev
  })
  ev <- bind_rows(Filter(Negate(is.null), parts))
  if (nrow(ev) == 0) stop("ESPN returned no league matches.")

  league <- tibble::tibble(
    date   = ev$date,
    home   = harmonise_names(ev$home_name),
    away   = harmonise_names(ev$away_name),
    hg     = ev$hg,
    ag     = ev$ag,
    season = ev$season
  )
  ids <- unique(c(ev$home_id, ev$away_id))
  list(league = league, team_ids = ids)
}

# UEFA matches for Eliteserien clubs (matched on ESPN team id, not name, so
# "Vikingur Reykjavik" is not mistaken for Viking). One row per Norwegian
# side; a Norwegian derby would give two rows.
fetch_europe_espn <- function(seasons, team_ids) {
  grid <- expand.grid(league = names(ESPN_EUROPE), year = seasons,
                      stringsAsFactors = FALSE)
  parts <- lapply(seq_len(nrow(grid)), function(i) {
    ev <- fetch_espn_events(grid$league[i], grid$year[i])
    if (is.null(ev)) return(NULL)
    ev$competition <- ESPN_EUROPE[[grid$league[i]]]
    ev
  })
  ev <- bind_rows(Filter(Negate(is.null), parts))
  if (nrow(ev) == 0) return(empty_europe())

  as_home <- ev %>% filter(home_id %in% team_ids) %>%
    transmute(date, team = home_name, opponent = away_name, competition,
              venue = "home", played = !is.na(hg))
  as_away <- ev %>% filter(away_id %in% team_ids) %>%
    transmute(date, team = away_name, opponent = home_name, competition,
              venue = "away", played = !is.na(hg))
  bind_rows(as_home, as_away) %>%
    mutate(team = harmonise_names(team)) %>%
    distinct() %>%
    arrange(date, team)
}

empty_europe <- function() {
  tibble::tibble(date = as.Date(character()), team = character(),
                 opponent = character(), competition = character(),
                 venue = character(), played = logical())
}

# Fetch everything and write both caches. Refuses to overwrite the league
# cache with data that has fewer played matches for any season than it
# already holds, so a bad API response can never wipe out results.
build_cache_from_espn <- function(seasons = default_seasons(),
                                  cache_path = CACHE_PATH,
                                  europe_path = EUROPE_PATH,
                                  updated_path = UPDATED_PATH) {
  lg <- fetch_league_espn(seasons)
  league <- lg$league %>% normalise_results() %>% arrange(date, home)

  old <- read_cache_local(cache_path)
  if (!is.null(old)) {
    old_n <- old %>% group_by(season) %>% summarise(n_old = sum(played))
    new_n <- league %>% group_by(season) %>% summarise(n_new = sum(played))
    chk <- inner_join(old_n, new_n, by = "season") %>% filter(n_new < n_old)
    if (nrow(chk) > 0) {
      stop(sprintf("New data has fewer played matches than the cache for season(s) %s; not overwriting.",
                   paste(chk$season, collapse = ", ")))
    }
  }

  europe <- fetch_europe_espn(seasons, lg$team_ids)

  readr::write_csv(league %>% dplyr::select(date, home, away, hg, ag, season), cache_path)
  readr::write_csv(europe, europe_path)
  writeLines(format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), updated_path)
  message(sprintf("Wrote %s: %d matches (%d played).",
                  cache_path, nrow(league), sum(league$played)))
  message(sprintf("Wrote %s: %d European matches (%d upcoming).",
                  europe_path, nrow(europe), sum(!europe$played)))
  invisible(list(league = league, europe = europe))
}

# ----- Fallback: building the league cache from FA xlsx exports --------------

# Parse one fotball.no "kamper" xlsx into the tidy schema.
# Columns in the export (Norwegian): Runde, Dato, Dag, Tid, Hjemmelag,
# Resultat, Bortelag, Bane, Turnering, Kampnummer, Spillform.
# Unplayed matches have Resultat = "-"; those become NA goals (i.e. fixtures).
read_kamper_xlsx <- function(path, season) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("readxl is required to read the FA export. install.packages('readxl')")
  }
  raw <- readxl::read_excel(path, sheet = 1)

  # Position-based rename so it works regardless of header text quirks.
  names(raw)[1:7] <- c("runde","dato","dag","tid","hjemmelag","resultat","bortelag")

  parse_goal <- function(res, which) {
    m <- regmatches(res, regexec("^\\s*(\\d+)\\s*-\\s*(\\d+)\\s*$", res))
    vapply(m, function(g) if (length(g) == 3) as.integer(g[which + 1]) else NA_integer_,
           integer(1))
  }

  res <- as.character(raw$resultat)
  tibble::tibble(
    date   = as.Date(raw$dato),
    home   = harmonise_names(raw$hjemmelag),
    away   = harmonise_names(raw$bortelag),
    hg     = parse_goal(res, 1),
    ag     = parse_goal(res, 2),
    season = as.integer(season)
  )
}

# Read xlsx exports (named list season -> path), stack, and write the league
# cache. Only needed if the ESPN source is unavailable.
build_cache_from_xlsx <- function(season_files = list(
                                    "2025" = "data/kamper_2025.xlsx",
                                    "2026" = "data/kamper_2026.xlsx"),
                                  cache_path = CACHE_PATH) {
  parts <- lapply(names(season_files), function(s) {
    f <- season_files[[s]]
    if (!file.exists(f)) {
      warning(sprintf("Missing file for %s: %s (skipped)", s, f))
      return(NULL)
    }
    read_kamper_xlsx(f, as.integer(s))
  })
  parts <- Filter(Negate(is.null), parts)
  if (length(parts) == 0) stop("No season files found to build the cache.")

  combined <- bind_rows(parts) %>%
    normalise_results() %>%
    arrange(date, home)

  readr::write_csv(combined %>% dplyr::select(date, home, away, hg, ag, season), cache_path)
  message(sprintf("Wrote %s: %d matches (%d played).",
                  cache_path, nrow(combined), sum(combined$played)))
  invisible(combined)
}
