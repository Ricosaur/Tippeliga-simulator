# fetch_data.R
# Data layer for the Eliteserien simulator.
#
# Produces a tidy data frame with columns:
#   date (Date), home, away, hg (home goals), ag (away goals),
#   season (int), played (lgl)
#
# Source: the Norwegian FA's own match export (fotball.no/fotballdata),
# downloaded as an .xlsx ("kamper" / matches). This is authoritative, includes
# real dates and the full remaining fixture list, and does not block requests
# the way FBref does. No scraping involved.
#
# Workflow:
#   - The canonical store is data/results_cache.csv (committed to GitHub).
#   - To refresh: download each season's kamper.xlsx from fotball.no, drop them
#     in data/, and run build_cache_from_xlsx(). That rewrites the CSV.
#   - The Shiny app only ever reads the CSV (from GitHub, with a local fallback).

suppressWarnings(suppressMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
}))

# ----- Configuration ---------------------------------------------------------

# Seasons to include, and the local xlsx file for each. Add a line per season.
# Download these from fotball.no: the tournament's "Kamper" view, Eksporter.
SEASON_FILES <- list(
  "2025" = "data/kamper_2025.xlsx",
  "2026" = "data/kamper_2026.xlsx"
)

# Raw-GitHub URLs the app reads from. EDIT after you create the repo:
CACHE_URL  <- "https://raw.githubusercontent.com/ricosaur/Tippeliga-simulator/main/data/results_cache.csv"
CACHE_PATH <- "data/results_cache.csv"

EURO_URL   <- "https://raw.githubusercontent.com/ricosaur/Tippeliga-simulator/main/data/european_fixtures_2026.csv"
EURO_PATH  <- "data/european_fixtures_2026.csv"

# Source spellings on fotball.no that need mapping to one canonical name.
NAME_MAP <- c(
  "Sandefjord Fotball" = "Sandefjord",
  "Sarpsborg 08"       = "Sarpsborg"
)

# ----- Reading the cache (used by the app) -----------------------------------

read_cache_remote <- function(url = CACHE_URL, local = CACHE_PATH) {
  out <- tryCatch(readr::read_csv(url, show_col_types = FALSE),
                  error = function(e) NULL)
  if (is.null(out) && file.exists(local)) {
    out <- readr::read_csv(local, show_col_types = FALSE)
  }
  if (is.null(out)) stop("Could not read results cache from URL or local file.")
  normalise_results(out)
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
  for (nm in names(NAME_MAP)) x[x == nm] <- NAME_MAP[[nm]]
  x
}

# ----- Reading European fixture dates (used by the app) ----------------------

# Returns a data frame (team, date, competition) of European match dates for
# Norwegian clubs. Empty data frame if neither the remote URL nor the local file
# is available -- the model treats that as "no fatigue effect".
read_european_fixtures <- function(url = EURO_URL, local = EURO_PATH) {
  empty <- data.frame(team = character(), date = as.Date(character()),
                      competition = character())
  out <- tryCatch(readr::read_csv(url, show_col_types = FALSE),
                  error = function(e) NULL)
  if (is.null(out) && file.exists(local)) {
    out <- tryCatch(readr::read_csv(local, show_col_types = FALSE),
                    error = function(e) NULL)
  }
  if (is.null(out) || nrow(out) == 0) return(empty)
  out %>% mutate(date = as.Date(date), team = as.character(team))
}

# ----- Building the cache from FA xlsx exports -------------------------------

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

# Read every configured season's xlsx, stack, and write data/results_cache.csv.
build_cache_from_xlsx <- function(season_files = SEASON_FILES,
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

  readr::write_csv(combined, cache_path)
  message(sprintf("Wrote %s: %d matches (%d played).",
                  cache_path, nrow(combined), sum(combined$played)))
  invisible(combined)
}
