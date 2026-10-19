# Eliteserien title-race simulator

A Shiny app that fits a time-weighted Dixon-Coles model to Eliteserien results
and runs a Monte Carlo over the remaining fixtures to estimate each team's
probability of winning the league, finishing top 2 / top 4, or being relegated.
It adjusts for European fixture congestion using the clubs' UEFA schedules.

Data comes from ESPN's public scoreboard API: one request per competition per
season returns every match, played or scheduled, with real dates. A scheduled
GitHub Action refreshes the data every evening and commits it, so there is
nothing to download by hand. The app reads the cached CSVs from GitHub and
never touches the source at runtime, so it deploys cleanly to shinyapps.io.

## What's here

```
app.R                        Shiny app (UI + server)
R/fetch_data.R               Data layer: fetch from ESPN, read caches
R/model.R                    Dixon-Coles fit (with congestion term) + Monte Carlo
update_cache.R               Rebuilds both CSVs from ESPN
data/results_cache.csv       League results and fixtures (read by the app)
data/europe_cache.csv        UEFA matches of Eliteserien clubs (read by the app)
data/last_updated.txt        When the data was last fetched (shown in the app)
data/kamper_*.xlsx           Old FA exports; only needed for the manual fallback
.github/workflows/update-data.yml   Scheduled refresh (daily)
```

## The data workflow

Automatic. The GitHub Action runs every day at 22:30 UTC (after the evening
kick-offs), calls `build_cache_from_espn()`, and commits the CSVs and a fresh
timestamp. New app sessions pick up the new numbers. You can also trigger it
from the Actions tab ("Run workflow"), or locally with `Rscript update_cache.R`.

The app header shows "Data updated" (the time of the last fetch, Oslo time)
with a green dot, which turns amber if the last fetch is more than 36 hours
old. That is the signal to check the Actions tab for a failed run.

What it fetches:

- **League:** Eliteserien (`nor.1`) for last season and the current one.
  Unplayed matches have empty scores, so the same table gives both results and
  remaining fixtures. Postponed matches move to their new date automatically.
- **Europe:** Champions League, Europa League and Conference League, including
  all qualifying rounds, for the same two years. Only matches involving a club
  seen in Eliteserien are kept (matched on ESPN team id, so "Vikingur
  Reykjavik" is never confused with Viking).

Safeguard: the update refuses to overwrite the league cache if the new data
has fewer played matches for a season than the cache already holds, so a bad
API response cannot wipe out results. A failed run leaves the old CSVs alone.

Seasons roll over on their own: the fetcher always takes the current calendar
year and the one before.

**Fallback.** If ESPN ever changes its API, the old route still works for the
league: download the "Kamper" export from fotball.no, save it as
`data/kamper_<year>.xlsx`, and run `build_cache_from_xlsx()` in
`R/fetch_data.R`. The congestion term then uses the last European cache.

## Running locally

```r
install.packages(c("shiny","dplyr","tidyr","ggplot2","DT","scales",
                   "readr","lubridate","readxl","tibble","MASS",
                   "jsonlite","curl"))
shiny::runApp()
```

The app runs the simulation on start; change settings and click
"Run simulation" to rerun.

## Deploying to shinyapps.io

1. Create a GitHub repo and push this folder (keep the structure intact; use
   git or GitHub Desktop, not drag-and-drop of individual files).
2. In `R/fetch_data.R`, set `GITHUB_RAW_BASE` to your repo's raw data folder:
   ```
   https://raw.githubusercontent.com/<your-user>/Tippeliga-simulator/main/data/
   ```
   Without this, the deployed app only ever sees the CSVs bundled at deploy
   time and the automatic updates never reach it.
3. In the repo's Settings > Actions > General, make sure workflows have
   "Read and write permissions" so the Action can commit.
4. Deploy:
   ```r
   install.packages("rsconnect")
   rsconnect::setAccountInfo(name=..., token=..., secret=...)  # from your account
   rsconnect::deployApp()
   ```
5. Open the app URL on your phone and add it to your home screen.

The repo must be public for the app to read the raw CSV URLs without a token.
The Action commits a new timestamp every day, which also keeps the repo active
so GitHub never pauses the schedule (it does that after 60 idle days).

## How the model works

- **Dixon-Coles.** Each team has an attack and a defence rating. Home expected
  goals = exp(attack_home - defence_away + home_advantage); away symmetrically.
  Goals are Poisson, with the low-score correlation correction (rho) for the
  0-0, 1-0, 0-1, 1-1 outcomes.
- **Time weighting.** Older matches count less, via an exponential decay with a
  half-life you set in the sidebar. 180 days means a six-month-old match counts
  half as much as today's. Lower it to let recent form dominate; raise it to
  lean on the larger previous-season sample.
- **European congestion.** A league match is flagged for a team if that team
  played a UEFA match 1 to N days earlier (N = 5 by default, set in the
  sidebar; in practice the gap is 3 to 5 days). Two parameters are estimated
  jointly with the ratings: the shift in the flagged team's own expected goals,
  and the shift in its opponent's. The simulation flags remaining fixtures from
  the scheduled European dates and applies the same effects. The estimates and
  their standard errors are shown in the sidebar; with roughly 40 flagged
  team-matches they are noisy, and parameter uncertainty carries that noise
  into the simulation.
- **Parameter uncertainty.** Team ratings are estimated from limited data, so
  each simulation draws a fresh set of ratings (and congestion effects) from
  the fit's sampling distribution (multivariate normal from the inverse
  Hessian). This stops the model treating noisy estimates as certain, and
  prevents strong-but-unlucky teams from showing a false 0% chance. On by
  default; toggle in the sidebar.
- **Shrinkage.** Pulls attack/defence ratings toward the league average by a
  tunable factor. Higher = more cautious, useful early in the season when the
  sample is thin. 0 is the raw fit.
- **Monte Carlo.** Each remaining fixture is sampled from its Poisson scoreline
  distribution, the table is resolved (points, then goal difference, then goals
  scored, per Eliteserien rules), and this repeats thousands of times.

## What you get

The app reports, per team: probability of the title, top 3, a European place
(top 4), top 6, the relegation playoff (14th), and relegation (15-16th), plus
expected final points and expected final position. Five views:

- **Title & Europe:** grouped bars for title / top 3 / European place.
- **Position heatmap:** probability each team finishes in each position. The
  most complete single view; a team's whole range of likely outcomes at a glance.
- **Full table:** every probability plus expected points and position.
- **Team strengths:** attack vs defence scatter for the current squad.
- **European schedule:** upcoming UEFA matches for Eliteserien clubs, and the
  remaining league fixtures the model treats as congested.

A note on reading the numbers: the title bar alone can understate a team's
season. A side sitting high on points but modest on underlying goals may show
a low title chance but a strong top-3 probability. That is the model
distinguishing league position from underlying performance, which is the point
of using goals rather than results.

## Notes

- **Early season:** with few current-season matches played, the model leans
  heavily on a small sample and looks overconfident. Including last season
  (via the decay prior) tempers this. As the season fills in, lower the
  half-life to trust current form more.
- **Squad changes between seasons:** relegated teams inform the strength
  priors of teams they played, and the simulation only runs over the 16
  current teams with remaining fixtures.
- **Club name spellings:** `NAME_MAP` in `R/fetch_data.R` maps source
  spellings (ESPN's "Bodo/Glimt", "SK Brann", "Hamarkameratene"; fotball.no's
  "Sarpsborg 08") to canonical names. When a club is promoted, check the next
  update's team list and add a line there if its ESPN name needs mapping.
- **Congestion is one league-wide effect.** It does not distinguish a
  Champions League trip to Munich from a qualifier, or squads that rotate well
  from those that do not. Matches *before* a big European game (rotation) are
  not flagged either.
