# Eliteserien title-race simulator

A Shiny app that fits a time-weighted Dixon-Coles model to Eliteserien results
and runs a Monte Carlo over the remaining fixtures to estimate each team's
probability of winning the league, finishing top 2 / top 4, or being relegated.

Data comes from the Norwegian FA's own match export (fotball.no/fotballdata),
not from scraping. That export is authoritative, includes real match dates and
the full remaining fixture list, and never blocks requests. The app reads a
cached CSV from GitHub and never touches the source at runtime, so it deploys
cleanly to shinyapps.io and reruns fast every round.

## What's here

```
app.R                        Shiny app (UI + server)
R/fetch_data.R               Data layer: read cache, parse FA xlsx exports
R/model.R                    Dixon-Coles fit + Monte Carlo
update_cache.R               Rebuilds the CSV from the xlsx exports
data/results_cache.csv       The results store (read by the app)
data/kamper_2025.xlsx        FA export, 2025 season
data/kamper_2026.xlsx        FA export, 2026 season
.github/workflows/update-data.yml   Rebuilds the CSV when you push a new export
```

## The data workflow

The Norwegian FA publishes each tournament's full match list. To refresh:

1. Go to the Eliteserien tournament on fotball.no/fotballdata, open the
   "Kamper" (matches) view, and use Eksporter to download an Excel file.
2. Save it into `data/` as `kamper_2026.xlsx` (overwrite the old one). Keep
   `kamper_2025.xlsx` for the historical prior.
3. Rebuild the cache:
   ```r
   source("R/fetch_data.R")
   build_cache_from_xlsx()     # or: Rscript update_cache.R
   ```
   This rewrites `data/results_cache.csv`.
4. Commit the new xlsx and the rebuilt CSV to GitHub. The deployed app picks up
   the new numbers on its next cold start. (If you've set up the GitHub Action,
   pushing the xlsx rebuilds the CSV for you.)

The export marks unplayed matches with "-" in the result column, so the same
file gives both played results and remaining fixtures. That is exactly what the
simulation needs, and why this source beats scraping.

## Running locally

```r
install.packages(c("shiny","dplyr","tidyr","ggplot2","DT","scales",
                   "readr","lubridate","readxl","tibble","MASS"))
shiny::runApp()
```

Click "Run simulation" to fit the model and run the Monte Carlo.

## Deploying to shinyapps.io

1. Create a GitHub repo and push this folder (keep the structure intact; use
   git or GitHub Desktop, not drag-and-drop of individual files).
2. In `R/fetch_data.R`, set `CACHE_URL` to your repo's raw CSV URL:
   ```
   https://raw.githubusercontent.com/<your-user>/eliteserien-sim/main/data/results_cache.csv
   ```
3. Deploy:
   ```r
   install.packages("rsconnect")
   rsconnect::setAccountInfo(name=..., token=..., secret=...)  # from your account
   rsconnect::deployApp()
   ```
4. Open the app URL on your phone and add it to your home screen.

## How the model works

- **Dixon-Coles.** Each team has an attack and a defence rating. Home expected
  goals = exp(attack_home - defence_away + home_advantage); away symmetrically.
  Goals are Poisson, with the low-score correlation correction (rho) for the
  0-0, 1-0, 0-1, 1-1 outcomes.
- **Time weighting.** Older matches count less, via an exponential decay with a
  half-life you set in the sidebar. 180 days means a six-month-old match counts
  half as much as today's. Lower it to let recent form dominate; raise it to
  lean on the larger 2025 sample.
- **Parameter uncertainty.** Team ratings are estimated from limited data, so
  each simulation draws a fresh set of ratings from the fit's sampling
  distribution (multivariate normal from the inverse Hessian). This stops the
  model treating noisy estimates as certain, and prevents strong-but-unlucky
  teams from showing a false 0% chance. On by default; toggle in the sidebar.
- **Shrinkage.** Pulls attack/defence ratings toward the league average by a
  tunable factor. Higher = more cautious, useful early in the season when the
  sample is thin. 0 is the raw fit.
- **Monte Carlo.** Each remaining fixture is sampled from its Poisson scoreline
  distribution, the table is resolved (points, then goal difference, then goals
  scored, per Eliteserien rules), and this repeats thousands of times.

## What you get

The app reports, per team: probability of the title, top 3, a European place
(top 4), top 6, the relegation playoff (14th), and relegation (15-16th), plus
expected final points and expected final position. Four views:

- **Title & Europe:** grouped bars for title / top 3 / European place.
- **Position heatmap:** probability each team finishes in each position. The
  most complete single view; a team's whole range of likely outcomes at a glance.
- **Full table:** every probability plus expected points and position.
- **Team strengths:** attack vs defence scatter for the current squad.

A note on reading the numbers: the title bar alone can understate a team's
season. A side sitting high on points but modest on underlying goals (Tromsø in
2026 is the live example) may show a low title chance but a strong top-3
probability. That is the model distinguishing league position from underlying
performance, which is the point of using goals rather than results.

## Notes

- **Early season:** with few 2026 matches played, the model leans heavily on a
  small sample and looks overconfident. Including 2025 (via the decay prior)
  tempers this. As 2026 fills in, lower the half-life to trust current form more.
- **Squad changes between seasons:** 2025 includes relegated teams (Bryne,
  Haugesund, Strømsgodset) and 2026 includes promoted ones (Lillestrøm, Start,
  Aalesund). This is handled automatically: relegated teams inform the strength
  priors of teams they played, and the simulation only runs over the 16 current
  teams with remaining fixtures.
- **Club name spellings:** `NAME_MAP` in `R/fetch_data.R` maps FA export
  spellings ("Sandefjord Fotball", "Sarpsborg 08") to canonical names. Add a
  line there if a newly promoted club needs it.
- **European congestion** is not modelled (relevant for Bodø/Glimt's deep runs).
  Add a fatigue term to the expected-goals calc in `R/model.R` if you want it.
