#!/usr/bin/env Rscript
# update_cache.R
# Rebuilds data/results_cache.csv from the FA xlsx exports in data/.
#
# Usage:
#   1. Download the current season's match list from fotball.no (the "Kamper"
#      view of the Eliteserien tournament, then Eksporter to Excel).
#   2. Save it as data/kamper_2026.xlsx (and data/kamper_2025.xlsx for history).
#   3. Run:  Rscript update_cache.R
#   4. Commit the updated data/results_cache.csv (and the xlsx files) to GitHub.
#
# The Shiny app reads results_cache.csv from GitHub, so once you commit, the
# deployed app shows the new numbers on its next cold start.

source("R/fetch_data.R")

build_cache_from_xlsx()
