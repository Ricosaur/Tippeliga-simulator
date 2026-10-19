#!/usr/bin/env Rscript
# update_cache.R
# Rebuilds data/results_cache.csv and data/europe_cache.csv from ESPN.
#
# Runs automatically via the GitHub Action (.github/workflows/update-data.yml).
# To run by hand:  Rscript update_cache.R
#
# The Shiny app reads the CSVs from GitHub, so once they are committed, new
# app sessions show the new numbers.

source("R/fetch_data.R")

build_cache_from_espn()
