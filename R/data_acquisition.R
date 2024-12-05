# Install and load required packages
# Note: only need to install once
# devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)
library(tidyverse)

# Get competition data
Comps <- FreeCompetitions()

# Filter for desired competitions
# Modify these filters as needed for different competitions/seasons
Comps <- Comps |>
    filter(competition_name == "UEFA Euro",
           season_name == "2024")

# Get matches and events data
Matches <- FreeMatches(Comps)
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)
StatsBombData <- allclean(StatsBombData)

# Save processed data
# Create 'data' directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")
saveRDS(StatsBombData, "data/statsbomb_data.rds")
