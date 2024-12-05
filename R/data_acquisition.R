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
    filter(competition_name == "La Liga",
           season_name == "2020/2021")

# Get matches and events data
Matches <- FreeMatches(Comps)
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)
StatsBombData <- allclean(StatsBombData)

# totals
data <- StatsBombData |>
    group_by(match_id,team.name, competition_id, season_id) |>
    summarise(shots = sum(type.name == "Shot", na.rm = T),
              goals = sum(shot.outcome.name == "Goal", na.rm = T),
              xg = sum(shot.statsbomb_xg, na.rm = T)) |>
    mutate(goals_to_shots = goals / shots)

# Save processed data
# Create 'data' directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")
saveRDS(StatsBombData, "data/statsbomb_data.rds")
