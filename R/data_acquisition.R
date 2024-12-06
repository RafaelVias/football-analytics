# Install and load required packages
# Note: only need to install once
# devtools::install_github("statsbomb/StatsBombR")
library(StatsBombR)
library(tidyverse)
library(zoo)
library(writexl)


# Get competition data
Comps <- FreeCompetitions()

# Filter for the three complete leagues from 2015/2016 season
Comps <- FreeCompetitions()
Comps <- Comps |>
    filter(season_name == "2015/2016",
           competition_name %in% c("La Liga", "Premier League", "Serie A"))

# Get matches and events data for these competitions
Matches <- FreeMatches(Comps)
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T)
StatsBombData <- allclean(StatsBombData)

# Calculate match statistics with dates and game numbers
StatsBombData <- StatsBombData |>
    group_by(match_id, team.name, competition_id, season_id) |>
    summarise(
        shots = sum(type.name == "Shot", na.rm = T),
        regular_goals = sum(shot.outcome.name == "Goal", na.rm = T),
        own_goals_for = sum(type.name == "Own Goal For", na.rm = T),
        total_goals = regular_goals + own_goals_for,
        xg = sum(shot.statsbomb_xg, na.rm = T)
    ) |>
    mutate(
        goals_to_shots_ratio = regular_goals / shots,
        goals_to_xg_diff = regular_goals - xg
    ) |>
    left_join(Matches |>
                  select(match_id, match_date, competition_name = competition.competition_name),
              by = "match_id") |>
    group_by(match_id) |>
    mutate(game_result = if_else(
        total_goals == max(total_goals),
        if_else(max(total_goals) == min(total_goals), "draw", "win"),
        "loss"
    )) |>
    ungroup() |>
    group_by(team.name, competition_name) |>
    arrange(match_date, .by_group = TRUE) |>
    mutate(
        game_number = row_number(),
        last_game_result = lag(game_result, default = "no previous game"),
        trailing_Wins = lag(sequence(rle(game_result == "win")$lengths) *
                                (game_result == "win"), default = 0),
        trailing_Losses = lag(sequence(rle(game_result == "loss")$lengths) *
                                  (game_result == "loss"), default = 0),
        trailing_Draws = lag(sequence(rle(game_result == "draw")$lengths) *
                                 (game_result == "draw"), default = 0),
        trailing_LossesAndDraws = lag(sequence(rle(game_result != "win")$lengths) *
                                          (game_result != "win"), default = 0),
        score = case_when(
            game_result == "win" ~ 3,
            game_result == "draw" ~ 1,
            game_result == "loss" ~ 0
        ),
        league_score_PreGame = lag(cumsum(score), default = 0),
        league_score_PostGame = cumsum(score),
        cumulative_goals = cumsum(total_goals),
        goals_PreGame = lag(cumulative_goals, default = 0),
        goals_PostGame = cumulative_goals,
        trailing_points_5games = case_when(
            game_number <= 5 ~ NA_real_,
            game_number == 6 ~ league_score_PreGame,
            TRUE ~ lag(zoo::rollapplyr(score, width = 5, FUN = sum, fill = NA))
        )
    ) |>
    group_by(competition_name, game_number) |>
    mutate(
        league_rank_PreGame = min_rank(-league_score_PreGame - goals_PreGame/100),
        league_rank_PostGame = min_rank(-league_score_PostGame - goals_PostGame/100)
    ) |>
    arrange(competition_name, team.name, game_number) |>
    ungroup()

# Save processed data
# Create 'data' directory if it doesn't exist
if (!dir.exists("data")) dir.create("data")
saveRDS(StatsBombData, "data/statsbomb_data.rds")

# Save as Excel file
write_xlsx(StatsBombData, "data/statsbomb_data.xlsx")
