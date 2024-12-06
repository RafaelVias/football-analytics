
# Load required packages
library(tidyverse)
library(ggplot2)
library(zoo)

# Read the processed data
data <- readRDS("data/statsbomb_data.rds")

# Figure 1. League Position Over Time (for Premier League) ---------------------------

data |>
    filter(competition_name == "Premier League") |>
    ggplot(aes(x = game_number, y = league_rank_PostGame, color = team.name)) +
    geom_line() +
    scale_y_reverse() +  # Reverse y-axis so rank 1 is at the top
    theme_minimal() +
    labs(title = "Premier League 2015/16 Position Changes",
         x = "Game Week",
         y = "League Position",
         color = "Team") +
    theme(legend.position = "right")

ggsave("Figures/Figure_1.jpg", width = 12, height = 8, dpi = 300)

# Figure 2. Points Accumulation Over Season ----------------------------------------

data |>
    filter(competition_name == "Premier League") |>
    ggplot(aes(x = game_number, y = league_score_PostGame, color = team.name)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Premier League 2015/16 Points Progression",
         x = "Game Week",
         y = "Points",
         color = "Team")

ggsave("Figures/Figure_2.jpg", width = 12, height = 8, dpi = 300)

# Figure 3. Analyze 5-day form: Visualizing points in last 5 games -----------------

# Selecting a few interesting teams (e.g., champion, relegated team, etc.)
teams_to_plot <- c("Leicester City", "Arsenal", "Manchester City", "Aston Villa")

data |>
    filter(competition_name == "Premier League",
           team.name %in% teams_to_plot) |>
    ggplot() +
    # Add line for trailing points
    geom_line(aes(x = game_number, y = trailing_points_5games),
              color = "gray50", size = 1) +
    # Add points colored by game result
    geom_point(aes(x = game_number, y = trailing_points_5games,
                   color = game_result), size = 3) +
    # Separate plots for each team
    facet_wrap(~team.name) +
    # Custom colors for results
    scale_color_manual(values = c("win" = "darkgreen",
                                  "draw" = "orange",
                                  "loss" = "red")) +
    theme_minimal() +
    labs(title = "5-Game Form and Match Results - Premier League 2015/16",
         subtitle = "Line shows points from previous 5 games, points show match results",
         x = "Game Week",
         y = "Points from Previous 5 Games",
         color = "Match Result") +
    theme(legend.position = "bottom")

ggsave("Figures/Figure_3.jpg", width = 12, height = 8, dpi = 300)

# Figure 4. Analyze 5-day form: Plot Win Rate ~ Points in Previous 5 Games--------------------

data |>
    filter(!is.na(trailing_points_5games)) |>  # Remove first 5 games
    mutate(next_game_won = game_result == "win") |>  # Binary outcome for logistic regression
    group_by(trailing_points_5games) |>
    summarise(
        total_games = n(),
        games_won = sum(next_game_won),
        win_rate = games_won / total_games
    ) |>
    ggplot(aes(x = trailing_points_5games, y = win_rate, size = total_games)) +
    geom_point() +
    geom_smooth(method = "loess", se = TRUE, show.legend = FALSE) +
    theme_minimal() +
    labs(title = "Relationship between 5-Game Form and Win Probability",
         x = "Points from Previous 5 Games",
         y = "Win Rate in Next Game",
         size = "Number of Games") +
    scale_y_continuous(labels = scales::percent)

ggsave("Figures/Figure_4.jpg", width = 12, height = 8, dpi = 300)


# Figure 5. Compare actual goals vs xG for selected teams ---------------------------------

data |>
    filter(competition_name == "Premier League",
           team.name %in% c("Leicester City", "Arsenal", "Tottenham Hotspur", "Manchester City")) |>
    group_by(team.name) |>
    mutate(cum_goals = cumsum(regular_goals),
           cum_xg = cumsum(xg)) |>
    ggplot() +
    geom_line(aes(x = game_number, y = cum_goals, color = "Actual Goals")) +
    geom_line(aes(x = game_number, y = cum_xg, color = "Expected Goals"), linetype = "dashed") +
    facet_wrap(~team.name) +
    theme_minimal() +
    labs(title = "Actual vs Expected Goals Accumulation",
         subtitle = "Top 4 Teams - Premier League 2015/16",
         x = "Game Week",
         y = "Goals",
         color = "Metric") +
    theme(legend.position = "bottom")
ggsave("Figures/Figure_5.jpg", width = 12, height = 8, dpi = 300)

