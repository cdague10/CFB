library(tidyverse)
library(ggimage)
library(gt)
library(ggplot2)
library(dplyr)
library(tidymodels)

numeric_cols <- sapply(X2024cfb_all_season_stats, is.numeric)

X2024cfb_all_season_stats <- X2024cfb_all_season_stats %>%
  mutate(across(7:68, ~ .x / games))
excluded_cols <- c("win_percent", "wins", "games","season","defense_openFieldYardsTotal","offense_standardDowns_rate","defense_passingDowns_totalPPA","defense_standardDowns_rate","interceptionsOpponent")
predictors <- setdiff(names(X2024cfb_all_season_stats)[numeric_cols], excluded_cols)

# Compute R-squared for each column
r_squared_results <- sapply(predictors, function(col) {
  formula <- as.formula(paste("win_percent ~", col))
  model <- lm(formula, data = X2024cfb_all_season_stats)
  summary(model)$r.squared
})

# Sort and print
r_squared_results <- sort(r_squared_results, decreasing = TRUE)


r_squared_df <- enframe(r_squared_results, name = "predictor", value = "r_squared") %>%
  arrange(desc(r_squared)) %>%
  slice_head(n=20)

ggplot(r_squared_df, aes(x = reorder(predictor, r_squared), y = r_squared)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 20 Predictors of Win Percentage (2024 FBS CFB Season)",
    x = "Predictor",
    y = "R-squared Value"
  ) +
  theme_minimal(base_size = 13)

#Top Quarter Teams
top_quart_teams <- X2024cfb_all_season_stats %>%
  arrange(desc(win_percent)) %>%
  slice_head(n= 34)

r_squared_top <- sapply(predictors, function(col) {
  formula <- as.formula(paste("win_percent ~", col))
  model <- lm(formula, data = top_quart_teams)
  summary(model)$r.squared
})

# Sort and print
r_squared_top_quart <- sort(r_squared_results, decreasing = TRUE)
r_squared_df <- enframe(r_squared_top_quart, name = "predictor", value = "r_squared") %>%
  arrange(desc(r_squared))








# Compute R-squared for all teams
r_squared_all <- sapply(predictors, function(col) {
  formula <- as.formula(paste("win_percent ~", col))
  model <- lm(formula, data = X2024cfb_all_season_stats)
  summary(model)$r.squared
})

# Compute R-squared for top quartile teams
top_quart_teams <- X2024cfb_all_season_stats %>%
  arrange(desc(win_percent)) %>%
  slice_head(n = 34)  # Adjust n as needed for quartile

r_squared_top <- sapply(predictors, function(col) {
  formula <- as.formula(paste("win_percent ~", col))
  model <- lm(formula, data = top_quart_teams)
  summary(model)$r.squared
})


# Combine both into one data frame for plotting
r_squared_comparison <- tibble(
  predictor = predictors,
  all_teams = r_squared_all[predictors],
  top_quartile = r_squared_top[predictors]
) %>%
  pivot_longer(cols = c(all_teams, top_quartile), names_to = "group", values_to = "r_squared")

# Arranging for graph
r_squared_comparison <- r_squared_comparison %>%
  group_by(predictor) %>%
  mutate(avg_r2 = mean(r_squared)) %>%
  ungroup() %>%
  arrange(desc(avg_r2)) %>%
  slice_head(n=30)


ggplot(r_squared_comparison, aes(x = reorder(predictor, avg_r2), y = r_squared, group = group, color = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0.1, 0.6)) +
  labs(
    title = "R-squared Comparison of Win Percent v Predictors",
    subtitle = "Top Quartile Teams vs All Teams (2024 FBS Season)",
    x = "Predictor",
    y = "R-squared Value",
    color = "Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1, size = 8)  # Vertical text
  )
