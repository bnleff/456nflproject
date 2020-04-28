library(nflscrapR)
library(tidyverse)
library(caTools)
library(caret)

nfl_data <- readRDS("/home/taudin/MiscFiles/Spring20/MATH456/NFLProject/data/version_1.rds")


#Build the model
set.seed(456)
split <- sample.split(nfl_data$play_call, SplitRatio = 0.8)

training_data <- nfl_data %>% 
  filter(split == TRUE)

testing_data <- nfl_data %>% 
  filter(split == FALSE)

covariates <- paste("qtr", "down", "time", "minutes_remaining_in_half", "seconds_remaining_in_game", "yardline", "distance_to_endzone", "yards_to_go", "net_yards", "goal_down",
                    "possession_team_score", "defensive_team_score", "score_differential", "preplay_timeouts_remaining_possession_team", "preplay_timeouts_remaining_hometeam",
                    "preplay_timeouts_remaining_awayteam", "no_score_within_half_prob", "defense_safety_prob", "possession_team_fieldgoal_prob", "possession_team_touchdown_prob",
                    "expected_points_possession_team", "expected_points_added", "preplay_hometeam_win_prob", "preplay_awayteam_win_prob", "win_prob_added_possession_team",
                    "success", sep = "+")

form <- as.formula(paste("play_call ~", covariates))


start_time <- proc.time()
nfl_glm_model <- glm(formula = form, data = training_data, family = binomial(link = "logit"), x = TRUE)

time_logistic <- proc.time() - start_time

time_logistic


summary(nfl_glm_model)

#Investigate if there exist any collinearities.
summary(nfl_glm_model)$coefficients[, 1:2]

#Score differential and preplay_awayteam_win_prob should be excluded.
findLinearCombos(nfl_glm_model$x)

#We must exclude the following variables from the model because of linear dependencies.
colnames(nfl_glm_model$x)[findLinearCombos(nfl_glm_model$x)$remove]


