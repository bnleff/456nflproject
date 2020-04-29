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

covariates_new <- paste("qtr", "down", "time", "minutes_remaining_in_half", "yardline", "distance_to_endzone", "yards_to_go", "net_yards", "goal_down",
                        "possession_team_score", "defensive_team_score", "preplay_timeouts_remaining_possession_team", "no_score_within_half_prob", "defense_safety_prob",
                        "possession_team_fieldgoal_prob", "possession_team_touchdown_prob", "expected_points_possession_team", "expected_points_added",
                        "win_prob_added_possession_team", "success", sep = "+")

covariates_alternate_new <- paste("time", "minutes_remaining_in_half", "yardline", "distance_to_endzone", "yards_to_go", "net_yards", "goal_down",
                                  "possession_team_score", "defensive_team_score", "preplay_timeouts_remaining_possession_team", "no_score_within_half_prob", "defense_safety_prob",
                                  "possession_team_fieldgoal_prob", "possession_team_touchdown_prob", "expected_points_possession_team", "expected_points_added",
                                  "win_prob_added_possession_team", "success", sep = "+")


form_new <- as.formula(paste("play_call ~ ", covariates_new))
form_alternate_new <- as.formula(paste("play_call ~ ", covariates_alternate_new))

start_time <- proc.time()
nfl_glm_model_without_linear_dependencies <- glm(formula = form_new, data = training_data, family = binomial(link = "logit"), x = TRUE)
nfl_glm_model_without_linear_dependences_alt <- glm(formula = form_alternate_new, data = training_data, family = binomial(link = "logit"), x = TRUE)
time_logistic <- proc.time() - start_time
time_logistic
summary(nfl_glm_model_without_linear_dependencies)
summary(nfl_glm_model_without_linear_dependences_alt)

#No more collinearities.
findLinearCombos(nfl_glm_model_without_linear_dependencies$x)

summary(nfl_glm_model_without_linear_dependencies)$null.deviance
summary(nfl_glm_model_without_linear_dependencies)$deviance

summary(nfl_glm_model_without_linear_dependences_alt)$null.deviance
summary(nfl_glm_model_without_linear_dependences_alt)$deviance

head(nfl_glm_model_without_linear_dependencies$fitted.values)

predicted_probabilities <- predict(nfl_glm_model_without_linear_dependencies, type = "response")  
head(predicted_probabilities)

predicted_probabilities_alt <- predict(nfl_glm_model_without_linear_dependences_alt, type = "response")

observed_values <- if_else(training_data$play_call  == "pass", 1, 0)

predicted_probabilities <- predict(nfl_glm_model_without_linear_dependencies, type = "response")
predicted_probabilities_alt <- predict(nfl_glm_model_without_linear_dependences_alt, type = "response")

predicted_response <- if_else(predicted_probabilities > 0.5, 1, 0)
predicted_response_alt <- if_else(predicted_probabilities > 0.5, 1, 0)

head(predicted_response, 10)
head(observed_values, 10)

#This is our model's match between observed and predicted values of the response variable play_call:
mean(observed_values == predicted_response)
mean(observed_values == predicted_response_alt)

predicted_playcall_training <- if_else(predicted_probabilities > 0.5, "pass", "rush")
predicted_playcall_training <- as_factor(predicted_playcall_training)
stat_log_training  <- confusionMatrix(data = predicted_playcall_training, reference = training_data$play_call, positive = levels(training_data$play_call)[1])
stat_log_training


predicted_playcall_test <- predict(nfl_glm_model_without_linear_dependencies, newdata = testing_data, type = "response") 

predicted_playcall_test <- ifelse(predicted_playcall_test > 0.5, "pass", "rush")
predicted_playcall_test <- as_factor(predicted_playcall_test)
stat_log_testing <- confusionMatrix(data = predicted_playcall_test, reference = testing_data$play_call, positive = levels(testing_data$play_call)[1])
stat_log_testing
