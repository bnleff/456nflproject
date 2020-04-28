library(nflscrapR)
library(tidyverse)
library(caTools)

nfl_play_by_play_data <- read_csv("/home/taudin/MiscFiles/Spring20/MATH456/NFLData/nflplaybyplay.csv")

#We need each row to also include a variable that indicates which team ended up winning the game in which the respective play took place. The nflscrapR package provides the ability to 
#obtain game result data. From this data, we can get the needed variable.

games_2016 <- scrape_game_ids(2016, type = "reg")
games_2015 <- scrape_game_ids(2015, type = "reg")
games_2014 <- scrape_game_ids(2014, type = "reg")
games_2013 <- scrape_game_ids(2013, type = "reg")
games_2012 <- scrape_game_ids(2012, type = "reg")
games_2011 <- scrape_game_ids(2011, type = "reg")
games_2010 <- scrape_game_ids(2010, type = "reg")
games_2009 <- scrape_game_ids(2009, type = "reg")

games <- bind_rows(games_2016, games_2015, games_2014, games_2013, games_2012, games_2011, games_2010, games_2009)

colnames(games)[2] <- "GameID"
games$GameID <- as.numeric(games$GameID)
nfl_pbp_data <- full_join(games, nfl_play_by_play_data, by = "GameID")

#Create a new binary variable that indicates whether or not the possession team ultimately wins the game.
nfl_pbp_data <- nfl_pbp_data %>% 
  mutate(winner = if_else(home_score > away_score, home_team, away_team))

nfl_pbp_data <- nfl_pbp_data %>% 
  mutate(possession_team_win = if_else(winner == posteam, "Yes", "No"))

nfl_pbp_data$qtr <- as_factor(nfl_pbp_data$qtr)
nfl_pbp_data$down <- as_factor(nfl_pbp_data$down)
nfl_pbp_data$possession_team_win <- as_factor(nfl_pbp_data$possession_team_win)

#Remove "No Play" plays and plays that did not occur during regulation.
nfl_pbp_data_filtered <- nfl_pbp_data %>% 
  filter(PlayType != "No Play" & qtr != 5 & down != "NA" & possession_team_win != "NA") %>% 
  select(GameID, Date, posteam, HomeTeam, AwayTeam, winner, qtr, down, ydstogo, TimeSecs, yrdline100, ScoreDiff, possession_team_win)

#Build the model
set.seed(456)
split <- sample.split(nfl_pbp_data_filtered$possession_team_win, SplitRatio = 0.8)

training_data <- nfl_pbp_data_filtered %>% 
  filter(split == TRUE)

testing_data <- nfl_pbp_data_filtered %>% 
  filter(split == FALSE)

win_prediction_model <- glm(possession_team_win ~ qtr + down + ydstogo + TimeSecs + yrdline100 + ScoreDiff, training_data, family = "binomial")
summary(win_prediction_model)

prediction_one <- predict(win_prediction_model, training_data, type = "response")

training_data <- cbind(training_data, prediction_one)
training_data <-training_data %>% 
  mutate(prediction_one_home = if_else(posteam == HomeTeam, prediction_one, 1 - prediction_one))

#Estimated win probability
ggplot(filter(training_data, GameID == "2013090500"), aes(x = TimeSecs, y = prediction_one_home)) +
  geom_line(size = 3, color = "orange") +
  scale_x_reverse() +
  ylim(c(0, 1)) +
  theme_minimal() +
  xlab("Time Remaining (seconds)") +
  ylab("Home Win Probability")
