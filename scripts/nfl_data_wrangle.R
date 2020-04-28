library(nflscrapR)
library(tidyverse)
library(caTools)
library(na.tools)

nfl_play_by_play_data <- read_csv("/home/taudin/MiscFiles/Spring20/MATH456/NFLData/nflplaybyplay.csv")

nfl_play_by_play_data <- nfl_play_by_play_data %>% 
  mutate_at(vars(HomeTeam, AwayTeam, posteam, DefensiveTeam), funs(case_when(.%in% "JAX" ~ "JAC", .%in% "STL" ~ "LA", .%in% "SD" ~ "LAC", TRUE ~.)))


nfl_play_by_play_data$qtr <- as_factor(nfl_play_by_play_data$qtr)
nfl_pbp_data$down <- as_factor(nfl_pbp_data$down)



#We're going to fix some plays that are misclassified. Focus on run and pass plays, throw out punts, kickoffs, field goals, and dead-ball penalties.
nfl_play_by_play_data_run_pass <- nfl_play_by_play_data %>% 
  filter(!is_na(EPA), PlayType == "No Play" | PlayType == "Pass" | PlayType == "Run")


nfl_play_by_play_data_run_pass %>% head

nfl_play_by_play_data_run_pass <- nfl_play_by_play_data_run_pass %>% 
  mutate(pass = if_else(str_detect(desc, "(pass) | (sacked) | (scramble)"), 1, 0),
         rush = if_else(str_detect(desc, "(left end) | (left tackle) | (left guard) | (up the middle) | (right guard) | (right tackle) | (right end)") & pass == 0, 1, 0),
         play_call = if_else(pass == 1, "pass", "rush"),
         success = ifelse(EPA > 0, 1, 0))


nfl_play_by_play_data_run_pass %>% filter(PlayType == "No Play") %>% 
  select(pass, rush, desc, play_call) %>% 
  head




play_by_play_run_pass <- nfl_play_by_play_data_run_pass %>% 
  filter(pass == 1 | rush == 1)


version1_nfl_data <- play_by_play_run_pass %>% 
  select(date = Date, game_id = GameID, qtr, down, time, minutes_remaining_in_half = TimeUnder, seconds_remaining_in_game = TimeSecs, scrimmage_side_of_field = SideofField,
         yardline = yrdln, distance_to_endzone = yrdline100, yards_to_go = ydstogo, net_yards = ydsnet, goal_down = GoalToGo, possession_team = posteam, defensive_team = DefensiveTeam,
         possession_team_score = PosTeamScore, defensive_team_score = DefTeamScore, score_differential = ScoreDiff, home_team = HomeTeam, away_team = AwayTeam,
         preplay_timeouts_remaining_possession_team = posteam_timeouts_pre, preplay_timeouts_remaining_hometeam = HomeTimeouts_Remaining_Pre, preplay_timeouts_remaining_awayteam = AwayTimeouts_Remaining_Pre,
         no_score_within_half_prob = No_Score_Prob, defense_safety_prob = Opp_Safety_Prob, possession_team_fieldgoal_prob = Field_Goal_Prob, possession_team_touchdown_prob = Touchdown_Prob,
         expected_points_possession_team = ExpPts, expected_points_added = EPA, preplay_hometeam_win_prob = Home_WP_pre, preplay_awayteam_win_prb = Away_WP_pre, win_prob_added_possession_team = Win_Prob,
         pass, rush, success, play_call)

version1_nfl_data <- na.omit(version1_nfl_data)

#saveRDS(version1_nfl_data, "/home/taudin/MiscFiles/Spring20/MATH456/NFLProject/data/version_1.rds")


