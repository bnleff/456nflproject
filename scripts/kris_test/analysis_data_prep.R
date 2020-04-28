library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)

nfl_data <- read.csv("/home/taudin/MiscFiles/Spring20/MATH456/NFLData/nflplaybyplay.csv", stringsAsFactors = FALSE)

filtered_nfl_data <- tibble(date = nfl_data$Date, game_id = nfl_data$GameID, drive = nfl_data$Drive, quarter = nfl_data$qtr,
                            down = nfl_data$down, start_play_time = nfl_data$time, minutes_left_in_half = nfl_data$TimeUnder,
                            seconds_left_in_game = nfl_data$TimeSecs, seconds_between_plays = nfl_data$PlayTimeDiff,
                            scrimmage_side_of_field = nfl_data$SideofField, yard_line = nfl_data$yrdln,
                            distance_to_endzone = nfl_data$yrdline100, yards_to_first_down = nfl_data$ydstogo,
                            drive_net_yards = nfl_data$ydsnet, goal_down = nfl_data$GoalToGo, first_down_success = nfl_data$FirstDown,
                            possession_team = nfl_data$posteam, defensive_team = nfl_data$DefensiveTeam, 
                            play_description = nfl_data$desc, yards_gained = nfl_data$Yards.Gained, scoring_play = nfl_data$sp,
                            touchdown = nfl_data$Touchdown, extra_point_result = nfl_data$ExPointResult, 
                            two_point_conversion = nfl_data$TwoPointConv, defensive_two_point = nfl_data$DefTwoPoint,
                            safety = nfl_data$Safety, onside_kick = nfl_data$Onsidekick, punt_result = nfl_data$PuntResult,
                            play_type = nfl_data$PlayType, pass_attempt = nfl_data$PassAttempt, pass_outcome = nfl_data$PassOutcome,
                            pass_length = nfl_data$PassLength, air_yards_attempted = nfl_data$AirYards,
                            yards_after_catch = nfl_data$YardsAfterCatch, qb_hit = nfl_data$QBHit, 
                            pass_location = nfl_data$PassLocation, interception_thrown = nfl_data$InterceptionThrown,
                            rush_attempt = nfl_data$RushAttempt, run_location = nfl_data$RunLocation, run_gap = nfl_data$RunGap,
                            reception = nfl_data$Reception, return_result = nfl_data$ReturnResult, 
                            field_goal_result = nfl_data$FieldGoalResult, field_goal_distance = nfl_data$FieldGoalDistance,
                            fumble = nfl_data$Fumble, fumble_recovery_team = nfl_data$RecFumbTeam, sack = nfl_data$Sack,
                            challenge = nfl_data$Challenge.Replay, challenge_result = nfl_data$ChalReplayResult,
                            penalty_accepted = nfl_data$Accepted.Penalty, penalized_team = nfl_data$PenalizedTeam,
                            penalty_type = nfl_data$PenaltyType, penalty_yards = nfl_data$Penalty.Yards, 
                            possession_team_score = nfl_data$PosTeamScore, defensive_team_score = nfl_data$DefTeamScore,
                            score_differential = nfl_data$ScoreDiff, absolute_score_differential = nfl_data$AbsScoreDiff,
                            home_team = nfl_data$HomeTeam, away_team = nfl_data$AwayTeam, timeout_charged = nfl_data$Timeout_Indicator,
                            timeout_team = nfl_data$Timeout_Team, possession_team_num_timouts_preplay = nfl_data$posteam_timeouts_pre,
                            hometeam_num_timeouts_preplay = nfl_data$HomeTimeouts_Remaining_Pre,
                            awayteam_num_timeouts_preplay = nfl_data$AwayTimeouts_Remaining_Pre,
                            hometeam_num_timeouts_postplay = nfl_data$HomeTimeouts_Remaining_Post,
                            awayteam_num_timeouts_postplay = nfl_data$AwayTimeouts_Remaining_Post,
                            prob_no_score_in_half = nfl_data$No_Score_Prob, prob_next_defensive_fieldgoal = nfl_data$Opp_Field_Goal_Prob,
                            prob_next_safety = nfl_data$Opp_Safety_Prob, prob_possession_team_next_fieldgoal = nfl_data$Field_Goal_Prob,
                            prob_next_offensive_safety = nfl_data$Safety_Prob, 
                            prob_touchdown_next_for_possession_team = nfl_data$Touchdown_Prob,
                            prob_extra_point = nfl_data$ExPoint_Prob, prob_two_point_conversion = nfl_data$TwoPoint_Prob,
                            preplay_expected_points_offense = nfl_data$ExpPts, postplay_expected_points_offense = nfl_data$EPA,
                            expected_points_from_air_yards = nfl_data$airEPA, expected_points_from_yac = nfl_data$yacEPA,
                            prob_win_hometeam_preplay = nfl_data$Home_WP_pre, prob_win_awayteam_preplay = nfl_data$Away_WP_pre,
                            prob_win_hometeam_postplay = nfl_data$Home_WP_post, prob_win_awayteam_postplay = nfl_data$Away_WP_post,
                            possession_team_added_win_probability = nfl_data$Win_Prob,
                            win_probability_added_with_respect_to_possession_team = nfl_data$WPA,
                            win_probability_added_from_air_yards = nfl_data$airWPA,
                            win_probability_added_from_yac = nfl_data$yacWPA, season = nfl_data$Season)

pass_and_run_plays <- filtered_nfl_data %>% 
  filter(!is.na(postplay_expected_points_offense), play_type == "No Play" | play_type == "Pass" | play_type == "Run")

pass_and_run_plays <- pass_and_run_plays %>% 
  mutate(pass = if_else(str_detect(play_description, "(pass) | (sacked) | (scramble)"), 1, 0), 
         rush = if_else(str_detect(play_description, "(left end) | (left tackle) | (left guard) | (up the middle) | (right guard) | (right tackle) | (right end)") & pass == 0, 1, 0),
         success = ifelse(postplay_expected_points_offense > 0, 1, 0))

pass_and_run_plays <- pass_and_run_plays %>% 
  filter(pass == 1 | rush == 1)

analysis_data <- pass_and_run_plays %>% 
  filter(pass == 1) %>% 
  group_by(possession_team) %>% 
  summarise(pass_attempts = n(), expected_points_added_pass_attempt = sum(postplay_expected_points_offense) / pass_attempts, 
            success_rate = sum(postplay_expected_points_offense > 0) / pass_attempts)

analysis_data <- pass_and_run_plays %>% 
  group_by(possession_team) %>% 
  filter(down <= 3) %>% 
  summarise(pass_attempts = sum(pass), rush_attempts = sum(rush), expected_points_added_pass_attempts = sum(postplay_expected_points_offense * pass) / pass_attempts,
            expected_points_added_rushes = sum(postplay_expected_points_offense * rush) / rush_attempts,
            success_per_pass_attempt = sum(success * pass) / pass_attempts, success_per_rush_attempt = sum(success * rush) / rush_attempts)

saveRDS(analysis_data , "/home/taudin/MiscFiles/Spring20/MATH456/NFLProject/scripts/kris_test/analysis_data2")
    