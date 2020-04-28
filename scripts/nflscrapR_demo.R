library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(ggrepel)

#Read in the data that we currently have.
nfl_play_by_play_data <- read_csv("/home/taudin/MiscFiles/Spring20/MATH456/NFLData/nflplaybyplay.csv")


# Load the Data -----------------------------------------------------------

#This reads in the 2008 play-by-play data from github.
play_by_play_data <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))

#Let's look at the first few rows of the data.
play_by_play_data %>% select(posteam, defteam, desc, play_type) %>% 
  head


nfl_play_by_play_data %>% select(posteam, DefensiveTeam, desc, PlayType) %>% 
  head

#Already sorted by time, these are the first 6 rows from the season opener, ATL at PHI.

#We're going to fix some plays that are misclassified. Focus on run and pass plays, throw out punts, kickoffs, field goals, and dead-ball penalties.
play_by_play_run_pass <- play_by_play_data %>% 
  filter(!is_na(epa), play_type == "no_play" | play_type == "pass" | play_type == "run")

nfl_play_by_play_data_run_pass <- nfl_play_by_play_data %>% 
  filter(!is_na(EPA), PlayType == "No Play" | PlayType == "Pass" | PlayType == "Run")

#Let's look at what the first few rows of this new data frame look like.
play_by_play_run_pass %>% select(posteam, desc, play_type) %>% 
  head

nfl_play_by_play_data_run_pass %>% select(posteam, desc, PlayType) %>% 
  head

#The kickoff is now gone.

#Look at the no_play plays.
play_by_play_run_pass %>% filter(play_type == "no_play") %>% 
  select(desc, rush_attempt, pass_attempt) %>% 
  head

nfl_play_by_play_data_run_pass %>% filter(PlayType == "No Play") %>% 
  select(desc, RushAttempt, PassAttempt) %>% 
  head

#The false start to open the game is still there, but the rest are attempted rush or pass plays that should count when computing EPA. rush_attempt and pass_attempt are 0 for all these
#plays. Search the desc variable to look for indicators of passing or rushing.

#There's an illegal formation penalty called on TEN during a TEN pass play that counts as an attempt. This is fine. We need to make sure all such plays are probperly attributed to their
#respective category of run or pass plays.

play_by_play_run_pass <- play_by_play_run_pass %>% 
  mutate(pass = if_else(str_detect(desc, "(pass) | (sacked) | (scamble)"), 1, 0), 
         rush = if_else(str_detect(desc, "(left end) | (left tackle) | (left guard) | (up the middle) | (right guard) | (right tackle) | (right end)") & pass == 0, 1, 0),
         success = ifelse(epa > 0, 1, 0))

nfl_play_by_play_data_run_pass <- nfl_play_by_play_data_run_pass %>% 
  mutate(pass = if_else(str_detect(desc, "(pass) | (sacked) | (scramble)"), 1, 0),
         rush = if_else(str_detect(desc, "(left end) | (left tackle) | (left guard) | (up the middle) | (right guard) | (right tackle) | (right end)") & pass == 0, 1, 0),
         success = ifelse(EPA > 0, 1, 0))

#We've created pass, which searches the desc variable for plays with "pass", "sacked", or "scramble", along with a variable for rush and a variable for a succussful play (positive EPA).

#Let's take a look.
play_by_play_run_pass %>% filter(play_type == "no_play") %>% 
  select(pass, rush, desc) %>% 
  head

nfl_play_by_play_data_run_pass %>% filter(PlayType == "No Play") %>% 
  select(pass, rush, desc) %>% 
  head

#We have variables for pass and rush that includes plays for penalties.

#Now save, keeping only the run or pass plays.
play_by_play_run_pass <- play_by_play_run_pass %>% 
  filter(pass == 1 | rush == 1)

#We call dropbacks pass plays and non-dropbacks rush plays.

##Look at how various Rams' running backs fared on run plays in 2018.
play_by_play_run_pass %>% filter(posteam == "LA", rush == 1, down <= 4) %>% 
  group_by(rusher_player_name) %>% 
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc = mean(yards_gained), plays = n()) %>% 
  arrange(desc(mean_epa)) %>% 
  filter(plays > 40)

#Look at how various Saints running backs fared on run plays over the 8 yer period.
nfl_play_by_play_data_run_pass %>% filter(posteam == "NO", rush == 1, down <= 4) %>% 
  group_by(Rusher) %>% 
  summarize(mean_epa = mean(EPA), success_rate = mean(success), ypc = mean(Yards.Gained), plays = n()) %>% 
  arrange(desc(mean_epa)) %>% 
  filter(plays > 40)

#By specifying down <= 4, I've excluded carries on two point conversions (those have down == NA), which are present in the data.

#Look at which teams were the most pass-heavy in the first half on early downs with win probability between 20 and 80, excluding the final two minutes of the half.
pass_heavy <- play_by_play_run_pass %>%
  filter((pass == 1 | rush == 1) & wp > 0.2 & wp < 0.8 & down <= 2 & qtr <= 2 & quarter_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(mean_pass = mean(pass), plays = n()) %>%
  arrange(mean_pass)

pass_heavy


period_pass_heavy <- nfl_play_by_play_data_run_pass %>% 
  filter((pass == 1 | rush == 1) & Win_Prob > 0.2 & Win_Prob < 0.8 & down <= 2 & qtr <= 2 & TimeSecs > 120) %>% 
  group_by(posteam) %>% 
  summarize(mean_pass = mean(pass), plays = n()) %>% 
  arrange(mean_pass)

period_pass_heavy


#See what that looks like.
ggplot(pass_heavy, aes(x = reorder(posteam, -mean_pass), y = mean_pass)) +
  geom_text(aes(label = posteam))

ggplot(period_pass_heavy, aes(x = reorder(posteam, -mean_pass), y = mean_pass)) +
  geom_text(aes(label = posteam))
#ggsave(".png", dpi = 1000)

#We used the pass variable that we created, counting plays with penalties.

#Make dropback success rate vs EPA plot
chart_data <- play_by_play_run_pass %>% 
  filter(pass == 1) %>% 
  group_by(posteam) %>% 
  summarise(num_db = n(), epa_per_db = sum(epa) / num_db, success_rate = sum(epa > 0) / num_db)

nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")

chart <- chart_data %>% 
  left_join(nfl_logos_df, by = c("posteam" = "team_code"))

#We're taking all plays with pass == 1 and grouping by team to get the mean of EPA / play and success rate. Take the team logos and join that to the chart data.

#Note the code below won't work since there is a problem with the Las Vegas Raiders logo upload url.
chart %>% ggplot(aes(x = success_rate, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Success Rate", y = "EPA per Play", caption = "Data from nflscrapR", title = "Dropback Success Rate & EPA/Play", subtitle = "2018") +
  theme_bw() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10), plot.title = element_text(size = 16), plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#Look at rushing and passing EPA/play at the team level on first and second down.
chart_data <- play_by_play_run_pass %>% 
  group_by(posteam) %>% 
  filter(down <= 2) %>% 
  summarise(n_dropbacks = sum(pass), n_rush = sum(rush), epa_per_db = sum(epa * pass) / n_dropbacks, epa_per_rush = sum(epa * rush) / n_rush,
            success_per_db = sum(success * pass) / n_dropbacks, success_per_rush = sum(success * pass) / n_rush)

chart <- chart_data %>% 
  left_join(nfl_logos_df, by = c("posteam" = "team_code"))

#This code won't run as well for the same reason as above (issues with Raiders url).
chart %>% ggplot(aes(x = epa_per_rush, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Rush EPA/Play", y = "Pass EPA/play", caption = "Data from nflscrapR", title = "Early-down Rush and Pass EPA/Play", subtitle = "2018") +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, alpha = 0.2) +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10), plot.title = element_text(size = 16), plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

#ggsave(".png", dpi = 1000)

#Let's look at the same graph but with success rate instead.
#This code won't run as well for the same reason as above (issues with Raiders url).
chart %>% ggplot(aes(x = success_per_rush, y = success_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Rush Success Rate", y = "Pass Success Rate", caption = "Data from nflscrapR", title = "Early-down Rush and Pass Success Rate", subtitle = "2018") +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0, alpha = 0.2) +
  theme(axis.title = element_text(size = 12), axis.title = element_text(size = 12), axis.text = element_text(size = 10), plot.title = element_text(size = 16),
        plot.title = element_text(size = 14), plot.caption = element_text(size = 12))

#ggsave(".png", dpi = 1000)

#Check player names on plays with penalties.
play_by_play_run_pass %>% filter(play_type == "no_play") %>% 
  select(desc, pass, passer_player_name, rusher_player_name, receiver_player_name) %>% 
  head

#NA's are missing player names for passer, runner, and receiver.
pbp_players <- play_by_play_run_pass %>% 
  mutate(passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                     str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3}) | (IV))?(?=\\s((oass) | (sack) | (scramble)))"),
                                     passer_player_name),
         receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), str_extract(desc,
                                                                                                      "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3}) | (IV))?"),
                                       receiver_player_name),
         rusher_player_name = ifelse(play_type == "no_play" & rush == 1, str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3}) | (IV))?(?=\\s((left end) |(left tackle) | (left guard) | (up the middle) | (right guard) | (right tackle) | (right end)))"),
                                     rusher_player_name))

qbs <- pbp_players %>% 
  mutate(name = ifelse(!is.na(passer_player_name), passer_player_name, rusher_player_name), rusher = rusher_player_name, receiver = receiver_player_name, play = 1) %>% 
  group_by(name, posteam) %>% 
  summarize(n_dropbacks = sum(pass), n_rush = sum(rush), n_plays = sum(play), epa_per_play = sum(epa) / n_plays, success_per_play = sum(success) / n_plays) %>% 
  filter(n_dropbacks >= 100)

#Make a plot for QB's. Created a name variable that's the passer name if it's a pass and the rusher's name if it's a rush. Filtered to have at least 100 dropbacks.

#Plot results.
qbs %>%
  ggplot(aes(x = success_per_play, y = epa_per_play)) +
  geom_hline(yintercept = mean(qbs$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(qbs$success_per_play), color = "red", linetype = "dashed") +
  geom_point(color = ifelse(qbs$posteam == "SF", "red", "black"), cex = qbs$n_plays / 60, alpha = 1 / 4) +
  geom_text_repel(aes(label = name), force = 1, point.padding = 0, segment.size = 0.1) +
  labs(x = "Success Rate", y = "EPA per Play", caption = "Data from nflscrapR", title = "QB Success Rate and EPA/Play", subtitle = "2018, min 100 pass attempts, includes all QB's rush and pass plays") +
  theme_bw() +
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 10), plot.title = element_text(size = 16, hjust = 0.5), plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))


#The following grabs the years of available data and also adds on the results from each game which are stored in a different file.
first <- 2009     #First season to grab
last <- 2018      #Most recent season

datalist <- list()

#for (yr in first:last) {
#  play_by_play_data <- read_csv(url(paste0("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_", yr, ".csv")))
#  games <- read_csv(url(paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", yr, ".csv")))
#  play_by_play_data <- play_by_play_data %>% inner_join(games %>% distinct(game_id, week, season)) %>% select(-fumble_recovery_2_yards, -blocked_player_id, - blocked_player_name, -fumble_recovery_2_team,
#                                                                                  -fumble_recovery_2_player_id, -fumble_recovery_2_player_name)
#  datalist[[yr]] <- play_by_play_data # add it to your list
#}

pbp_2009 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2009.csv"))
pbp_2010 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2010.csv"))

saveRDS(chart_data, file = "/home/taudin/MiscFiles/Spring20/MATH456/NFLProject/scripts/kris_test/analysis_data")
