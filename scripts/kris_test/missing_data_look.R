library(tidyverse)
library(Hmisc)
library(ggplot2)
library(mice)
library(VIM)

nfl <- read.csv("/home/taudin/MiscFiles/Spring20/MATH456/NFLData/nflplaybyplay.csv")

#Make a smaller tibble of the variables that we've decided to consider for the project.
nfl_data <- tibble(first_down = nfl$FirstDown, offensive_team = nfl$posteam, defensive_team = nfl$DefensiveTeam,
                   yards_gained = nfl$Yards.Gained, extra_point_result = nfl$ExPointResult, two_point_conv = nfl$TwoPointConv,
                   defensive_two_point_conversion = nfl$DefTwoPoint, pass_result = nfl$PassOutcome, pass_length = nfl$PassLength,
                   yards_after_catch = nfl$YardsAfterCatch, run_location = nfl$RunLocation, run_gap = nfl$RunGap,
                   field_goal_result = nfl$FieldGoalResult, field_goal_distance = nfl$FieldGoalDistance, 
                   fumble_recovery_team = nfl$RecFumbTeam, penalized_team = nfl$PenalizedTeam, offensive_team_score = nfl$PosTeamScore,
                   defensive_team_score = nfl$DefTeamScore, score_difference = nfl$ScoreDiff)


#Take a look at the first few lines.
head(nfl_data)

str(nfl_data, vec.len = 2, strict.width = "no", width = 30)

#As we can see most of the variables are factors with various level amounts along with some integers.
#We have 362,447 observations of 19 variables which is quite a trim down from the 102 variables of the original dataset.

levels_factors <- function(mydata){
  col_names <- names(mydata)
  for (i in 1:length(col_names)){
    if (is.factor(mydata[, col_names[i]])){
      message(noquote(paste("Covariate ", "*", col_names[i], "*", " with factor levels:", sep = "")))
      print(levels(mydata[, col_names[i]]))
    }
  }
}

levels_factors(nfl_data)

#Lets get a quick summary of the variables to get a look at what we're dealing with.
describe(nfl_data)


#Identify rows with missing data.
table(is.na(nfl_data))

#There are 3,565,887 missing values.

#What percent of the dataset is missing?
round(prop.table(table(is.na(nfl_data))) * 100, 1)

#This tells us that 51.8% percent of the data here is missing.

#How much is missing per variable?
prop_missing <- apply(nfl_data, 2, function(x)round(sum(is.na(x)) / NROW(x), 4))
prop_missing


# Visualize Missing Patterns ----------------------------------------------

proportion_missing_per_variable <- data.frame(variable = names(nfl_data), percentage_missing = prop_missing)
ggplot(proportion_missing_per_variable, aes(x = variable, y = percentage_missing)) +
  geom_bar(stat = "identity") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  geom_text(data = proportion_missing_per_variable, aes(label = paste0(round(percentage_missing * 100, 1),
                                                                       "%"), y = percentage_missing + 0.25), size = 4) +
  coord_flip()


# Using mice --------------------------------------------------------------

md.pattern(nfl_data, rotate.names = TRUE)


# Using VIM ---------------------------------------------------------------

aggr(nfl_data, col = c("chartreuse3", "mediumvioletred"), numbers = TRUE, sortVars = TRUE, labels = names(nfl_data), cex.axis = 0.7,
     gap = 3, ylab = c("Missing Data", "Pattern"))

  