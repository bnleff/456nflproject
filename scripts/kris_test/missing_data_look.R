nfl <- read.csv("/home/taudin/MiscFiles/Spring20/MATH456/NFLData/nflplaybyplay.csv")

table(is.na(nfl))

round(prop.table(table(is.na(nfl))) * 100, 1)

proportion_missing <- apply(nfl, 2, function(x) round(sum(is.na(x))) / NROW(x))

unique(proportion_missing)

library(ggplot2)
pmpv1_20 <- data.frame(variable = names(nfl)[1:20], pct.miss = proportion_missing[1:20])

ggplot(pmpv1_20, aes(x = variable, y = pct.miss)) +
  geom_bar(stat = "identity") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  geom_text(data = pmpv1_20, aes(label = paste0(round(pct.miss * 100, 1), "%"), y = pct.miss + 0.25), size = 4) +
  coord_flip()

pmpv21_40 <- data.frame(variable = names(nfl)[21:40], pct.miss = proportion_missing[21:40])

ggplot(pmpv21_40, aes(x = variable, y = pct.miss)) +
  geom_bar(stat = "identity") +
  ylab("Percent") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  geom_text(data = pmpv21_40, aes(label = paste0(round(pct.miss * 100, 1), "%"), y = pct.miss + 0.25), size = 4) +
  coord_flip()
