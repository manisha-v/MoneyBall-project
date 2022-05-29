library(dplyr)
library(ggplot2)

bat <- read.csv('Batting.csv')
sal <- read.csv('salaries.csv')

bat$BA <- bat$H/bat$AB
bat$OBP <- (bat$H + bat$BB + bat$HBP)/(bat$AB + bat$BB + bat$HBP + bat$SF)
bat$X1B <- bat$H - bat$X2B -bat$X3B - bat$HR
bat$SLG <- (bat$X1B + 2*bat$X2B + 3*bat$X3B + 4*bat$HR)/bat$AB

bat <- subset(bat, yearID >= 1985)

combo <- merge(bat, sal, by = c('playerID', 'yearID'))

lost_players <- subset(combo, playerID %in% c('giambja01', 'damonjo01', 'saenzol01'))

lost_players <- subset(lost_players, yearID == 2001)
lost_players <- lost_players[, c('playerID','H','X2B','X3B','HR','OBP','SLG','AB','BA')]

avail_players <- subset(combo, yearID == 2001)

print(ggplot(avail_players, aes(OBP, salary))  + geom_point())

avail_players <-  avail_players %>% filter(salary<8000000,OBP>0) %>% filter(AB >= 500)
possible <- head(arrange(avail_players,desc(OBP)),10)
possible <- possible[, c('playerID','OBP','SLG','AB')]
print(possible)
replace <- possible[2:4,]
print(replace)
