library(dplyr)
library(ggplot2)

#We'll be using data from Sean Lahaman's Website a very useful source for baseball statistics. The documentation for the csv files is located in the readme2013.txt file. You may need to reference this to understand what acronyms stand for.
bat <- read.csv('Batting.csv')
sal <- read.csv('salaries.csv')

######################
## Feature Engineering
######################

print(str(bat))

#We need to add three more statistics that were used in Moneyball! These are:

#Batting Average
bat$BA <- bat$H/bat$AB

#On baset Percentage
bat$OBP <- (bat$H + bat$BB + bat$HBP)/(bat$AB + bat$BB + bat$HBP + bat$SF)

# Creating X1B (Singles)
bat$X1B <- bat$H - bat$X2B -bat$X3B - bat$HR

#Slugging Percentage
bat$SLG <- (bat$X1B + 2*bat$X2B + 3*bat$X3B + 4*bat$HR)/bat$AB

#Use subset() to reassign batting to only contain data from 1985 and onwards
bat <- subset(bat, yearID >= 1985)
print(str(bat))

#We know we don't just want the best players, we want the most undervalued players, meaning we will also need to know current salary information! 
combo <- merge(bat, sal, by = c('playerID', 'yearID'))
summary(combo)

###############################
## Analyzing the Lost Players
##############################

#the Oakland A's lost 3 key players during the off-season. We'll want to get their stats to see what we have to replace.
lost_players <- subset(combo, playerID %in% c('giambja01', 'damonjo01', 'saenzol01'))

#Since all these players were lost in after 2001 in the offseason, let's only concern ourselves with the data from 2001.
lost_players <- subset(lost_players, yearID == 2001)

#Reduce the lost_players data frame to the following columns: playerID,H,X2B,X3B,HR,OBP,SLG,BA,AB
lost_players <- lost_players[, c('playerID','H','X2B','X3B','HR','OBP','SLG','AB','BA')]
head(lost_players)

#######################
## Replacement Players
#######################

#Now we have all the information we need! Here is our final task - Find Replacement Players for the key three players we lost! However, you have three constraints:
#1. The total combined salary of the three players can not exceed 15 million dollars.
#2. Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
#3. Their mean OBP had to equal to or greater than the mean OBP of the lost players

#There's lost of different ways you can do this, so be creative! It should be relatively simple to find 3 players that satisfy the requirements, note that there are many correct combinations available!


#First only grab available players from year 2001
avail_players <- subset(combo, yearID == 2001)

# a quick plot to see where I should cut-off for salary in respect to OBP
print(ggplot(avail_players, aes(OBP, salary))  + geom_point())

#Looks like there is no point in paying above 8 million or so (I'm just eyeballing this number). I'll choose that as a cutt off point. There are also a lot of players with OBP==0. Let's get rid of them too.
#The total AB of the lost players is 1469. This is about 1500, meaning I should probably cut off my avail.players at 1500/3= 500 AB.
avail_players <-  avail_players %>% filter(salary<8000000,OBP>0) %>% filter(AB >= 500)

#Now let's sort by OBP and see what we've got!
possible <- head(arrange(avail_players,desc(OBP)),10)
possible <- possible[, c('playerID','OBP','SLG','AB')]
print(possible)

#Can't choose giambja again, but the other ones look good (2-4). I choose them!
replace <- possible[2:4,]
print(replace)
