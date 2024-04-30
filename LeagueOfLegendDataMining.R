
#first of all we import our dataset
library(arules)
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
data <- read.csv('/home/pandaman/dataMiningProject/high_diamond_ranked_10min.csv')

# summary and correlations before pre processing our data
summary(data)

# Plot of the correlation matrix
corr_matrix <- cor(data)
heatmap(corr_matrix,
        col = colorRampPalette(c("white", "red", "black"))(100),
        scale = "none",
        symm = TRUE,
        margins = c(10,10),
        
)

# Pre-Processing : 

# our goal is to find patterns in the player's behaviors in the winning team
# we will split our data into 2 subsets
# the dataset blueWinsDF  when blue wins and the dataset redWinsDF when red wins
# we will also drop the columns that add no more information in our dataframe
#for example, we don't need blueFirstBlood and redFirstBlood, redKills and blueDeaths
# a player can be killed by a monster but fortunately it never happen in our data
# according to the coefficient correlation between blueDeaths and redKills = 1, and blueKills and redDeaths = 1

# in blueWinsDf we will drop all the columns concerning the red team
# and do the same in redWinsDf

blueWinsDf <- subset(data, blueWins == 1, select = -c(blueWins, redFirstBlood, redKills, redDeaths, redGoldDiff, redExperienceDiff))
redWinsDf <- subset(data, blueWins == 0, select = -c(blueWins, blueFirstBlood, blueKills, blueDeaths, blueGoldDiff, blueExperienceDiff))
summary(redWinsDf)
summary(blueWinsDf)

#We will rename the columns to have the Winning Team's point of view

columnsNameRedWinsDfModif <- c("gameId", "opponentWardsPlaced", "opponentWardsDestroyed","opponentAssists", "opponentEliteMonsters", "opponentDragons", "opponentHeralds", "opponentTowersDestroyed", "opponentTotalGold", "opponentAvgLevel", "opponentTotalExperience", "opponentTotalMinionsKilled", "opponentTotalJungleMinionsKilled", "opponentCSPerMin", "opponentGoldPerMin", "WardsPlaced", "WardsDestroyed", "FirstBlood", "Kills", "Deaths", "Assists", "EliteMonsters", "Dragons", "Heralds", "TowersDestroyed", "TotalGold", "AvgLevel", "TotalExperience", "TotalMinionsKilled", "TotalJungleMinionsKilled", "GoldDiff", "ExperienceDiff", "CSPerMin", "GoldPerMin" )
columnsNameBlueWinsDfModif <- c("gameId", "WardsPlaced", "WardsDestroyed", "FirstBlood", "Kills", "Deaths", "Assists", "EliteMonsters", "Dragons", "Heralds", "TowersDestroyed", "TotalGold", "AvgLevel", "TotalExperience", "TotalMinionsKilled", "TotalJungleMinionsKilled", "GoldDiff", "ExperienceDiff", "CSPerMin", "GoldPerMin", "opponentWardsPlaced", "opponentWardsDestroyed","opponentAssists", "opponentEliteMonsters", "opponentDragons", "opponentHeralds", "opponentTowersDestroyed", "opponentTotalGold", "opponentAvgLevel", "opponentTotalExperience", "opponentTotalMinionsKilled", "opponentTotalJungleMinionsKilled", "opponentCSPerMin", "opponentGoldPerMin")
colnames(blueWinsDf) <- columnsNameBlueWinsDfModif
colnames(redWinsDf) <- columnsNameRedWinsDfModif
summary(redWinsDf)
summary(blueWinsDf)

# Now we will re ordinate the columns on the RedWinsDf data frame to have the same order of columns than the BlueWinsDf dataframe

print(colnames(blueWinsDf))
print(colnames(redWinsDf))

redWinsDf <- redWinsDf[,c("gameId","WardsPlaced","WardsDestroyed","FirstBlood","Kills","Deaths","Assists","EliteMonsters","Dragons","Heralds","TowersDestroyed","TotalGold","AvgLevel","TotalExperience","TotalMinionsKilled","TotalJungleMinionsKilled","GoldDiff","ExperienceDiff","CSPerMin","GoldPerMin","opponentWardsPlaced","opponentWardsDestroyed","opponentAssists","opponentEliteMonsters","opponentDragons","opponentHeralds","opponentTowersDestroyed","opponentTotalGold","opponentAvgLevel","opponentTotalExperience","opponentTotalMinionsKilled","opponentTotalJungleMinionsKilled","opponentCSPerMin","opponentGoldPerMin")]
print(colnames(blueWinsDf))
print(colnames(redWinsDf))

summary(redWinsDf)
summary(blueWinsDf)

#Now we can merge the two subsets 

df_merged <- rbind(blueWinsDf, redWinsDf)

df_mergedBinarized <- df_merged
rm(df_merged)
gc()
rm(blueWinsDf)
gc()
rm(redWinsDf)
gc()
# We have now 34 features. As our goal is to apply apriori algorithm on our data, we have to binarize all the features. 


# WardsPlaced Min : 5 Max 276, Mean: 22.55, Median : 17
#For this feature we can see that the max is very high compared to the mean
# Let's see the distribution and the sparsity for the feature. 



#we will do three new features for this one "<20(frequent)", "<50(rare)", ">50(very rare)"

df_mergedBinarized$WardsPlacedLessThan20 <- as.integer(df_mergedBinarized$WardsPlaced < 20)
df_mergedBinarized$WardsPlacedLessThan50 <- as.integer(df_mergedBinarized$WardsPlaced < 50 & df_mergedBinarized$WardsPlaced >= 20)
df_mergedBinarized$WardsPlacedMoreThan50 <- as.integer(df_mergedBinarized$WardsPlaced > 50)
df_mergedBinarized$WardsPlaced <- NULL

#We will now apply this process as much as needed 
#WardsDestroyed : 

df_mergedBinarized$WardsDestroyedLessthan5 <- as.integer(df_mergedBinarized$WardsDestroyed < 5)
df_mergedBinarized$WardsDestroyedMorethan5 <- as.integer(df_mergedBinarized$WardsDestroyed >= 5)
df_mergedBinarized$WardsDestroyed <- NULL

#Kills

df_mergedBinarized$KillsLessthan7 <- as.integer(df_mergedBinarized$Kills <7)
df_mergedBinarized$KillsMorethan7 <- as.integer(df_mergedBinarized$Kills >=7)
df_mergedBinarized$Kills <- NULL


# Deaths

df_mergedBinarized$DeathsLessthan5 <- as.integer(df_mergedBinarized$Deaths <5)
df_mergedBinarized$DeathsMorethan5 <- as.integer(df_mergedBinarized$Deaths >=5)
df_mergedBinarized$Deaths <- NULL

# Assists

df_mergedBinarized$AssistsLessthan7 <- as.integer(df_mergedBinarized$Assists <7)
df_mergedBinarized$AssistsMorethan7 <- as.integer(df_mergedBinarized$Assists >=7)
df_mergedBinarized$Assists <- NULL


#EliteMonsters

df_mergedBinarized$EliteMonsters0 <- as.integer(df_mergedBinarized$EliteMonsters ==0)
df_mergedBinarized$EliteMonsters1 <- as.integer(df_mergedBinarized$EliteMonsters == 1)
df_mergedBinarized$EliteMonsters2 <- as.integer(df_mergedBinarized$EliteMonsters == 2)
df_mergedBinarized$EliteMonsters <- NULL

# TowerDestroyed

df_mergedBinarized$TowersDestroyedEqual0 <- as.integer(df_mergedBinarized$TowersDestroyed == 0)
df_mergedBinarized$TowersDestroyedEqual1 <- as.integer(df_mergedBinarized$TowersDestroyed == 1)
df_mergedBinarized$TowersDestroyedEqual2 <- as.integer(df_mergedBinarized$TowersDestroyed == 2)
df_mergedBinarized$TowersDestroyedEqual3 <- as.integer(df_mergedBinarized$TowersDestroyed == 3)
df_mergedBinarized$TowersDestroyedEqual4 <- as.integer(df_mergedBinarized$TowersDestroyed == 4)
df_mergedBinarized$TowersDestroyed <- NULL

# TotalGold

df_mergedBinarized$TotalGoldGreaterThan17000 <- as.integer(df_mergedBinarized$TotalGold > 17000)
df_mergedBinarized$TotalGoldLowerThan17000 <- as.integer(df_mergedBinarized$TotalGold <= 17000)
df_mergedBinarized$TotalGold <- NULL


# AvgLevel

df_mergedBinarized$AvgLevelGreaterThan7 <- as.integer(df_mergedBinarized$AvgLevel > 7)
df_mergedBinarized$AvgLevelLowerThan7 <- as.integer(df_mergedBinarized$AvgLevel <= 7)
df_mergedBinarized$AvgLevel <- NULL

# TotalExperience

df_mergedBinarized$TotalExperienceGreaterThan18500 <- as.integer(df_mergedBinarized$TotalExperience >= 18500)
df_mergedBinarized$TotalExperienceLowerThan18500 <- as.integer(df_mergedBinarized$TotalExperience < 18500)
df_mergedBinarized$TotalExperience <- NULL

# TotalMinionsKilled

df_mergedBinarized$TotalMinionsKilledGreaterThan222 <- as.integer(df_mergedBinarized$TotalMinionsKilled >= 222)
df_mergedBinarized$TotalMinionsKilledLowerThan222 <- as.integer(df_mergedBinarized$TotalMinionsKilled < 222)
df_mergedBinarized$TotalMinionsKilled <- NULL



#TotalJungleMinionsKilled

df_mergedBinarized$TotalJungleMinionsKilledGreaterThan52 <- as.integer(df_mergedBinarized$TotalJungleMinionsKilled > 52)
df_mergedBinarized$TotalJungleMinionsKilledLowerThan52 <- as.integer(df_mergedBinarized$TotalJungleMinionsKilled <= 52)
df_mergedBinarized$TotalJungleMinionsKilled <- NULL

#GoldDiff

df_mergedBinarized$GoldDiffPositive <- as.integer(df_mergedBinarized$GoldDiff >= 0 )
df_mergedBinarized$GoldDiffNegative <- as.integer(df_mergedBinarized$GoldDiff < 0)
df_mergedBinarized$GoldDiff <- NULL

#ExperienceDiff

df_mergedBinarized$ExperienceDiffPositive <- as.integer(df_mergedBinarized$ExperienceDiff >= 0 )
df_mergedBinarized$ExperienceDiffNegative <- as.integer(df_mergedBinarized$ExperienceDiff < 0)
df_mergedBinarized$ExperienceDiff <- NULL

#CSPerMin

df_mergedBinarized$CSPerMinLessThan22 <- as.integer(df_mergedBinarized$CSPerMin < 22)
df_mergedBinarized$CSPerMinMoreThan22 <- as.integer(df_mergedBinarized$CSPerMin >= 22)
df_mergedBinarized$CSPerMin <- NULL
#GoldPerMin

df_mergedBinarized$GoldPerMinLessThan1700 <- as.integer(df_mergedBinarized$GoldPerMin < 1700)
df_mergedBinarized$GoldPerMinMoreThan1700 <- as.integer(df_mergedBinarized$GoldPerMin >= 1700)
df_mergedBinarized$GoldPerMin <- NULL

#OpponentWardsPlaced

df_mergedBinarized$opponentWardsPlacedLessThan20 <- as.integer(df_mergedBinarized$opponentWardsPlaced < 20)
df_mergedBinarized$opponentWardsPlacedLessThan50 <- as.integer(df_mergedBinarized$opponentWardsPlaced < 50 & df_mergedBinarized$opponentWardsPlaced >= 20)
df_mergedBinarized$opponentWardsPlacedMoreThan50 <- as.integer(df_mergedBinarized$opponentWardsPlaced > 50)
df_mergedBinarized$opponentWardsPlaced <- NULL

#OpponentWardsDestroyed : 
df_mergedBinarized$opponentWardsDestroyedLessthan5 <- as.integer(df_mergedBinarized$opponentWardsDestroyed < 5)
df_mergedBinarized$opponentWardsDestroyedMorethan5 <- as.integer(df_mergedBinarized$opponentWardsDestroyed >= 5)
df_mergedBinarized$opponentWardsDestroyed <- NULL

# opponentAssists

df_mergedBinarized$opponentAssistsLessthan5 <- as.integer(df_mergedBinarized$opponentAssists <5)
df_mergedBinarized$opponentAssistsMorethan5 <- as.integer(df_mergedBinarized$opponentAssists >=5)
df_mergedBinarized$opponentAssists <- NULL

# opponentEliteMonsters

df_mergedBinarized$opponentEliteMonsters0 <- as.integer(df_mergedBinarized$opponentEliteMonsters ==0)
df_mergedBinarized$opponentEliteMonsters1 <- as.integer(df_mergedBinarized$opponentEliteMonsters == 1)
df_mergedBinarized$opponentEliteMonsters2 <- as.integer(df_mergedBinarized$opponentEliteMonsters == 2)
df_mergedBinarized$opponentEliteMonsters <- NULL

# opponentTowerDestroyed

df_mergedBinarized$opponentTowersDestroyedEqual0 <- as.integer(df_mergedBinarized$opponentTowersDestroyed == 0)
df_mergedBinarized$opponentTowersDestroyedEqual1 <- as.integer(df_mergedBinarized$opponentTowersDestroyed == 1)
df_mergedBinarized$opponentTowersDestroyedEqual2 <- as.integer(df_mergedBinarized$opponentTowersDestroyed == 2)

df_mergedBinarized$opponentTowersDestroyed <- NULL

# opponentTotalGold

df_mergedBinarized$opponentTotalGoldGreaterThan15800 <- as.integer(df_mergedBinarized$opponentTotalGold > 15800)
df_mergedBinarized$opponentTotalGoldLowerThan15800 <- as.integer(df_mergedBinarized$opponentTotalGold <= 15800)
df_mergedBinarized$opponentTotalGold <- NULL


# opponentAvgLevel

df_mergedBinarized$opponentAvgLevelGreaterThan7 <- as.integer(df_mergedBinarized$opponentAvgLevel > 7)
df_mergedBinarized$opponentAvgLevelLowerThan7 <- as.integer(df_mergedBinarized$opponentAvgLevel <= 7)
df_mergedBinarized$opponentAvgLevel <- NULL

# opponentTotalExperience

df_mergedBinarized$opponentTotalExperienceGreaterThan17500 <- as.integer(df_mergedBinarized$opponentTotalExperience >= 17500)
df_mergedBinarized$opponentTotalExperienceLowerThan17500 <- as.integer(df_mergedBinarized$opponentTotalExperience < 17500)
df_mergedBinarized$opponentTotalExperience <- NULL

# opponentTotalMinionsKilled

df_mergedBinarized$opponentTotalMinionsKilledGreaterThan212 <- as.integer(df_mergedBinarized$opponentTotalMinionsKilled >= 212)
df_mergedBinarized$opponentTotalMinionsKilledLowerThan212 <- as.integer(df_mergedBinarized$opponentTotalMinionsKilled < 212)
df_mergedBinarized$opponentTotalMinionsKilled <- NULL

# opponentTotalJungleMinionsKilled

df_mergedBinarized$opponentTotalJungleMinionsKilledGreaterThan50 <- as.integer(df_mergedBinarized$opponentTotalJungleMinionsKilled > 50)
df_mergedBinarized$opponentTotalJungleMinionsKilledLowerThan50 <- as.integer(df_mergedBinarized$opponentTotalJungleMinionsKilled <= 50)
df_mergedBinarized$opponentTotalJungleMinionsKilled <- NULL

# opponentCSPerMin

df_mergedBinarized$opponentCSPerMinLessThan22 <- as.integer(df_mergedBinarized$opponentCSPerMin < 22)
df_mergedBinarized$opponentCSPerMinMoreThan22 <- as.integer(df_mergedBinarized$opponentCSPerMin >= 22)
df_mergedBinarized$opponentCSPerMin <- NULL

# opponentGoldPerMin

df_mergedBinarized$opponentGoldPerMinLessThan1600 <- as.integer(df_mergedBinarized$opponentGoldPerMin < 1600)
df_mergedBinarized$opponentGoldPerMinMoreThan1600 <- as.integer(df_mergedBinarized$opponentGoldPerMin >= 1600)
df_mergedBinarized$opponentGoldPerMin <- NULL
head(df_mergedBinarized)
summary(df_mergedBinarized)



# This function allows us to chose the best number of clusters
# The following link provides the source of the wssplot function
# https://github.com/shetravel/K-Means-Clustering-Mini-Project/blob/master/1505932415_clustering.R
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="within groups sum of squares")
}

# According to the plot we can chose the hyper-parameter "number of clusters" to 3

wssplot(df_mergedBinarized)

km = kmeans(df_mergedBinarized, 3)
autoplot(km, df_mergedBinarized, frame=TRUE)

# Now we can analyse the centers of different clusters
km$centers

# Then our goal is to apply apriori algorithm
# to find interesting and underlying in our data


# Convert binary features to logical format
df_mergedBinarized$FirstBlood <- as.logical(df_mergedBinarized$FirstBlood)
df_mergedBinarized$Dragons <- as.logical(df_mergedBinarized$Dragons)
df_mergedBinarized$Heralds <- as.logical(df_mergedBinarized$Heralds)
df_mergedBinarized$opponentDragons <- as.logical(df_mergedBinarized$opponentDragons)
df_mergedBinarized$opponentHeralds <- as.logical(df_mergedBinarized$opponentHeralds)
df_mergedBinarized$WardsPlacedLessThan20 <- as.logical(df_mergedBinarized$WardsPlacedLessThan20)
df_mergedBinarized$WardsPlacedLessThan50 <- as.logical(df_mergedBinarized$WardsPlacedLessThan50)
df_mergedBinarized$WardsPlacedMoreThan50 <- as.logical(df_mergedBinarized$WardsPlacedMoreThan50)
df_mergedBinarized$WardsDestroyedLessthan5 <- as.logical(df_mergedBinarized$WardsDestroyedLessthan5)
df_mergedBinarized$WardsDestroyedMorethan5 <- as.logical(df_mergedBinarized$WardsDestroyedMorethan5)
df_mergedBinarized$KillsLessthan7 <- as.logical(df_mergedBinarized$KillsLessthan7)
df_mergedBinarized$KillsMorethan7 <- as.logical(df_mergedBinarized$KillsMorethan7)
df_mergedBinarized$DeathsLessthan5 <- as.logical(df_mergedBinarized$DeathsLessthan5)
df_mergedBinarized$DeathsMorethan5 <- as.logical(df_mergedBinarized$DeathsMorethan5)
df_mergedBinarized$AssistsLessthan7 <- as.logical(df_mergedBinarized$AssistsLessthan7)
df_mergedBinarized$AssistsMorethan7 <- as.logical(df_mergedBinarized$AssistsMorethan7)
df_mergedBinarized$EliteMonsters0 <- as.logical(df_mergedBinarized$EliteMonsters0)
df_mergedBinarized$EliteMonsters1 <- as.logical(df_mergedBinarized$EliteMonsters1)
df_mergedBinarized$EliteMonsters2 <- as.logical(df_mergedBinarized$EliteMonsters2)
df_mergedBinarized$TowersDestroyedEqual0 <- as.logical(df_mergedBinarized$TowersDestroyedEqual0)
df_mergedBinarized$TowersDestroyedEqual1 <- as.logical(df_mergedBinarized$TowersDestroyedEqual1)
df_mergedBinarized$TowersDestroyedEqual2 <- as.logical(df_mergedBinarized$TowersDestroyedEqual2)
df_mergedBinarized$TowersDestroyedEqual3 <- as.logical(df_mergedBinarized$TowersDestroyedEqual3)
df_mergedBinarized$TowersDestroyedEqual4 <- as.logical(df_mergedBinarized$TowersDestroyedEqual4)
df_mergedBinarized$TotalGoldGreaterThan17000 <- as.logical(df_mergedBinarized$TotalGoldGreaterThan17000)
df_mergedBinarized$TotalGoldLowerThan17000 <- as.logical(df_mergedBinarized$TotalGoldLowerThan17000)
df_mergedBinarized$AvgLevelGreaterThan7 <- as.logical(df_mergedBinarized$AvgLevelGreaterThan7)
df_mergedBinarized$AvgLevelLowerThan7 <- as.logical(df_mergedBinarized$AvgLevelLowerThan7)
df_mergedBinarized$TotalExperienceGreaterThan18500 <- as.logical(df_mergedBinarized$TotalExperienceGreaterThan18500)
df_mergedBinarized$TotalExperienceLowerThan18500 <- as.logical(df_mergedBinarized$TotalExperienceLowerThan18500)
df_mergedBinarized$TotalMinionsKilledGreaterThan222 <- as.logical(df_mergedBinarized$TotalMinionsKilledGreaterThan222)
df_mergedBinarized$TotalMinionsKilledLowerThan222 <- as.logical(df_mergedBinarized$TotalMinionsKilledLowerThan222)
df_mergedBinarized$TotalJungleMinionsKilledGreaterThan52 <- as.logical(df_mergedBinarized$TotalJungleMinionsKilledGreaterThan52)
df_mergedBinarized$TotalJungleMinionsKilledLowerThan52 <- as.logical(df_mergedBinarized$TotalJungleMinionsKilledLowerThan52)
df_mergedBinarized$CSPerMinLessThan22 <- as.logical(df_mergedBinarized$CSPerMinLessThan22)
df_mergedBinarized$CSPerMinMoreThan22 <- as.logical(df_mergedBinarized$CSPerMinMoreThan22)
df_mergedBinarized$GoldPerMinLessThan1700 <- as.logical(df_mergedBinarized$GoldPerMinLessThan1700)
df_mergedBinarized$GoldPerMinMoreThan1700 <- as.logical(df_mergedBinarized$GoldPerMinMoreThan1700)
df_mergedBinarized$opponentWardsPlacedLessThan20 <- as.logical(df_mergedBinarized$opponentWardsPlacedLessThan20)
df_mergedBinarized$opponentWardsPlacedLessThan50 <- as.logical(df_mergedBinarized$opponentWardsPlacedLessThan50)
df_mergedBinarized$opponentWardsPlacedMoreThan50 <- as.logical(df_mergedBinarized$opponentWardsPlacedMoreThan50)
df_mergedBinarized$opponentWardsDestroyedLessthan5 <- as.logical(df_mergedBinarized$opponentWardsDestroyedLessthan5)
df_mergedBinarized$opponentWardsDestroyedMorethan5 <- as.logical(df_mergedBinarized$opponentWardsDestroyedMorethan5)
df_mergedBinarized$opponentAssistsLessthan5 <- as.logical(df_mergedBinarized$opponentAssistsLessthan5)
df_mergedBinarized$opponentAssistsMorethan5 <- as.logical(df_mergedBinarized$opponentAssistsMorethan5)
df_mergedBinarized$opponentEliteMonsters0 <- as.logical(df_mergedBinarized$opponentEliteMonsters0)
df_mergedBinarized$opponentEliteMonsters1 <- as.logical(df_mergedBinarized$opponentEliteMonsters1)
df_mergedBinarized$opponentEliteMonsters2 <- as.logical(df_mergedBinarized$opponentEliteMonsters2)
df_mergedBinarized$opponentTowersDestroyedEqual0 <- as.logical(df_mergedBinarized$opponentTowersDestroyedEqual0)
df_mergedBinarized$opponentTowersDestroyedEqual1 <- as.logical(df_mergedBinarized$opponentTowersDestroyedEqual1)
df_mergedBinarized$opponentTowersDestroyedEqual2 <- as.logical(df_mergedBinarized$opponentTowersDestroyedEqual2)
df_mergedBinarized$opponentTotalGoldGreaterThan15800 <- as.logical(df_mergedBinarized$opponentTotalGoldGreaterThan15800)
df_mergedBinarized$opponentTotalGoldLowerThan15800 <- as.logical(df_mergedBinarized$opponentTotalGoldLowerThan15800)
df_mergedBinarized$opponentAvgLevelGreaterThan7 <- as.logical(df_mergedBinarized$opponentAvgLevelGreaterThan7)
df_mergedBinarized$opponentAvgLevelLowerThan7 <- as.logical(df_mergedBinarized$opponentAvgLevelLowerThan7)
df_mergedBinarized$opponentTotalExperienceGreaterThan17500 <- as.logical(df_mergedBinarized$opponentTotalExperienceGreaterThan17500)
df_mergedBinarized$opponentTotalExperienceLowerThan17500 <- as.logical(df_mergedBinarized$opponentTotalExperienceLowerThan17500)
df_mergedBinarized$opponentTotalMinionsKilledGreaterThan212 <- as.logical(df_mergedBinarized$opponentTotalMinionsKilledGreaterThan212)
df_mergedBinarized$opponentTotalMinionsKilledLowerThan212 <- as.logical(df_mergedBinarized$opponentTotalMinionsKilledLowerThan212)
df_mergedBinarized$opponentTotalJungleMinionsKilledGreaterThan50 <- as.logical(df_mergedBinarized$opponentTotalJungleMinionsKilledGreaterThan50)
df_mergedBinarized$opponentTotalJungleMinionsKilledLowerThan50 <- as.logical(df_mergedBinarized$opponentTotalJungleMinionsKilledLowerThan50)
df_mergedBinarized$opponentCSPerMinLessThan22 <- as.logical(df_mergedBinarized$opponentCSPerMinLessThan22)
df_mergedBinarized$opponentCSPerMinMoreThan22 <- as.logical(df_mergedBinarized$opponentCSPerMinMoreThan22)
df_mergedBinarized$opponentGoldPerMinLessThan1600 <- as.logical(df_mergedBinarized$opponentGoldPerMinLessThan1600)
df_mergedBinarized$opponentGoldPerMinMoreThan1600 <- as.logical(df_mergedBinarized$opponentGoldPerMinMoreThan1600)
df_mergedBinarized$GoldDiffNegative <- as.logical(df_mergedBinarized$GoldDiffNegative)
df_mergedBinarized$GoldDiffPositive <- as.logical(df_mergedBinarized$GoldDiffPositive)
df_mergedBinarized$ExperienceDiffNegative <- as.logical(df_mergedBinarized$ExperienceDiffNegative)
df_mergedBinarized$ExperienceDiffPositive <- as.logical(df_mergedBinarized$ExperienceDiffPositive)



df_mergedBinarized$gameId <- NULL

# Now we will consider each row as transaction

transactions <- as(df_mergedBinarized, "transactions")

#Now we can use the apriori function with support of 0.5 and confidence of 0.8
# Why this values for support and confidence ? 
# The goal was to limit the number of rules
# Indeed, previously we tested low value of support but the number of rules increase exponantially

rules <- apriori(transactions, parameter = list(support = 0.2, confidence = 0.8))

rules_df <- as(rules, "data.frame")

#To facilitate the reading, we created a csv file 
write.csv(rules_df, file="rules.csv", row.names=FALSE)
