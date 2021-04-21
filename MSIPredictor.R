preData = read.csv(file = "2021_LoL_esports_match_data_from_OraclesElixir_20210419.csv")

# Handle Data Completeness
data = preData[which(preData$datacompleteness != "partial"),]

summary(data)
colnames(data)

# playerid is a number 1 - 10 for the players and it is 100 for blue team and 200 for red team
removeColumns = c("datacompleteness","gameid","url","year","date","game" )
data = data[,!(names(data) %in% removeColumns)]

#probably should seperate by region
unique(data["league"])

# player processing
players = data[which(data$position != "team"),]
players <- players[,colSums(is.na(players))<nrow(players)]
dropInhibStuff = c("inhibitors","opp_inhibitors")
players = players[,!(names(players) %in% dropInhibStuff)]
summary(players)

players$result <- as.numeric(players$result)
players$firstbloodkill = as.factor(players$firstbloodkill)
players$firstbloodassist = as.factor(players$firstbloodassist)
players$firstbloodvictim = as.factor(players$firstbloodvictim)
basedPlayersModel = lm(result ~ .-league-player-split-team-ban1-ban2-ban3-ban4-ban5-champion-playoffs-patch-playerid-golddiffat15-xpdiffat15-csdiffat15
                       -golddiffat10-xpdiffat10-csdiffat10-monsterkills-firstbloodassist, data = players)
summary(basedPlayersModel)

 

# Humoring Yumply with how to use vision score to predict the victory
visionScorePred = lm(result ~ visionscore, data = players) #bad way
summary(visionScorePred)

supports = data[which(data$position == "sup"),]
visionScorePredSupp = lm(result ~ vspm + wpm + wcpm + assistsat10 + assistsat15, data = supports) # might be the best we can do with just vision stats
visionScorePredSupp = lm(result ~ vspm, data = supports)
summary(visionScorePredSupp)
visionScorePredSupp = lm(result ~ visionscore, data = supports)
summary(visionScorePredSupp)
# what we learned from the Yumply Theory, per minute is better than total

# team processing
library(tree)

team = data[which(data$position == "team"),]
team <- team[,colSums(is.na(team))<nrow(team)]

team$result = as.factor(team$result)

team$firstdragon = as.factor(team$firstdragon)
team$firstblood = as.factor(team$firstblood)
team$firstbaron = as.factor(team$firstbaron)
team$firstherald = as.factor(team$firstherald)
team$firstmidtower = as.factor(team$firstmidtower)
team$firsttower = as.factor(team$firsttower)
team$firsttothreetowers = as.factor(team$firsttothreetowers)

team$firstdragon = as.numeric(team$firstdragon)
team$firstblood = as.numeric(team$firstblood)
team$firstbaron = as.numeric(team$firstbaron)
team$firstherald = as.numeric(team$firstherald)
team$firstmidtower = as.numeric(team$firstmidtower)
team$firsttower = as.numeric(team$firsttower)
team$firsttothreetowers = as.numeric(team$firsttothreetowers)
team$result = as.numeric(team$result)

team$

lmTeam = lm(result~-league-player-split-team-ban1-ban2-ban3-ban4-ban5-champion-playoffs-patch-playerid-golddiffat15-xpdiffat15-csdiffat15
            -golddiffat10-xpdiffat10-csdiffat10-monsterkills-infernals-mountains-clouds-oceans-elders, data=team)
summary(lmTeam)
treeTeam=tree(result~.,team)
plot(treeTeam)
text(treeTeam,pretty=0)






# All Regions in MSI
lcs = data[which(data$league == "LCS"),]
lec = data[which(data$league == "LEC"),]
lpl = data[which(data$league == "LPL"),]
lck = data[which(data$league == "LCK"),]
vcs = data[which(data$league == "VCS"),]
lcl = data[which(data$league == "LCL"),]
lco = data[which(data$league == "LCO"),]
pcs = data[which(data$league == "PCS"),]
tcl = data[which(data$league == "TCL"),]
cblol = data[which(data$league == "CBLOL"),]
lla = data[which(data$league == "LLA"),]
ljl = data[which(data$league == "LJL"),]

champCols = data[c("ban1","ban2","ban3","ban4","ban5", "champion")]





