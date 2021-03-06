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
glmPlayer = glm(result ~ .-league-player-split-team-ban1-ban2-ban3-ban4-ban5-champion-playoffs-patch-playerid-golddiffat15-xpdiffat15-csdiffat15
                       -golddiffat10-xpdiffat10-csdiffat10-monsterkills-firstbloodassist, data = players)
 
summary(glmPlayer)

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
team = team[,!(names(team) %in% "position")]
team = na.omit(team)

team$result = as.factor(team$result)
team$result = as.numeric(team$result)
n=nrow(team)
train = sample(1:n, n/2)


library(dplyr)
lmTeam = lm(result~ ., data= subset(team, select=-c(league, split, playoffs, patch, playerid, side,                                                     player, team, champion, ban1, ban2, ban3, ban4, ban5)))
# Michael
summary(lmTeam)

treeTeam = tree(result~.-inhibitors-opp_towers-opp_inhibitors-towers-earnedgold-deaths-assists
                -damagetochampions-teamdeaths-ckpm-damagetakenperminute-opp_barons-league, data = team, subset=train)
summary(treeTeam)
test=team[-train,"result"]
yhat=predict(treeTeam,newdata=team[-train,])
mean((yhat-test)^2)

treeTeam15 = tree(result~golddiffat15+firsttothreetowers,data=team,subset=train)
summary(treeTeam15)
yhat15=predict(treeTeam15,newdata=team[-train,])
mean((yhat15-test)^2)

treeKDA = tree(result~kills+deaths+assists,data=team,subset=train)
summary(treeKDA)
yhatKDA=predict(treeKDA,newdata=team[-train,])
mean((yhatKDA-test)^2)

# smells
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
lcs[1:20]

library(elo)
# datapoints that matter.
# gold earned
# gamelength
# For top, bot, mid: cspm, KDA, 

View(lcs)
treeTeamFT=tree(result~firsttower,data = team)
summary(treeTeamFT)
treeTeamFT3 = tree(result~firsttothreetowers, data = team)
summary(treeTeamFT3)
treeTeamEGPM=tree(result~earned.gpm,data = team)
summary(treeTeamEGPM)
treeTeamInhib = tree(result~inhibitors, data = team)
summary(treeTeamInhib)
treeTeam