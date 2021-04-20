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

basedPlayersModel = lm(result ~ .-league-player-split-team-ban1-ban2-ban3-ban4-ban5-champion-playoffs-patch-playerid-golddiffat15-xpdiffat15-csdiffat15
                       -golddiffat10-xpdiffat10-csdiffat10-monsterkills-firstbloodassist, data = players)
summary(basedPlayersModel)

(playoffs-patch-playerid-deaths-doublekills-quadrakills-pentakills-
    firstblood-firstbloodkill-firstbloodassist-damagetochampions-damagemitigatedperminute-wardskilled-totalgold-earnedgold-monsterkills-cspm-
    opp_goldat10-opp_csat10-golddiffat10-xpdiffat10-csdiffat10-deathsat10-opp_killsat10-opp_assistsat10-opp_deathsat10-opp_xpat15-
    opp_csat15-golddiffat15-xpdiffat15-csdiffat15)
(triplekills-wcpm-visionscore-opp_deathsat15)
(firstbloodvictim-dpm-vspm-opp_xpat10)
 

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
team = data[which(data$position == "team"),]










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





