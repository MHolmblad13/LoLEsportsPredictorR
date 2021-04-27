library(elo)
data(tournament)
summary(tournament)

summary(elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
        data = tournament, k = 20))
x = elo.run(score(points.Home, points.Visitor) ~ team.Home + team.Visitor,
            data = tournament, k = 20)
x
head(x)
score(points.Home, points.Visitor)

preData = read.csv(file = "2021_LoL_esports_match_data_from_OraclesElixir_20210419.csv")
data = preData[which(preData$datacompleteness != "partial"),]

team1 = data[which(data$position == "team"),]
team1 <- team1[,colSums(is.na(team1))<nrow(team1)]
team1 = na.omit(team1)

library(dplyr)
new = select(team1, gameid, team, result, league, gamelength, earned.gpm,kills, deaths, assists)
new$KDA = (new$kills+new$assists)/new$deaths

test = do.call("cbind", split(new, rep(c(1, 2), length.out = nrow(new))))
mean(test$`1.gamelength`)

big4 = new[new$league == 'LCS' | new$league == 'LCK' | new$league == 'LPL' | new$league == 'LEC' |
           new$league == "LCL" | new$league == 'LCO' | new$league == 'PCS' | new$league == 'TCL' |
             new$league == "CBLOL" | new$league == "LLA" | new$league == "LCL" | new$league == "LJL",]
colbig4 = do.call("cbind", split(big4, rep(c(1, 2), length.out = nrow(big4))))
colbig4 = select(colbig4, `1.gameid`, `1.team`, `2.team`, `1.result`, `2.result`,`1.gamelength`,
                 `1.league`, `1.earned.gpm`,`2.earned.gpm`,`1.KDA`,`2.KDA`)
colbig4$gpmDiff = colbig4$`1.earned.gpm` - colbig4$`2.earned.gpm`

names(colbig4)[names(colbig4) == '1.team'] = 'blue'
names(colbig4)[names(colbig4) == '2.team'] = 'red'
names(colbig4)[names(colbig4) == '1.result'] = 'blueRes'
names(colbig4)[names(colbig4) == '2.result'] = 'redRes'
names(colbig4)[names(colbig4) == '1.KDA'] = 'blueKDA'
names(colbig4)[names(colbig4) == '2.KDA'] = 'redKDA'
testb4Res = elo.run(score(blueRes, redRes) ~ adjust(blue, 30) + red,
                 data = colbig4, k = 20)
testb4KDA = elo.run(score(blueKDA, redKDA) ~ adjust(blue, 30) + red,
                    data = colbig4, k = 20)
testb4NoAdjRes = elo.run(score(blueRes, redRes) ~ blue + red,
                 data = colbig4, k = 20)
testb4NoAdjKDA = elo.run(score(blueKDA, redKDA) ~ blue + red,
                      data = colbig4, k = 20)
final.elos(testb4Res)
final.elos(testb4KDA)
final.elos(testb4NoAdjRes)
final.elos(testb4NoAdjKDA)



big4 = big4 %>% group_by(gameid)
big4 = big4 %>% separate(team, c("team1", "team2"), sep=",")
%>% separate(result, c("result1", "result2"), sep=",")
%>% separate(earned.gpm, c("t1egpm", "t2egpm"), sep=",")
%>% separate(gamelength, c("gamelength", "gamelength2"), sep=",")

testb4 = elo.run(score(result1, result2) ~ team1 + team2,
                  data = big4, k = 20)
final.elos(testb4)

lcs = new[which(new$league == "LCS"),]
lcs = lcs %>% group_by(gameid) %>% summarise_all(funs(trimws(paste(., collapse = ','))))
lcs = lcs %>% separate(team, c("team1", "team2"), sep=",") 
%>% separate(result, c("result1", "result2"), sep=",") 



lec = new[which(new$league == "LEC"),]
lec = lec %>% group_by(gameid) %>% summarise_all(funs(trimws(paste(., collapse = ','))))
lec = lec %>%
  separate(team, c("team1", "team2"), sep=",") %>%
  separate(result, c("result1", "result2"), sep=",")
testLec = elo.run(score(result1, result2) ~ team1 + team2,
                  data = lec, k = 20)

lck = new[which(new$league == "LCK"),]
lck = lck %>% group_by(gameid) %>% summarise_all(funs(trimws(paste(., collapse = ','))))
lck = lck %>%
  separate(team, c("team1", "team2"), sep=",") %>%
  separate(result, c("result1", "result2"), sep=",")
testLck = elo.run(score(result1, result2) ~ team1 + team2,
                  data = lck, k = 20)

lpl = new[which(new$league == "LPL"),]
lpl = lpl %>% group_by(gameid) %>% summarise_all(funs(trimws(paste(., collapse = ','))))
lpl = lpl %>%
  separate(team, c("team1", "team2"), sep=",") %>%
  separate(result, c("result1", "result2"), sep=",")
testLpl = elo.run(score(result1, result2) ~ team1 + team2,
                  data = lpl, k = 20)

final.elos(testLCS)
final.elos(testLec)
final.elos(testLck)
final.elos(testLpl)

new = new %>% group_by(gameid) %>% summarise_all(funs(trimws(paste(., collapse = ','))))

library(tidyr)
new = new %>%
  separate(team, c("team1", "team2"), sep=",") %>%
  separate(result, c("result1", "result2"), sep=",")



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
           