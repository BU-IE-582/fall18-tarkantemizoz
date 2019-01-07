#@param matches
add_elo <- function(matches,test_date,test_date_end){
  matches$Home=gsub("united","utd",matches$Home)
  matches$Away=gsub("united","utd",matches$Away)
  matches$Home=gsub(" ","-",matches$Home)
  matches$Away=gsub(" ","-",matches$Away)
  matches$Home=gsub("newcastle-utd","newcastle",matches$Home)
  matches$Away=gsub("newcastle-utd","newcastle",matches$Away)
  library(dplyr)
  teams <- data.frame(team = unique(c(matches$Home, matches$Away)))
  teams <- teams %>%
    mutate(elo = 1500)
  teams <- teams %>%
    mutate(diff = 1)
  matches <- matches %>%
    mutate(result = if_else(Home_Score > Away_Score, 1,
                            if_else(Home_Score == Away_Score, 0.5, 0)))
  copy=as.data.table(matches)
  copy=copy[,Home_Elo:=1500]
  copy=copy[,Away_Elo:=1500]
  copy_train=copy[Match_Date<test_date]
  copy_test=copy[Match_Date>=test_date & Match_Date<test_date_end]
  library(elo)
  for (i in (seq_len(nrow(copy_train)))){
    match <- copy_train[i, ]
    # Pre-match ratings
    teamA_elo <- subset(teams, team == match$Home)$elo
    teamB_elo <- subset(teams, team == match$Away)$elo
    copy_train[i,]$Home_Elo=teamA_elo
    copy_train[i,]$Away_Elo=teamB_elo
    new_elo <- elo.calc(wins.A = match$result,
                        elo.A = teamA_elo,
                        elo.B = teamB_elo,
                        k = 30)
    teamA_new_elo <- new_elo[1, 1]
    teamB_new_elo <- new_elo[1, 2]
    unique(matches$result)
    # We then update the ratings for teams A and B
    # and leave the other teams as they were
    teams <- teams %>%
      mutate(elo = if_else(team == match$Home, teamA_new_elo,
                           if_else(team == match$Away, teamB_new_elo, elo)))
  }
  for (i in (seq_len(nrow(copy_test)))){
    match <- copy_test[i, ]
    # Pre-match ratings
    teamA_elo <- subset(teams, team == match$Home)$elo
    teamB_elo <- subset(teams, team == match$Away)$elo
    copy_test[i,]$Home_Elo=teamA_elo
    copy_test[i,]$Away_Elo=teamB_elo
  }
  copy=rbind(copy_train,copy_test)
  copy[,EloDifference:=(Home_Elo-Away_Elo)]
  return(copy)
}