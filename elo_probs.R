#@param matches
elo_train <- function(matches,testStart,testStartend){
  copy_train=matches_elo[Match_Date>='2017-07-15' & Match_Date<testStart]
  
  copy_train=copy_train[,Home_Prob:=elo.prob(Home_Elo,Away_Elo)]
  copy_train=copy_train[,Tie_Prob:=0]
  copy_train=copy_train[,Away_Prob:=1-Home_Prob]
  copy_train=copy_train[,Tie_Prob:=(1-abs(Home_Prob-Away_Prob))/3]
  homefrac=c(seq(0.1,0.5,by=0.01))
  meanrps=c()
  copy_train1=copy_train
  for(i in 1:length(homefrac))
  {
  copy_train1$Home_Prob=copy_train$Home_Prob-(copy_train$Tie_Prob*homefrac[i])
  copy_train1$Away_Prob=1-(copy_train1$Home_Prob+copy_train$Tie_Prob)
  RPS_TEST=rps_calc(copy_train1,19)
  meanrps[i]=  mean(RPS_TEST)
  }
  bestfrac=homefrac[which.min(meanrps)]

  copy_test=matches_elo[Match_Date>=testStart & Match_Date<testStartend]
  
  copy_test=copy_test[,Home_Prob:=elo.prob(Home_Elo,Away_Elo)]
  copy_test=copy_test[,Tie_Prob:=0]
  copy_test=copy_test[,Away_Prob:=1-Home_Prob]
  copy_test=copy_test[,Tie_Prob:=(1-abs(Home_Prob-Away_Prob))/3]
  copy_test=copy_test[,Home_Prob:=Home_Prob-bestfrac*Tie_Prob]
  copy_test=copy_test[,Away_Prob:=1-(Home_Prob+Tie_Prob)]
  
  return(copy_test)
}