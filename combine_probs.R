combine_probs <- function(matchdat){
  matchdat=matchdat[,Match_Result:=NULL]
  combined=merge(matchdat,predictions_gbm$predictions,by='matchId')
  combined=combined[,H1:= (Home.y+Home_Prob)/2]
  combined=combined[,T1:= (Tie+Tie_Prob)/2]
  combined=combined[,A1:= (Away.y+Away_Prob)/2]
  
  # a=combined2[RPS.x<RPS.y]
  # b=combined2[RPS.x>RPS.y]
  # mean(b$RPS.x)
  # mean(b$RPS.y)
  # mean(b$Home_Prob)
  # mean(b$Home.y)
  # mean(b$Tie_Prob)
  # mean(b$Tie)
  # mean(b$Away_Prob)
  # mean(b$Away.y)
  # 
  # a1=a[Home_Prob<Home.y]
  # a2=a[Tie_Prob<Tie]
  # a3=a[Away_Prob<Away.y]
  # 
  # b1=b[Home_Prob<Home.y]
  # b2=b[Tie_Prob<Tie]
  # b3=b[Away_Prob<Away.y]
  # 
  # mean(a$RPS.x)
  # mean(a$RPS.y)
  # mean(a$Home_Prob)
  # mean(a$Home.y)
  # mean(a$Tie_Prob)
  # mean(a$Tie)
  # mean(a$Away_Prob)
  # mean(a$Away.y)
  # 
  # max(a$Home_Prob)
  # max(a$Home.y)
  # max(a$Tie_Prob)
  # max(a$Tie)
  # max(a$Away_Prob)
  # mean(a$Away.y)
  # hist(a$Home_Prob)
  
  combined2=merge(matchdat,predictions_gbm$predictions,by='matchId')
  combined2=combined2[,H1:= ifelse(Tie_Prob>Tie,Home.y,Home_Prob)]
  combined2=combined2[,T1:= ifelse(Tie_Prob>Tie,Tie,Tie_Prob)]
  combined2=combined2[,A1:= ifelse(Tie_Prob>Tie,Away.y,Away_Prob)]

  return(combined2)
}