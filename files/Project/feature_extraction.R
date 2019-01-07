
extract_features.openclose <- function(matches,odd_details,pMissThreshold=0.01,trainStart,testStart){
  details = copy(odd_details)
  matches = copy(matches)
  
  details=details[order(OddChangeDateTime)]
  feature_odd_details=details[,list(Odd_Open=odd[1],Odd_Close=odd[.N]),list(matchId,betType,oddtype,bookmaker)]
  
  feature_odd_details = merge(matches[,list(matchId,Match_Date)], feature_odd_details,by="matchId")
  
  
  #HANDLE MISSINGS
  details_temp = dcast(feature_odd_details, matchId+betType ~ paste0("Odd_Close_",bookmaker)+oddtype, value.var = c("Odd_Close"))
  details_melt = melt(details_temp, id.vars = c("matchId","betType"), measure.vars = names(details_temp)[names(details_temp) %like% "Odd_Close"], value.name = "odd")
  details_melt[,c("OpenClose","bookmaker","oddtype"):=tstrsplit(variable,split="_",keep=c(2:4))]
  details_melt[,variable:=NULL]
  details_melt = merge(matches[,list(matchId,Match_Date)], details_melt,by="matchId",all=T)
  
  bookieMissingness = details_melt[Match_Date >= trainStart,list(.N,percMiss=sum(is.na(odd))/.N),by=list(bookmaker,betType)]
  bookiesToKeep = unique(bookieMissingness[percMiss <= pMissThreshold]$bookmaker)
  cat("Number of bookmakers with proportion of missings below",pMissThreshold,"since",as.character(trainStart),":",length(bookiesToKeep),"\n")
  
  nonmissingBookmakers_sinceTestStart = unique(details_melt[Match_Date >= testStart, list(.N,NA_SUM=sum(is.na(odd))),by=list(bookmaker,betType)][NA_SUM==0]$bookmaker)
  bookiesToKeep = intersect(bookiesToKeep,nonmissingBookmakers_sinceTestStart)
  cat("Number of bookmakers with no missings since testStart", as.character(testStart), ":", length(bookiesToKeep), "\n")
  
  details = dcast(feature_odd_details,matchId~oddtype+bookmaker,value.var = c("Odd_Open","Odd_Close"))
  columnsToKeep = grep(paste(bookiesToKeep,collapse="|"),names(details),value=T)
  details = details[,c('matchId',columnsToKeep),with=F]
  #HANDLE MISSINGS END
  details=details[,-1]
  
  details = merge(matches[,-c('Home','Away','Home_Score','Away_Score','Total_Score','Result_Home','Result_Tie','Result_Away','type'),with=F],
                  details,by="matchId",all=T)
  
  
  return(features = details)
}

