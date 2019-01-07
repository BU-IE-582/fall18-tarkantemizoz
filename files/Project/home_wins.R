home_wins<-function(matches){
  ct=0
  home=0
  for( i in 2:nrow(matches)){
    temp=matches[i,2]
    for(j in 1:(i-1)){
      if (matches[(i-j),2]==temp)
      {
        home=home+1
        if(home>5){
          break
        }
        if(matches[(i-j),10]=="Home"){
          
          ct=ct+1
        }
        
      }
    }
    
    matches[i,19]=ct
    ct=0
    home=0
  }
 return(matches) 
}
