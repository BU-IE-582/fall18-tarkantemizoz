rforest<-function(traindat,cvindices,mtry,noftrees){
  rforestresults<-NULL
 
    for(j in 1:noffold) {
      testindices=cvindices[[1]][[j]]
      cvtrain<-traindat[-testindices,-1]
      train_class<-traindat[-testindices,1]
      cvtest<-traindat[testindices,-1]
      test_class<-traindat[testindices,1]
      for(m in mtry){
        for(n in noftrees){
          r1 = randomForest(Match_Result~ ., data = traindat[-testindices,],ntree=n, nodesize=5, mtry=m)  
          randomForestPredictions = predict(r1,cvtest,type="prob")
          
          order_of_class=attr(randomForestPredictions,'dimnames')[[2]]
          new_order=c(which(order_of_class=='Home'),which(order_of_class=='Tie'),which(order_of_class=='Away'))
          predictions=randomForestPredictions[,new_order]
          
          outcomes=matrix(0,nrow(predictions),ncol(predictions))
          for(i in 1:nrow(predictions))
          {
            if(test_class[i]=='Home')
            {
              outcomes[i,1]=1
            }
            if(test_class[i]=='Tie' )
            {
              outcomes[i,2]=1
            }
            if(test_class[i]=='Away' )
            {
              outcomes[i,3]=1
            }
          }
          
          
          
          RPS_TEST=RPS_matrix(predictions,outcomes)
          
          error=mean(RPS_TEST)
          
          temp=data.table(Fold=j,Method="Rforest",mtry=mtry,Error=error)
          
          rforestresults=rbind(rforestresults,temp)
        
          
        }
      }
      }
  
  
  return(rforestresults)
}

