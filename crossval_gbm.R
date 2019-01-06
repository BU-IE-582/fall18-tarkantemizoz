crossValidationTrain=function(train.data) #returns mean train accuracy based on cross validation
{
  set.seed(1)
  nFolds=10
  nofReplications=2
  cvindices=generateCVRuns(train.data$Match_Result,nofReplications,nFolds,stratified=TRUE)
  k=1
  accuracy=c()
  depth=c(1,3,5)
  shrinkage=c(0.1,0.01,0.001)
  for(i in 1:nofReplications) {
    thisReplication=cvindices[[i]]
    for(j in 1:nFolds){
      testindices=order(thisReplication[[j]])
      cvtrain=train.data[-testindices,]    
      cvtest=train.data[testindices,]
      for(l in 1:length(depth))
      {
        for(m in 1:length(shrinkage))
        {
          predictions_gbm=train_gbm(cvtrain, cvtest,depth[l],shrinkage[m])
          RPS_TEST=rps_calc(predictions_gbm$predictions,3)
          accuracy[k]=mean(RPS_TEST)
          k=k+1
        }
      }
    }
  }
  depth_best= rep(depth,60)[which.min(accuracy)]
  shrinkage_best=rep(cbind(rep(shrinkage[1],3),rep(shrinkage[2],3),rep(shrinkage[3],3)),20)[which.min(accuracy)]
  best_parameters=c(depth_best,shrinkage_best)
  return(best_parameters)
}