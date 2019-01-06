train_glmnet <- function(train_features, test_features){
  
  set.seed(1)
  not_included_feature_indices=c(1:5)
  alpha=1
  nlambda=50
  tune_lambda=TRUE
  trace=T
  nFolds=10
  nofReplications=2
  
  # glmnet works with complete data
  train_features=train_features[,lapply(.SD,na.mean)]
  
  glm_features=train_features[complete.cases(train_features)]
  train_class=glm_features$Match_Result
  glm_train_data=glm_features[,-not_included_feature_indices,with=F]
  glm_test_data=test_features[,-not_included_feature_indices,with=F]
  if(tune_lambda){
    # to set lambda parameter, cross-validation will be performed and lambda is selected based on RPS performance
    
    cvindices=generateCVRuns(train_class,nofReplications,nFolds,stratified=TRUE)
    
    # first get lambda sequence for all data
    glmnet_alldata = glmnet(as.matrix(glm_train_data), as.factor(train_class), family="multinomial", alpha = alpha, nlambda=nlambda)
    lambda_sequence = glmnet_alldata$lambda
    
    cvresult=vector('list',nofReplications*nFolds)
    iter=1
    for(i in 1:nofReplications) {
      thisReplication=cvindices[[i]]
      for(j in 1:nFolds){
        if(trace){
          cat(sprintf('Iteration %d: Fold %d of Replication %d\n',iter,j,i))
        }
        testindices=order(thisReplication[[j]])
        
        cvtrain=glm_train_data[-testindices]    
        cvtrainclass=train_class[-testindices]   
        cvtest=glm_train_data[testindices]
        cvtestclass=train_class[testindices] 
        
        inner_cv_glmnet_fit = glmnet(as.matrix(cvtrain),as.factor(cvtrainclass),family="multinomial", alpha = alpha,lambda=lambda_sequence)
        valid_pred = predict(inner_cv_glmnet_fit, as.matrix(cvtest), s = lambda_sequence, type = "response")
        
        #check order of predictions
        order_of_class=attr(valid_pred,'dimnames')[[2]]
        new_order=c(which(order_of_class=='Home'),which(order_of_class=='Tie'),which(order_of_class=='Away'))
        foldresult=rbindlist(lapply(c(1:length(lambda_sequence)),function(x) { data.table(repl=i,fold=j,lambda=lambda_sequence[x],valid_pred[,new_order,x],result=cvtestclass)}))
        cvresult[[iter]]=foldresult
        iter=iter+1
      }
    }
    
    cvresult=rbindlist(cvresult)
    
    # creating actual targets for rps calculations
    cvresult[,pred_id:=1:.N]
    outcome_for_rps=data.table::dcast(cvresult,pred_id~result,value.var='pred_id')
    outcome_for_rps[,pred_id:=NULL]
    outcome_for_rps[is.na(outcome_for_rps)]=0
    outcome_for_rps[outcome_for_rps>0]=1
    setcolorder(outcome_for_rps,c('Home','Tie','Away'))
    
    # calculate RPS
    overall_results=data.table(cvresult[,list(repl,fold,lambda)],RPS=RPS_matrix(cvresult[,list(Home,Tie,Away)],outcome_for_rps))
    
    # summarize performance for each lambda
    overall_results_summary=overall_results[,list(RPS=mean(RPS)),list(repl,fold,lambda)]
    
    # find best lambdas as in glmnet based on RPS
    overall_results_summary=overall_results_summary[,list(meanRPS=mean(RPS),sdRPS=sd(RPS)),list(lambda)]
    overall_results_summary[,RPS_mean_lb := meanRPS - sdRPS]
    overall_results_summary[,RPS_mean_ub := meanRPS + sdRPS]
    
    cv_lambda_min=overall_results_summary[which.min(meanRPS)]$lambda
    
    semin=overall_results_summary[lambda==cv_lambda_min]$RPS_mean_ub
    cv_lambda.1se=max(overall_results_summary[meanRPS<semin]$lambda)
    
    cvResultsSummary = list(lambda.min =cv_lambda_min, lambda.1se = cv_lambda.1se,
                            meanRPS_min=overall_results_summary[lambda==cv_lambda_min]$meanRPS,
                            meanRPS_1se=overall_results_summary[lambda==cv_lambda.1se]$meanRPS)
    
  }
  
  # fit final glmnet model with the lambda with minimum error
  final_glmnet_fit = glmnet(as.matrix(glm_train_data),as.factor(train_class),family="multinomial", alpha = alpha,lambda=cvResultsSummary$lambda.min)
  # obtain predictions
  predicted_probabilities=predict(final_glmnet_fit, as.matrix(glm_test_data), type = "response")
  
  #check order of predictions
  order_of_class=attr(predicted_probabilities,'dimnames')[[2]]
  new_order=c(which(order_of_class=='Home'),which(order_of_class=='Tie'),which(order_of_class=='Away'))
  
  final_result=data.table(test_features[,list(matchId,Match_Result)],predicted_probabilities[,new_order,1])
  
  return(list(predictions=final_result,cv_stats=cvResultsSummary))
}