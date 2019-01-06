train_gbm <- function(train_features, test_features,depth,shrinkage)
  {
  not_included_feature_indices=c(1:5)
  set.seed(1)
  # tie=train_features[train_features$Match_Result=='Tie',]
  # home=train_features[train_features$Match_Result=='Home',]
  # away=train_features[train_features$Match_Result=='Away',]
  # 
  # homeindices = sample(nrow(home), nrow(tie))
  # awayindices = sample(nrow(away), nrow(tie))
  # home=home[homeindices,]
  # away=away[awayindices,]
  # train_features=rbind(home,away,tie)
 #  glm_features=train_features[complete.cases(train_features)]
 # 
 #  opens<-glm_features[ ,grepl("Open",names(glm_features)),with=FALSE]
 #  opens[,Ortalama := rowMeans(opens)]
 #  closes<-glm_features[ ,-grepl("Open",names(glm_features)),with=FALSE]
 #  closes<-closes[,-c(1:5)]
 #  closes[,Ortalama := rowMeans(closes)]
 # ort_change<-closes$Ortalama-opens$Ortalama
 #  glm_features[,ort_change := ort_change]
 #  train_class=glm_features$Match_Result
 # 
 #  openst<-test_features[ ,grepl("Open",names(test_features)),with=FALSE]
 #  openst[,Ortalama := rowMeans(openst)]
 #  closest<-test_features[ ,-grepl("Open",names(test_features)),with=FALSE]
 #   closest<-closest[,-c(1:5)]
 #  closest[,Ortalama := rowMeans(closest)]
 # ort_change2<-closest$Ortalama-openst$Ortalama
 #  test_features[,ort_change := ort_change2]
 # 
 #  glm_train_data=glm_features[,-not_included_feature_indices,with=F]
 #  glm_test_data=test_features[,-not_included_feature_indices,with=F]
 #  glm_train_data=glm_train_data[,-grepl("Open",names(glm_train_data)),with=F]
 #  glm_test_data=test_features[,-grepl("Open",names(test_features)),with=F]
 #   glm_test_data=glm_test_data[,-not_included_feature_indices,with=F]

  # m <- polr(as.factor(train_class) ~ ., data = glm_train_data, Hess=TRUE)
  # predicted_probabilities<-predict(m,glm_test_data,type = "p")
  # order_of_class=attr(predicted_probabilities,'dimnames')[[2]]
  # new_order=c(which(order_of_class=='Home'),which(order_of_class=='Tie'),which(order_of_class=='Away'))
  # final_result=data.table(test_features[,list(matchId,Match_Result)],predicted_probabilities[,new_order])
  # # glmnet works with complete data
  # tie=train_features[train_features$Match_Result=='Tie',]
  # home=train_features[train_features$Match_Result=='Home',]
  # away=train_features[train_features$Match_Result=='Away',]
  # 
  # homeindices = sample(nrow(home), nrow(tie))
  # awayindices = sample(nrow(away), nrow(tie))
  # home=home[homeindices,]
  # away=away[awayindices,]
  # train_features=rbind(home,away,tie)
  train_features=train_features[,lapply(.SD,na.mean)]
  glm_features=train_features[complete.cases(train_features)]
  train_class=glm_features$Match_Result
  glm_train_data=glm_features[,-not_included_feature_indices,with=F]
  glm_test_data=test_features[,-not_included_feature_indices,with=F]
  dim(train_features)
  noftrees=100
  learning_rate=shrinkage
  sampling_fraction=0.5
  gbmnet_alldata=gbm(as.factor(train_class)~., data=glm_train_data,distribution = "multinomial", n.trees = noftrees,
                     interaction.depth = depth, n.minobsinnode = 5, shrinkage =learning_rate ,
                     bag.fraction = sampling_fraction,cv.folds = 10)
  bestd=gbm.perf(gbmnet_alldata,method = "cv")
  summary(gbmnet_alldata,n.trees =bestd)
  predicted_probabilities=predict(gbmnet_alldata,as.data.frame(glm_test_data),type = "response",bestd)
  order_of_class=attr(predicted_probabilities,'dimnames')[[2]]
  new_order=c(which(order_of_class=='Home'),which(order_of_class=='Tie'),which(order_of_class=='Away'))
  final_result=data.table(test_features[,list(matchId,Match_Result)],predicted_probabilities[,new_order,1])
  return(list(predictions=final_result))
}