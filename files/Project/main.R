
require(data.table)
require(TunePareto)

setwd('/users/tarkantemizoz/desktop/IE582-proje')

testStart=as.Date('2018-11-29')
testStartend=as.Date('2019-01-05')
trainStart=as.Date('2012-07-15')
trainLateStart=as.Date('2015-07-15')

rem_miss_threshold=0.01 #parameter for removing bookmaker odds with missing ratio greater than this threshold

source('data_preprocessing.r')
source('feature_extraction.r')
source('performance_metrics.r')
source('train_models.r')
source('train_gbm.r')
source('addElo.r')
source('rps_calc.r')
source('elo_probs.r')
source('combine_probs.r')


matches_data_path='/users/tarkantemizoz/desktop/IE582-proje/df9b1196-e3cf-4cc7-9159-f236fe738215_matches.rds'
odd_details_data_path='/users/tarkantemizoz/desktop/IE582-proje/df9b1196-e3cf-4cc7-9159-f236fe738215_odd_details.rds'

# read data
matches_raw=readRDS(matches_data_path)
odd_details_raw=readRDS(odd_details_data_path)
# preprocess matches
matches=matches_data_preprocessing(matches_raw)
# preprocess odd data
odd_details=details_data_preprocessing(odd_details_raw,matches)
# extract open and close odd type features from multiple bookmakers
features=extract_features.openclose(matches,odd_details,pMissThreshold=rem_miss_threshold,trainStart,testStart)

# divide data based on the provided dates 
train_features=features[Match_Date>=trainStart & Match_Date<testStart] 
test_features=features[Match_Date>=testStart & Match_Date<testStartend] 

# run glmnet on train data with tuning lambda parameter based on RPS and return predictions based on lambda with minimum RPS
predictions_glm=train_glmnet(train_features, test_features)
RPS_TEST=rps_calc(predictions_glm$predictions,3)
mean(RPS_TEST)
predictions_glm$predictions[,RPS:=RPS_TEST]
rps=predictions_glm$predictions[,mean(RPS),by='Match_Result']
rps

# gbm
gbm_parameters=crossValidationTrain(train_features)
predictions_gbm=train_gbm(train_features, test_features,gbm_parameters[1],gbm_parameters[2])
RPS_TEST=rps_calc(predictions_gbm$predictions,3)
mean(RPS_TEST)
predictions_gbm$predictions[,RPS:=RPS_TEST]
rps=predictions_gbm$predictions[,mean(RPS),by='Match_Result']
rps

# elo probabilities
matches_elo=add_elo(matches,testStart,testStartend)
elo_prob_Matches=elo_train(matches_elo,testStart,testStartend)
RPS_TEST=rps_calc(elo_prob_Matches,19)
mean(RPS_TEST)
elo_prob_Matches[,RPS:=RPS_TEST]
rps=elo_prob_Matches[,mean(RPS),by='Match_Result']
rps

# elo-gbm combined
combined_probs=combine_probs(elo_prob_Matches)
RPS_TEST=rps_calc(combined_probs,27)
mean(RPS_TEST)
combined_probs[,RPS:=RPS_TEST]
rps=combined_probs[,mean(RPS),by='Match_Result']
rps

# gbm with elo ratings and differences
features_elo=merge(features,matches_elo[,list(matchId,Home_Elo,Away_Elo,EloDifference)],by="matchId")
train_features_elo=features[Match_Date>=trainLateStart & Match_Date<testStart] 
test_features_elo=features[Match_Date>=testStart & Match_Date<testStartend] 
predictions_gbm_elo=train_gbm(train_features_elo, test_features_elo,gbm_parameters[1],gbm_parameters[2])
RPS_TEST=rps_calc(predictions_gbm_elo$predictions,3)
mean(RPS_TEST)
predictions_gbm_elo$predictions[,RPS:=RPS_TEST]
rps=predictions_gbm_elo$predictions[,mean(RPS),by='Match_Result']
rps








