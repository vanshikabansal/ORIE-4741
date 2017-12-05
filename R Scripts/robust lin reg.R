library(MASS)
set.seed(8)
errorRLM = rep(0,5)

abse <- function(errorx)
{
  mean(abs(errorx))
}

crank <- function(prediction,data)
{
  return((round(prediction * data$race_total))/data$race_total)
}

#changes percentiles to a rounded rank and calculates absolute error
cre <- function(prediction,data)
{
  x<- (round(prediction * data$race_total))
  return(mean(abs(data$finishing_position - x)/data$race_total))
}

#race1
train <-race1
model1 <- rlm(percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedRLM <- predict(model1, train)
error1x <- crank(predictedRLM,train)
errorRLM[1] <- cre(predictedRLM,train)

#race2
train <-race2
model2 <- rlm(percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedRLM <- predict(model2, train)
error2x <- crank(predictedRLM,train)
errorRLM[2] <- cre(predictedRLM,train)

#race3
train <-race3
model3 <- rlm(percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedRLM <- predict(model3, train)
error3x <- crank(predictedRLM,train)
errorRLM[3] <- cre(predictedRLM,train)

#race4
train <-race4
model4 <- rlm(percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedRLM <- predict(model4, train)
error4x <- crank(predictedRLM,train)
errorRLM[4] <- cre(predictedRLM,train)

#race5
train <-race5
model5 <- rlm(percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedRLM <- predict(model5, train)
error5x <- crank(predictedRLM,train)
errorRLM[5] <- cre(predictedRLM,train)

#testing portion

#race1

predictedRLM <- predict(model1, test1)
error1_rx <- crank(predictedRLM,test1)
error1<- cre(predictedRLM,test1)


#race2
predictedRLM <- predict(model2, test2)
error2_rx <- crank(predictedRLM,test2)
error2<- cre(predictedRLM,test2)


#race3
predictedRLM <- predict(model3, test3)
error3_rx <- crank(predictedRLM,test3)
error3<- cre(predictedRLM,test3)

#race4
predictedRLM <- predict(model4, test4)
error4_rx <- crank(predictedRLM,test4)
error4<- cre(predictedRLM,test4)


#race5
predictedRLM <- predict(model5, test5)
error5_rx <- crank(predictedRLM,test5)
error5<- cre(predictedRLM,test5)

#combined races

train <-mydata

model6 <- rlm(percentile ~  declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedRLM <- predict(model6, train)

error6x <- crank(predictedRLM,train)
errorRLM6 <- cre(predictedRLM,train)

predictedRLM <- predict(model6, testdata)

error6_rx <- crank(predictedRLM,testdata)
error6 <- cre(predictedRLM,testdata)




