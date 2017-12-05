library(caTools)
set.seed(88)
library(Matrix)
library(e1071)

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

#training portion

errorLIN = rep(0,5)
errorSVM = rep(0,5)

rmse <- function(errorx)
{
  sqrt(mean(errorx^2))
}

abse <- function(errorx)
{
  mean(abs(errorx))
}

#race1
train <-race1
model1 <- lm( percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedLIN <- predict(model1, train)

errorx <- crank(predictedLIN,train)

errorLIN[1] <- cre(predictedLIN,train)

model1s <- svm(percentile ~  declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedSVM <- predict(model1s, train)

error1x <- crank(predictedSVM,train)

errorSVM[1] <- cre(predictedSVM,train)

#race2
train <-race2
model2 <- lm( percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedLIN <- predict(model2, train)

errorx <- crank(predictedLIN,train)

errorLIN[2] <- cre(predictedLIN,train)

model2s <- svm( percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile , train)
predictedSVM <- predict(model2s, train)

error2x <- crank(predictedSVM,train)

errorSVM[2] <- cre(predictedSVM,train)

#race3
train <-race3
model3 <- lm( percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedLIN <- predict(model3, train)

errorx <- crank(predictedLIN,train)

errorLIN[3] <- cre(predictedLIN,train)

model3s <- svm(percentile ~  declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedSVM <- predict(model3s, train)

error3x <- crank(predictedSVM,train)

errorSVM[3] <- cre(predictedSVM,train)

#race4
train <-race4
model4 <- lm( percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedLIN <- predict(model4, train)

errorx <- crank(predictedLIN,train)

errorLIN[4] <- cre(predictedLIN,train)

model4s <- svm(percentile ~   declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedSVM <- predict(model4s, train)

error4x <- crank(predictedSVM,train)

errorSVM[4] <- cre(predictedSVM,train)

#race5
train <-race5
model5 <- lm( percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedLIN <- predict(model5, train)

errorx <- crank(predictedLIN,train)

errorLIN[5] <- cre(predictedLIN,train)

model5s <- svm( percentile ~ declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedSVM <- predict(model5s, train)

error5x <- crank(predictedSVM,train)

errorSVM[5] <- cre(predictedSVM,train)


#testing portion

#race1
predictedSVM <- predict(model1s, test1)
error1_1x <- crank(predictedSVM,test1)
error1<- cre(predictedSVM,test1)


#race2
predictedSVM <- predict(model2s, test2)
error2_1x <- crank(predictedSVM,test2)
error2<- cre(predictedSVM,test2)

#race3
predictedSVM <- predict(model3s, test3)
error3_1x <- crank(predictedSVM,test3)
error3<- cre(predictedSVM,test3)

#race4
predictedSVM <- predict(model4s, test4)
error4_1x <- crank(predictedSVM,test4)
error4<- cre(predictedSVM,test4)

#race5
predictedSVM <- predict(model5s, test5)
error5_1x <- crank(predictedSVM,test5)
error5<- cre(predictedSVM,test5)

#combined races

train <-mydata

model6s <- svm(percentile ~  declared_horse_weight + actual_weight + trainer_avg_percentile + horse_avg_percentile + draw + win_odds + jockey_avg_percentile, train)
predictedSVM <- predict(model6s, train)

error6x <- crank(predictedSVM,train)
errorSVM6 <- cre(predictedSVM,train)

predictedSVM <- predict(model6s, testdata)

error6_1x <- crank(predictedSVM,testdata)
error6 <- cre(predictedSVM,testdata)
