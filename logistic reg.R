library(caTools)
set.seed(88)
library(Matrix)
library(e1071)

errorLIN = rep(0,16)
errorSVM = rep(0,16)

rmse <- function(errorx)
{
  sqrt(mean(errorx^2))
}

#race1
train <-race1
model1 <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[1] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[1] <- rmse(errorx)

#race2
train <-race2
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[2] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[2] <- rmse(errorx)

#race3
train <-race3
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[3] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[3] <- rmse(errorx)

#race4
train <-race4
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[4] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[4] <- rmse(errorx)

#race5
train <-race5
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[5] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[5] <- rmse(errorx)

#race6
train <-race6
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[6] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[6] <- rmse(errorx)

#race7
train <-race7
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[7] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[7] <- rmse(errorx)

#race8
train <-race8
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[8] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[8] <- rmse(errorx)

#race9
train <-race9
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[9] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[9] <- rmse(errorx)

#race10
train <-race10
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[10] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[10] <- rmse(errorx)

#race11
train <-race11
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[11] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[11] <- rmse(errorx)

#race12
train <-race112
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[12] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[12] <- rmse(errorx)

#race13
train <-race13
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[13] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[13] <- rmse(errorx)

#race14
train <-race14
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[14] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[14] <- rmse(errorx)

#race15
train <-race15
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[15] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[15] <- rmse(errorx)

#race16
train <-race16
mode1l <- lm(finishing_position ~ ., train)
predictedLIN <- predict(model1, train)

errorx <- train$finishing_position - predictedLIN

errorLIN[16] <- rmse(errorx)

model1s <- svm(finishing_position ~ . , train)
predictedSVM <- predict(model1s, train)

errorx <- train$finishing_position - predictedSVM

errorSVM[16] <- rmse(errorx)