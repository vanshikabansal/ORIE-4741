require(randomForest)
require(rfUtilities)

model1 = randomForest(finishing_position~.,data=race1)
cv1 <- rf.crossValidation(model1, race1, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model2 = randomForest(finishing_position~.,data=race2)
cv2 <- rf.crossValidation(model2, race2, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model3 = randomForest(finishing_position~.,data=race3)
cv3 <- rf.crossValidation(model3, race3, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model4 = randomForest(finishing_position~.,data=race4)
cv4 <- rf.crossValidation(model4, race4, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model5 = randomForest(finishing_position~.,data=race5)
cv5 <- rf.crossValidation(model5, race5, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model6 = randomForest(finishing_position~.,data=race6)
cv6 <- rf.crossValidation(model6, race6, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model7 = randomForest(finishing_position~.,data=race7)
cv7 <- rf.crossValidation(model7, race7, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model8 = randomForest(finishing_position~.,data=race8)
cv8 <- rf.crossValidation(model8, race8, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model9 = randomForest(finishing_position~.,data=race9)
cv9 <- rf.crossValidation(model9, race9, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model10 = randomForest(finishing_position~.,data=race10)
cv10 <- rf.crossValidation(model10, race10, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model11 = randomForest(finishing_position~.,data=race11)
cv11 <- rf.crossValidation(model11, race11, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model12 = randomForest(finishing_position~.,data=race12)
cv12 <- rf.crossValidation(model12, race12, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model13 = randomForest(finishing_position~.,data=race13)
cv13 <- rf.crossValidation(model13, race13, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model14 = randomForest(finishing_position~.,data=race14)
cv14 <- rf.crossValidation(model14, race14, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model15 = randomForest(finishing_position~.,data=race15)
cv15 <- rf.crossValidation(model15, race15, p = 0.1, n = 10, seed = NULL, plot = TRUE)

model16 = randomForest(finishing_position~.,data=race16)
cv16 <- rf.crossValidation(model16, race16, p = 0.1, n = 10, seed = NULL, plot = TRUE)