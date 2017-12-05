#Separate races by class and train
library(randomForest)
classes = unique(mydata$race_class)
set.seed(0)

model1 = randomForest(percentile~., subset(race1, select=-c(race_totals, finishing_position)))
finishing_percentile <- predict(model1, subset(test1, select = -c(percentile, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * test1$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error1 = abs((test1$finishing_position - finishing_percentile$finishing_position))/test1$race_totals
summary(error1)

model2 = randomForest(percentile~., subset(race2, select=-c(race_totals, finishing_position)))
finishing_percentile <- predict(model2, subset(test2, select = -c(percentile, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * test2$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error2 = abs((test2$finishing_position - finishing_percentile$finishing_position))/test2$race_totals
summary(error2)

model3 = randomForest(percentile~., subset(race3, select=-c(race_totals, finishing_position)))
finishing_percentile <- predict(model3, subset(test3, select = -c(percentile, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * test3$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error3 = abs((test3$finishing_position - finishing_percentile$finishing_position))/test3$race_totals
summary(error3)

model4 = randomForest(percentile~., subset(race4, select=-c(race_totals, finishing_position)))
finishing_percentile <- predict(model4, subset(test4, select = -c(percentile, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * test4$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error4 = abs((test4$finishing_position - finishing_percentile$finishing_position))/test4$race_totals
summary(error4)

model5 = randomForest(percentile~., subset(race5, select=-c(race_totals, finishing_position)))
finishing_percentile <- predict(model5, subset(test5, select = -c(percentile, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * test5$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error5 = abs((test5$finishing_position - finishing_percentile$finishing_position))/test5$race_totals
summary(error5)

model_total = randomForest(percentile~., subset(mydata, select=-c(race_totals, finishing_position,race_id, race_class, race_totals)))
finishing_percentile <- predict(model_total, subset(mydata_test, select=-c(race_totals, finishing_position,race_id, race_class, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * mydata_test$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error_total = abs((mydata_test$finishing_position - finishing_percentile$finishing_position))/mydata_test$race_totals
summary(error_total)
