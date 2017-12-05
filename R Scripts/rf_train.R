#Separate races by class and train
library(randomForest)
classes = unique(mydata$race_class)
set.seed(0)


finishing_percentile <- predict(model1, subset(race1, select = -c(percentile, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * race1$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error1_train = abs((race1$finishing_position - finishing_percentile$finishing_position))/race1$race_totals


finishing_percentile <- predict(model2, subset(race2, select = -c(percentile, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * race2$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error2_train = abs((race2$finishing_position - finishing_percentile$finishing_position))/race2$race_totals



finishing_percentile <- predict(model3, subset(race3, select = -c(percentile, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * race3$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error3_train = abs((race3$finishing_position - finishing_percentile$finishing_position))/race3$race_totals



finishing_percentile <- predict(model4, subset(race4, select = -c(percentile, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * race4$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error4_train = abs((race4$finishing_position - finishing_percentile$finishing_position))/race4$race_totals



finishing_percentile <- predict(model5, subset(race5, select = -c(percentile, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * race5$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error5_train = abs((race5$finishing_position - finishing_percentile$finishing_position))/race5$race_totals



finishing_percentile <- predict(model_total, subset(mydata, select=-c(race_totals, finishing_position,race_id, race_class, race_totals, finishing_position)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * mydata$race_totals
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
error_total_train = abs((mydata$finishing_position - finishing_percentile$finishing_position))/mydata$race_totals


plot(as.numeric(summary(error1)), as.numeric(summary(error1_train)), type = "o", col = "red", xlab = "test errors", ylab = "training errors")
lines(as.numeric(summary(error2)), as.numeric(summary(error2_train)), col = "blue", type = "o")
lines(as.numeric(summary(error3)), as.numeric(summary(error3_train)), col = "green", type = "o")
lines(as.numeric(summary(error4)), as.numeric(summary(error4_train)), col = "purple", type = "o")
lines(as.numeric(summary(error5)), as.numeric(summary(error5_train)), col = "pink", type = "o")
lines(as.numeric(summary(error_total)), as.numeric(summary(error_total_train)), type = "o")

plot(as.numeric(summary(error1)), as.numeric(summary(error1_train)), type = "l", col = "red", xlab = "test errors", ylab = "training errors")
lines(as.numeric(summary(error2)), as.numeric(summary(error2_train)), col = "blue")
lines(as.numeric(summary(error3)), as.numeric(summary(error3_train)), col = "green")
lines(as.numeric(summary(error4)), as.numeric(summary(error4_train)), col = "purple")
lines(as.numeric(summary(error5)), as.numeric(summary(error5_train)), col = "pink")
lines(as.numeric(summary(error_total)), as.numeric(summary(error_total_train)))

legend(-0.05, 0.4, "1: red , 2: blue , 3: green , 4: purple , 5: pink , train: black" , cex = 0.75, fill = NULL, border = NULL)

legend(0, 0.4, c("1: red","2: blue","3: green","4: purple","5: pink","train: black"), cex = 0.75)

legend(0, 0.4, c("1: red","2: blue","3: green","4: purple","5: pink","train: black"), cex = 0.75)
