horse<- read.csv("race-result-horse.csv", stringsAsFactors = FALSE)
race <- read.csv("race-result-race.csv", stringsAsFactors = FALSE)

# Merge race and horse information
alldata <- merge(horse, race, by = "race_id")
alldata <- alldata[!is.na(alldata$finishing_position), ]

#Removing columns deemed unnecessary
alldata <- subset(alldata, select = -c(src, sectional_time, incident_report, length_behind_winner, horse_number, race_date, race_number, track, track_condition,race_name, race_course))
#Assuming no positions/times given in race set as race hasn't started yet
alldata <- subset(alldata, select = -c(running_position_1, running_position_2, running_position_3, running_position_4, running_position_5, running_position_6, finish_time))

#Cleaning positions to remove ties
alldata$finishing_position[alldata$finishing_position == "1 DH"] = 1
alldata$finishing_position[alldata$finishing_position == "2 DH"] = 2
alldata$finishing_position[alldata$finishing_position == "3 DH"] = 3
alldata$finishing_position[alldata$finishing_position == "4 DH"] = 4
alldata$finishing_position[alldata$finishing_position == "5 DH"] = 5
alldata$finishing_position[alldata$finishing_position == "6 DH"] = 6
alldata$finishing_position[alldata$finishing_position == "7 DH"] = 7
alldata$finishing_position[alldata$finishing_position == "8 DH"] = 8
alldata$finishing_position[alldata$finishing_position == "9 DH"] = 9
alldata$finishing_position[alldata$finishing_position == "10 DH"] = 10
alldata$finishing_position[alldata$finishing_position == "11 DH"] = 11
alldata$finishing_position[alldata$finishing_position == "12 DH"] = 12
alldata$finishing_position[alldata$finishing_position == "13 DH"] = 13

non_placing_horses <- alldata[alldata$finishing_position == "WV-A" |alldata$finishing_position== "WV"  | alldata$finishing_position=="WX"  | alldata$finishing_position=="PU" | alldata$finishing_position== "UR" |  alldata$finishing_position=="FE"  | alldata$finishing_position=="TNP" |alldata$finishing_position== "DISQ" | alldata$finishing_position=="WX-A" | alldata$finishing_position=="DNF",]
withdrawn_horses <- alldata[alldata$finishing_position == "WV-A" |alldata$finishing_position== "WV"  | alldata$finishing_position=="WX"  | alldata$finishing_position=="WX-A" ,]

#Use non withdrawn horses for predictions
alldata <- alldata[!(alldata$finishing_position %in%  withdrawn_horses$finishing_position),]

#Allotting non placing horses that weren't withdrawn the last position
alldata$finishing_position <- as.numeric(alldata$finishing_position)
race_ids = unique(alldata$race_id)
x = rep(0,length(race_ids))
for(i in 1:length(race_ids)){
  x[i] = sum(race_ids[i] == alldata$race_id)
}
num_horses = data.frame(race_ids, x)
colnames(num_horses) = c("race_id", "race_total")
alldata <- merge(alldata, num_horses, by = "race_id")
for(i in 1:length(alldata$finishing_position)){
  if(is.na(alldata$finishing_position[i])){
    alldata$finishing_position[i] = alldata$race_total[i]
  }
}

#change factors
alldata$win_odds = as.numeric(alldata$win_odds)
alldata$draw = as.numeric(alldata$draw)
alldata$actual_weight = as.numeric(alldata$actual_weight)
alldata$race_distance = as.numeric(alldata$race_distance)
alldata$declared_horse_weight = as.numeric(alldata$declared_horse_weight)

#create training set
training_race_ids = sample(race_ids, floor(length(race_ids)*0.8))
mydata = subset(alldata, alldata$race_id %in% training_race_ids)
testdata = subset(alldata, !(alldata$race_id %in% training_race_ids))

#create percentile column
mydata$percentile = mydata$finishing_position/mydata$race_total
testdata$percentile = testdata$finishing_position/testdata$race_total

#find average percentiles for horses, trainers, and jockeys
horseaverages = data.frame(horse_id = unique(mydata$horse_id))
for(i in 1:length(unique(mydata$horse_id))){
  horseaverages[i,2] = mean(mydata$percentile[which(mydata$horse_id == horseaverages[i,1])])
}
colnames(horseaverages)[2] = "horse_avg_percentile"

jockeyaverages = data.frame(jockey = unique(mydata$jockey))
for(i in 1:length(unique(mydata$jockey))){
  jockeyaverages[i,2] = mean(mydata$percentile[which(mydata$jockey == jockeyaverages[i,1])])
}
colnames(jockeyaverages)[2] = "jockey_avg_percentile"

traineraverages = data.frame(trainer = unique(mydata$trainer))
for(i in 1:length(unique(mydata$trainer))){
  traineraverages[i,2] = mean(mydata$percentile[which(mydata$trainer == traineraverages[i,1])])
}
colnames(traineraverages)[2] = "trainer_avg_percentile"

#add that data to the training and test set data
mydata <- merge(mydata, traineraverages, by = "trainer", all.x = TRUE)
mydata <- merge(mydata, jockeyaverages, by = "jockey", all.x = TRUE)
mydata <- merge(mydata, horseaverages, by = "horse_id", all.x = TRUE)

testdata <- merge(testdata, traineraverages, by = "trainer", all.x = TRUE)
testdata <- merge(testdata, jockeyaverages, by = "jockey", all.x = TRUE)
testdata <- merge(testdata, horseaverages, by = "horse_id", all.x = TRUE)

#for first time horses, jockeys, or trainers, set average historical percentile to the average
for(i in 1:length(testdata$trainer_avg_percentile)){
  if(is.na(testdata$trainer_avg_percentile[i])){
    testdata$trainer_avg_percentile[i] = mean(traineraverages$trainer_avg_percentile)
  }
  if(is.na(testdata$jockey_avg_percentile[i])){
    testdata$jockey_avg_percentile[i] = mean(jockeyaverages$jockey_avg_percentile)
  }
  if(is.na(testdata$horse_avg_percentile[i])){
    testdata$horse_avg_percentile[i] = mean(horseaverages$horse_avg_percentile)
  }
}

write.csv(mydata,file="training_data.csv",row.names=FALSE)
write.csv(testdata,file="test_data.csv",row.names=FALSE)

#Random Forest
#Separate races by class and train
classes = unique(mydata$race_class)
classes = classes[order(classes)]
errors = rep(0,5)

race1 = mydata[mydata$race_class == classes[2], ]
race1 = subset(race1, select = -c(race_class, race_id))
test1 = testdata[testdata$race_class == classes[2], ]
test1 = subset(test1, select = -c(race_class, race_id))


race2 = mydata[mydata$race_class == classes[3], ]
race2 = subset(race2, select = -c(race_class, race_id))
test2 = testdata[testdata$race_class == classes[3], ]
test2 = subset(test2, select = -c(race_class, race_id))

race3 = mydata[mydata$race_class == classes[5], ]
race3 = subset(race3, select = -c(race_class, race_id))
test3 = testdata[testdata$race_class == classes[5], ]
test3 = subset(test3, select = -c(race_class, race_id))

race4 = mydata[mydata$race_class == classes[8], ]
race4 = subset(race4, select = -c(race_class, race_id))
test4 = testdata[testdata$race_class == classes[8], ]
test4 = subset(test4, select = -c(race_class, race_id))

race5 = mydata[mydata$race_class != classes[2] & mydata$race_class != classes[3] & mydata$race_class != classes[5] & mydata$race_class != classes[8], ]
race5 = subset(race5, select = -c(race_class, race_id))
test5 = testdata[testdata$race_class != classes[2] & testdata$race_class != classes[3] & testdata$race_class != classes[5] & testdata$race_class != classes[8], ]
test5 = subset(test5, select = -c(race_class, race_id))

library(randomForest)
set.seed(0)

race1 = mydata[mydata$race_class == classes[2], ]
race1 = subset(race1, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total))

model1 = randomForest(percentile~., race1)
test = testdata[testdata$race_class == classes[2], ]
finishing_percentile = predict(model1, subset(test, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * test$race_total
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
errors[1] = mean(abs((test$finishing_position - finishing_percentile$finishing_position)))

race2 = mydata[mydata$race_class == classes[3], ]
race2 = subset(race2, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total))

model2 = randomForest(percentile~., race2)
test = testdata[testdata$race_class == classes[3], ]
finishing_percentile = predict(model2, subset(test, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * test$race_total
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
errors[2] = mean(abs((test$finishing_position - finishing_percentile$finishing_position)))

race3 = mydata[mydata$race_class == classes[5], ]
race3 = subset(race3, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total))

model3 = randomForest(percentile~., race3)
test = testdata[testdata$race_class == classes[5], ]
finishing_percentile = predict(model3, subset(test, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * test$race_total
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
errors[3] = mean(abs((test$finishing_position - finishing_percentile$finishing_position)))

race4 = mydata[mydata$race_class == classes[8], ]
race4 = subset(race4, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total))

model4 = randomForest(percentile~., race4)
test = testdata[testdata$race_class == classes[8], ]
finishing_percentile = predict(model4, subset(test, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * test$race_total
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
errors[4] = mean(abs((test$finishing_position - finishing_percentile$finishing_position)))

race5 = mydata[mydata$race_class != classes[2] & mydata$race_class != classes[3] & mydata$race_class != classes[5] & mydata$race_class != classes[8], ]
race5 = subset(race5, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total))

model5 = randomForest(percentile~., race5)
test = testdata[testdata$race_class != classes[2] & testdata$race_class != classes[3] & testdata$race_class != classes[5] & testdata$race_class != classes[8], ]
finishing_percentile = predict(model5, subset(test, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * test$race_total
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
errors[5] = mean(abs((test$finishing_position - finishing_percentile$finishing_position)))

#No classes (allows us to have more data)
model6 = randomForest(percentile~., subset(mydata, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total)))
finishing_percentile = predict(model6, subset(testdata, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * testdata$race_total
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
no_class_error = mean(abs((testdata$finishing_position - finishing_percentile$finishing_position)))

#Plot
finishing_percentile = finishing_percentile[order(testdata$percentile),]
testdata = testdata[order(testdata$percentile),]
plot(testdata$percentile, col="red")
points(finishing_percentile$x, col = "green")

#Look at no classes training error
finishing_percentile = predict(model6, subset(mydata, select = -c(race_class, race_id, finishing_position, jockey, trainer, horse_id, horse_name, race_total)))
write.csv(finishing_percentile,file="fp.csv",row.names=FALSE)
finishing_percentile <- read.csv("fp.csv", stringsAsFactors = FALSE)
finishing_percentile$finishing_position <- finishing_percentile$x * mydata$race_total
finishing_percentile$finishing_position <- round(finishing_percentile$finishing_position)
no_class_training_error = mean(abs((mydata$finishing_position - finishing_percentile$finishing_position)))
