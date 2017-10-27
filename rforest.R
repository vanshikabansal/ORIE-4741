
horse<- read.csv("race-result-horse.csv", stringsAsFactors = FALSE)
race <- read.csv("race-result-race.csv", stringsAsFactors = FALSE)

#Assuming Cleaning.R had been run

# Merge race and horse information
mydata <- merge(horse, race, by = "race_id")
mydata <- mydata[!is.na(mydata$finishing_position), ]

#separate races by distance
race_lengths <- unique(mydata$race_distance)
race_lengths <- sort(race_lengths)

#Removing columns deemed unnecessary
mydata <- subset(mydata, select = -c(horse_id, src, sectional_time, incident_report, length_behind_winner, horse_number, race_date, race_number))

#Cleaning positions to remove ties
mydata[mydata$finishing_position == "1 DH",] = 1
mydata[mydata$finishing_position == "2 DH",] = 2
mydata[mydata$finishing_position == "3 DH",] = 3
mydata[mydata$finishing_position == "4 DH" ,] = 4
mydata[mydata$finishing_position == "5 DH",] = 5
mydata[mydata$finishing_position == "6 DH",] = 6
mydata[mydata$finishing_position == "7 DH",] = 7
mydata[mydata$finishing_position == "8 DH",] = 8
mydata[mydata$finishing_position == "9 DH",] = 9
mydata[mydata$finishing_position == "10 DH",] = 10
mydata[mydata$finishing_position == "11 DH",] = 11
mydata[mydata$finishing_position == "12 DH",] = 12
mydata[mydata$finishing_position == "13 DH",] = 13
mydata[mydata$finishing_position == "14 DH",] = 14

non_placing_horses <- mydata[mydata$finishing_position == "WV-A" |mydata$finishing_position== "WV"  | mydata$finishing_position=="WX"  | mydata$finishing_position=="PU" | mydata$finishing_position== "UR" |  mydata$finishing_position=="FE"  | mydata$finishing_position=="TNP" |mydata$finishing_position== "DISQ" | mydata$finishing_position=="WX-A" | mydata$finishing_position=="DNF",]
withdrawn_horses <- mydata[mydata$finishing_position == "WV-A" |mydata$finishing_position== "WV"  | mydata$finishing_position=="WX"  | mydata$finishing_position=="WX-A" ,]

#Use non withdrawn horses for predictions
mydata <- mydata[!(mydata$finishing_position %in%  withdrawn_horses$finishing_position),]

#Allotting non placeing horses that weren't withdrawn the last or a bad position of 14
mydata$finishing_position <- as.numeric(mydata$finishing_position)
mydata[is.na(mydata$finishing_position) & is.na(mydata$running_position_1), "running_position_1"] = 14
mydata[is.na(mydata$finishing_position) & is.na(mydata$running_position_2), "running_position_2"] = 14
mydata[is.na(mydata$finishing_position) & is.na(mydata$running_position_3), "running_position_3"] = 14
mydata[is.na(mydata$finishing_position) & is.na(mydata$running_position_4), "running_position_4"] = 14
mydata[is.na(mydata$finishing_position) & is.na(mydata$running_position_5), "running_position_5"] = 14
mydata[is.na(mydata$finishing_position) & is.na(mydata$running_position_6), "running_position_6"] = 14

mydata$finishing_position[is.na(as.numeric(mydata$finishing_position))] = 14

#Convert data to numeric to run random forest
mydata$actual_weight <- as.numeric(mydata$actual_weight)
mydata$declared_horse_weight <- as.numeric(mydata$declared_horse_weight)
mydata$draw <- as.numeric(mydata$draw)
mydata$finish_time <- as.numeric(mydata$finish_time)
mydata$win_odds <- as.numeric(mydata$finish_time)

#Separate races by distance
race1 <- mydata[mydata$race_distance == race_lengths[1],]
race2 <- mydata[mydata$race_distance  == race_lengths[2],]
race3 <- mydata[mydata$race_distance == race_lengths[3],]
race4 <- mydata[mydata$race_distance == race_lengths[4],]
race5 <- mydata[mydata$race_distance == race_lengths[5],]
race6 <- mydata[mydata$race_distance == race_lengths[6],]
race7 <- mydata[mydata$race_distance == race_lengths[7],]
race8 <- mydata[mydata$race_distance == race_lengths[8],]
race9 <- mydata[mydata$race_distance == race_lengths[9],]

race_frames <- c(race1, race2, race3, race4, race5, race6, race7, race8, race9)


#Try to remove NAs (remove later cols with empty positions for short races)
race1 <- subset(race1, select = -c(running_position_4, running_position_5, running_position_6))
race2 <- subset(race2, select = -c(running_position_4, running_position_5, running_position_6))
race3 <- subset(race3, select = -c(running_position_5, running_position_6))
race4 <- subset(race4, select = -c(running_position_5, running_position_6))
race5 <- subset(race5, select = -c(running_position_5, running_position_6))
race6 <- subset(race6, select = -c(running_position_6))
race7 <- subset(race7, select = -c(running_position_6))
