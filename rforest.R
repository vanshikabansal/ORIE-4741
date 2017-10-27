
horse<- read.csv("race-result-horse.csv", stringsAsFactors = FALSE)
race <- read.csv("race-result-race.csv", stringsAsFactors = FALSE)

#Assuming Cleaning.R had been run

# Merge race and horse information
mydata <- merge(horse, race, by = "race_id")

#separate races by distance
race_lengths <- unique(mydata$race_distance)
race_lengths <- sort(race_lengths)

#Removing columns deemed unnecessary
mydata <- subset(mydata, select = -c(horse_name, src, sectional_time, incident_report))

#Cleaning positions to remove ties
mydata[mydata$finishing_position == "1 DH" & !is.na(mydata$finishing_position),] = 1
mydata[mydata$finishing_position == "2 DH" & !is.na(mydata$finishing_position),] = 2
mydata[mydata$finishing_position == "3 DH" & !is.na(mydata$finishing_position),] = 3
mydata[mydata$finishing_position == "4 DH" & !is.na(mydata$finishing_position),] = 4
mydata[mydata$finishing_position == "5 DH" & !is.na(mydata$finishing_position),] = 5
mydata[mydata$finishing_position == "6 DH" & !is.na(mydata$finishing_position),] = 6
mydata[mydata$finishing_position == "7 DH" & !is.na(mydata$finishing_position),] = 7
mydata[mydata$finishing_position == "8 DH" & !is.na(mydata$finishing_position),] = 8
mydata[mydata$finishing_position == "9 DH" & !is.na(mydata$finishing_position),] = 9
mydata[mydata$finishing_position == "10 DH" & !is.na(mydata$finishing_position),] = 10
mydata[mydata$finishing_position == "11 DH" & !is.na(mydata$finishing_position),] = 11
mydata[mydata$finishing_position == "12 DH" & !is.na(mydata$finishing_position),] = 12
mydata[mydata$finishing_position == "13 DH" & !is.na(mydata$finishing_position),] = 13
mydata[mydata$finishing_position == "14 DH" & !is.na(mydata$finishing_position),] = 14

non_placing_horses <-mydata[mydata$finishing_position == "WV-A" |mydata$finishing_position== "WV"  | mydata$finishing_position=="WX"  | mydata$finishing_position=="PU" | mydata$finishing_position== "UR" |  mydata$finishing_position=="FE"  | mydata$finishing_position=="TNP" |mydata$finishing_position== "DISQ" | mydata$finishing_position=="WX-A" | mydata$finishing_position=="DNF" ,]
placing_horses <- mydata[!(mydata$finishing_position %in%  non_placing_horses$finishing_position),]


#mydata[,c("finishing_position", "actual_weight", "declared_horse_weight", "draw")] <- as.integer(mydata[,c("finishing_position", "actual_weight", "declared_horse_weight", "draw")])


# If no finishing position, we assume the horse was disqualified and allot it the last pos


race1 <- mydata[mydata$race_distance == race_lengths[1] & !is.na(mydata$race_distance),]
race2 <- mydata[mydata$race_distance  == race_lengths[2] & !is.na(mydata$race_distance),]
race3 <- mydata[mydata$race_distance == race_lengths[3] & !is.na(mydata$race_distance),]
race4 <- mydata[mydata$race_distance == race_lengths[4] & !is.na(mydata$race_distance),]
race5 <- mydata[mydata$race_distance == race_lengths[5] & !is.na(mydata$race_distance),]
race6 <- mydata[mydata$race_distance == race_lengths[6] & !is.na(mydata$race_distance),]
race7 <- mydata[mydata$race_distance == race_lengths[7] & !is.na(mydata$race_distance),]
race8 <- mydata[mydata$race_distance == race_lengths[8] & !is.na(mydata$race_distance),]
race9 <- mydata[mydata$race_distance == race_lengths[9] & !is.na(mydata$race_distance),]

race_frames <- c(race1, race2, race3, race4, race5, race6, race7, race8, race9)


#Try to remove NAs (remove later cols with empty positions for short races)
race1 <- subset(race1, select = -c(running_position_4, running_position_5, running_position_6))
race2 <- subset(race2, select = -c(running_position_4, running_position_5, running_position_6))
race3 <- subset(race3, select = -c(running_position_5, running_position_6))
race4 <- subset(race4, select = -c(running_position_5, running_position_6))
race5 <- subset(race5, select = -c(running_position_5, running_position_6))
race6 <- subset(race6, select = -c(running_position_6))
race7 <- subset(race7, select = -c(running_position_6))
