setwd("C:/Users/meaus/OneDrive/Junior/1st Semester/ORIE 4741/hong-kong-horse-racing") #setting the working directory

horse<- read.csv("race-result-horse.csv")
race <- read.csv("race-result-race.csv")

names(horse)
names(race)

n <- nrow(horse)

rr <- horse$finishing_position
marks <- c(1)
for (k in 1:n){
  #if the line is character and the next line is a 1, then that completes a race
  if (k!= n && !is.na(rr[k+1])){
    if (rr[k+1]==1){
      marks <- c(marks,k)
    }
  }
}

marks <- c(marks,n) #includes a marker for the last race
#marks is a vector that includes the index of the last horse in a given race

jk <- c(levels(horse$jockey)) #collects all the unique names of the jockeys/trainers
tr <- c(levels(horse$trainer))
hrs <- c(levels(horse$horse_name))
empty1 <- rep(0,length((jk)))
empty2 <- rep(0,length((tr)))
empty3 <- rep(0,length(hrs))
jockey <- data.frame(jk,empty1) #created a data frame for these
trainer <- data.frame(tr,empty2)
hnames <- data.frame(hrs,empty3)

lastplace<- rep(0,n)
hnumber <- horse$horse_number

#returns the greatest value in the list that does not exceed the index
floor <- function(index,list){
  val <- 1
  for (i in list){
    if (index >i){
      val = i
    }
  }
  return(val)
}

#returns the smallest value in the list that does not exceed the index
ceil <- function(index,list){
  val <- 0
  for (i in rev(list)){
    if (index <= i){
      val = i
    }
  }
  return(val)
}



#get all the last place rankings
#if you fall between a race, then that means your index is between the greatest item in the list that does not exceed yours
#and the smallest item in the list that exceeds your value
for (k in 2:n){
  prevrace <- floor(k,marks)
  thisrace <- ceil(k,marks)
  lastplace[k] = thisrace-prevrace
}
lastplace[1:14] = 14

rr <- as.numeric(as.character(rr))

#convert all the text into last place rankings
for (k in 1:n){
        if (is.na(rr[k])){
            rr[k] = lastplace[k] 
        }
  }






for (i in 1:length(jk)){
  count <- 0 #count helps identify how many races the trainer/jockey is involved in
  for (j in 1:n){
    if (horse[j,5] == jk[i]){
      count <- count + 1
      jockey[i,2] <- rr[j]/lastplace[j]
    }
  }
  jockey[i,2] <- jockey[i,2]/count #the jockey/trainer's metric is the average rank
}

for (i in 1:length(tr)){
  count <- 0
  for (j in 1:n){
    if (horse[j,6] == tr[i]){
      count <- count + 1
      trainer[i,2] <- rr[j]/lastplace[j]
    }
  }
  trainer[i,2] <- trainer[i,2]/count
}

for (i in 1:length(hrs)){
  count <- 0
  for (j in 1:n){
    if (horse[j,3] == hrs[i]){
      count <- count + 1
      hnames[i,2] <- rr[j]/lastplace[j]
    }
  }
  hnames[i,2] <- hnames[i,2]/count
}

