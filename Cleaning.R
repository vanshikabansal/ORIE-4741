setwd("C:/Users/meaus/OneDrive/Junior/1st Semester/ORIE 4741/hong-kong-horse-racing") #setting the working directory

horse<- read.csv("race-result-horse.csv")
race <- read.csv("race-result-race.csv")

names(horse)
names(race)

n = nrow(horse)

rr <- horse$horse_number

for (k in 1:n){
  #if the line is character and the next line is a 1, then that completes a race
  if (is.character(class(rr[k]))){
    if (k!= n && rr[k+1] ==1){
       
    }
  }
}