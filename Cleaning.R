setwd("C:/Users/meaus/OneDrive/Junior/1st Semester/ORIE 4741/hong-kong-horse-racing") #setting the working directory

horse<- read.csv("race-result-horse.csv")
race <- read.csv("race-result-race.csv")

names(horse)
names(race)

n <- nrow(horse)

rr <- horse$horse_number
marks <- c()
for (k in 1:n){
  #if the line is character and the next line is a 1, then that completes a race
  if (k!= n && !is.na(rr[k+1])){
    if (rr[k+1]==1){
      marks <- c(marks,k)
    }
  }
}

marks <- c(marks,n)

jk <- unique(horse$jockey)
tr <- unique(horse$trainer)
empty1 <- rep(0,length((jk)))
empty2 <- rep(0,length((tr)))
jockey <- data.frame(jk,empty1)
trainer <- data.frame(tr,empty2)
for (i in 1:length(jk)){
  count <- 0
  for (j in 1:n){
    if (horse$jockey[k] == jk[i]){
      count <- count + 1
      jockey[i,2] <- rr[j]
    }
  }
  jockey[i,2] <- jockey[i,2]/count
}