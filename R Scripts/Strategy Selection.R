races = read.csv("race-result-horse.csv", stringsAsFactors = FALSE)
all_first = subset(races, as.numeric(races$finishing_position) == 1)
all_second = subset(races, as.numeric(races$finishing_position) == 2)
all_third = subset(races, as.numeric(races$finishing_position) == 3)
all_total = subset(races, as.numeric(races$finishing_position) <= 3)

all_ids = intersect(intersect(unique(all_first$race_id), unique(all_second$race_id)), 
                unique(all_third$race_id))

all_total = subset(all_total, all_total$race_id %in% all_ids)
races = subset(races, races$race_id %in% all_ids)

#N represents how many races to put in your training set
N = 1000

trainingset = subset(races, is.element(races$race_id, all_ids[1:N]))

training_first = subset(trainingset, as.numeric(trainingset$finishing_position) == 1)
training_second = subset(trainingset, as.numeric(trainingset$finishing_position) == 2)
training_third = subset(trainingset, as.numeric(trainingset$finishing_position) == 3)
training_total = subset(trainingset, as.numeric(trainingset$finishing_position) <= 3)

training_ids = intersect(intersect(unique(training_first$race_id), 
                unique(training_second$race_id)), unique(training_third$race_id))

training_first = subset(training_first, training_first$race_id %in% training_ids)
training_second = subset(training_second, training_second$race_id %in% training_ids)
training_third = subset(training_third, training_third$race_id %in% training_ids)
training_total = subset(training_total, training_total$race_id %in% training_ids)

training_horsedata = data.frame(horse_id = unique(training_total$horse_id))
x = rep(0,length(unique(training_total$horse_id)))
for (i in 1:length(unique(training_total$horse_id))){
  x[i] = sum(training_total$horse_id == training_horsedata[i,1])
}
training_horsedata[,2] = x
colnames(training_horsedata)[2] = "freq_top_three"
y = rep(0,length(unique(training_total$horse_id)))
for (i in 1:length(unique(training_total$horse_id))){
  y[i] = sum(trainingset$horse_id == training_horsedata[i,1])
}
training_horsedata[,3] = y
colnames(training_horsedata)[3] = "freq_races"
training_horsedata[,4] = x/y
colnames(training_horsedata)[4] = "percent_top_tree"

picks = subset(training_horsedata, training_horsedata$freq_races > 7 
               & training_horsedata$percent_top_tree > 0.75)
picks = as.character(picks$horse_id)

#Testing
test_ids = subset(all_ids, !is.element(all_ids, training_ids))
test_total = subset(races, is.element(races$race_id, test_ids))
test_top_three = subset(test_total, as.numeric(test_total$finishing_position) <= 3)
sum(is.element(test_top_three$horse_id, picks))

a = subset(test_total, is.element(test_total$horse_id, picks))
test_betting_races = subset(test_total, is.element(test_total$race_id, unique(a$race_id)))
length(unique(a$race_id))