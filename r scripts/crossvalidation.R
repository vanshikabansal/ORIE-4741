
#divide the data in folds k = 5
k = 5


nchunk <- function(k,n,df){
  length = nrow(df)
  chunkk = as.integer(length/k)
  chunk1 = length-chunkk*(k-1)
  if (n!= k){
    return (df[(n-1)*chunkk+1:chunkk*n,])
  }
  else{
    return (df[(n-1)*chunkk+1:chunk1,])
  }
}


#race1
error1 = rep(0,5)
set1 = vector("list",5)
for (z in 1:k){
  race1a = nchunk(k,z,race1)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error1[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set1[[z]] = model1a
  
}

q=set1[[1]]

for (i in 2:k){
  q = combine(set1[[i]],q)
}


error2 = rep(0,5)
set2 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race2)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error2[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set2[[z]] = model1a
  
}


error3 = rep(0,5)
set3 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race3)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error3[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set3[[z]] = model1a
  
}


error4 = rep(0,5)
set4 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race4)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error4[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set4[[z]] = model1a
  
}

error5 = rep(0,5)
set5 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race5)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error5[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set5[[z]] = model1a
  
}

error6 = rep(0,5)
set6 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race6)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error6[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set6[[z]] = model1a
  
}

error7 = rep(0,5)
set7 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race7)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error7[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set7[[z]] = model1a
  
}

error8 = rep(0,5)
set8 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race8)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error8[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set8[[z]] = model1a
  
}

error9 = rep(0,5)
set9 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race9)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error9[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set9[[z]] = model1a
  
}

error10 = rep(0,5)
set10 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race10)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error10[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set10[[z]] = model1a
  
}

error11 = rep(0,5)
set11 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race11)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error11[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set11[[z]] = model1a
  
}

error12 = rep(0,5)
set12 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race12)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error12[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set12[[z]] = model1a
  
}

error13 = rep(0,5)
set13 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race13)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error13[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set13[[z]] = model1a
  
}

error14 = rep(0,5)
set14 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race14)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error14[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  
}

error15 = rep(0,5)
set15 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race15)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error15[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set15[[z]] = model1a
  
}

error16 = rep(0,5)
set16 = vector("list",5)

for (z in 1:k){
  race1a = nchunk(k,z,race16)
  model1a = randomForest(finishing_position~., race1a[sample(nrow(race1a)/4),])
  test1a = race1a[sample(nrow(race1a)/4),]
  finishing_position <- predict(model1a, test1a)
  write.csv(finishing_position,file="fp.csv",row.names=FALSE)
  finishing_position <- read.csv("fp.csv", stringsAsFactors = FALSE)
  error16[z] = sum((test1a$finishing_position - round(finishing_position$x))^2)/nrow(finishing_position)
  set16[[z]] = model1a
  
}