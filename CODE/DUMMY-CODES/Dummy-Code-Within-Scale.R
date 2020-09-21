data <- read.table("~/desktop/SPM-P-data-gathered-BLIMP.csv", sep = ",")
# data <- data[1:114,]
dummies <- matrix(0, nrow = nrow(data), ncol = 114)


for(i in 1:13){
  for(p in 1:nrow(data)){
    if(data[p,2] == i){dummies[p,i] <- 1}
  }
}
for(i in 15:29){
  for(p in 1:nrow(data)){
    if(data[p,2] == i){dummies[p,i] <- 1}
  }
}
for(i in 31:41){
  for(p in 1:nrow(data)){
    if(data[p,2] == i){dummies[p,i] <- 1}
  }
}
for(i in 43:64){
  for(p in 1:nrow(data)){
    if(data[p,2] == i){dummies[p,i] <- 1}
  }
}
for(i in 66:72){
  for(p in 1:nrow(data)){
    if(data[p,2] == i){dummies[p,i] <- 1}
  }
}
for(i in 74:86){
  for(p in 1:nrow(data)){
    if(data[p,2] == i){dummies[p,i] <- 1}
  }
}
for(i in 88:102){
  for(p in 1:nrow(data)){
    if(data[p,2] == i){dummies[p,i] <- 1}
  }
}
for(i in 104:113){
  for(p in 1:nrow(data)){
    if(data[p,2] == i){dummies[p,i] <- 1}
  }
}

test <- cbind(data, dummies)
colMeans(dummies)

write.table(cbind(data, dummies), "~/desktop/SPM.csv", sep = ",", row.names = F, col.names = F)
