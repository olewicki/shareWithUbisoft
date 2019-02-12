################### ASSIGNMENT 2 ###################
# Decision Tree Learning : Mini-Project
#
# @Authors Maxime Dimidschstein and Doriane Olewicki
# @Date 1 Mars 2018
####################################################
library('rpart')
library('rpart.plot')

##### Auxiliary Functions #####

# Clean environment
clean <- function() {
  env = .GlobalEnv
  rm(list=ls(envir=env), envir=env)
}

accuracy <- function(pred, real) {
  error <- 0
  for (i in 1:length(real))
    if (pred[i, real[i]] != max(pred[i,])) {
      error <- error +1
    }
  (length(real) - error) / length(real)
}

sampleOnData <- function (my_data, percent, resample = FALSE) {
  len <- dim(my_data)[1]
  indices <- sample.int(n=len, size=min(len, floor(len * (percent/100))), replace = resample)  # Indices splitting
  new_data <- my_data[indices, ]
  new_data
}

repetitionRpart <- function(my_data, my_test, percent, n, pre_pruning = 0.0, post_pruning = 0.0, do_plot = 0, formu = formula(class ~ .)) {
  tab_nbrLeafs_accur <- data.frame(nbr_leaves=rep(0,n+1), accur_train=rep(0,n+1), accur_test=rep(0,n+1), row.names=c(1:n, 'average') ) # Average error
  for(i in 1:n) {
    sampledData <- sampleOnData(my_data, percent)
    local_rpart <- rpart(formula = formu, data=sampledData, control = rpart.control(minsplit = pre_pruning, cp = post_pruning))
    tab_nbrLeafs_accur[i,1] <- (dim(local_rpart$frame[local_rpart$frame$var == '<leaf>',]))[1]
    tab_nbrLeafs_accur[i,2] <- accuracy(predict(local_rpart, sampledData), sampledData[,"class"])
    tab_nbrLeafs_accur[i,3] <- accuracy(predict(local_rpart, my_test), my_test[,"class"])
    tab_nbrLeafs_accur[n+1,1] <- tab_nbrLeafs_accur[n+1,1] + tab_nbrLeafs_accur[i,1]
    tab_nbrLeafs_accur[n+1,2] <- tab_nbrLeafs_accur[n+1,2] + tab_nbrLeafs_accur[i,2]
    tab_nbrLeafs_accur[n+1,3] <- tab_nbrLeafs_accur[n+1,3] + tab_nbrLeafs_accur[i,3]
    if (do_plot == 1) {
      plot(local_rpart)
    }
  }
  
  tab_nbrLeafs_accur[n+1,1] <- tab_nbrLeafs_accur[n+1,1]/n
  tab_nbrLeafs_accur[n+1,2] <- tab_nbrLeafs_accur[n+1,2]/n
  tab_nbrLeafs_accur[n+1,3] <- tab_nbrLeafs_accur[n+1,3]/n
  tab_nbrLeafs_accur
}

repetitionRpartPercent <- function(my_data, my_test, n, pre_pruning = 0.0, post_pruning = 0.0, do_plot = 0, formu = formula(class ~ .)) {
  percent <- c(5, 10, 20, 50, 99)
  tab_result <- data.frame(nbr_sample_index=rep(0,n*length(percent)) ,nbr_train=rep(0,n*length(percent)), nbr_leaves=rep(0,n*length(percent)), accur_train=rep(0,n*length(percent)), accur_test=rep(0,n*length(percent)))
  for (i in 1:length(percent)) {
    local_result <- repetitionRpart(my_data, my_test, percent[i], n, pre_pruning, post_pruning, formu)
    tab_result[(i-1)*n + 1:n,   1] <- 1:n
    tab_result[(i-1)*n + 1:n,   2] <- rep((percent[i]*(dim(my_data))[1]/100), n)
    tab_result[(i-1)*n + 1:n, 3:5] <- local_result[1:n,]
  }
  tab_result
}



##### Question 5 ######
print("QUESTION 5 :")
playTennis <- read.csv("playTennis.csv", row.names=1) #import the data

fitPlayTennis <- rpart(formula = Class ~ ., data=playTennis, control = rpart.control(minsplit = 1, cp = 0.0)) #build the learning tree
jpeg('q5_tree_tennis.jpg')
rpart.plot(fitPlayTennis) #plot the learning tree
dev.off()

##### Question 6 ######
bostonHouseTrain <- read.csv("BostonHouse/bostonHouseTrain.csv", row.names=1) #import the data
bostonHouseTest <- read.csv("BostonHouse/bostonHouseTest.csv", row.names=1) #import the data

fitBostonHouseTrain <- rpart(formula = class ~ ., data=bostonHouseTrain, control = rpart.control(minsplit = 1, cp = 0.0)) #build the learning tree
#plot(fitBostonHouseTrain)

nbr_leaves <- (dim(fitBostonHouseTrain$frame[fitBostonHouseTrain$frame$var == '<leaf>',]))[1]
predict_Train <- predict(fitBostonHouseTrain, bostonHouseTrain)
predict_Test <- predict(fitBostonHouseTrain, bostonHouseTest)

print("QUESTION 6 :")
print(c("Number of leaves : ", nbr_leaves))
print(c("Accuracy on the training examples : ", accuracy(predict_Train, bostonHouseTrain[,"class"])))
print(c("Accuracy on the test examples", accuracy(predict_Test, bostonHouseTest[,"class"])))


##### Question 7 ######
print("QUESTION 7 :")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 25, 5))

##### Question 8 ######
print("QUESTION 8 :")
test <- repetitionRpartPercent(bostonHouseTrain, bostonHouseTest, 10)
# print(test)

jpeg('q8_withoutprunning_accur.jpg')
boxplot(formula = accur_test ~ nbr_train, data = test, xlab="Number of training example", ylab="Test prediction accuracy", main="Learning curve without pruning")
dev.off()

##### Question 9 ######
print("QUESTION 9 :")
test_whitoutpruning <- repetitionRpartPercent(my_data=bostonHouseTrain, my_test=bostonHouseTest, n=30)
test_withpruning1 <- repetitionRpartPercent(my_data=bostonHouseTrain, my_test=bostonHouseTest, n=30, pre_pruning=0, post_pruning=0.01)
test_withpruning2 <- repetitionRpartPercent(my_data=bostonHouseTrain, my_test=bostonHouseTest, n=30, pre_pruning=10, post_pruning=0.0)
test_withpruning3 <- repetitionRpartPercent(my_data=bostonHouseTrain, my_test=bostonHouseTest, n=30, pre_pruning=10, post_pruning=0.01)
# print(test_whitoutpruning)
# print(test_withpruning1)
# print(test_withpruning2)
# print(test_withpruning3)

jpeg('q9_withoutprunning_accur.jpg')
boxplot(formula = accur_test ~ nbr_train, data = test_whitoutpruning, xlab="Number of training example", ylab="Test prediction accuracy", main="Learning curve without pruning")
dev.off()
jpeg('q9_withpreprunning_accur.jpg')
boxplot(formula = accur_test ~ nbr_train, data = test_withpruning1, xlab="Number of training example", ylab="Test prediction accuracy", main="Learning curve with post-pruning")
dev.off()
jpeg('q9_withpostprunning_accur.jpg')
boxplot(formula = accur_test ~ nbr_train, data = test_withpruning2, xlab="Number of training example", ylab="Test prediction accuracy", main="Learning curve with pre-pruning")
dev.off()
jpeg('q9_withprepostprunning_accur.jpg')
boxplot(formula = accur_test ~ nbr_train, data = test_withpruning3, xlab="Number of training example", ylab="Test prediction accuracy", main="Learning curve with pre- and post-pruning")
dev.off()

jpeg('q9_withoutprunning_leaves.jpg')
boxplot(formula = nbr_leaves ~ nbr_train, data = test_whitoutpruning, xlab="Number of training example", ylab="Number of leaves", main="Number of leaves without pruning")
dev.off()
jpeg('q9_withpreprunning_leaves.jpg')
boxplot(formula = nbr_leaves ~ nbr_train, data = test_withpruning1, xlab="Number of training example", ylab="Number of leaves", main="Number of leaves with post-pruning")
dev.off()
jpeg('q9_withpostprunning_leaves.jpg')
boxplot(formula = nbr_leaves ~ nbr_train, data = test_withpruning2, xlab="Number of training example", ylab="Number of leaves", main="Number of leaves with pre-pruning")
dev.off()
jpeg('q9_withprepostprunning_leaves.jpg')
boxplot(formula = nbr_leaves ~ nbr_train, data = test_withpruning3, xlab="Number of training example", ylab="Number of leaves", main="Number of leaves with pre- and post-pruning")
dev.off()

##### Question 10 ######
print("QUESTION 10 :")

print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01))
print("crim")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ crim)))
print("zn")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ zn)))
print("indus")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ indus)))
# print("chas", repetitionRpart(bostonHouseTrain, bostonHouseTest, 1, 0.01, 0.01, do_plot = 1, formu = formula(class ~ chas))) # does not work because there is too much 0, only a root is returned.
print("nox")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ nox)))
print("rm")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ rm)))
print("age")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ age)))
print("dis")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ dis)))
print("rad")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ rad)))
print("tax")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ tax)))
print("ptratio")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ ptratio)))
print("black")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 10, 0.01, formu = formula(class ~ black)))
print("lstat")
print(repetitionRpart(bostonHouseTrain, bostonHouseTest, 100, 1, 0.01, 0.01, formu = formula(class ~ lstat)))

test_10 <- rpart(formula = class ~ ., data=bostonHouseTrain, control = rpart.control(minsplit = 10, cp = 0.01)) #build the learning tree
jpeg('q10_features.jpg')
rpart.plot(test_10)
dev.off()

##### Question 11 ######
print("QUESTION 11 :")

bagging <- function(my_data, my_test, B = 10, mymaxdepth = 2) {
  local_pred <- matrix(0, (dim(my_test))[1], 3)
  for (i in 1:B) {
    local_sample <- sampleOnData(my_data, percent = 100, resample = TRUE)
    local_rpart <- rpart(formula = class ~ ., data=local_sample, control = rpart.control(minsplit = 1, cp = 0.0, maxdepth=mymaxdepth)) #build the learning tree
    local_pred <- local_pred + predict(local_rpart, my_test)
  }
  for (i in 1:(dim(my_test))[1]) {
    local_pred[i,] <- (local_pred[i,] == max(local_pred[i,]))
  }
  local_pred
}
sol <- bagging(bostonHouseTrain, bostonHouseTest)
print(accuracy(sol, bostonHouseTest[,'class']))

iter_bagging <- function(my_data, my_test, maxB = 30, maxdepth = 2) {
  local_acc <- rep(0, maxB)
  for (i in 1:maxB) {
    local_test <- bagging(my_data, my_test, B = i*1)
    local_acc[i] <- accuracy(local_test, bostonHouseTest[,'class'])
  }
  local_acc
}

sol <- (iter_bagging(my_data=bostonHouseTrain, my_test=bostonHouseTest))
jpeg('q11_bagging.jpg')
plot(sol, type='o', xlab="Number of bagging", ylab="Test prediction accuracy")
dev.off()

