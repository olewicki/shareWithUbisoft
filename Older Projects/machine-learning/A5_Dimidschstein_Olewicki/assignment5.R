################### ASSIGNMENT 5 ###################
# Concours
#
# @Authors Maxime Dimidschstein and Doriane Olewicki
# @Date 9 Mai 2018
####################################################
#library('mRMRe')
library('rpart')
library('e1071')


##### Auxiliary Functions #####

BCR <- function(pred, real, true_val = "Metastasis", false_val = "No Metastasis", toColumn = FALSE) {
  back2meta = rep("None", length(pred[,1]))
  back2meta[pred[,1] == 1] = true_val
  back2meta[pred[,2] == 1] = false_val
  
  Pos <- real == true_val
  Neg <- Pos == FALSE
  good <- back2meta == real
  wrong <- good == FALSE
  
  TP <- sum(good[Pos])
  FP <- sum(wrong[Neg])
  TN <- sum(good[Neg])
  FN <- sum(wrong[Pos])
  
  left <- if (TP+FN == 0) 0 else TP/(TP+FN)
  right <- if (TN+FP == 0) 0 else TN/(TN+FP)
  1/2 * (left + right)
}

##### Cleaning data #####
# load("project.RData")
# NumCol <- apply(data.matrix(trainSet[1,]), 2, class)
# n <- length(trainSet)
# nNum <- 22283
# toRemove <- rep(FALSE, n)
# 
# initSdTrain <- apply(data.matrix(trainSet[1:nNum]), 2, sd, na.rm=TRUE)
# initMeanTrain <- apply(data.matrix(trainSet[1:nNum]), 2, mean, na.rm=TRUE)
# 
# realSdTrain <- rep(0, nNum)
# 
# for (i in 1:nNum) {
#   toUse = (abs(trainSet[,i]) < abs(initMeanTrain[i]+initSdTrain[i]*3))
#   realSdTrain[i] <- sd(trainSet[toUse, i], na.rm=TRUE)
# }
# 
# sortedSd = sort(x=realSdTrain, decreasing=TRUE, index.return=TRUE)
# indices = sortedSd$ix
# 
# trainSetCpy <- trainSet
# testSetCpy <- testSet
# trainSetCpy[indices[1001:length(indices)]] <- NULL
# testSetCpy[indices[1001:length(indices)]] <- NULL
# 
# cleanedTrainSet <- trainSetCpy
# cleanedTestSet <- testSetCpy
# 
# cleanedTrainSet[,dim(cleanedTrainSet)[2] - 3]<- NULL
# cleanedTrainSet[,dim(cleanedTrainSet)[2] - 1]<- NULL
# cleanedTestSet[,dim(cleanedTestSet)[2] - 3]<- NULL
# cleanedTestSet[,dim(cleanedTestSet)[2] - 1]<- NULL
# 
# cleanedTrainSet$class <- (trainLabels)# == "Metastasis")
# 

##### rpart prediction ##### 
CpyCleanedTrainSet <- cleanedTrainSet

rep_rpart <- function(my_data, my_test, n = 10) {
  percent <- 50
  len <- dim(my_data)[1]
  dim_cut_1 <- min(len, floor(len * (percent/100)))
  dim_cut_2 <- len - dim_cut_1
  indices_count <- rep(0, len)
  bcr_count <- 0
  
  local_pred_train <- matrix(0, len, 2)
  local_pred_test <- matrix(0, (dim(my_test))[1], 2)
  for (i in 1:n) {
    indices <- sample.int(n=len, size=min(len, floor(len * (percent/100))), replace = FALSE)  # Indices splitting
    indices_count[-indices] <- indices_count[-indices] + 1
    cut_1 <- my_data[indices, ]
    cut_2 <- my_data[-indices, ]
    local_sample <- cut_1 #sampleOnData(cut_1, percent = 100, resample = FALSE)
    local_rpart <- rpart(formula = class ~ ., data=local_sample, control = rpart.control(minsplit = 10, cp = 0.01)) #build the learning tree
    
    local_BCR <- BCR(predict(local_rpart, cut_2), cut_2$class)
    bcr_count <- bcr_count + local_BCR
    
    local_pred_train[-indices,] <- local_pred_train[-indices,] + predict(local_rpart, cut_2) * local_BCR 
    local_pred_test <- local_pred_test + predict(local_rpart, my_test) * local_BCR 
  }
  for (i in 1:len) {
    local_pred_train[i,] <- (local_pred_train[i,] == max(local_pred_train[i,]))
  }
  for (i in 1:(dim(my_test))[1]) {
    local_pred_test[i,] <- (local_pred_test[i,] == max(local_pred_test[i,]))
  }
  
  bcr <- BCR(local_pred_train[indices_count>0,], my_data$class[indices_count>0])
  back2meta <- rep("None", length(local_pred_test[,1]))
  back2meta[local_pred_test[,1] == 1] <- "Metastasis"
  back2meta[local_pred_test[,2] == 1] <- "No Metastasis"
  
  #print(bcr_count/B)
  
  data.frame(bcr = bcr, pred_test = local_pred_test, test = back2meta)
}

##### Bagging prediction ##### 
CpyCleanedTrainSet <- cleanedTrainSet#[,1:1000]

sampleOnData <- function (my_data, percent, resample = FALSE) {
  len <- dim(my_data)[1]
  indices <- sample.int(n=len, size=min(len, floor(len * (percent/100))), replace = resample)  # Indices splitting
  new_data <- my_data[indices, ]
  new_data
}

bagging <- function(my_data, my_test, B = 10, mymaxdepth = 2) {
  percent <- 80
  len <- dim(my_data)[1]
  indices <- sample.int(n=len, size=min(len, floor(len * (percent/100))), replace = FALSE)  # Indices splitting
  cut_1 <- my_data[indices, ]
  cut_2 <- my_data[-indices, ]
  local_pred_train <- matrix(0, (dim(cut_2))[1], 2)
  local_pred_test <- matrix(0, (dim(my_test))[1], 2)
  for (i in 1:B) {
    local_sample <- sampleOnData(cut_1, percent = 100, resample = TRUE)
    local_rpart <- rpart(formula = class ~ ., data=local_sample, control = rpart.control(minsplit = 1, cp = 0.0, maxdepth=mymaxdepth)) #build the learning tree
    local_BCR <- BCR(predict(local_rpart, cut_2), cut_2$class)
    local_pred_train <- local_pred_train + predict(local_rpart, cut_2) * local_BCR 
    local_pred_test <- local_pred_test + predict(local_rpart, my_test) * local_BCR 
  }
  for (i in 1:(dim(cut_2))[1]) {
    local_pred_train[i,] <- (local_pred_train[i,] == max(local_pred_train[i,]))
  }
  for (i in 1:(dim(my_test))[1]) {
    local_pred_test[i,] <- (local_pred_test[i,] == max(local_pred_test[i,]))
  }

  back2meta <- rep("None", length(local_pred_test[,1]))
  back2meta[local_pred_test[,1] == 1] <- "Metastasis"
  back2meta[local_pred_test[,2] == 1] <- "No Metastasis"
  
  this_pred_train <- matrix(0, (dim(my_data))[1], 2)
  this_pred_train[-indices,] = local_pred_train
  data.frame(bcr = BCR(local_pred_train, cut_2$class), pred_test = local_pred_test, test = back2meta)
}

iter_bagging <- function(my_data, my_test, B = 20, mymaxdepth = 2, n=50) {
  mem_test <- matrix(0, (dim(my_test))[1], 2)
  max_bcr <- 0
  
  for (i in 1:n) {
    bag <- bagging(my_data, my_test, B, mymaxdepth)
    if (bag$bcr[1] > max_bcr) {
      max_bcr <- bag$bcr[1]
      mem_test[,1]  <- bag$pred_test.Metastasis
      mem_test[,2]  <- bag$pred_test.No.Metastasis
    }
  }

  back2meta <- rep("None", length(mem_test[,1]))
  back2meta[mem_test[,1] == 1] <- "Metastasis"
  back2meta[mem_test[,2] == 1] <- "No Metastasis"
  
  data.frame(bcr = max_bcr, test = back2meta)
}

bagging2 <- function(my_data, my_test, B = 10, mymaxdepth = 2, n=10) {
  global_pred_train <- matrix(0, (dim(my_data))[1], 2)
  global_pred_test <- matrix(0, (dim(my_test))[1], 2)
  percent <- 80
  len <- dim(my_data)[1]
  dim_cut_1 <- min(len, floor(len * (percent/100)))
  dim_cut_2 <- len - dim_cut_1
  indices_count <- rep(0, len)
  
  for (i in 1:n) {
    indices <- sample.int(n=len, size=min(len, floor(len * (percent/100))), replace = FALSE)  # Indices splitting
    indices_count[-indices] <- indices_count[-indices] + 1
    cut_1 <- my_data[indices, ]
    cut_2 <- my_data[-indices, ]
    local_pred_train <- matrix(0, (dim(cut_2))[1], 2)
    local_pred_test <- matrix(0, (dim(my_test))[1], 2)
    for (i in 1:B) {
      local_sample <- sampleOnData(cut_1, percent = 100, resample = TRUE)
      local_rpart <- rpart(formula = class ~ ., data=local_sample, control = rpart.control(minsplit = 1, cp = 0.0, maxdepth=mymaxdepth)) #build the learning tree
      local_BCR <- BCR(predict(local_rpart, cut_2), cut_2$class)
      local_pred_train <- local_pred_train + predict(local_rpart, cut_2) * local_BCR
      local_pred_test <- local_pred_test + predict(local_rpart, my_test) * local_BCR
    }
    for (i in 1:(dim(cut_2))[1]) {
      local_pred_train[i,] <- (local_pred_train[i,] == max(local_pred_train[i,]))
    }
    for (i in 1:(dim(my_test))[1]) {
      local_pred_test[i,] <- (local_pred_test[i,] == max(local_pred_test[i,]))
    }
    
    local_bcr <- BCR(local_pred_train, cut_2$class)
    global_pred_train[-indices,] <- global_pred_train[-indices,] + local_pred_train * local_bcr
    global_pred_test <- global_pred_test + local_pred_test * local_bcr
  }
  
  for (i in 1:(dim(my_data))[1]) {
    global_pred_train[i,] <- (global_pred_train[i,] == max(global_pred_train[i,]))
  }
  for (i in 1:(dim(my_test))[1]) {
    global_pred_test[i,] <- (global_pred_test[i,] == max(global_pred_test[i,]))
  }
  
  back2meta <- rep("None", length(global_pred_test[,1]))
  back2meta[global_pred_test[,1] == 1] <- "Metastasis"
  back2meta[global_pred_test[,2] == 1] <- "No Metastasis"
  
  data.frame(bcr = BCR(global_pred_train[indices_count>0,], my_data$class[indices_count>0]), pred_test = global_pred_test, test = back2meta)
}

##### SVM prediction #####
CpyCleanedTrainSet <- cleanedTrainSet[,1:1000]
CpyCleanedTrainSet$class <- (trainLabels)

repeatSVMPercent <- function (my_data, my_test, n, percent) {
  bcr <- rep(0, n)

  len <- dim(my_data)[1]
  dim_cut_1 <- min(len, floor(len * (percent/100)))
  dim_cut_2 <- len - dim_cut_1
  indices_count <- rep(0, len)
  bcr_count <- 0

  local_pred_train <- matrix(0, len, 2)
  local_pred_test <- matrix(0, (dim(my_test))[1], 2)
  
  for (i in 1:n) {
    indices <- sample.int(n=len, size=min(len, floor(len * (percent/100))), replace = FALSE)  # Indices splitting
    indices_count[-indices] <- indices_count[-indices] + 1
    cut_1 <- my_data[indices, ]
    cut_2 <- my_data[-indices, ]
    
    mySVM <- svm(class ~ ., data = cut_1, kernel="poly", gamma=(1/(dim(my_data)[2]-2)) *10, degree=2, coef0=10)
    
    pred <- predict(mySVM, cut_2)
    pred_col <- matrix(0, dim(cut_2)[1],2)
    
    pred_col[,1] <- pred == "Metastasis"
    pred_col[,2] <- pred == "No Metastasis"
    bcr[i] <- BCR(pred_col, cut_2$class)
    local_pred_train[-indices,] <- local_pred_train[-indices,] + pred_col * bcr[i]
    
    pred_test <- predict(mySVM, my_test)
    pred_test_col <- matrix(0, dim(my_test)[1],2)
    pred_test_col[,1] <- pred_test == "Metastasis"
    pred_test_col[,2] <- pred_test == "No Metastasis"
    local_pred_test <- local_pred_test + pred_test_col * bcr[i]
  }
  
  for (i in 1:len) {
    local_pred_train[i,] <- (local_pred_train[i,] == max(local_pred_train[i,]))
  }
  for (i in 1:(dim(my_test))[1]) {
    local_pred_test[i,] <- (local_pred_test[i,] == max(local_pred_test[i,]))
  }
  
  back2meta <- rep("None", length(local_pred_test[,1]))
  back2meta[local_pred_test[,1] == 1] <- "Metastasis"
  back2meta[local_pred_test[,2] == 1] <- "No Metastasis"
  print(back2meta)
  data.frame(bcr = BCR(local_pred_train[indices_count>0,], my_data$class[indices_count>0]), pred_test = back2meta)
}

meanTrain <- colMeans(data.matrix(CpyCleanedTrainSet[1:length(CpyCleanedTrainSet)-1]))
sdTrain <- apply(data.matrix(CpyCleanedTrainSet[1:length(CpyCleanedTrainSet)-1]), 2, sd)
normData = as.data.frame(scale(CpyCleanedTrainSet[1:length(CpyCleanedTrainSet)-1]), names(CpyCleanedTrainSet[2:length(CpyCleanedTrainSet)-1]))
normData$labels = CpyCleanedTrainSet$labels
CpyCleanedTrainSet <- normData
CpyCleanedTrainSet$class <- (trainLabels)

CpyCleanedTestSet <- cleanedTestSet[,1:1000]

normData = as.data.frame(scale(CpyCleanedTestSet[1:length(CpyCleanedTestSet)], center=meanTrain, scale=sdTrain), names(CpyCleanedTestSet[2:length(CpyCleanedTestSet)]))
normData$labels = CpyCleanedTestSet$labels
CpyCleanedTestSet <- normData


##### Tests #####
# print("rpart pred")
# sol_rep_rpart <- rep_rpart(CpyCleanedTrainSet, testSet, n = 20)
# print(sol_rep_rpart$bcr[1])
# 
# # print("Bagging pred")
# # sol_iter_bagging <- iter_bagging(CpyCleanedTrainSet, testSet, mymaxdepth = 5, n=10)
# # print(sol_iter_bagging$bcr[1])
# 
# print("Bagging2 pred")
# sol_bagging2 <- bagging2(CpyCleanedTrainSet, testSet, mymaxdepth = 5, n = 20)
# print(sol_bagging2$bcr[1])

print("SVM pred")
sol_svm <- repeatSVMPercent(CpyCleanedTrainSet, CpyCleanedTestSet, n=20, percent=60)
print(sol_svm$bcr[1])
  

bcr <- data.frame(Prediction = sol_svm$bcr[1], row.names = "BCR")
pred <- data.frame(Prediction = sol_svm$pred_test, row.names = row.names(testSet))

final <- rbind(bcr, pred)
write.csv(x=final, file="Predictions.csv")

  
  