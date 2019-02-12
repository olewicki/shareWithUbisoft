################### ASSIGNMENT 3 ###################
# Linear Discriminants and Support Vector Machines
#
# @Authors Maxime Dimidschstein and Doriane Olewicki
# @Date 1 Mars 2018
####################################################
library('e1071')

sampleOnData <- function (my_data, percent, resample = FALSE) {
  if (percent == 100) {
    my_data
  }
  else {
    len <- dim(my_data)[1]
    indices <- sample.int(n=len, size=min(len, floor(len * (percent/100))), replace = resample)  # Indices splitting
    new_data <- my_data[indices, ]
    new_data
  }
}

accuracy <- function(mySVM, data) {
  expected = data$labels  # Expected result
  predicted = predict(mySVM, data[1:length(data)-1])
  
  acc = sum(expected==predicted)/length(expected)
  # err = sum(abs(as.numeric(expected)-as.numeric(predicted)))/length(expected)
  acc
}

repeatSVMPercent <- function (data_train, data_test, n, percent, param) {
  accur <- rep(0, n)
  for (i in 1:n) {
    sample <- sampleOnData(my_data=data_train, percent=percent, resample = FALSE)
    if (param$gamma == -1) {
      mySVM <- svm(labels ~ ., data = sample, kernel=param$kernel, degree=param$degree, coef0=param$coef0, cost=param$cost)
    }
    else {
      mySVM <- svm(labels ~ ., data = sample, kernel=param$kernel, degree=param$degree, gamma=param$gamma, coef0=param$coef0, cost=param$cost)
    }
    accur[i] <- accuracy(mySVM, data_test)
  }
  accur
}

repeatSVM <- function(data_train, data_test, n, param) {
  percent <- 20* 1:5
  ret <- data.frame(accur = rep(0,n*length(percent)), index_percent = rep(0, n*length(percent)), median = rep(0,n), percent = percent)
  for (i in 1:length(percent)) {
    sol <- repeatSVMPercent(data_train, data_test, n, percent[i], param)
    ret[(i-1)*n + 1:n,1] <- sol
    ret[(i-1)*n + 1:n,2] <- rep(percent[i], n)
    ret[i,3] <- median(sol)
  }
  ret
}

lettersTrain <- read.csv("Letters/LettersTrain.csv", row.names=1) #import the data
lettersValid <- read.csv("Letters/LettersValid.csv", row.names=1)
lettersTest <- read.csv("Letters/LettersTest.csv", row.names=1)

# lettersTrain[2:(length(lettersTrain)-1)] <- standardize(lettersTrain[2:(length(lettersTrain)-1)])
initSdTrain <- apply(data.matrix(lettersTrain[2:length(lettersTrain)-1]), 2, sd)
#remove the irrelevant sd 
toRemove = initSdTrain < 0.1
lettersTrain[toRemove] <- NULL
lettersValid[toRemove] <- NULL
lettersTest[toRemove] <- NULL

meanTrain <- colMeans(data.matrix(lettersTrain[2:length(lettersTrain)-1]))
sdTrain <- apply(data.matrix(lettersTrain[2:length(lettersTrain)-1]), 2, sd)
normData = as.data.frame(scale(lettersTrain[2:length(lettersTrain)-1]), names(lettersTrain[2:length(lettersTrain)-1]))
normData$labels = lettersTrain$labels
lettersTrain <- normData

normData = as.data.frame(scale(lettersValid[2:length(lettersValid)-1], center=meanTrain, scale=sdTrain), names(lettersValid[2:length(lettersValid)-1]))
normData$labels = lettersValid$labels
lettersValid <- normData

normData = as.data.frame(scale(lettersTest[2:length(lettersTest)-1], center=meanTrain, scale=sdTrain), names(lettersTest[2:length(lettersTest)-1]))
normData$labels = lettersTest$labels
lettersTest <- normData

##### Linear ######
# param <- data.frame(kernel='linear', degree=3, gamma=-1, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_lin_ref_graph.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('linear1')
# 
# ## variation of the cost
# param <- data.frame(kernel='linear', degree=3, gamma=-1, coef0=0, cost=0.5)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_lin_cost_small.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('linear2')
# 
# param <- data.frame(kernel='linear', degree=3, gamma=-1, coef0=0, cost=2)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_lin_cost_big.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('linear3')

##### Polynomial ######
# param <- data.frame(kernel='poly', degree=3, gamma=-1, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_ref_graph.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly1')
# 
# ## variation of the degree
# param <- data.frame(kernel='poly', degree=1, gamma=-1, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_degree_1.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly2')
# 
# param <- data.frame(kernel='poly', degree=2, gamma=-1, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_degree_2.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly3')
# 
# param <- data.frame(kernel='poly', degree=4, gamma=-1, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_degree_4.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly4')
# 
# ## variation of the gamma
# param <- data.frame(kernel='poly', degree=3, gamma=(1/(dim(lettersTrain)[2]-2)) /2, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_gamma_div_2.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly5')
# 
# param <- data.frame(kernel='poly', degree=3, gamma=(1/(dim(lettersTrain)[2]-2)) *2, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_gamma_times_2.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly6')
# 
# ## variation of the coef0
# param <- data.frame(kernel='poly', degree=3, gamma=-1, coef0=-1, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_coef0_minus1.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly7')
# 
# param <- data.frame(kernel='poly', degree=3, gamma=-1, coef0=1, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_coef0_plus1.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly8')
# 
# ## variation of the cost
# param <- data.frame(kernel='poly', degree=3, gamma=-1, coef0=0, cost=0.5)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_cost_small.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly9')
# 
# param <- data.frame(kernel='poly', degree=3, gamma=-1, coef0=0, cost=2)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_cost_big.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly10')
# 
# param <- data.frame(kernel='poly', degree=1, gamma=(1/(dim(lettersTrain)[2]-2)) *2, coef0=1, cost=2)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_poly_best.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('poly11')

##### Radial basis ######
# param <- data.frame(kernel='radial', degree=3, gamma=-1, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_rad_ref_graph.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('radial1')
# 
# ## variation of the gamma
# param <- data.frame(kernel='radial', degree=3, gamma=(1/(dim(lettersTrain)[2]-2)) /2, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_rad_gamma_div_2.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('radial2')
# 
# param <- data.frame(kernel='radial', degree=3, gamma=(1/(dim(lettersTrain)[2]-2)) *2, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_rad_gamma_times_2.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('radial3')
# 
# ## variation of the cost
# param <- data.frame(kernel='radial', degree=3, gamma=-1, coef0=0, cost=0.5)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_rad_cost_small.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('radial4')
# 
# param <- data.frame(kernel='radial', degree=3, gamma=-1, coef0=0, cost=2)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_rad_cost_big.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('radial5')


##### Sigmoid ######
# param <- data.frame(kernel='sigmoid', degree=3, gamma=-1, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_sig_ref_graph.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('sig1')
# 
# ## variation of the gamma
# param <- data.frame(kernel='sigmoid', degree=3, gamma=(1/(dim(lettersTrain)[2]-2)) /2, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_sig_gamma_div_2.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('sig2')
# 
# param <- data.frame(kernel='sigmoid', degree=3, gamma=(1/(dim(lettersTrain)[2]-2)) *2, coef0=0, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_sig_gamma_times_2.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('sig3')
# 
# ## variation of the coef0
# param <- data.frame(kernel='sigmoid', degree=3, gamma=-1, coef0=-1, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_sig_coef0_minus1.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('sig4')
# 
# param <- data.frame(kernel='sigmoid', degree=3, gamma=-1, coef0=1, cost=1)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_sig_coef0_plus1.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('sig5')
# 
# # ## variation of the cost
# param <- data.frame(kernel='sigmoid', degree=3, gamma=-1, coef0=0, cost=0.5)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_sig_cost_small.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('sig6')
# 
# param <- data.frame(kernel='sigmoid', degree=3, gamma=-1, coef0=0, cost=2)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_sig_cost_big.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('sig7')
# 
# param <- data.frame(kernel='sigmoid', degree=3, gamma=-1, coef0=0, cost=2)
# a<-repeatSVM(lettersTrain, lettersValid, 5, param)
# jpeg('q4_sig_best.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('sig8')

##### Best : ######
# param <- data.frame(kernel='poly', degree=1, gamma=(1/(dim(lettersTrain)[2]-2)) *2, coef0=1, cost=2)
# a<-repeatSVM(lettersTrain, lettersValid, 20, param)
# jpeg('q4_poly_best_valid.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('best1')
# 
# param <- data.frame(kernel='poly', degree=1, gamma=(1/(dim(lettersTrain)[2]-2)) *2, coef0=1, cost=2)
# a<-repeatSVM(lettersTrain, lettersTest, 20, param)
# jpeg('q4_poly_best_test.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('best2')
# 
# param <- data.frame(kernel='radial', degree=3, gamma=-1, coef0=0, cost=2)
# a<-repeatSVM(lettersTrain, lettersValid, 20, param)
# jpeg('q4_rad_best_valid.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('best3')
# 
# param <- data.frame(kernel='radial', degree=3, gamma=-1, coef0=0, cost=2)
# a<-repeatSVM(lettersTrain, lettersTest, 20, param)
# jpeg('q4_rad_best_test.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('best4')
# 
# param <- data.frame(kernel='sigmoid', degree=3, gamma=-1, coef0=0, cost=2)
# a<-repeatSVM(lettersTrain, lettersValid, 20, param)
# jpeg('q4_sig_best_valid.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('best5')
# 
# param <- data.frame(kernel='sigmoid', degree=3, gamma=-1, coef0=0, cost=2)
# a<-repeatSVM(lettersTrain, lettersTest, 20, param)
# jpeg('q4_sig_best_test.jpg')
# boxplot(formula = accur ~ index_percent, data = a, xlab="Percentage of training examples per sample", ylab="Accuracy")
# dev.off()
# print('best6')

##### Q5 ######
n = 20
acc_train <- rep(0, n)
acc_test <- rep(0, n)
nbr_SV <- rep(0, n)
for (i in 1:n) {
  local <- svm(labels ~ ., data=lettersTrain, kernel="poly", degree=1, gamma=(1/(dim(lettersTrain)[2]-2)) /i, coef0=1, cost=2)
  acc_train[i] <- accuracy(local, lettersTrain)
  acc_test[i] <- accuracy(local, lettersTest)
  nbr_SV[i] <- dim(local$SV)[1]
}

jpeg('q4_accur_SV.jpg')
plot(x=nbr_SV, y=acc_train, col='green', xlab="Number of SV", ylab="Accuracy", pch=16, ylim=c(0.5, 1))
points(x=nbr_SV, y=acc_test, col='red', pch=17)
dev.off()

