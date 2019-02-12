################### ASSIGNMENT 3 ###################
# TITRE
#
# @Authors Maxime Dimidschstein and Doriane Olewicki
# @Date 1 Mars 2018
####################################################
library('e1071')

sampleOnData <- function (my_data, percent, resample = FALSE) {
  len <- dim(my_data)[1]
  indices <- sample.int(n=len, size=min(len, floor(len * (percent/100))), replace = resample)  # Indices splitting
  new_data <- my_data[indices, ]
  new_data
}

errorRate <- function(mySVM, data) {
  expected = data$labels  # Expected result
  
  pred = predict(mySVM, data[2:length(data)-1])
  pred[pred>26] = 26        # Round high values
  pred[pred<1] = 1          # Round low values
  predicted = factor(LETTERS[pred])
  
  # err = sum((as.numeric(expected)==as.numeric(predicted))==FALSE)/length(expected)
  err = sum(abs(as.numeric(expected)-as.numeric(predicted)))/length(expected)
  err
}

repeatSVMPercent <- function (data_train, data_test, n, percent) {
  accur <- rep(0, n)
  for (i in 1:n) {
    sample <- sampleOnData(my_data=data_train, percent=percent, resample = TRUE)
    mySVM <- svm(x=sample[2:length(lettersTrain)-1], y=as.numeric(sample$labels))
    accur[i] <- errorRate(mySVM, data_test)
  }
  accur
}

repeatSVM <- function(data_train, data_test, n) {
  percent <- 20 * 1:5
  ret <- data.frame(accur = rep(0,n*length(percent)), index_percent = rep(0, n*length(percent)), median = rep(0,n), percent = percent)
  for (i in 1:length(percent)) {
    sol <- repeatSVMPercent(data_train, data_test, n, percent[i])
    ret[(i-1)*n + 1:n,1] <- sol
    ret[(i-1)*n + 1:n,2] <- rep(percent[i], n)
    ret[i,3] <- median(sol)
  }
  ret
}

lettersTrain <- read.csv("Letters/LettersTrain.csv", row.names=1) #import the data
lettersValid <- read.csv("Letters/LettersValid.csv", row.names=1)
lettersTest <- read.csv("Letters/LettersTest.csv", row.names=1)

a<-repeatSVM(lettersTrain, lettersValid, 5)

#plot(x=a$percent[1:5]/10, y=a$median[1:5], col="green", type="l")
boxplot(formula = accur ~ index_percent, data = a)#, add=TRUE)



