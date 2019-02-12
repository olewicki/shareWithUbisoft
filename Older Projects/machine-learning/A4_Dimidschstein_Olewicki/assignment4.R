################### ASSIGNMENT 4 ###################
# Performance Assessment
#
# @Authors Maxime Dimidschstein and Doriane Olewicki
# @Date 6 April 2018
####################################################
library('rpart')
library('rpart.plot')

BostonHouseTest <- read.csv("BostonHouse/BostonHouseTest.csv", row.names=1) #import the data
BostonHouseTrain <- read.csv("BostonHouse/BostonHouseTrain.csv", row.names=1)

##### Auxiliary Functions #####
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

inter95 <- function(local_rpart, my_test, percent, numberTest) {
  sampleTest <- sampleOnData(my_test, 100 * numberTest/106)
  p <- accuracy(predict(local_rpart, sampleTest), sampleTest[,"class"])
  
  sigma_p <- sqrt(p * (1-p) / numberTest)
  z_n <- 1.96
  
  interval <- c(p - z_n * sigma_p, p + z_n * sigma_p)
  
  data.frame(p = p, sigma_p = sigma_p, lower = interval[1], upper = interval[2])
}

repetitionInter95 <- function(local_rpart, my_test, percent, numberTest, n) {
  ret <- data.frame(p=rep(0, n+1), sigma_p=rep(0, n+1), lower=rep(0, n+1), upper=rep(0, n+1))
  for (i in 1:n) {
    local_val <- inter95(local_rpart, my_test, percent, numberTest)
    ret[i,] <- local_val
  }
  ret[n+1,"p"] <- ave(ret[1:n,"p"])[1]
  ret[n+1,"sigma_p"] <- sqrt(ret[n+1,"p"] * (1-ret[n+1,"p"]) / (numberTest))
  ret[n+1,"lower"] <- min(ret[1:n,"p"])
  ret[n+1,"upper"] <- max(ret[1:n,"p"])
  
  ret
}


##### QUESTION 2.1 #####
print("question 2.1")
n <- 100
p_RF <- 78/n
sigma_RF <- sqrt(p_RF * (1-p_RF) / n)
p_SVM <- 75/n
sigma_SVM <- sqrt(p_SVM * (1-p_SVM) / n)
z_n <- 1.96

interval_RF <- c(p_RF - z_n * sigma_RF, p_RF + z_n * sigma_RF)
print(c('interval_RF', interval_RF))

interval_SVM <- c(p_SVM - z_n * sigma_SVM, p_SVM + z_n * sigma_SVM)
print(c('interval_SVM', interval_SVM))

diff <- interval_SVM - interval_RF
print(c('diff', diff))

##### QUESTION 2.2 #####
print("question 2.2")

alpha <- 5 #%
T <- p_RF - p_SVM
sigma_p <- sqrt(p_RF * (1- p_RF) / n + p_SVM * (1- p_SVM) / n)
p <- 0 # hypothesis H_0
t <- (T - p) / sigma_p 

print(c('sigma_p', sigma_p))
print(c('t', t))

P <- pnorm(t)-pnorm(-t)
p_value <- 1-P

print(c('p_value /2', p_value/2))
print(c('p_value', p_value))
print(c('1 - p_value', P))

##### QUESTION 2.3 #####
print("question 2.3")

z_n_99 <- 2.58

n <- (z_n_99 / T)^2 * (p_RF * (1- p_RF) + p_SVM * (1- p_SVM))

print(c('n', n))

##### QUESTION 3 #####
accur <- function(k) {
  x <- runif(k, min=0, max=1)
  mean(x)
}

repeat100 <- function(k) {
  runs = rep(0,100)
  for (i in 1:100) {
    runs[i] = accur(k)
  }
  runs
}

experiment <- function(K) {
  runs = array(0, c(100, length(K)))
  for (i in 1:length(K)){
    runs[,i] = repeat100(K[i])
  }
  jpeg('A4Q3_2.jpg')
  boxplot(runs, names=K, xlab="Number of test sets", ylab="Quality measure")
  dev.off()
}

K = c(2, 5, 10, 20, 50, 100)
experiment(K)

##### QUESTION 4.1 #####
print("question 4.1")
numberTest <- 50
sampledData <- sampleOnData(BostonHouseTrain, 10)
local_rpart <- rpart(formula = formula(class ~ .), data=sampledData)
interval1 <- inter95(local_rpart, BostonHouseTest, 10, numberTest)

print("interval 4.1")
print(interval1)

##### QUESTION 4.2 #####
print("question 4.2")
numberTests <- 50
n <- 100
interval2 <- repetitionInter95(local_rpart, BostonHouseTest, 10, numberTest, n)


s <- sqrt(interval2[n+1, "p"] * (1-interval2[n+1, "p"]) / (numberTest))
interval3 <- c(interval2[n+1, "p"] - 1.96 * s, interval2[n+1, "p"] + 1.96 * s)

print(c(interval2[n+1,"lower"], interval2[n+1,"p"],interval2[n+1,"upper"]))
print(interval3)
