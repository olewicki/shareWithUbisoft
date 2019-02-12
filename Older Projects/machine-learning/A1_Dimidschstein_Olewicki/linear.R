################### ASSIGNMENT 1 ###################
# Part 3 : A simple linear regression example
#
# @Authors Maxime Dimidschstein and Doriane Olewicki
# @Date 12 Feb 2018
####################################################

##### Auxiliary Functions #####

# Clean environment
clean <- function() {
  env = .GlobalEnv
  rm(list=ls(envir=env), envir=env)
}

# Linear model
linearmodel1 <- function(w0, w, x) {
  w0 + w * x
}

# Squared loss
sqloss <- function(w, w0, x, y) {
  sum((linearmodel1(w0, w, x) - y)^2)
}

##### SCRIPT #####

x <- 1:10
y <- 3*x + runif(length(x), 0, 1)
plot(x,y)

w0 <- 1
w1 <- 3
abline (w0, w1, col = "red")

sqloss(w1, w0, x, y)

w1.range <- seq(-2,5,by=0.1)
loss.values <- sapply(w1.range, sqloss, w0, x, y)
plot(w1.range, loss.values, col="green", type="l")
optimize(f=sqloss, w1.range, w0, x, y)
