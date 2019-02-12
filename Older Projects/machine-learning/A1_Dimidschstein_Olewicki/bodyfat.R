################### ASSIGNMENT 1 ###################
# Part 4 : A multi-dimensional linear model
#
# @Authors Maxime Dimidschstein and Doriane Olewicki
# @Date 12 Feb 2018
####################################################

##### Auxiliary Functions #####

# Predict values from data contained in x with coefficients w
predict <- function (x, w) {
  intercept <- w[1]
  intercept + as.matrix(x) %*% w[2:length(w)]
}

# Compute the squared loss between real and predicted (task 1)
squareloss <- function(real, predicted) {
  sum((real-predicted)^2)
}

# Model of the BFI depending on Weight and Height
model1 <- function(training, test=training, coef=FALSE, draw=FALSE) {
  real <- test[,c("BFI")]         # Real BFI values
  
  linearmodel <- lm(BFI ~ Weight + Height, data = training)
  w <- linearmodel$coefficients   # Model
  
  x <- test[,c("Weight", "Height")]
  predicted <- predict(x,w)       # Prediction
  
  if(draw) {
    plot (real, predicted, xlab="Actual BFI", ylab="Predicted BFI")
    abline (0,1, col="blue")
  }
  
  if(coef)
    cat("Coefficients of model 1:", w[1], "(Intercept)", w[2], "(Weight)" , w[3], "(Height)\n")
  
  squareloss(real, predicted)     # Squared loss
}

# Model of the BFI depending on Weight, Abdomen and Biceps (task 2)
model2 <- function(training, test=training, coef=FALSE, draw=FALSE) {
  real <- test[,c("BFI")]         # Real BFI values
  
  linearmodel <- lm(BFI ~ Weight + Abdomen + Biceps, data = training)
  w <- linearmodel$coefficients   # Model
  
  x <- test[,c("Weight", "Abdomen", "Biceps")]
  predicted <- predict(x,w)       # Prediction
  
  if(draw) {
    plot (real, predicted, xlab="Actual BFI", ylab="Predicted BFI")
    abline (0,1, col="blue")
  }
  
  if(coef)
    cat("Coefficients of model 2:", w[1], "(Intercept)", w[2], "(Weight)" , w[3], "(Abdomen)", w[4], "(Biceps)\n")
  
  squareloss(real, predicted)     # Squared loss
}

# Perform an experiment by splitting the data and applying the model (task 3)
experiment <- function(bodyfat) {
  len <- dim(bodyfat)[1]
  indices <- sample.int(n=len, size=len/2)  # Indices splitting
  training <- bodyfat[indices,]             # Training set
  test <- bodyfat[-indices,]                # Test set
  
  model2(training, test, draw=TRUE)         # Apply model
}

# Call experiment function n times and compute average error
repetition <- function(bodyfat, n) {
  error <- 0  # Average error
  for(i in 1:n) {
    error <- error + experiment(bodyfat)
  }
  error/n
}

##### SCRIPT #####

# Input reading
bodyfat <- read.csv("bodyfat.csv", row.names=1)

model1(bodyfat, coef=TRUE)
error2 <- model2(bodyfat, coef=TRUE)
cat("Full set: Squared error =", error2, "; Mean square error =", error2/dim(bodyfat)[1], "\n")

# Open file for plots
pdf(file="BFI.pdf")

# Perform 100 experiments
errorR <- repetition(bodyfat, 100)
cat("Splitting: Squared error =", errorR, "; Mean square error =", errorR/(dim(bodyfat)[1]/2), "\n")

# Close file
dev.off()

