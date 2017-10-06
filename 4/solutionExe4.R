#
# Solution Exercise 4
# Statistical Learning - 2017
# University of Neuchâtel
#

setwd("/home/schaetti/Projets/TURING/Cours/Cours-StatLearning/2017/Exercices/Exercice 4")

##################################################################################
# QUESTION 1
##################################################################################

myData <- read.table("Education4.txt", header=T)
dim(myData)
attach(myData)
summary(myData)

# Remove ID
usefulData <- myData[,-1]

# Select each gender
men <- Gender == "male"
women <- Gender == "female"

# Linear model for men
modMen.lm <- lm(Wage[men] ~ Education[men], data=usefulData)
summary(modMen.lm)

# Linear model for women
modWomen.lm <- lm(Wage[women] ~ Education[women], data=usefulData)
summary(modWomen.lm)

plot(y = myData$Wage, x = myData$Education)
abline(modWomen.lm)
abline(modMen.lm)

##################################################################################
# QUESTION 2
##################################################################################

myData <- read.table("ComputerData.txt", header=T)
dim(myData)
names(myData)
attach(myData)
summary(myData)

# We can remove the variable model (name), vendor (name) and ERP
usefulData <- myData[, c("MYCT", "MMIN", "MMAX", "CACH", "CGMIN", "CHMAX", "PRP")] 
predictors <- names(usefulData)

summary(usefulData) # Always check the data

##################################################################################
# QUESTION 3
##################################################################################

nbP <- 6
modelQuality <- numeric(nbP)
nb <- dim(usefulData)[1]

# Compute the mean RSS as the criterion to select the best predictor
for (i in 1:nbP) 
{
  # Linear model with the ith predictor
  computer.lm <- lm(PRP ~ usefulData[,i], data=usefulData)
  
  # Get residuals
  epsilon <- computer.lm$residuals
  
  # Compute MRS
  MRS <- sum(epsilon^2) / nb
  modelQuality[i] <- MRS
  
  #  Print MRS for this predictor
  cat(predictors[i], MRS,"\n")
}

# Select best predictor
predictors[which(modelQuality == min(modelQuality))]  

# Single best predictor is MMAX

##################################################################################
# QUESTION 4
##################################################################################

computer.lm <- lm(PRP ~ usefulData[,"MMAX"], data=usefulData)
summary(computer.lm)

plot(PRP ~ usefulData[,"MMAX"], main="Computer performance explained by MMAX",
     xlab="Main memory size (max)", ylab="PRP (Computer system performance)")
abline(computer.lm)

##################################################################################
# QUESTION 5 : need to predict mpg
##################################################################################

# Load dataset
myData <- read.table("Cars2Data.txt", header=T)
dim(myData)
names(myData)
attach(myData)
summary(myData)

# We can remove the variable "name" 
usefulData <- myData[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year", "origin")] 
predictors <- names(usefulData)
summary(usefulData)

##################################################################################
# QUESTION 6
##################################################################################

# Array of models quality
modelQuality <- rep(0,7)

# For each predictors
for (i in 2:8)
{
  # Linear model
  cars.lm <- lm(mpg ~ usefulData[,i], data=usefulData)
  
  # Compute MRS
  RSS <- mean(cars.lm$residuals^2)
  modelQuality[i-1] <- RSS
}

# Get best predictors
predictors[which(modelQuality == min(modelQuality))+1]  
# Single best predictor is WEIGHT

# Get best linear model and display summary
cars.lm <- lm(mpg ~ usefulData[,"weight"], data=usefulData)
summary(cars.lm)
confint(cars.lm)

##################################################################################
# QUESTION 7
##################################################################################

# Plot
plot(mpg ~ usefulData[,"weight"], main="Car performance explained by the weight", xlab="Car weight", ylab="Car performance (MPG: Miles Per Gallon)")
abline(cars.lm)



