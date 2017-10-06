set.seed(10)

# load dataset
data <- read.table("DataHeart.txt", header=T)

# first we check the data
summary(data)

# cleaning the data and removing the id variable
data <- data[-1]
data <- data[which(data$age >= 20),]
data <- data[which(data$gramstein > 6),]

# change categorical data to factor
data$sex <- as.factor(data$sex)
data$pain <- as.factor(data$pain)
data$disease <- as.factor(data$disease)
data$electro <- as.factor(data$electro)
data$vessels <- as.factor(data$vessels)
data$thal <- as.factor(data$thal)
data$sugar <- as.factor(data$sugar)

# check if everything's okay now
summary(data)


# this is the best model (logistic model)
logistic.mod <- glm(disease ~ thal + vessels + pain + sex + pres + peak + slope + angina + rate + age, data, family = binomial)
cv.error <- cv.glm(data, logistic.mod4, K=10)$delta[1]
