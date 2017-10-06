#
# Solution Exercise 1
# Statistical Learning - 2017
# University of Neuchâtel
# Nils Schaetti - nils.schaetti@unine.ch
#

# Load the dataset
myData = read.table("Education.txt", header=T)

##################################################################################
# QUESTION 1
# For	each of	the	variable,	compute	the	mean,	the	median,	the	standard deviation,	
# the	minimum	and	maximum	value.	
##################################################################################

# Show the summary of the data
summary(myData)

# Select the usefull predictors.
cleanedData = myData[,c("Education","Gender","Wage","Country")]

# -12 years of education makes no sense, let's remove it.
which(cleanedData$Education == min(cleanedData$Education))
#cleanedData = cleanedData[-which(cleanedData$Education == min(cleanedData$Education)),]

# Gender with value 20 makes no sense, let's remove it.
which(cleanedData$Gender == max(cleanedData$Gender))
#cleanedData = cleanedData[-which(cleanedData$Gender == max(cleanedData$Gender)),]

# 41.8 is an odd income, let's remove it.
which(cleanedData$Wage == min(cleanedData$Wage))

# Let's remove outliers
cleanedData = cleanedData[-c(234, 107, 435),]

# Let's print the summary and the std error
summary(cleanedData)
sd(cleanedData$Education)
sd(cleanedData$Wage)

##################################################################################
# QUESTION 2
# Select	the	variables	according	to the	underlying	country.	
# For	each	country,	
##################################################################################

# Select data by country
usData = cleanedData[cleanedData$Country == "US",]
caData = cleanedData[cleanedData$Country == "Canada",]

# Summarize each set
summary(usData)
sd(usData$Education)
sd(usData$Wage)
summary(caData)
sd(caData$Education)
sd(caData$Wage)

##################################################################################
# QUESTION 3
# What do you think / infer from all these variables when the main focus is to
# predict the value of the variable Wage?
# => Wage can be explained by Education and Gender.
##################################################################################

# Plot Wage against Education
plot(cleanedData$Wage ~ cleanedData$Education, main="Wage explained by Education", ylab="Income per month", xlab="Years of education", col="blue", pch=20)

# Select data by gender
menData = cleanedData$Gender == "1"
womenData = cleanedData$Gender == "2"

# Summarize each selection
summary(cleanedData$Wage[menData])
summary(cleanedData$Wage[womenData])
sd(cleanedData$Wage[menData])
sd(cleanedData$Wage[womenData])

# Use a linear model
cleanedData.lm = lm(Wage ~ ., data = cleanedData)
summary(cleanedData.lm)

##################################################################################
# QUESTION 4
# As the variable Gender is categorical, select the wage and education values 
# corresponding to each of the two possible gender values.
# Compute the mean, the median, the standard deviation, the minimum and maximum
# value for each gender separately. What can you infer from these values?
##################################################################################

# Summarize each selection
summary(cleanedData$Wage[menData])
summary(cleanedData$Wage[womenData])
sd(cleanedData$Wage[menData])
sd(cleanedData$Wage[womenData])

# Difference between Wages
median(cleanedData$Wage[menData]) - median(cleanedData$Wage[womenData])


