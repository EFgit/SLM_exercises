
computerData <- read.table('ComputerData.txt', header = T)

computerData <- computerData[c("MYCT","MMIN","MMAX","CACH","CGMIN","CHMAX","PRP")]

summary.lm <- summary(lm(PRP ~ ., data=computerData))

vars <- names(computerData)

vars <- vars[which(vars != "PRP")]

tol <- 0.01

while(length(which(summary.lm$coefficients[,4] > tol)) != 0){
  
  coef <- summary.lm$coefficients
  
  var_with_max_p.value <- rownames(coef)[apply(coef,2,which.max)][4] 
  
  vars <- vars[which(vars != var_with_max_p.value)]
  
  lm2 <- lm(computerData$PRP~., data = computerData[vars])
  
  summary.lm <- summary(lm2)
  
}

