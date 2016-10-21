library("nnet")

#imports data into dataframe, replacing missing values{?} with NA
myData <- read.table("E:\\IUB MS\\Spring 2016\\B555 Machine Learning\\Project\\crx.data", header = F,sep = ",", na.strings = c("NA","NULL","?"), col.names = paste0("A",seq_len(16)))

#Descriptive statistics
summary(myData)

random.imp <- function(a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample(a.obs, n.missing, replace = TRUE)
  return(imputed)
}

myData.imp <- random.imp(myData)

impute <- function(a, a.impute){
  ifelse(is.na(a),a.impute,a)
}

linearRegImpute <- function(predic, respo, sis, regType){
  if(regType == "lm"){
    lm.imp.1 <- lm(respo ~predic , data = sis, subset = !is.na(respo))
  }else{
    lm.imp.1 <- multinom(respo ~predic , data = sis, subset = !is.na(respo))
  }
  
  
  
  pred.1 <- predict(lm.imp.1,sis)
  
  return(A.imp.1)
}

#Handling Missing Values 
library(mice)
md.pattern(myData)

#To adjust plot size and margins
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  op
}
par(resetPar())

library(VIM)
aggr_plot <- aggr(myData, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(myData), cex.axis=.7, gap=1, ylab=c("Histogram of missing data","Pattern"))

tempData1 <- mice(myData,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData1)

library("lattice")
xyplot(tempData1,A14 ~ A1+A2+A7+A6+A5+A4,pch=18,cex=1)

# Data with imputed values
completedData <- complete(tempData1,1)

#Data with complete cases
cc <- complete.cases(myData)
myData.completeCase <- myData[cc,]

##scatter plot of numerical variables
plot(completedData[,c(2,3,8,11,14)], col="Blue")

#####PCA
library(PCAmixdata)
PCAmix(myData[,c(2,3,8,11,14,15)],myData[,c(1,4,5,6,7,9,10,12,13)])

train <- completedData[1:552,] 
test <- completedData[553:690,]

train <- myData.completeCase[1:522,] 
test <- myData.completeCase[523:653,]

#### Logistic regression 
model <- glm(A16 ~.,family=binomial(link='logit'),data=train)

fitted.results <- predict(model,newdata=subset(test,select=c(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results <- factor(fitted.results, labels=c("-","+"))

misClasificError <- mean(fitted.results != test$A16)
print(paste('Accuracy',1-misClasificError))    ###"Accuracy 0.91304347826087"

###### Confusion matrix
#fitted.results  -  +
#             - 92 16
#             + 20 68

library(ROCR)
p <- predict(model,newdata=subset(test,select=c(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15)),type='response')
pr <- prediction(p, test$A16)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

########## SVM
library("e1071")
svm_model <- svm(A16 ~ ., data=train, kernel = "sigmoid")

pred <- predict(svm_model,newdata=subset(test,select=c(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15)),type='response')
table(pred,test$A16)

pr <- prediction(as.numeric(pred), test$A16)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

misClasificError <- mean(pred != test$A16)
print(paste('Accuracy',1-misClasificError))   ####"Accuracy 0.920289855072464"

