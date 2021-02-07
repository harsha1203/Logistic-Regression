library(readr)
bank_data <- read.csv(file.choose()) 
View(bank_data)

summary(bank_data)

table(y)
mean(balance)
mean(housing)
hist(bank_data$default)
library(moments)
skewness(bank_data$age)
skewness(bank_data$default)
skewness(bank_data$housing)#negative skewed
skewness(bank_data$loan)
skewness(bank_data$duration)
skewness(bank_data$campaign)
skewness(bank_data$divorced)
skewness(bank_data$married)#negative

kurtosis(bank_data$age)
kurtosis(bank_data$default)
kurtosis(bank_data$housing)#negative skewed
kurtosis(bank_data$loan)
kurtosis(bank_data$duration)
kurtosis(bank_data$campaign)
kurtosis(bank_data$divorced)
kurtosis(bank_data$married)#negative

#correlation coefficients
cor(bank_data)
colnames(bank_data)
sum(is.na(bank_data))
dim(bank_data)
str(bank_data)

var(bank_data) #variance of the data
# visualization
boxplot(bank_data)
hist(bank_data$age) # we can observe that most of the subscription taken 25yrs to 60yrs 
hist(bank_data$campaign) # maximum subscription were taken in 0 to 5 compaign
hist(bank_data$single)
hist(bank_data$married)# there is not much difference between married and unmarried but still married people are more
hist(bank_data$loan) # loan is not contribution much because very less people had taken loan
hist(bank_data$housing) # housing contribution  is a mix response of 0's and 1's
hist(bank_data$con_cellular)
attach(bank_data)
install.packages("UsingR")
library("UsingR")
densityplot(age)
densityplot(housing)

#Building model
model <- glm(y~.,data=bank_data,family = "binomial")
summary(model)

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))
names(bank_data)
#age,default,pdays,previous,poutunknown,joadmin.,joblue.collar,joentrepreneur,johousemaid,jomanagement
#joself.employed,joservices,jotechnician,jounemployed are insignificant

# preparing the model with rest of the variables
model1 <- glm(y ~ age + factor(default)+balance + factor(housing) + factor(loan) + duration + campaign + pdays + previous 
               + factor(poutfailure) + factor(poutother) + factor(poutsuccess)+ factor(con_cellular) + factor(con_telephone)
               + factor(divorced) + factor(married) + factor(joadmin.) + factor(joblue.collar) + factor(joentrepreneur)
               + factor(johousemaid) + factor(jomanagement) + factor(joretired) + factor(joself.employed) + factor(joservices)
               + factor(jostudent) + factor(jotechnician) + factor(jounemployed), data = bank_data)
summary(model1)
# deleting some varibles which are not significant
model2 <- glm(y ~balance + factor(housing) + factor(loan) + duration + campaign 
               + factor(poutfailure) + factor(poutother) + factor(poutsuccess)+ factor(con_cellular) + factor(con_telephone)
               + factor(divorced) + factor(married)    + factor(jomanagement) + factor(joretired)+ factor(jostudent), data = bank_data)
summary(model2)
plot(model2)
# Confusion matrix table 
prob <- predict(model2,bank_data,type="response")
prob
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,bank_data$y)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 0.89

# So My model accuracy is 89.8% so I can consider this model
diag = diag(confusion) # number of correctly classified instances per class 
rowsums = apply(confusion, 1, sum) # number of instances per class
colsums = apply(confusion, 2, sum) # number of predictions per class
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)
#Specificity=1-FPR
#The model is able to predict 0's (FAlSE) better than 1's(TRUE)

# ROCR Curve
library(ROCR)
rocrpred<-prediction(prob,bank_data$y)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)

plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
plot(rocrperf,colorize=T, main= "ROCR Curve", ylab="Sensitivity",xlab="1-Specificity")
abline(a=0,b=1)
# More area under the ROC Curve better is the logistic regression model obtained

## Getting cutt off or threshold value along with true positive and false positive rates in a data frame 
str(rocrperf)
rocr_cutoff <- data.frame(cut_off = rocrperf@alpha.values[[1]],fpr=rocrperf@x.values,tpr=rocrperf@y.values)
colnames(rocr_cutoff) <- c("cut_off","FPR","TPR")
View(rocr_cutoff)

library(dplyr)
rocr_cutoff$cut_off <- round(rocr_cutoff$cut_off,6)
# Sorting data frame with respect to tpr in decreasing order 
rocr_cutoff <- arrange(rocr_cutoff,desc(TPR))
View(rocr_cutoff)

#Area under the Curve
auc<-performance(rocrpred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
legend(.7,.2, auc, title="AUC",cex=0.5)

#Thus the target no. of customers to be focused upon for term deposits by the bank are predicted 
#successfully using logistic regression model with an accuracy of 89.79% 


