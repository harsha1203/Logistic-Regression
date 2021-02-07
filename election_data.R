library(readr)
election_data <- read.csv(file.choose()) # 
View(election_data)
summary(election_data)
election_data<-election_data[,-1]
election_data <- na.omit(election_data)
summary(election_data)
plot(election_data)

table(Result)

colnames(election_data)
attach(election_data)

# Measures of Dispersion / Second moment business decision
var(Year) # variance
sd(Year) #standard deviation
range <- max(Year) - min(Year) # range
range

var(Amount.Spent) # variance
sd(Amount.Spent) #standard deviation
range <- max(Amount.Spent) - min(Amount.Spent) # range
range
var(Popularity.Rank) # variance
sd(Popularity.Rank) #standard deviation
range <- max(Popularity.Rank) - min(Popularity.Rank) # range
range

#Third moment business decision
install.packages("moments")
library(moments)
skewness(Year) # negative skewness
skewness(Amount.Spent) # positive skewness
skewness(Popularity.Rank) # negative skewness
#Fourth moment business decision
kurtosis(Year)   # positive
kurtosis(Amount.Spent)   # positive
kurtosis(Popularity.Rank)   # positive

#Graphical Representation
barplot(Year) 
barplot(Amount.Spent)
barplot(Popularity.Rank)

dotchart(Year)
hist(Year) #histogram
hist(Amount.Spent) # the range of maximum amount spent is between 3.5 to 4.5
hist(Popularity.Rank)

cor(election_data)
cor(Result,Year)
cor(Result,Amount.Spent)
cor(Result,Popularity.Rank) # n
boxplot(election_data) #boxplot
y <- boxplot(election_data)
y$out # 6.32 is a outlier which is a valid data

#Probability Distribution
install.packages("UsingR")
library("UsingR")
densityplot(Year)


model <- glm(Result ~ ., data = election_data)
summary(model)
# Residual Deviance is less than Null Deviance that's mean input variable are significance.

# Checking best fit model
library(MASS)
stepAIC(model)

# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
prob <- predict(model,election_data,type="response")
prob
# We are going to use NULL and Residual Deviance to compare the between different models

# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,election_data$Result)
confusion
# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy 

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>0.5,1,0)
yes_no <- ifelse(prob>0.5,"yes","no")
yes_no
# Creating new column to store the above values
election_data[,"prob"] <- prob
election_data[,"pred_values"] <- pred_values
election_data[,"yes_no"] <- yes_no

View(election_data[,c(1,5:7)])

table(election_data$Result,election_data$pred_values)
# So My model accuracy is 89.8% so I can consider this model
diag = diag(confusion) # number of correctly classified instances per class 
rowsums = apply(confusion, 1, sum) # number of instances per class
colsums = apply(confusion, 2, sum) # number of predictions per class
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)
Specificity=1-FPR
#The model is able to predict 0's (FAlSE) better than 1's(TRUE)
pred<-prediction(prob,election_data$Result)
perf<-performance(rocrpred,'tpr','fpr')
eval<-performance(rocrpred,"acc")
plot(eval)
max<-which.max(slot(eval,"y.values")[[1]])
max
eval
acc<-slot(eval,"y.values")[[1]][max]
acc
cut<-slot(eval,"x.values")[[1]][max]
cut
print(c(Accuracy=acc,Cutoff=cut))
# ROCR Curv
library(ROCR)
rocrpred<-prediction(prob,election_data$Result)
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
