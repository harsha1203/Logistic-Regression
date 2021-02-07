library(readr)
affairs1 <- read.csv(file.choose())

summary(affairs1)

table(affairs1$affairs)
affairs1$ynaffairs[affairs1$affairs > 0] <- 1
affairs1$ynaffairs[affairs1$affairs == 0] <- 0
affairs1$gender <- as.factor(revalue(Affairs$gender,c("male"=1, "female"=0)))
affairs1$children <- as.factor(revalue(Affairs$children,c("yes"=1, "no"=0)))
View(affairs1)

colnames(affairs1)
cor(affairs1)
class(affairs1)
attach(affairs1)

#different visulaizations
boxplot(affairs1) #outliers in age and affairs

barplot(affairs1$'age')
barplot(affairs1$'yearsmarried')

hist(affairs1$age) # peope ranging from 20 to 45 have maximum affairs
hist(affairs1$yearsmarried)# most of the alories consumed from 0 to 200

#3rd business moment
library(moments)
skewness(affairs1$age)#positive skewness
skewness(affairs1$education) #negative skewness
skewness(affairs1$occupation)#negative skewness
#4th business moment
kurtosis(affairs1$age) #positive kurtosis
kurtosis(affairs1$education) # positive kurtosis

#Normal Quantile-Quantile Plot
qqnorm(affairs1$education)## Normally distributed
qqline(affairs1$education)
qqnorm(affairs1$age)
qqline(affairs1$age)

mod_lm <- lm(affairs ~ factor(gender) + age+ yearsmarried+ factor(children) + religiousness+
               education+occupation+rating, data = affairs1)
summary(mod_lm)
# gender, children, education and occupation are insignificant
pred1 <- predict(mod_lm,affairs1)
pred1
# We can no way use the linear regression technique to classify the data
plot(pred1)

# GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(ynaffairs ~ factor(gender) + age+ yearsmarried+ factor(children) + religiousness+
               education+occupation+rating, data = affairs1,family = "binomial")
summary(model)
plot(model)
# To calculate the odds ratio manually we going r going to take exp of coef(model)
exp(coef(model))

# Confusion matrix table 
prob <- predict(model,affairs1,type="response")
prob <- predict(model,affairs1,type="response")
# Confusion matrix and considering the threshold value as 0.5 
confusion<-table(prob>0.5,affairs1$ynaffairs)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy #0.7653

diag = diag(confusion) # number of correctly classified instances per class 
rowsums = apply(confusion, 1, sum) # number of instances per class
colsums = apply(confusion, 2, sum) # number of predictions per class
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * precision * recall / (precision + recall) 
data.frame(precision, recall, f1)#The model is able to predict 0's (FAlSE) better than 1's(TRUE)
#Specificity=1-FPR

# Creating empty vectors to store predicted classes based on threshold value
pred_values <- NULL
yes_no <- NULL

pred_values <- ifelse(prob>=0.5,1,0)
yes_no <- ifelse(prob>=0.5,"yes","no")

# Creating new column to store the above values
affairs1[,"prob"] <- prob
affairs1[,"pred_values"] <- pred_values
affairs1[,"yes_no"] <- yes_no

View(affairs1[,c(1,9:11)])

table(affairs1$ynaffairs,affairs1$pred_values)


# ROC Curve => used to evaluate the betterness of the logistic model
# more area under ROC curve better is the model 
# We will use ROC curve for any classification technique not only for logistic
install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,affairs1$ynaffairs)
rocrperf<-performance(rocrpred,'tpr','fpr')

str(rocrperf)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
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
abline(a=0,b=1)

#Area under the Curve
auc<-performance(rocrpred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
legend(.7,.2, auc, title="AUC",cex=0.5)
