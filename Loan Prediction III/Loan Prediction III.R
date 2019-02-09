### Data Import -----
rm(list=ls())
setwd("D:/Tobecopied/Personal/Latest addins/Practise Problems/Loan Prediction III") 
train = read.csv("train.csv",na.strings = c(""))
test = read.csv("test.csv",na.strings = c(""))
sample = read.csv("Sample_Submission.csv",na.strings = c(""))

##Hypothesis---------
#1. Credit Score: Numeric: Categorical
#2. Income/Loan: Numeric: Continuous
#3. Age/Married: Character: Categorical
#4. Dependents: Numeric: Categorical
#5. Source of Income/Employed? : Character: Categorical
#6. Property Value or Property Region: Character: Categorical
#7. Debt/Income: Numeric: Continuous
#8. Dependents?: Numeric: Categorical
summary(train)
library(Hmisc) ## Package to describe variables
describe(train)
barplot(table(train$Loan_Status))
##Univariate Analysis ----------

##Continuous Variables

#Applicant Income ----
boxplot(train$ApplicantIncome~train$Loan_Status)
hist(train$ApplicantIncome[train$Loan_Status =="Y"], breaks=100,col="green")
rug(train$ApplicantIncome[train$Loan_Status =="Y"])
abline(v=median(train$ApplicantIncome[train$Loan_Status =="Y"]),col ="blue",lwd=2)
hist(train$ApplicantIncome, breaks=100,col="green")
rug(train$ApplicantIncome)
abline(v=median(train$ApplicantIncome),col ="blue",lwd=2)
par(mfrow = c(2,1),mar=c(4,4,2,1))
hist(subset(train$ApplicantIncome,train$Loan_Status =="Y"),col="green", breaks=100)
hist(subset(train$ApplicantIncome,train$Loan_Status =="N"),col="green", breaks=100)
par(mfrow = c(1,1))
### Checking Linearity of App Income -----
breaks = seq(1000,10000, by=1000)
AI_loanstatus = cut(train$ApplicantIncome[train$Loan_Status == "Y"],breaks, right =FALSE)
AI_tot = cut(train$ApplicantIncome,breaks, right =FALSE)
AI_freqtot = table(AI_tot)
AI_freqloanstatus = table(AI_loanstatus)
aa <- cbind(AI_freqtot,AI_freqloanstatus)
aa = data.frame(aa)
breaks = data.frame(breaks)
breaks = breaks[1:(nrow(breaks)-1),1]
aa$perc = aa$AI_freqloanstatus/aa$AI_freqtot               
aa$perc[is.na(aa$perc)] = 0
plot(breaks,aa$perc,type="l",main ="Applicant Income",xlab = "Applicant Income", ylab = "%events")
grid()

##Inf: 1.Have to remove/average Outliers
#      2.Doesn't influence much
#      3.Right Skewed Data
#Loan Amount Term & Missing Variable Treatment-----
boxplot(train$Loan_Amount_Term~train$Loan_Status)
hist(train$Loan_Amount_Term, breaks =100, col="green")
## Inf: Mode of Terms is 360; Hence replacing all #NA's with 360
train$Loan_Amount_Term[train$Loan_Amount_Term == 'NA'] = 360
test$Loan_Amount_Term[test$Loan_Amount_Term == 'NA'] = 360
#Categorical Variables ----
#Credit History --------
table(train$Credit_History,train$Loan_Status)
# tapply(train$Credit_History,na.rm =TRUE)O
barplot(as.matrix(table(train$Loan_Status,train$Credit_History)))
## Inf: 1.Credit History is a good predictor
#       2.Missing Values 

# #Missing Value treatment
# pMiss <- function(x){sum(is.na(x))/length(x)*100}
# apply(train,2,pMiss)
# apply(train,1,pMiss)


# 
# train$dti = train$LoanAmount/train$ApplicantIncome
# test$dti = test$LoanAmount/test$ApplicantIncome
# hist(train$dti, breaks =100, col="green")


# frmla = as.formula(paste("Loan_Status ~",paste(colnames(train[-13,1]),collapse = "+"))) 
## Basic Log Model1------------- ##0.756944-------
frmla = "Loan_Status ~ Married + Education + 
   Credit_History + Property_Area + dti"
model1 = glm(frmla, data = train, method = glm.fit, family = binomial)  ##AIC: 509
testpred1 = predict(model1, newdata = test, type = "response")
sapply(train, function(x) sum(is.na(x)))
testpred1[testpred1 > 0.5] = 'Y'
testpred1[testpred1!= 'Y' ] = 'N'
testpred1[is.na(testpred1)] = 'N'
sample = data.frame(Loan_ID = test$Loan_ID,Loan_Status = testpred1)
write.csv(sample,"model1.csv")

## Basic Model2 based Credit History------------- ##0.763889   ---------
str(test$Credit_History)

testpred2 = vector(mode = "numeric", length(test))
testpred2[1:length(test$Credit_History)] = 'N'
testpred2[test$Credit_History == 1] = 'Y'
table(testpred2)
sample = data.frame(Loan_ID = test$Loan_ID,Loan_Status = testpred2)
write.csv(sample,"model2.csv")

## Ensemble Model1 ------------ 
testpred1[testpred1=='Y'] = 1
testpred1[testpred1!=1] = 0
testpred2[testpred2=='Y'] = 1
testpred2[testpred2!=1] = 0
testpred1 = as.numeric(testpred1)
testpred2 = as.numeric(testpred2)
testpred3 = 0.4*(testpred1)+0.6*(testpred2)

# sample = data.frame(Loan_ID = test$Loan_ID,Loan_Status = testpred2)
# write.csv(sample,"model2.csv")

### Outlier Treatment & Missing Value Impute ---- 
combined = rbind(train[-13],test)
combined$Dependents[is.na(combined$Dependents)] = 0
combined$Self_Employed[is.na(combined$Self_Employed)] = 'No'
combined$Married[is.na(combined$Married) & combined$Gender == 'Male'] = 'Yes'
combined$Married[is.na(combined$Married) & combined$Gender == 'Female'] = 'No'
# Decision Tree for Loan Amount
library(rpart)
library(rattle)
library(RColorBrewer)
fit = rpart(LoanAmount ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + Loan_Amount_Term,method = "anova",data = combined)
# plotcp(fit)
# plot(fit, uniform=TRUE, main="Regression Tree for LoanAmount")
# text(fit, use.n=TRUE, all=TRUE, cex=.8)
fancyRpartPlot(fit)
combined$LoanAmount[is.na(combined$LoanAmount) & combined$ApplicantIncome < 4904] = 118
combined$LoanAmount[is.na(combined$LoanAmount) & combined$ApplicantIncome >= 4904 & combined$ApplicantIncome < 7764] = 160
combined$LoanAmount[is.na(combined$LoanAmount) & combined$ApplicantIncome >= 7764 & combined$ApplicantIncome < 18000] = 220
combined$LoanAmount[is.na(combined$LoanAmount) & combined$ApplicantIncome >= 18000] = 416
combined$Loan_Amount_Term[is.na(combined$Loan_Amount_Term)] = 360
combined$Credit_History = as.factor(combined$Credit_History)
# fit = rpart(Credit_History ~ Gender + Married + Dependents + Education + Self_Employed + ApplicantIncome + Loan_Amount_Term,method = "class",data = combined)
# summary(combined$Credit_History)
combined$Credit_History[is.na(combined$Credit_History)] = 1
# knn(train = combined[na.omit(combined$Credit_History),],test = combined[is.na(combined$Credit_History),],cl = combined$CreditHistory, k=10)
## Feature Engineering ----
combined$LoanAmount = combined$LoanAmount*1000
combined$EMI = combined$LoanAmount/combined$Loan_Amount_Term
combined$DTI = combined$EMI/combined$ApplicantIncome
## Splitting Data into Train and Test -----
train1 = combined[1:614,]
train = cbind(train1,train$Loan_Status)
names(train)[ncol(train)] = "Loan_Status"
test = combined[615:nrow(combined),]
sapply(train, function(x) sum(is.na(x)))
## Random Forest -----
set.seed(415)
library("randomForest")
frmla = as.formula(paste("Loan_Status ~ ",paste(colnames(train[3:13]),collapse = "+")))
fit1 <- randomForest(frmla, data=train,importance=TRUE, ntree=250)
pred1=predict(fit1,test)
sample = data.frame(Loan_ID = test$Loan_ID,Loan_Status = pred1)
write.csv(sample,"model7.csv")
### Logistic Regression Model 2 -----------  0.784772 -----

frmla = "Loan_Status ~ Married + Education + Dependents + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area "
model2 = glm(frmla, data = train, method = glm.fit, family = binomial)  ##AIC: 509
testpred2 = predict(model2, newdata = test, type = "response")
testpred2[testpred2 > 0.5] = 'Y'
testpred2[testpred2!= 'Y' ] = 'N'
sample = data.frame(Loan_ID = test$Loan_ID,Loan_Status = testpred2)
write.csv(sample,"model3.csv")
### Decision Trees Model 2 --------------- 0.777  ----
fit = rpart(Loan_Status ~ Married + Education + Dependents + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area,method = "class",data = train)
testpred3 = predict(fit,newdata = test, type ="class")
sample = data.frame(Loan_ID = test$Loan_ID,Loan_Status = testpred3)
write.csv(sample,"model4.csv")
### Logistic Regression Model 3 ----------- 0.791667  -----

frmla = "Loan_Status ~ Married + Education + Dependents + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area +EMI"
model5 = glm(frmla, data = train, method = glm.fit, family = binomial)  ##AIC: 509
testpred5 = predict(model5, newdata = test, type = "response")
testpred5[testpred5 > 0.5] = 'Y'
testpred5[testpred5!= 'Y' ] = 'N'
sample = data.frame(Loan_ID = test$Loan_ID,Loan_Status = testpred5)
write.csv(sample,"model5.csv")

### Logistic Regression Model 4 ----------- 0.784772 -----

frmla = "Loan_Status ~ Married + Education + Dependents + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area +EMI+DTI"
model6 = glm(frmla, data = train, method = glm.fit, family = binomial)  ##AIC: 509
testpred6 = predict(model6, newdata = test, type = "response")
testpred6[testpred6 > 0.5] = 'Y'
testpred6[testpred6!= 'Y' ] = 'N'
sample = data.frame(Loan_ID = test$Loan_ID,Loan_Status = testpred6)
write.csv(sample,"model6.csv")

### Logistic Regression Model 5 ----------- 0.791667 -----
# Capping App Income & Co App Income to 33000 & 11000
combined$ApplicantIncome[combined$ApplicantIncome>33000] = 33000
combined$CoapplicantIncome[combined$CoapplicantIncome>11000] = 11000
combined$ApplicantIncome[combined$ApplicantIncome==0] = mean(combined$ApplicantIncome)

frmla = "Loan_Status ~ Married + Education + Dependents + Self_Employed + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + Property_Area +EMI"
model8 = glm(frmla, data = train, method = glm.fit, family = binomial)  ##AIC: 509
testpred8 = predict(model8, newdata = test, type = "response")
testpred8[testpred8 > 0.5] = 'Y'
testpred8[testpred8!= 'Y' ] = 'N'
sample = data.frame(Loan_ID = test$Loan_ID,Loan_Status = testpred8)
write.csv(sample,"model8.csv")

### Threshold Value Function ----- 
threshold_perf = function(cut, mod, y)
{
  yhat = (mod$fit>cut)
  w = which(y==1)
  sensitivity = mean( yhat[w] == 1 ) 
  specificity = mean( yhat[-w] == 0 ) 
  c.rate = mean( y==yhat ) 
  d = cbind(sensitivity,specificity)-c(1,1)
  d = sqrt( d[1]^2 + d[2]^2 ) 
  out = t(as.matrix(c(sensitivity, specificity, c.rate,d)))
  colnames(out) = c("sensitivity", "specificity", "c.rate", "distance")
  return(out)
}
##Plotting
y = rep(1, times = nrow(train))
y[train[,15]=='N'] = 0
s = seq(.01,.99,length=1000)
OUT = matrix(0,1000,4)
for(i in 1:1000) OUT[i,]=threshold_perf(s[i],model5,y)
plot(s,OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
lines(s,OUT[,4],col="darkred",lwd=2)
box()
legend(0,.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Classification Rate","Distance"))

## Ensemble Model 2 -----
# Decision Tree 2, Log Model 3, Random Forest
temp = c(pred1,testpred3)



