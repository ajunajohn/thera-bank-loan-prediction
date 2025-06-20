#set working directory
setwd("D:/DCIM") 
getwd()
install.packages("caret")
install.packages("DMwR")
install.packages("readxl")
install.packages("randomForest")
install.packages("tidyverse")
install.packages("ROCR")
install.packages("ModelMetrics")
install.packages("corrplot")
install.packages("ineq")
install.packages("e1071")
install.packages(rpart.plot)
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("grobs")
library(grid)
library(gridExtra)
library(lattice)
library(ModelMetrics)
library(randomForest)
library(corrplot)
library(ineq)
library(ROCR)
library(caret)
library(tidyverse)
library(readxl)
library(dplyr)
library(randomForest)
library(rpart)
library(ggplot2)
library(rpart.plot)

#Read the dataset
bankdata=read_xlsx("D:/DCIM/Thera Bank_dataset.xlsx")
View(bankdata)

#Structure of data
dim(bankdata)
head(bankdata)
names(bankdata)
any(is.na(bankdata)) 
colSums(is.na(bankdata))
sum(is.na(bankdata))
bankdata$`Family members`[is.na(bankdata$`Family members`)]=median(bankdata$`Family members`,na.rm=T)
sum(is.na(bankdata))# checking again after replacing with median value

str(bankdata) # All the variables are numeric data

# Exploratory Data Analysis and Descriptive Statistics


colnames(bankdata)=c('ID','Age_in_years','Experience(years)','Income(K/year)',
        'Zip_code','Family_members','CCAvg','Education','Mortgage','Personal_loan',
        'Securities_Account','CD_Amount','Online','CreditCard')
bankdata=bankdata[,-c(1,5)] ## removing ID and Zip code column from the dataset

#Converting few numeric columns into factors
col=c('Family_members','Education','Personal_loan',
      'Securities_Account','CD_Amount','Online','CreditCard')
bankdata[col]=lapply(bankdata[col],factor)

## Converting Education into ordered factors
bankdata$Education =factor(bankdata$Education,levels = c("1","2","3"),order = TRUE)

bankdata[bankdata$'Experience(years)'<0,]  ## checking for rows having negative values in Experience
bankdata$'Experience(years)' = abs(bankdata$'Experience(years)')# fixing negative values 
dim(bankdata)


#summary of the dataset

summary(bankdata)



## Univariate analysis
### Boxplot for numerical data

boxplot(bankdata$Age_in_years,main ="Boxplot of Age",ylab = "Age in years") 
hist(bankdata$Age_in_years,main = "Histogram of Age",xlab = "Age in Years")

#We can observe that Age is very close to the normal distribution
#There is no outliers present in the Age data

boxplot(bankdata$'Experience(years)',main ="Boxplot of Experience",ylab ="Experience in years")
hist(bankdata$'Experience(years)',main ="Histogram of Experience",ylab ="Experience in years")
#Inference : No outliers in Experience data

boxplot(bankdata$'Income(K/year)',main="Boxplot of Annual Income",ylab = "Monthly Income")
#Inference : There are outliers in the monthly income data
hist(bankdata$`Income(K/year)`)
subset(bankdata,bankdata$`Income(K/year)`<(quantile(bankdata$`Income(K/year)`,.25)
                                           -IQR(bankdata$`Income(K/year)`))|bankdata$`Income(K/year)`>
         (quantile(bankdata$`Income(K/year)`,.75)+IQR(bankdata$`Income(K/year)`)))

boxplot(bankdata$CCAvg,main="Boxplot of Average Spending of credit card per month",ylab = "Average Spending")
#There are outliers in the average spending of credit card per month 
hist(bankdata$CCAvg)
subset(bankdata,bankdata$'CCAvg'<(quantile(bankdata$'CCAvg',.25)
                                  -IQR(bankdata$'CCAvg'))|bankdata$'CCAvg'>
         (quantile(bankdata$'CCAvg',.75)+IQR(bankdata$'CCAvg')))


boxplot(bankdata$Mortgage,main ="Boxplot of House Mortgage",ylab = "House Mortgag")
#Inference:There are outliers in Morgage data
hist(bankdata$Mortgage)
subset(bankdata,bankdata$Mortgage<(quantile(bankdata$Mortgage,.25)
                                   -IQR(bankdata$Mortgage))|bankdata$Mortgage>
         (quantile(bankdata$Mortgage,.75)+IQR(bankdata$Mortgage)))


# Barplot of multiple dimensions
counts = table(bankdata$Family_members, bankdata$Personal_loan)
barplot(counts, main="Family members vs Personal Loan",
        xlab="Personal Loan No vs Yes", col=c("darkblue","red","green","yellow"),
        legend = rownames(counts), beside=TRUE)

#Families having more members have higher liklihood to take loan
counts2 = table(bankdata$Education, bankdata$Personal_loan)
barplot(counts2, main="Education Category vs Personal Loan",
        xlab="Personal Loan No vs Yes", col=c("darkblue","red","green"),
        legend = c("1 Undergrad", "2 Graduate","3 Advanced/Professional"), beside=TRUE)
#Advanced/Professional require loan 


# Table to view relation between variables
table(bankdata$Family_members, bankdata$Personal_loan)
table(bankdata$Education, bankdata$Personal_loan)

g1=ggplot(bankdata, aes(bankdata$'Income(K/year)', fill= Personal_loan)) + geom_density(alpha=0.4)
g2=ggplot(bankdata,aes(bankdata$'Mortgage', fill= Personal_loan)) + geom_density(alpha=0.4)
g3 =ggplot(bankdata, aes(bankdata$Age_in_years, fill= Personal_loan)) + geom_density(alpha=0.4)
g4 = ggplot(bankdata, aes(bankdata$`Experience(years)`, fill= Personal_loan)) + geom_density(alpha=0.4)
g5 = ggplot(bankdata, aes(bankdata$`Income(K/year)`, fill= Education)) + geom_histogram(alpha=0.4, bins = 70)
g6 = ggplot(bankdata, aes(bankdata$`Income(K/year)`, bankdata$Mortgage, color = Personal_loan)) + 
  geom_point(alpha = 0.7)
grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2,nrow=3)

### Correlation between the numeric variables

mydata = bankdata[, c(1,2,3,5)]
mydata1 = cor(mydata,)
round(mydata1, 2)
#Inference
#1) Age in Years and Experience are highly positively correlated
#2) Monthly Income and Average credit card spend is also positively correlated

prop.table(table(bankdata$Personal_loan))*100
## the customers who took personal loan vs no personal loan was 90.4% and 9.6% respectively

# Cart and RandomForest algorithm

#splitting the dataset into train and test dataset
set.seed(100)
train=createDataPartition(bankdata$Personal_loan,p=0.7,list=FALSE,times=1)
traindata=bankdata[train,1:length(bankdata)]
testdata=bankdata[-train,1:length(bankdata)]
dim(testdata)
dim(traindata)
prop.table(table(traindata$Personal_loan))#90.4% of train data says No and only 9.6% says Yes to personal loan 
prop.table(table(testdata$Personal_loan))#90.4% of test data says No and only 9.6% says Yes to personal loan
## As this is unbalanced data, we need to balance it 
table(testdata$Personal_loan)
table(traindata$Personal_loan)

#Balancing train dataset
train.pos=subset(traindata,traindata$Personal_loan==1)
train.neg=subset(traindata,traindata$Personal_loan==0)
sum(is.na(train.pos))
sum(is.na(train.neg))
nrow(subset(traindata,traindata$Personal_loan==1))/nrow(traindata) 
nrow(subset(traindata,traindata$Personal_loan==0))/nrow(traindata)
#Duplicate the data 
set.seed(500)
train_new.index=sample(c(1:nrow(train.neg)),nrow(train.pos),replace = FALSE)
train_new=train.neg[train_new.index,]
dim(train_new)
#Now rbind 336 "0" and 336 "1" observations
train_new=rbind(train.pos,train_new)
dim(train_new)
summary(train_new)
str(train_new)
levels(train_new$Personal_loan)
table(train_new$Personal_loan)
prop.table(table(train_new$Personal_loan)) #This dataset is 50:50 proportionate
#After doing rbind, we need to shuffle the data to create randomness in the data 
train_new=train_new[sample(1:nrow(train_new)),]  
view(train_new)

#Balancing Test dataset
test.pos=subset(testdata,testdata$Personal_loan==1)
test.neg=subset(testdata,testdata$Personal_loan==0)
sum(is.na(test.pos))
sum(is.na(test.neg))
nrow(subset(testdata,testdata$Personal_loan==1))/nrow(testdata) 
nrow(subset(testdata,testdata$Personal_loan==0))/nrow(testdata)
#Duplicate the data 
set.seed(500)
test_new.index=sample(c(1:nrow(test.neg)),nrow(test.pos),replace = FALSE)
test_new=test.neg[test_new.index,]
dim(test_new)
#Now rbind 144 "0" and 144 "1" observations
test_new=rbind(test.pos,test_new)
dim(test_new)
summary(test_new)
str(test_new)
levels(test_new$Personal_loan)
table(test_new$Personal_loan)
prop.table(table(test_new$Personal_loan)) #This dataset is 50:50 proportionate
#After doing rbind, we need to shuffle the data to create randomness in the data 
test_new=test_new[sample(1:nrow(test_new)),]  
view(test_new)

##CART Model##

r.ctrl=rpart.control(minsplit = 20,minbucket = 10,xval = 5)
DTModel=rpart(train_new$Personal_loan~.,data = train_new,method="class",control = r.ctrl)
rpart.plot(DTModel,cex=0.6)
print(DTModel)
printcp(DTModel)
attributes(DTModel)
DTModel$cptable
plotcp(DTModel)

#To compare with the original plot
DTModel1=rpart(bankdata$Personal_loan~.,data = bankdata,method="class",control = r.ctrl)
rpart.plot(DTModel1,cex=0.6)
print(DTModel1)


#We will have to prune the data considering .039 as the pruned parameter from the rpart plot
ptree=prune(DTModel,cp=.039,"cp")
printcp(ptree)
#rpart(formula=ptree$Personal_loan~.,data=ptree,method = "class",control = r.ctrl)
rpart.plot(ptree)
ptree
path.rpart(ptree,c(1:12))
#train_new$prediction=predict(ptree,data=train_new,type="class")
#train_new$score=predict(ptree,data=train_new,type="prob")
#View(train_new)

#predict on test data
predDT_test=predict(DTModel,newdata = test_new,type="class")
predDT_test$prediction=predict(ptree,newdata = test_new,type="class")
predDT_test$predictscore=predict(ptree,newdata = test_new,type="prob")
predDT_test
#Confusion matrix
#tab=table(test_new$Personal_loan,predDT_test) 
#tab  ##we get the confusion matrix
#sum(diag(tab))/nrow(test_new) # overall accuracy is 97.22%
##confusionMatrix(test_new$Personal_loan,predDT_test)
confusionMatrix(table(as.factor(test_new$Personal_loan),predDT_test$prediction ))

##ROC for pruned tree
pred.cart=predict(ptree,newdata=test_new,type = "prob")[,2]
Pred2=prediction(pred.cart,test_new$Personal_loan)
plot(performance(Pred2,"tpr","fpr"))
abline(0,1,lty=2)

#plotting AUC
auc.perf=performance(Pred2,"auc")
auc=as.numeric(auc.perf@y.values)
print(auc)  #Area under the curve is around 0.935


#CART Model is close to 93.5% accurate in predicting personal loan on test data

##Random Forest Model##
set.seed(100)
train_new1=train_new[1:12]

ctrl=trainControl(method="cv",number=5,sampling = "up")
RFmodel=train(Personal_loan~.,data=train_new1,method="rf",trcontrol="ctrl")
print(RFmodel)##Accuracy is 97.08%
plot(RFmodel)

##importance(RFmodel)

#Prediction on test data
pred=predict(RFmodel,newdata = test_new)
confusionMatrix(test_new$Personal_loan, pred)
tab=table(test_new$Personal_loan,pred)
tab # Confusion matrix
sum(diag(tab)/sum(tab))  ##Accuracy provided is 97.57%
##confusionMatrix(test_new$Personal_loan,pred)

##ROC for Random forest

pred_rf=predict(RFmodel,test_new,type='prob')[,2]
require(pROC)
rf.roc=roc(test_new$Personal_loan,pred_rf)
plot(rf.roc)
rf.roc
##ROC is close to ideal one
#as.numeric(auc.perf@y.values)
auc(test_new$Personal_loan,predict(RFmodel,test_new,type='prob')[,2])
##Area under the curve is .9974

##varImpPlot(RFmodel,sort = T,n.var=10,main = "Top 12 - Variable Importance")

##-------------------------------------------------------------##
  
  
  
  
