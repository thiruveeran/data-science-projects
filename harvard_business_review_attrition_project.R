#setting the working directory
setwd("i:\\r\\")



#reading the data
data<-read.csv("Attrition.csv")

#checking the datatypes of features
str(data)


# checking for missing values
sapply(data, function(x) sum(is.na(x)))
# There are no missing values


#lets get the summary of our data
summary(data)
library(Hmisc)
describe(data)

#since the over18,empcount,standard hours  having only one level they are dropped
data$Over18<-NULL
data$EmployeeCount<-NULL
data$StandardHours<-NULL
head(data)

#now lets see the columns left with us
colnames(data)

boxplot(data$Age)
boxplot(data$DailyRate)
boxplot(data$DistanceFromHome)
boxplot(data$Education)
boxplot(data$DailyRate)
boxplot(data$EmployeeNumber)
boxplot(data$EnvironmentSatisfaction)
boxplot(data$HourlyRate)
boxplot(data$JobInvolvement)
boxplot(data$JobLevel)
boxplot(data$JobSatisfaction)
boxplot(data$MonthlyIncome) 
boxplot(data$NumCompaniesWorked)
boxplot(data$PercentSalaryHike) 
boxplot(data$PerformanceRating)
hist(data$PerformanceRating)
boxplot(data$RelationshipSatisfaction)
boxplot(data$StockOptionLevel) 
boxplot(data$TotalWorkingYears)   
boxplot(data$TrainingTimesLastYear) 
boxplot(data$WorkLifeBalance)  
boxplot(data$YearsAtCompany)  
boxplot(data$YearsInCurrentRole)  
boxplot(data$YearsSinceLastPromotion) 
boxplot(data$YearsWithCurrManager)  


#since few of the boxplot shows presence of outlier but on seeing the variable it also possible
# of real values so here we are not going for outlier treatment



#since our dependent variable have imbalanced class proportion there is a large possibility 
#for overfitting,so we haVE TO DO SMOTTING
set.seed(2200)
library(sqldf)

smot<-sqldf("SELECT * FROM data WHERE Attrition='No'")

sample(smot)
smot<-sqldf("SELECT * FROM smot LIMIT 450")



# so far we have selected 237 rows of attrition value"no"
# now we will take 237 rows of attrition yes and combine them together
smot1<-sqldf("SELECT * FROM data WHERE Attrition='Yes'")
datanew<-sqldf("SELECT* FROM smot union all SELECT * FROM smot1")
sample(datanew)
# smotting have been done




#now we can divide it into train test set for our model buliding and cross validation
sample<-sample(2,nrow(datanew),replace = T,prob=c(0.8,0.2))
traindata<-datanew[sample==1,]
testdata<-datanew[sample==2,]



#ttaining the logistic regression model
model=glm(Attrition ~.,data=traindata,family = "binomial")


#checking the summary of our model
summary(model)

# Nagelkerke r-squared
library("fmsb")
NagelkerkeR2(model)


#separating the dependent variable from test data for future comparison
testdatacompare<-testdata[,2]

#changing the "no","yes" values to 0,1 respectively
testdatacompare<-ifelse(testdatacompare=='No',0,1)
testdatacompare<-as.data.frame(testdatacompare)

#removing the dependent variable from testdata to plug it in to the model
testdata<-testdata[,-2]

 #predicyting the test data
predict<-predict.glm(model,testdata,type="response")



#converting the probabality values to 0 or 1 based on cutoff value(0.5)

predict<-ifelse(predict<0.5,0,1)
predict<-as.data.frame(predict)



# Checking accuracy
mis<-mean(testdatacompare != predict)
accuracy<-1-mis
accuracy


# Area Under Curve
library("ROCR")
pred <- ROCR::prediction(predict,testdatacompare)
perf <- ROCR::performance(prediction.obj = pred, measure="tpr", 
                          x.measure="fpr")
plot(perf)




#now let us run the model after removing the insignificant variables
newtraindata<-traindata[,c("Attrition","Age","EmployeeNumber","EnvironmentSatisfaction","Gender","JobInvolvement","JobRole","MaritalStatus","NumCompaniesWorked","OverTime","PercentSalaryHike","RelationshipSatisfaction","StockOptionLevel","YearsInCurrentRole")]
newtestdata<-testdata[,c("Attrition","Age","EmployeeNumber","EnvironmentSatisfaction","Gender","JobInvolvement","JobRole","MaritalStatus","NumCompaniesWorked","OverTime","PercentSalaryHike","RelationshipSatisfaction","StockOptionLevel","YearsInCurrentRole")]

#storing dependent var from testdata for future comparison
newtestcompare<-newtestdata[,1]

#removing the dependent variable for model fitting
newtestdata<-newtestdata[,-1]

#changing its values to 0's and1's
newtestcompare<-ifelse(newtestcompare=='Yes',1,0)


#fitting the model
newmodel<-glm(Attrition~.,data=newtraindata,family = "binomial")

#seeing the summary of an model
summary(newmodel)


#predicting the newtest data
newpredict<-predict(newmodel,newtestdata,type="response")


#converting probabilities to 0,1 cutoff=0.5 
#converting probabliities to 0,1 cutoff=.5
newpredict<-ifelse(newpredict<0.5,0,1)

#accuracy
mis<-mean( newtestcompare!= newpredict)
accuracy<-1-mis
accuracy
#now we can see there is a slight increase in accuracy score




# Area Under Curve - run syntax by syntax and see the plot
library("ROCR")
pred <- ROCR::prediction(newpredict,newtestcompare)
perf <- ROCR::performance(prediction.obj = pred, measure="tpr", 
                          x.measure="fpr")
plot(perf)
#we can also observe there is an slight deviation of curve towards northeast side

 







#now let us work with random forest
#loading rf package
require(randomForest)

#reading the file
data<-read.csv("Attrition.csv")

#now we can divide it into train test set for our model buliding and cross validation
sample<-sample(2,nrow(data),replace = T,prob=c(0.8,0.2))
traindata<-data[sample==1,]
testdata<-data[sample==2,]
table(traindata$Attrition)
table(testdata$Attrition)


#model fiiting
rf<-randomForest(Attrition~.,data=traindata)
rf
plot(rf)

#refitting by tuning the parameters
rf1<-randomForest(Attrition~.,data=traindata,replace=TRUE,ntree=650,importance=T)
rf1
plot(rf1)


varImpPlot(rf,
          sort = T,
          main="Variable Importance",
          n.var=10)
testdata$predict<-predict(rf1,testdata)
predict
require(e1071)
require(caret)
confusionMatrix(data=testdata$predict,reference=testdata$Attrition)
#now we can see that there is increase in accuracy score after we have increased the no.of trees to.650





#now let us work with svm
#setting the working directory
setwd("i:\\r\\")



#reading the data
data<-read.csv("Attrition.csv")

#checking the datatypes of features
str(data)


# checking for missing values
sapply(data, function(x) sum(is.na(x)))
# There are no missing values


#lets get the summary of our data
summary(data)
library(Hmisc)
describe(data)

#since the over18,empcount,standard hours  having only one level they are dropped
data$Over18<-NULL
data$EmployeeCount<-NULL
data$StandardHours<-NULL
head(data)

#now lets see the columns left with us
colnames(data)

set.seed(3233)
#now we can divide it into train test set for our model buliding and cross validation
sample<-sample(2,nrow(data),replace = T,prob=c(0.8,0.2))
traindata<-data[sample==1,]
testdata<-data[sample==2,]



require(caret)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

svm_Linear <- train(Attrition ~., data = traindata, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear
test_pred<-predict(svm_Linear,newdata=testdata)
test_pred


#cofusion matrix
confusionMatrix(test_pred,testdata$Attrition)


#plotting our model accuracies
predictionaccuracy<-read.csv("predictionaccuracy.csv")
plot(predictionaccuracy$model_name,predictionaccuracy$accuracy,xlab="modelname",ylab="accuracy")
