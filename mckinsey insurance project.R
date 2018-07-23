#setting the working directory
setwd("i:\\r\\")


#reading the data
data<-read.csv("mctrain.csv")

#checking the structure of data
str(data)


# checking for missing values
sapply(data, function(x) sum(is.na(x)))
#Count_3.6_months_late, Count_6.12_months_late, Count_more_than_12_months_late have 97 missing values
#application_underwriting_score has 2974 missing values



#at any way we have lot information ie 79553 in our hand delting missing rows about 3000 doesnt make 
#us loss of information
data1<-na.omit(data)


sapply(data1, function(x) sum(is.na(x)))
#now we habe no missing values

#lets get the summary of our data
summary(data)
library(Hmisc)
describe(data1)


#feature engineering is done based on the importance of the variable
data1$sourcing_channel_D<-ifelse(data1$sourcing_channel =='D',1,0)
data1$sourcing_channel_c<-ifelse(data1$sourcing_channel =='C',1,0)
sapply(data1,function(x) sum(is.na(x)))


#dropping insignificant variable from our data
data1<-data1[,-c(4,10,11,12)]
head(data1)


#now we can divide it into train test set for our model buliding and cross validation
sample<-sample(2,nrow(data1),replace = T,prob=c(0.8,0.2))
traindata<-data1[sample==1,]
testdata<-data1[sample==2,]



#running logistic regression model
model1<-glm(renewal~.,data=traindata,family="binomial")
summary(model1)

#cross validating
predicted1<-predict(model1,testdata,type="response")
head(predicted1)
length(predicted1)


#checking the accuracy
testdata$predictedprob<-predicted1
testdata$predictedprob<-ifelse(testdata$predictedprob<0.5,0,1)
deviation<-mean(traindata$renewal!=testdata$predictedprob)
accuracy<- 1 - deviation
accuracy



#reading the test data
originaltest<-read.csv("mctest.csv")
sum(is.na(originaltest))
sapply(originaltest, function(x) sum(is.na(x)))



#missing value imputation for testdata
#using argImpute
# impute with random value
originaltest$Count_3.6_months_late<- with(originaltest, impute( Count_3.6_months_late, 'random'))
originaltest$Count_6.12_months_late<- with(originaltest, impute( Count_6.12_months_late, 'random'))
originaltest$Count_more_than_12_months_late<- with(originaltest, impute(Count_more_than_12_months_late, 'random'))
originaltest$application_underwriting_score<- with(originaltest, impute(application_underwriting_score , 'random'))
sum(is.na(originaltest))

#changing test data as per our train data
originaltest$sourcing_channel_D<-ifelse(originaltest$sourcing_channel =='D',1,0)
originaltest$sourcing_channel_c<-ifelse(originaltest$sourcing_channel =='C',1,0)
originaltest<-originaltest[,-c(4,10,11,12)]



#fitting model on test data
predicted2<-predict(model1,originaltest,type="response")
predicted2

#na checkinng
sum(is.na(predicted2))
head(predicted2)
length(predicted2)

#adding the proability values to renewal column
originaltest$renewal<-predicted2

#writing the predicted values of test file as csv file
write.csv(originaltest,file="twentysub.csv")



