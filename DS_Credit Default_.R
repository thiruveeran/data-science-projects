# Set working directory as per Mac/Windows
setwd("i:\\r\\")
loan<-read.csv("credit_dataset_final.csv")

source("data_preparation.R")  

## separate feature and class variables
# Take all except dependent variable
test.feature.vars <- test.data[,-1] 

#Just taking the dependent variable
test.class.var <- test.data[,1] 
head(test.class.var)

table(train.data$credit.rating)
prop.table(table(train.data$credit.rating))

table(test.data$credit.rating)
prop.table(table(test.data$credit.rating))

options(scipen=0)
# build a logistic regression model -
lr.model <- glm(credit.rating ~ ., data=train.data, family="binomial")
# view model details
summary(lr.model)

# Nagelkerke r-squared
library("fmsb")
NagelkerkeR2(lr.model)

# McFadden's pseudo r-squared
library("pscl")
pR2(lr.model)

# perform and evaluate predictions
lr.predictions <- predict(lr.model, test.data, type="response")
head(lr.predictions)
length(lr.predictions)

test.data$predictedprobabilities <- lr.predictions
test.data$predictedvalues <- ifelse(test.data$predictedprobabilities > 0.4, 1, 0)

# Checking accuracy
misClasificError <- mean(test.data$predictedvalues != test.data$credit.rating)
misClasificError
print(paste('This is my Accuracy',1-misClasificError))

# Area Under Curve - run syntax by syntax and see the plot
library("ROCR")
pred <- ROCR::prediction(lr.predictions,test.class.var)
perf <- ROCR::performance(prediction.obj = pred, measure="tpr", 
                          x.measure="fpr")
plot(perf)

#Hosmer lemeshow goodness of fit test for logistic regression
#Ho: Model fits the data well
#Ha: Model does not fit the data well
library("ResourceSelection")
hoslem.test(test.data$credit.rating, test.data$predictedvalues)

# Confusion Matrix
library(SDMTools)
cm <- SDMTools::confusion.matrix(test.class.var, test.data$predictedvalues)
cm; 
prop.table(cm)


##########################





