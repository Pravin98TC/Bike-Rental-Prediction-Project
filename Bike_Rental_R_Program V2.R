
library(corrplot)
library(dplyr)
library(randomForest)

#REMOVE ALL THE OBJECTS STORED IN CURRENT WORKING R ENVIRONMENT ( cleans the RAM )
rm(list=ls())

#Set the Directory 
setwd("F:/Data Science/Projects/Bike Rental/R Program");

#Verify if Directory  is set or not
getwd() 

Bike_Rental = read.csv("day.csv", row.names = 1)
head(Bike_Rental)

#Lets find out NULL or missing values

sapply(Bike_Rental, function(x) sum(is.null(x))) #to find NULL values column wise
sapply(Bike_Rental, function(x) sum(is.na(x)))  #to find NA values column wise



######################################Outliers###################################
#find outliers in data
boxplot(select(Bike_Rental,atemp,temp,windspeed), horizontal = TRUE)

boxplot(select(Bike_Rental,casual,registered), horizontal = TRUE)

#So We are haivng oulier in  windspeed and casual features

###################Removing Outlier
# 1. Casual
Casual_outliers <- boxplot(Bike_Rental$casual, plot=FALSE)$out
Bike_Rental <- Bike_Rental[-which(Bike_Rental$casual %in% Casual_outliers),]

# 2. windspeed
windspeed_outliers <- boxplot(Bike_Rental$windspeed, plot=FALSE)$out
Bike_Rental <- Bike_Rental[-which(Bike_Rental$windspeed %in% windspeed_outliers),]



#Verify if Outliers are removed
boxplot(select(Bike_Rental,atemp,temp,hum,windspeed), horizontal = TRUE) #no outliers
boxplot(select(Bike_Rental,casual,registered), horizontal = TRUE) #no outliers 


#To get the no of rows of a dataset
nrow(Bike_Rental)


#no missing value of null are found 

#########################################Correlations##########################################
cor(Bike_Rental[,-1])

#Lets drop not required variables
Bike_Rental = subset(Bike_Rental, select = -c(temp,casual,registered,holiday,weekday,dteday))

#Verify 
head(Bike_Rental) #so columns are removed


#datatype conversion
Bike_Rental$season = as.factor(Bike_Rental$season)
Bike_Rental$yr = as.factor(Bike_Rental$yr)
Bike_Rental$mnth = as.factor(Bike_Rental$mnth)
Bike_Rental$workingday = as.factor(Bike_Rental$workingday)


# Train - Test Data
set.seed(42)
train_index = sample(1:nrow(Bike_Rental), 0.8 * nrow(Bike_Rental))
train = Bike_Rental[train_index,]
test = Bike_Rental[-train_index,]

#Model Development
#Random Forest
rf_model = randomForest(cnt~., data = train, ntree = 1000,set.seed(42))
#Predict the test cases
rf_predictions = predict(rf_model, test[,-length(Bike_Rental)])

#calculate MAPE
actual = test[,length(Bike_Rental)]
pred = rf_predictions
Error = mean(abs((actual - pred)/actual)) * 100
Accuracy = 100 - Error
Accuracy

#Accuracy = 87.57

#Linear regression model
lm_model = lm(cnt ~., data = train)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test[,-length(Bike_Rental)])


#calculate MAPE
actual = test[,length(Bike_Rental)]
pred = predictions_LR
Error = mean(abs((actual - pred)/actual)) * 100
Accuracy = 100 - Error
Accuracy

# Accuracy = 84.09

#So we are having 84.09 Accuracy in Linear regression model whereas we are having 87.57 Accuracy in 
#random Forest model.

#Hence we are choosing randomForest model.

