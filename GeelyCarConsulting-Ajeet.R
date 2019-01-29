
#Install required packages 
install.packages('MASS')
install.packages('car')
install.packages('tidyr')
install.packages('stringr')
library(MASS)
library(car)
library(tidyr)
library(stringr)

#setting up working directory

getwd()
setwd("~/Personal/Data Science/Linear Regression Case Study")
getwd()

#Reading the Cars Data file provided
CarData <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)
str(CarData)

#Define new variable to derive only the Car Company Name

plot(CarData$carlength, CarData$price)

CarData$CarName <- gsub(' ','//',CarData$CarName)
#separate(CarData,CarName,CompanyName,'//')
CarData$CarName <- word(CarData$CarName, sep = fixed("//"))
#replacing space with separator to derive only Company name
  

    # setting the seed so that we get same results everytime
  set.seed(100)  

#setting training data
  PractiseDataSetIndices = sample(1:nrow(CarData), 0.7*nrow(CarData))
#Getting the indices of the training data and storing those indices to PractiseDataSet
  PractiseDataSet = CarData[PractiseDataSetIndices,]
  View(PractiseDataSet)
#Creating the Practise Data Set
  TestDataSet = CarData[-PractiseDataSetIndices,]
  
  model1 <- lm(carlength~price, data = PractiseDataSet)
  model2 <- lm(carwidth~price, data = PractiseDataSet)
  
  summary(model1)
  summary(model2)
  #model3 <- lm(price~.,data=PractiseDataSet)
  model3 <- lm(price~.,data = CarData)
  summary(model3)
  # summary of model 3 provides us with high p-values for all Car NAmes i.e 
  #independent variable CarName can be dropped.
  
  model4 <- lm(price~.-CarName,data=PractiseDataSet)
  summary(model4)
  
  # we see that 
  #converting Categorical variables into dummy variables
  