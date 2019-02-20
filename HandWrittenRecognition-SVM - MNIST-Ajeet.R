## To identify the hand-written digits

## Installing and Loading the Libraries
install.packages("caret")
install.packages("kernlab")
install.packages('dplyr')
install.packages("readr")
install.packages("ggplot2")
install.packages("gridExtra")

library(kernlab)
library(readr)
library(caret)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(data.table)

#Reading the input data
# Making colnames as FALSE since I want R to assign same column names to both Train and Test
# Data set

Data <- read_csv(file = "mnist_train.csv", col_names = FALSE)
Data_Test <- read_csv(file = "mnist_test.csv", col_names = FALSE)

View(Data)
View(Data_Test)

#Understanding Dimensions
dim(Data)

#Structure of the dataset
str(Data)

#printing first few rows
head(Data)

#Exploring the data
summary(Data)

#checking missing value
sapply(Data, function(x) sum(is.na(x))) # No missing or NA values
Data <- na.omit(Data)

#MAking our target dependent variable as a factor
Data$X1 <- factor(Data$X1)
Data_Test$X1 <- factor(Data_Test$X1)

# Split the data into train and test set

# Taking only 10% of data as suggested since computing take lot of time
set.seed(1)
train.indices = sample(1:nrow(Data), 0.10*nrow(Data))
train = Data[train.indices, ]
View(train)

test.indices = sample(1:nrow(Data_Test), 0.10*nrow(Data_Test))
test = Data_Test[test.indices, ]
View(test)

#Model Evaluation and Model Building

#Starting with Linear model (vanilla)
# Trying different models

Model_linear <- ksvm(X1 ~., data = train, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$X1)

#Above gives an accuracy of 91%
#Each class wise (digit wise) statistics vary a lot from 80% to 90+% specificity, sensitivity

#Using RBF Kernel
#Trying 2nd model
Model_RBF <- ksvm(X1~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

Model_RBF # to view the model to get the optimal and approximate sigma & c values to use later

#parameter : cost C = 1 
#Gaussian Radial Basis kernel function. 
#Hyperparameter : sigma =  1.66040805079759e-07 

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$X1)

#Using RBF evaluation model, the accuracy slightly shoots up to 95.8%
#The overall stats for all other classes also shoot up
#So we will use RBF Kernel algorithm

############   Hyperparameter tuning and Cross Validation #####################

trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
# Making grid of "sigma" and C values. 
# Using the sigma value suggested via the RBF model (1.66e-07)

set.seed(2)
grid <- expand.grid(.sigma=c(1.66e-07, 1.66e+07), .C=c(0.1,0.5,1,2) )


# Performing 5-fold cross validation
fit.svm_radial <- train(X1~., data=train, method="svmRadial", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

# Printing cross validation result
print(fit.svm_radial)
#The final values used for the model were sigma = 1.66e-07 and C = 2. 
#Accuracy = 95.43%

# Plotting model results
plot(fit.svm_radial)

# Validating the model results on test data
evaluate_non_linear<- predict(fit.svm_radial, test)
confusionMatrix(evaluate_non_linear, test$X1)

# Getting Accuracy = 96.2% on Test Data