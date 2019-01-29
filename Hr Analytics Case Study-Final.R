#We are working for company called XYZ which have 4000 employees.
#But every year, around 15% of its employees leave the company and need to be replaced with the talent pool available in the job market.
#This percentage of attrition impacts the company negatively because of the following reasons:
  
  ##The former employees' projects get delayed, which makes it difficult to meet timelines, resulting in a reputation loss among consumers and partners
  ##A sizeable department has to be maintained, for the purposes of recruiting new talent
  ##More often than not, the new employees have to be trained for the job and/or given time to acclimatise themselves to the company


#There are total of 5 files:
  #1)employee_survey_data.csv: This file contain information about how an employees feels about his/her job.
  #2)general_data.csv: This is the master file which contain other information about an employee.
  #3)manager_survey_data.csv: This file contains information about the performance of an employee under manager.
  #4)in_time.csv : This file contain reporting information of an employee.
  #5)out_time.csv: This file contain information about what time an employee leaves the office.

#Goal of the case study
  ##The main goal of this case study is to understand what changes the managemnet should make to their workplace,
  #in order to get most of their employees to stay.
  ## we have to use logistic regression to achieve the desired goal.
## To also indentify the most pressing or obvious reasons so that HR can take immediate action

#Loading all these required libraries libraries-
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(gridExtra)
library(MASS)
library(car)
library(e1071)
library(caret)
library(caTools)
library(cowplot)
library(ggplot2)
library(ROCR)
library(GGally)

#Reading all the 5 csv files-

Employee_Survey<-read.csv("employee_survey_data.csv",stringsAsFactors = F)
Manager_Survey<-read.csv("manager_survey_data.csv",stringsAsFactors = F)
General_Data<-read.csv("general_data.csv",stringsAsFactors = F)
In_Time <- read.csv("in_time.csv",stringsAsFactors = F)
Out_Time <- read.csv("out_time.csv",stringsAsFactors = F)

colnames(In_Time) [1]  <- "EmployeeID"
colnames(Out_Time) [1] <- "EmployeeID"

#view all the 5 files
View(Employee_Survey)
View(Manager_Survey)
View(General_Data)
View(In_Time)
View(Out_Time)


#Checking the structure of all 5 files-
str(Employee_Survey)  #4410 obs. of  4 variables
str(Manager_Survey)   #4410 obs. of  3 variables
str(General_Data)     #4410 obs. of  24 variables
str(In_Time)          #4410 obs. of  262 variables
str(Out_Time)         #4410 obs. of  262 variables
  ##total number of obervations are same for all 5 files =4410

# We have to check duplicate values in all 5 files
sum(duplicated(Employee_Survey$EmployeeID)) 
sum(duplicated(Manager_Survey$EmployeeID))
sum(duplicated(General_Data$EmployeeID))  
sum(duplicated(In_Time$EmployeeID))        
sum(duplicated(Out_Time$EmployeeID))
  ##no duplicate values found


#We have to check for all tha missing values in all 5 files
sum(is.na(Employee_Survey))  # this file contains 83 NA values
sum(is.na(Manager_Survey))   #this file have 0 NA values
sum(is.na(General_Data))     #this file have 28 NA values
sum(is.na(In_Time))         # this file contains 109080 NA values
sum(is.na(Out_Time))        #this file contain 109080 NA values
   ## In_Time and Out_Time contain equal number of NA values 
## Except for In_Time and Out_Time, we need to treat all other NA values.

# we have to calculate the average working hours of all the employees with the help of in_time and out_time

In_Time[,-1] <-sapply(In_Time[,-1], function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
In_Time<-as.data.frame(In_Time)

Out_Time[,-1] <- sapply(Out_Time[,-1], function(x) as.POSIXlt(x, origin="1970-01-01","%y-%m-%d %H:%M:%S"))
Out_Time<-as.data.frame(Out_Time)


   
  # we have to calculate the difference by subtracting out time - in time
    Difference_time<-Out_Time[,-1]-In_Time[,-1]
    View(Difference_time)
    # Adding the column - EmployeeID back to In_Time and then to Difference Time  
  in_time_employee <- In_Time[1]  
  Difference_time <- cbind(in_time_employee,Difference_time)
  
  #As we can see Na values in Difference_time , so we have to remove all the NA values
  Difference_time <- Difference_time[,colSums(is.na(Difference_time))<nrow(Difference_time)]
    View(Difference_time)
    
    Difference_time<-sapply(Difference_time,function(x) as.numeric(x))
    Difference_time<-as.data.frame(Difference_time)
    Difference_time$Avg_time<-apply(Difference_time,1,mean,na.rm=TRUE)
    
    Avg_time<-cbind(Difference_time$EmployeeID,Difference_time$Avg_time)
    Avg_time<-as.data.frame(Avg_time)
    
    #Renaming the columns
    colnames(Avg_time) <- c("EmployeeID", "Avg_working_time") 
    Avg_time$Avg_working_time <- round(Avg_time$Avg_working_time, 2)
    
    ## Collate the data together in one single file
    ## Before collating, making sure that Emp ID is the key on which collation/combining can be done
    length(unique(tolower(Employee_Survey$EmployeeID)))
    length(unique(tolower(Manager_Survey$EmployeeID)))
    length(unique(tolower(General_Data$EmployeeID)))
    length(unique(tolower(Avg_time$EmployeeID)))
    
    
    ## Identical data (customer ID) across data sets
    setdiff(Employee_Survey$EmployeeID, Manager_Survey$EmployeeID)
    setdiff(Employee_Survey$EmployeeID, General_Data$EmployeeID)
    setdiff(Employee_Survey$EmployeeID, Avg_time$EmployeeID)
    
    
## MErge Data into single file
    AllEmployee <- merge(Employee_Survey,Manager_Survey, by="EmployeeID", all = F)
    AllEmployee <- merge(AllEmployee,General_Data,by="EmployeeID",all=F)
    AllEmployee <- merge(AllEmployee,Avg_time,by="EmployeeID",all=F) 
    
    View(AllEmployee)
    
    ## Data Preparation and EDA
    
    str(AllEmployee) 
    ## continuous variables, categorical variables

    ## For Employee_survey data, 83NA values turn out to be 83/4410 ~1.8% so these can be removed.
    ## Similarly for General_Data, 28 values are NA, making it ~0.6%
    
    #Remove NA values from data frame
    AllEmployee <- na.omit(AllEmployee)
    
    
    
# Outlier Treatment
# Boxplots for numeric variables
  ggplot(AllEmployee, aes(DistanceFromHome))+ geom_histogram(binwidth = 10)
  
  ggplot(AllEmployee, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)
  
  ggplot(AllEmployee, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)
  
  ggplot(AllEmployee, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)
  
  ggplot(AllEmployee, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)
  
  ggplot(AllEmployee, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)
  
  ggplot(AllEmployee, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)
  
  ggplot(AllEmployee, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)
  
  ## Few Outliers in MonthlyIncome,TotalWorkingYears,YearsAtCompany,YearsSinceLastPromotion & YearsWithCurrManager
  
  ## Factors causing attrition in the company
  # 1. Overtime should be limited to reduce chances for an employee to leave the company.
  # 2. Before hiring an employee check the reason for leaving their previous company.
  # 3. Hire more experienced person.
  # 4. If an employee have balance in their job and personal life they are lesslikely to switch jobs often.
  # 5. If an employee have more experience and the companies worked is less.
  # 6. Manager of an employee should be understanding the exact problem an employee and try to resolve issues.
  # 7. If an employee is performing well and not getting promotions.
  
  # Checking impact of variables on attrition
  
  plot_theme<- theme(panel.background = element_rect(fill = "lightblue",
                                                     colour = "lightblue",
                                                     size = 0.5, linetype = "solid") , 
                     legend.position="none")
  
  plot_grid(ggplot(AllEmployee, aes(x=factor(EnvironmentSatisfaction),fill=factor(Attrition)))+ geom_bar() +plot_theme, 
            ggplot(AllEmployee, aes(x=factor(JobSatisfaction),fill=factor(Attrition)))+ geom_bar()+plot_theme,
            ggplot(AllEmployee, aes(x=factor(WorkLifeBalance),fill=factor(Attrition)))+ geom_bar()+plot_theme,
            ggplot(AllEmployee, aes(x=factor(JobInvolvement),fill=factor(Attrition)))+ geom_bar()+plot_theme,
            align = "h")
  
  
  plot_grid(ggplot(AllEmployee, aes(x=factor(PerformanceRating),fill=factor(Attrition)))+ geom_bar() +plot_theme, 
            ggplot(AllEmployee, aes(x=factor(BusinessTravel),fill=factor(Attrition)))+ geom_bar()+plot_theme,
            ggplot(AllEmployee, aes(x=factor(Department),fill=factor(Attrition)))+ geom_bar()+plot_theme,
            ggplot(AllEmployee, aes(x=factor(Education),fill=factor(Attrition)))+ geom_bar()+plot_theme,
            align = "h")
  
  plot_grid(ggplot(AllEmployee, aes(x=factor(EducationField),fill=factor(Attrition)))+ geom_bar() +plot_theme, 
            ggplot(AllEmployee, aes(x=factor(Gender),fill=factor(Attrition)))+ geom_bar()+plot_theme,
            ggplot(AllEmployee, aes(x=factor(JobRole),fill=factor(Attrition)))+ geom_bar()+plot_theme,
            ggplot(AllEmployee, aes(x=factor(MaritalStatus),fill=factor(Attrition)))+ geom_bar()+plot_theme,
            align = "h")
  
  
  # Correlation between numeric variables
  
  ggpairs(AllEmployee[, c("MonthlyIncome", "TotalWorkingYears","PercentSalaryHike")])
  
  ggpairs(AllEmployee[, c("MonthlyIncome", "TotalWorkingYears","Age")])
  
  # TotalWorkingYears and Age are highly correlated (corr 0.68)
  
  # creating new variable to check whether employee is doing overtime or not
  
  AllEmployee$overtime <- ifelse(AllEmployee$Avg_working_time > 8,"Yes","No")
  
  AllEmployee_overtime     <- AllEmployee %>% group_by(Attrition, overtime ) %>% summarise(EmployeeCount = n())
  ggplot(AllEmployee_overtime,aes(overtime,y=EmployeeCount,fill=overtime))+
    geom_bar(stat="identity") + facet_grid(~Attrition) + geom_text(aes(label=EmployeeCount),vjust = 2)
  
  # Conclusion 1 : It shows that to a certain extent, Employees working overtime tend to leave the company
  
  # Normalising continuous features
  AllEmployee$MonthlyIncome<- scale(AllEmployee$MonthlyIncome) 
  AllEmployee$TotalWorkingYears<- scale(AllEmployee$TotalWorkingYears) 
  AllEmployee$Age<- scale(AllEmployee$Age) 
  AllEmployee$DistanceFromHome<- scale(AllEmployee$DistanceFromHome) 
  
 # AllEmployee$Education<- scale(AllEmployee$Education) ## these are not scaled since these have levels, hence
  # more like categorical variables
# AllEmployee$JobLevel<- scale(AllEmployee$JobLevel) 
  AllEmployee$NumCompaniesWorked<- scale(AllEmployee$NumCompaniesWorked) 
 # AllEmployee$StockOptionLevel<- scale(AllEmployee$StockOptionLevel) 

  AllEmployee$PercentSalaryHike<- scale(AllEmployee$PercentSalaryHike) 
  AllEmployee$TotalWorkingYears<- scale(AllEmployee$TotalWorkingYears) 
  AllEmployee$TrainingTimesLastYear<- scale(AllEmployee$TrainingTimesLastYear) 
  AllEmployee$YearsAtCompany<- scale(AllEmployee$YearsAtCompany) 
  AllEmployee$YearsSinceLastPromotion<- scale(AllEmployee$YearsSinceLastPromotion) 
  AllEmployee$YearsWithCurrManager<- scale(AllEmployee$YearsWithCurrManager) 
  AllEmployee$Avg_working_time<- scale(AllEmployee$Avg_working_time) 
  
  # converting target variable Attrition from Yes/No character to factorwith levels 0/1 
  AllEmployee$Attrition<- ifelse(AllEmployee$Attrition=="Yes",1,0)
  
  # Checking attrition rate of employees
  
  Att_percent <- sum(AllEmployee$Attrition)/nrow(AllEmployee)
  #Attrition rate is 16.2 % 
  
  
  # Treating the Gender column which has only M & F #Newly added
  AllEmployee_F$Gender<-ifelse(AllEmployee_F$Gender=="Male",1,0)
  
  
  # creating a dataframe of categorical features
  
  AllEmployee_Categ<- AllEmployee[,c(2,3,4,5,6,9,10,12,13,15,16,17,18,24)]
  
  AllEmployee_Categ_2<- data.frame(sapply(AllEmployee_Categ, function(x) factor(x)))
  str(AllEmployee_Categ_2)
  
  # creating dummy variables for factor attributes
  dummies<- data.frame(sapply(AllEmployee_Categ_2, 
                              function(x) data.frame(model.matrix(~x-1,data =AllEmployee_Categ_2))))
  
  #Removing unwanted variables
  AllEmployee_final$StandardHours <- NULL #Removing the column since it is common 
  AllEmployee_final$Over18 <- NULL #Removing the column since value is same
  
  # Final dataset
  AllEmployee_final<- cbind(AllEmployee[,-c(2,3,4,5,6,9,10,12,13,15,16,17,18,24)],dummies)
  View(AllEmployee_final) #4300 obs. of  72 variables
  
  
  
  ########################################################################
  # splitting the data between train and test
  set.seed(100)
  
  #setting training data
  TrainIndices = sample(1:nrow(AllEmployee_final), 0.7*nrow(AllEmployee_final))
  #Getting the indices of the training data and storing those indices to PractiseDataSet
  TrainSet = AllEmployee_final[TrainIndices,]
  View(TrainSet)
  #Creating the Test Data Set
  TestSet = AllEmployee_final[-TrainIndices,]
  View(TestSet)
  
  
  ########################################################################
  # Logistic Regression: 
  
  #Initial model
  model_1 = glm(Attrition ~ ., data = TrainSet, family = "binomial")
  
  summary(model_1) #AIC 2140.3 
  
  ## We see many columns with NA values as well in summary of model_1
  ## Hence we proceed with StepAIC
  
  # Stepwise selection
  library("MASS")
  model_2<- stepAIC(model_1, direction="both")
  
  summary(model_2) #AIC 2103.8 
  
  # Removing multicollinearity through VIF check
  library(car)
  vif(model_2)
  
  #Removing variables with high vif: YearsAtCompany
  model_3 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   Avg_working_time + EnvironmentSatisfaction.x1 + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + JobSatisfaction.x1 + JobSatisfaction.x2 + 
                   JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                   JobInvolvement.x3 + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                   Department.xHuman.Resources + EducationField.xLife.Sciences + 
                   JobLevel.x2 + JobRole.xHuman.Resources + JobRole.xLaboratory.Technician + 
                   JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xDivorced + MaritalStatus.xMarried + StockOptionLevel.x0 + 
                   JobLevel.x5, family = "binomial", data = TrainSet)
  
  summary(model_3) #AIC 2218.9 Null Dev 2681 Residual 2152
  
  vif(model_3) # Gives almost all variables except TotalWorkingYears have value <2, 
  #so next iteration will consider only p-value
  #Cannot remove TotalWorkingYears cos it has highly significant p-value
  
  #next we remove JobRole as per p-value
  
  model_4 <- glm(formula = Attrition ~ Age + MonthlyIncome + 
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   Avg_working_time + EnvironmentSatisfaction.x1 + EnvironmentSatisfaction.x2 + 
                   EnvironmentSatisfaction.x3 + JobSatisfaction.x1 + JobSatisfaction.x2 + 
                   JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                   JobInvolvement.x3 + BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                   Department.xHuman.Resources + EducationField.xLife.Sciences + 
                   JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xDivorced + MaritalStatus.xMarried + StockOptionLevel.x0, 
                 family = "binomial", data = TrainSet)
  
  summary(model_4) #AIC 2226.1 Null DEv..2681 Residual Dev 2170
  
  ## NExt, we remove MonthlyIncome, EnvironmentSatisfaction.X2 & .X3, JobInvolvement.X3
  
  model_5 <- glm(formula = Attrition ~ Age +  
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   Avg_working_time + EnvironmentSatisfaction.x1 + JobSatisfaction.x1 + 
                   JobSatisfaction.x2 + 
                   JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                   BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                   Department.xHuman.Resources + EducationField.xLife.Sciences + 
                   JobRole.xResearch.Director + 
                   JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                   MaritalStatus.xDivorced + MaritalStatus.xMarried + StockOptionLevel.x0, 
                 family = "binomial", data = TrainSet)
  
  summary(model_5) #AIC 2226.3 Null Dev 2681 ResDev: 2178
  
  ## Studying the p values, we next remove:
  ## Avg_Working_Time, EduFieldLifeScience, JobRoleRschScientist
  
  model_6 <- glm(formula = Attrition ~ Age +  
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction.x1 + JobSatisfaction.x1 + 
                   JobSatisfaction.x2 + 
                   JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                   BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                   Department.xHuman.Resources +  
                   JobRole.xResearch.Director + 
                   JobRole.xSales.Executive + 
                   MaritalStatus.xDivorced + MaritalStatus.xMarried + StockOptionLevel.x0, 
                 family = "binomial", data = TrainSet)
  
  summary(model_6) #AIC 2234.3 NullDev:2681 ResDev: 2192
  
  # Next we notice that these variables have high p-value, hence removed:
  # JobRole.xResearch.Director, JobRole.xSales.Executive
  
  model_7 <- glm(formula = Attrition ~ Age +  
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction.x1 + JobSatisfaction.x1 + 
                   JobSatisfaction.x2 + 
                   JobSatisfaction.x3 + WorkLifeBalance.x1 + WorkLifeBalance.x3 + 
                   BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                   Department.xHuman.Resources +  
                   MaritalStatus.xDivorced + MaritalStatus.xMarried + StockOptionLevel.x0, 
                 family = "binomial", data = TrainSet)
  
  summary(model_7) #AIC 2240, NullDev 2681 ResDual: 2202  
  
  ## NExt iteration by removing: StockOptionLevel, JobSatisfaction.X2, JobSatisfaction.X3
  ## WorkLifeBalance.X3
  
  
  model_8 <- glm(formula = Attrition ~ Age +  
                   NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction.x1 + JobSatisfaction.x1 + 
                   WorkLifeBalance.x1 +  
                   BusinessTravel.xNon.Travel + BusinessTravel.xTravel_Frequently + 
                   Department.xHuman.Resources +  
                   MaritalStatus.xDivorced + MaritalStatus.xMarried, 
                 family = "binomial", data = TrainSet)
  
  summary(model_8) # AIC 2258.4 NullDev: 2681 ResDev:2228.4
  
#Further analysis provides that Business.NonTravel & TrainingTimes has higher value, so removed

  model_9 <- glm(formula = Attrition ~ Age +  
                   NumCompaniesWorked + TotalWorkingYears +  
                   YearsSinceLastPromotion + YearsWithCurrManager + 
                   EnvironmentSatisfaction.x1 + JobSatisfaction.x1 + 
                   WorkLifeBalance.x1 +  
                   BusinessTravel.xTravel_Frequently + 
                   Department.xHuman.Resources +  
                   MaritalStatus.xDivorced + MaritalStatus.xMarried, 
                 family = "binomial", data = TrainSet) 
  
  summary(model_9) #AIC 2280.9 NullDev: 2681 ResDev: 2254.9
  #We stillkeeping age though it has significant p-value compared to other variables
  
  ########################################################################
  
  # so with 13 variables we consider this as Final model
  
    model_Final <- model_9
  ########################################################################
  
  ### Model Evaluation
  
  ### Test Data ####
  View(TestSet)
  #Viewing the data to ascertain the position/column of Attrition (col 3)
  
  #Here trying to get predictions for TestSet based on final model

    Test_Pred = predict(model_Final, type = "response", newdata = TestSet[,-3])
  
    Predict_1 <- predict(model_35,test[,-c(1,20)])
    
      summary(Test_Pred)
    
    TestSet$NewAttrition <- Test_Pred
    
    # Trying with probability cutoff of 50%.
    
    Test_Pred_Attr <- factor(ifelse(Test_Pred >= 0.50, "Yes", "No"))
    Test_Actual_Attr <- factor(ifelse(TestSet$Attrition==1,"Yes","No"))
    
    
    table(Test_Actual_Attr,Test_Pred_Attr)
    # Sensitivity is too low with cut-off of 50%
    
    # Checking with cut-off 40%
    
    Test_Pred_Attr <- factor(ifelse(Test_Pred >= 0.40, "Yes", "No"))
  
    Test_Conf <- confusionMatrix(Test_Pred_Attr, Test_Actual_Attr, positive = "Yes")
    Test_Conf

    Test_Pred_Attr <- factor(ifelse(Test_Pred >= 0.30, "Yes", "No"))
    #########################################################################################
    
## so need to iterate to find the best cut-off
    
    
    perform_fn <- function(cutoff) 
    {
      predicted_attr <- factor(ifelse(Test_Pred >= cutoff, "Yes", "No"))
      conf <- confusionMatrix(predicted_attr, Test_Actual_Attr, positive = "Yes")
      acc <- conf$overall[1]
      sens <- conf$byClass[1]
      spec <- conf$byClass[2]
      out <- t(as.matrix(c(sens, spec, acc))) 
      colnames(out) <- c("sensitivity", "specificity", "accuracy")
      return(out)
    }
    
    # Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.
    
    # Summary of test probability
    
    summary(Test_Pred)
    
    s = seq(.01,.80,length=100)
    
    OUT = matrix(0,100,3)
    
    
    for(i in 1:100)
    {
      OUT[i,] = perform_fn(s[i])
    } 
    
    
    plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
    axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
    axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
    lines(s,OUT[,2],col="darkgreen",lwd=2)
    lines(s,OUT[,3],col=4,lwd=2)
    box()
    legend(0,.50,col=c(2,"darkgreen",4,"darkred"),
           lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
    
    
    cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
    cutoff
    
    # So we get a cutoff value of 0.1456566 for final model, rounding up to 0.15
    
    Test_Pred_Attr <- factor(ifelse(Test_Pred >= 0.15, "Yes", "No"))
    
    Final_Conf <- confusionMatrix(Test_Pred_Attr, Test_Actual_Attr, positive = "Yes")
    Final_Conf
    
    ##
    acc <- Final_Conf$overall[1]
    
    sens <- Final_Conf$byClass[1]
    
    spec <- Final_Conf$byClass[2]
    
    acc
    
    sens
    
    spec
    
    View(TestSet)
    ##################################################################################################
    ### KS -statistic - Test Data ######
    
    Test_Pred_Attr <- ifelse(Test_Pred_Attr=="Yes",1,0)
    Test_Actual_Attr <- ifelse(Test_Actual_Attr=="Yes",1,0)
    
    
    library(ROCR)
    #on testing  data
    pred_object_test<- prediction(Test_Pred_Attr, Test_Actual_Attr)
    
   ### 
    performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
    
    ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
      (attr(performance_measures_test, "x.values")[[1]])
    
    max(ks_table_test)
    
    
    ####################################################################
    # Lift & Gain Chart 
    
    # plotting the lift chart
    
    # Loading dplyr package 
    require(dplyr)
    library(dplyr)
    
    lift <- function(labels , predicted_prob,groups=10) {
      
      if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
      if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
      helper = data.frame(cbind(labels , predicted_prob))
      helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
      gaintable = helper %>% group_by(bucket)  %>%
        summarise_at(vars(labels ), funs(total = n(),
                                         totalresp=sum(., na.rm = TRUE))) %>%
        
        mutate(Cumresp = cumsum(totalresp),
               Gain=Cumresp/sum(totalresp)*100,
               Cumlift=Gain/(bucket*(100/groups))) 
      return(gaintable)
    }
    
    Attr_decile = lift(Test_Actual_Attr, Test_Pred, groups = 10)
    Attr_decile
    plot_grid(ggplot(Attr_decile,aes(x=Attr_decile$bucket,y=Attr_decile$Gain, color=""))+geom_line()+geom_point(),
              ggplot(Attr_decile,aes(x=Attr_decile$bucket,y=Attr_decile$Cumlift))+geom_line()+geom_point(), 
              align = "h",ncol = 1)
    # GRaph for KS statistic & Gain
    
    
    
    
    
    
    
    