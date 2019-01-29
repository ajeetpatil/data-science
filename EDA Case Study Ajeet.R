#Loading all the libraries 

library(lubridate)
library(ggplot2)

#Reading the data

loan <- read.csv(file="loan.csv",stringsAsFactors=F)
View(loan)
#DATA CLEANING
 ##Loan contains NA values as well as 0 so we need to remove all NA values and more than 70 per columns contain 0 so we need to remove these.

loan <- loan[,!apply(loan , 2 , function(x)all(is.na(x)))]
loan <- loan[, -which(colMeans(loan == 0 | is.na(loan)) > 0.7)]
View(loan)


#we need to remove all the columns which contains duplicate values.

loan <- loan[, !apply(loan , 2 , function(x)length(unique(x)) == 1)]


#we need to remove url column as we don't need that.

loan$url <- NULL


#we can see some decimal values in the follwing columns so we need to round it off.
 
 ## funded_amnt_inv column
loan$funded_amnt_inv <- round(loan$funded_amnt_inv)

 ##installment column
loan$installment <- round(loan$installment)

 ##annual_inc column
loan$annual_inc <- round(loan$annual_inc)

 ##dti column
loan$dti <- round(loan$dti)

 ##total_pymnt column
loan$total_pymnt <- round(loan$total_pymnt)

 ##total_pymnt_inv column
loan$total_pymnt_inv <- round(loan$total_pymnt_inv)

 ##total_rec_prncp column
loan$total_rec_prncp <- round(loan$total_rec_prncp)

 ##total_rec_int column
loan$total_rec_int <- round(loan$total_rec_int)

 ##last_pymnt_amnt column
loan$last_pymnt_amnt <- round(loan$last_pymnt_amnt)

#we can see some percentage symbol in the column int_rate and revol_util so we need to remove those.

loan$int_rate <- as.character(loan$int_rate)
loan$int_rate <-as.numeric(substr(loan$int_rate, 0, nchar(loan$int_rate) - 1))

loan$revol_util <- as.character(loan$revol_util)
loan$revol_util <-as.numeric(substr(loan$revol_util, 0, nchar(loan$revol_util) - 1))

#we need to remove all the NA values from loan file.

for (i in 1:ncol(loan)) 
  {
  loan[is.na(loan[, i]), i] <- 0
  }

#column term is in months so we need to remove that.
loan$term <- substr(loan$term, 2, 3)

###
loan$issue_d <- paste("01-",loan$issue_d,sep="")

loan$issue_d <- as.Date(loan$issue_d,"%d-%B-%y")

loan$issue_month <- as.factor(format(loan$issue_d,"%B"))

## Month on month trend of loans provided

library(scales) # to be able to use percent and other terms

ggplot(loan,aes(issue_month)) + geom_bar(fill="blue") + scale_x_discrete(limits = month.name) +
theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

## Categorization of loans based on the prupose 

ggplot(loan, aes(factor(purpose))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue") + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
 scale_y_continuous(labels=percent) + labs(title = "Loan Purpose", y = "Percent", x = "Purpose") +
scale_x_discrete() +  theme(axis.text.x=element_text(angle=90,hjust=1))

## Top 4 loan purpose categories based on the graph, greater than 5% (excluding Other) : 
# debt_consolidation
# credit_card
# home_improvement
# major_purchase


## default rate for these categories, 
# the customers labelled as 'charged-off' are the 'defaulters'

loan_subset <- subset(loan,loan$purpose %in% c("credit_card","debt_consolidation","home_improvement","major_purchase"))

ggplot(loan_subset, aes(factor(loan_status))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue") + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels=percent) + labs(title = "Loan Status", y = "Percent", x = "Status") +
  scale_x_discrete() +  theme(axis.text.x=element_text(angle=90,hjust=1)) 

# 13.4 % total defaulters in 4 categories

# defaulters (charged_off) % for each purpose 

ggplot(loan_subset, aes(factor(loan_status))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue") + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.1) +
  scale_y_continuous(labels=percent) + labs(title = "Default %", y = "Percent", x = "Status") +
  scale_x_discrete() +  theme(axis.text.x=element_text(angle=90,hjust=1)) +  facet_wrap(purpose ~ .)
  
## as per understanding,identified lifestyle and behavioral factors that can cause defaults
# grade (credit history)
# interest rate
# homeownership

loan_subset2 <- subset(loan, loan$loan_status %in% ("Charged Off")) # creating a subset only for defaulters

# analysing the top reason for borrowing loan among defaulters 

ggplot(loan_subset2, aes(factor(loan_status))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue") + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.1) +
  scale_y_continuous(labels=percent) + labs(title = "Default %", y = "Percent", x = "Status") +
  scale_x_discrete() +  theme(axis.text.x=element_text(angle=90,hjust=1)) +  facet_wrap(purpose ~ .)

# 1 potential indicator is that borrowers taking loan for Debt_Consolidation could be an indicator
# graph to show grade distribution 

# this one is for all loan statuses
ggplot(loan_subset, aes(factor(grade))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue") + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.1) +
  scale_y_continuous(labels=percent) + labs(title = "Grade distribution", y = "Percent", x = "Grade") +
  scale_x_discrete() +  theme(axis.text.x=element_text(angle=90,hjust=1)) +  facet_wrap(purpose ~ .)

# this one is for all loan statuses = Charged Off
ggplot(loan_subset2, aes(factor(grade))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue") + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.1) +
  scale_y_continuous(labels=percent) + labs(title = "Grade distribution", y = "Percent", x = "Grade") +
  scale_x_discrete() +  theme(axis.text.x=element_text(angle=90,hjust=1)) +  facet_wrap(purpose ~ .)

# we see that major reason for loan is Debt_Consolidation

#interest rate distribution

# graph to show average interest rate for each category(purpose) of loan

mean(loan_subset$int_rate)

library(plyr)

avg_int <- ddply(loan_subset, .(purpose), summarize,  avg_interest_rate=round(mean(int_rate),2))

ggplot(avg_int,aes(factor(avg_interest_rate))) + geom_bar() + labs(title = "Average interest rate", y = "Percent", x = "Purpose") + facet_wrap(purpose ~ .)

# graph to show home ownership distribution

ggplot(loan_subset, aes(factor(home_ownership))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue") + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.1) +
  scale_y_continuous(labels=percent) + labs(title = "Home ownership distribution", y = "Percent", x = "Home ownership") +
  scale_x_discrete() +  theme(axis.text.x=element_text(angle=90,hjust=1)) +  facet_wrap(purpose ~ .)

ggplot(loan_subset2, aes(factor(home_ownership))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),fill="blue") + 
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = 0.1) +
  scale_y_continuous(labels=percent) + labs(title = "Home ownership distribution", y = "Percent", x = "Home ownership") +
  scale_x_discrete() +  theme(axis.text.x=element_text(angle=90,hjust=1)) +  facet_wrap(purpose ~ .)









