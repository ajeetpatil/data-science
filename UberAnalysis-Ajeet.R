#UBER CASE STUDY
#GUIDE LINES
#Creating appropriate no. of time slots with valid reasons is expected.
#Appropriate trends for both Day wise and hourly plots are expected when doing their Analysis. 
#Adding appropriate comments whenever required.
#All plot codes should be in R, Tableau should be only used in presentation for aesthetic purposes
#Analysis of NA Values- Appropriate code/comments are expected whenever NA values are treated.
#Quantifying and forecasting Supply-Demand Gap
#Clearly stating assumptions in both code and presentation

# DATA LOADING
setwd("~/Module 4 Exloratory Data Analysis/1. CaseStudy UBER")
uber <- read.csv("Uber Request Data.csv", stringsAsFactors = F)
str(uber)
summary(uber)

library(lubridate)
library(stringr)
library(dplyr)

# DATA CLEANING

# Check duplicate data
uber.u <- length(unique(uber$Request.id))
# All Request IDs are unique there are no duplicate rows

# Check NA values
uber.na <- is.na(uber)
#NA Values are only existing for Drop.timestamp when the status is cancelled or no cars available. 
#Choosing not to treat them.

# Check formats for date and time for inconsistency
# Convert date and time with mixed formats
# Reference:https://stackoverflow.com/questions/25463523/convert-variable-with-mixed-date-formats-to-one-format

uber$Request.timestamp <- parse_date_time(x=uber$Request.timestamp, orders = c("d/m/Y H:M:S" , "d-m-Y H:M"), locale = "eng")
uber$Drop.timestamp <- parse_date_time(x=uber$Drop.timestamp, orders = c("d/m/Y H:M:S" , "d-m-Y H:M"), locale = "eng")

# FURTHER FORMATTING
# Split timestamp columns into two different columns

uber$RDate <- date(uber$Request.timestamp)
uber$DDate <- date(uber$Drop.timestamp)
uber$RTime = format(as.POSIXct(uber$Request.timestamp,format="%H:%M:%S"),"%H:%M:%S")
uber$DTime = format(as.POSIXct(uber$Drop.timestamp,format="%H:%M:%S"),"%H:%M:%S")

# HOURLY AND DAY WISE TREND ANALYSIS 
# ASSUMPTION IS WHEN CITY IS THE PICKUP POINT THE DROP OFF IS AIRPORT AND VICE VERSA
# Extract Day of week and add it as an additional column to the main data set
uber$RDay <- weekdays(as.Date(uber$RDate))

# Extract Hours from RTime for hourly Analysis
uber$Hours = format(as.POSIXct(uber$RTime,format="%H:%M:%S"),"%H")

#Adding Timeslots column to the data set based on the hour of the day
uber$Timeslots[(uber$Hours >= "00") & (uber$Hours <= "04")]<-"AFTER MIDNIGHT"
uber$Timeslots[(uber$Hours >= "04") & (uber$Hours <= "08")]<-"EARLY MORNING"
uber$Timeslots[(uber$Hours >= "08") & (uber$Hours <= "12")]<-"MORNING"
uber$Timeslots[(uber$Hours >= "12") & (uber$Hours <= "16")]<-"AFTERNOON"
uber$Timeslots[(uber$Hours >= "16") & (uber$Hours <= "20")]<-"EVENING"
uber$Timeslots[(uber$Hours >= "20") & (uber$Hours <= "24")]<-"NIGHT"

View(uber)

# PLOT THE ANALYSIS
# Following plots show the Analysis based on each day and with status, and also based on the pickup point
# Sort based on RDay, add additional column to number the weekday
uber$Weekday[uber$RDay == 'Monday'] <- 1
uber$Weekday[uber$RDay == 'Tuesday'] <- 2
uber$Weekday[uber$RDay == 'Wednesday'] <- 3
uber$Weekday[uber$RDay == 'Thursday'] <- 4
uber$Weekday[uber$RDay == 'Friday'] <- 5
uber <- uber[order(uber$Weekday),]

# PLOTS
# Total statistics based on pickup point and corresponding status
ggplot(uber, aes(x=factor(Pickup.point), fill=factor(Status)))+geom_bar() + labs(x="Pickup.Point", y="Number of Requests", fill = "Status", title = "Uber Analysis Pickup Point vs Status Plot")
# Analysis per weekday
ggplot(uber,aes(x=factor(Status))) + geom_bar() + facet_wrap(~Weekday) + labs(x="Status", y="Number of Requests", title = "Uber Analysis Weekdays")
ggplot(uber,aes(x=factor(Status),fill=factor(Pickup.point)))+geom_bar()+facet_wrap(~RDay)+labs(x="Status", y="Number of Requests", fill = "Pickup.Point", title = "Uber Analysis Weekdays-Pickup Point")
# Analysis on a specific date
ggplot(uber,aes(x=factor(Status),fill=factor(Pickup.point)))+geom_bar()+facet_wrap(~RDate)

# Analysis per hour
ggplot(uber,aes(x=factor(Status)))+geom_bar()+ facet_wrap(~Hours) + labs(x="Status", y="Number of Requests", title = "Uber Hourly Analysis")
ggplot(uber,aes(x=factor(Status),fill=factor(Pickup.point)))+geom_bar()+facet_wrap(~Hours) + labs(x="Status", y="Number of Requests", fill = "Pickup.Point", title = "Uber Hourly Analysis - Pickup Point")

# Plots based on Pickup point and Status as well as Timeslots showing supply and demand
# Based on the status given, "Trip Completed" indicates Supply is positive and available and "No Cars Available" as well
# as "Cancelled" indicate that there is no supply. Adding additional column to represent the same in 1 or 0
# 1 is supply available, 0 as not available

uber$Analysis[uber$Status == "Trip Completed"] <- 1
uber$Analysis[uber$Status == "No Cars Available"] <- 0
uber$Analysis[uber$Status == "Cancelled"] <- 0

View(uber)

#Suppy-Demand based on pickup point
ggplot(uber,aes(x=factor(Status), fill=factor(Analysis)))+geom_bar()+ facet_wrap(~Pickup.point) + labs(x="Status", y="Number of Requests", fill = "Analysis", title = "Supply-Demand-Gap Analysis Plot - Pickup Point")

#Suppy-Demand based on the time of the day
ggplot(uber,aes(x=factor(Status), fill=factor(Analysis)) )+geom_bar()+ facet_wrap(~Timeslots) + labs(x="Status", y="Number of Requests", fill = "Analysis", title = "Supply-Demand-Gap Analysis Plot - Timeslots")

# Quantifying and forecasting Supply-Demand Gap
# Demand is total number of requests Uber has received. 
# From this data set the number of requests is 6745.Total demand is 6745.
# Supply is total number of requests that were with status "Trip Completed"
# Gap is all those requests that were either cancelled or not fulfilled because of unavailability of cars.

#SUPPLY-DEMAND GAP ANALYSIS - PICKUP POINT
supply <- uber %>% group_by(Pickup.point) %>% summarise(Analysis = sum(Analysis ==1))
colnames(supply) <- c("Pickup.Point", "Supply")
gap <- uber %>% group_by(Pickup.point) %>% summarise(Analysis = sum(Analysis ==0))
colnames(gap) <- c("Pickup.Point", "Gap")
supply.gap.ppoint <- merge(supply, gap, by="Pickup.Point")

#SUPPLY-DEMAND GAP ANALYSIS - TIMESLOTS
hsupply <- uber %>% group_by(Timeslots) %>% summarise(Analysis = sum(Analysis ==1))
colnames(hsupply) <- c("Timeslots", "Supply")
hgap <- uber %>% group_by(Timeslots) %>% summarise(Analysis = sum(Analysis ==0))
colnames(hgap) <- c("Timeslots", "Gap")
supply.gap.hours <- merge(hsupply, hgap, by="Timeslots")

#DEMAND CALCULATION PER PICKUP POINT & TIMESLOTS
#To perform addition operation of two columns they should be numeric type
#DEMAND = REQUESTS
#DEMAND = REQUESTS COMPLETED + CANCELLED + NOT AVAILABLE
supply.gap.ppoint$Supply <- as.numeric(supply.gap.ppoint$Supply)
supply.gap.ppoint$Gap <- as.numeric(supply.gap.ppoint$Gap)
supply.gap.ppoint$Demand <- supply.gap.ppoint$Supply + supply.gap.ppoint$Gap
supply.gap.ppoint
supply.gap.hours$Supply <- as.numeric(supply.gap.hours$Supply)
supply.gap.hours$Gap <- as.numeric(supply.gap.hours$Gap)
supply.gap.hours$Demand <- supply.gap.hours$Supply + supply.gap.hours$Gap
supply.gap.hours

#***********************************************************************************************

