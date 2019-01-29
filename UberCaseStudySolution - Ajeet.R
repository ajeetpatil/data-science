#setting up working directory

getwd()
setwd("~/Personal/Data Science")
getwd()

#Reading the Uber Data file provided

 UberOriginalData<- read.csv("Uber Request Data.csv", stringsAsFactors = F)
 
  str(UberOriginalData)
# summary(UberOriginalData)
 
 View(UberOriginalData)
 sum(is.na(UberOriginalData))

 # convert to NA for cancelled trip to something else
 
 install.packages("lubridate") # for time and date manipulation
 
  
 # there are lots of entries with NA value for Driver.id and Drop.Timestamp
 #'NO cars available' and cancelled trips

 
 
 #clean the request timestamp and Drop timestamp 
 # get these 2 to same format across: dd/mm/yyyy hh24:mi 24hour format
 # Remove seconds portion from data since it denotes too much of precision
 # 
 
 as.Date(UberOriginalData[2,5], format="%d/%m/%Y")
 
 parse_date_time(dt, "dmY HM")
 parse_date_time(d, c("dmY H:M:S", "d/m/Y H:M"))
 Uber2[,5] <- parse_date_time(Uber2[,5], c("dmY H:M:S", "d/m/Y H:M"))
 UberCleanedData[,5] <- parse_date_time(UberCleanedData[,5], c("dmY H:M:S", "d/m/Y H:M"))
 
 parse_date_time(UberCleanedData[,6], c("dmY H:M:S", "d/m/Y H:M"))
 
 lubridate::hour(newdt)
 lubridate::hour(newdt)
 
 UberOriginalData[,5] <- parse_date_time(UberOriginalData[,5], c("dmY H:M:S", "d/m/Y H:M"))
 UberOriginalData[,6] <- parse_date_time(UberOriginalData[,6], c("dmY H:M:S", "d/m/Y H:M"))
 
 
 
 #derive Request_Hour and Pickup_Hour from the Request timestamp and Drop timestamp
 #add a new column to netire data set
 
 UberOriginalData$ReqHour <- lubridate::hour(UberOriginalData[,5])
 UberOriginalData$DropHour <- lubridate::hour(UberOriginalData[,6])
 
 #plot the graphs - ggplot, histogram 
 install.packages("ggplot2")
 library(ggplot2)
 
 
 #weekend and weekdays trend
 #any weekly trends - any derived data for this
 
 ggplot(UberOriginalData, aes(x=Request.id)) + geom_histogram()
 
 #plot the graphs - ggplot, histogram 
 
 
 
 
#demand = trips requested - trips completed
 
 
 if {UberOriginalData$Status == 'Cancelled' or 'No Cars Available'
   then 
   UberOriginalData$ReqCount
 }
    
 
 
  
 