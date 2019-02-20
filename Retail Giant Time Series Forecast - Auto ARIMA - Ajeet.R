
#Objective
#Subset the data from the transactional database to form the 21 Market Buckets
#To forecast the sales and the demand for the next 6 months, that would help you manage the revenue and inventory accordingly.
#Find out 2 most profitable (and consistent) segment from these 21 and forecast the sales and demand for these segments.

#Business understanding:
#"Global Mart" is an online store super giant having worldwide operations.
#The store caters to 7 different market segments and in 3 major categories.1. Technology 2. Furniture 3. Office Supplies.
#It serves customers from 3 consumer segments 1. Consumer 2. Home Office 3. Corporate
#The Sales/Operations requires to finalize the plan for the next six months by forecasting the demand and sales for the nect six months.


#Packages to install 
install.packages("forecast")
install.packages("tseries")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("graphics")
install.packages("lubridate")
install.packages("stats")


#Loading the given packages
library(forecast)
library(tseries)
library(dplyr)
library(ggplot2)
library(graphics)
library(lubridate)
library(stats)



#DATA UNDERSTANDING
#Importing the dataset.
Global_Superstore<- read.csv("Global Superstore.csv", stringsAsFactors = FALSE)

#Now we neew to check the dimesion of Global Superstore dataset.
dim(Global_Superstore)   #51291 Records present with 24 attributes.


#checking structure of Global Superstore dataset.
str(Global_Superstore)  
##As we can see that Order date and Ship date is character so we have to convert them into date.
Global_Superstore<- Global_Superstore[,-1]
Global_Superstore$Ship.Date<- parse_date_time(Global_Superstore$Ship.Date, orders = c("dmy", "mdy"))
Global_Superstore$Order.Date<- parse_date_time(Global_Superstore$Order.Date, orders = c("dmy", "mdy"))

#Checking summary of Global Superstore dataset.
summary(Global_Superstore)

#Checking for duplicate values in the dataset.
length(which(duplicated(Global_Superstore)==TRUE))  # Returns 0 means no duplicate value is present.


#Checking for missing values.
sapply(Global_Superstore, function(x) sum(is.na(x)))
max(sapply(Global_Superstore, function(x) sum(is.na(x)))) 
## only postal code contains missing values so removing postal code column
Global_Superstore$Postal.Code<-NULL

# we need to check earliest and latest order date
min(Global_Superstore$Order.Date)   #1st jan 2011
max(Global_Superstore$Order.Date)   #31st dec 2014
##we have order date range from 1st january to 31st dec 2014


# We have to derive new metric Monthly_Order to store orders month wise.
Global_Superstore$Monthly_Order <- 
  sapply(Global_Superstore$Order.Date, function(x) length(seq(from= min(Global_Superstore$Order.Date), to=x, by='month')))
range(Global_Superstore$Monthly_Order)

#DATA PREPARATION 
unique(Global_Superstore$Market)    # 7 types of market segment
#1)Africa 
#2)APAC 
#3)Canada 
#4)EMEA 
#5)EU
#6)LATAM
#7)US

unique(Global_Superstore$Segment)   # 3 types of consumer segmant
#1)Consumer 
#2)Corporate 
#3)Home Office

unique(Global_Superstore$Category)  # 3 product categories
#1)Technology
#2)Furniture
#3)Office Supplies

#Based on market and customer segment we have to create market buckets for our 7 market segments

#(1)Africa
Africa_Consumer<- subset(Global_Superstore, Global_Superstore$Segment=="Consumer" & Global_Superstore$Market=="Africa")
Africa_Corporate<- subset(Global_Superstore, Global_Superstore$Segment=="Corporate" & Global_Superstore$Market=="Africa")
Africa_Home_Office<- subset(Global_Superstore, Global_Superstore$Segment=="Home Office" & Global_Superstore$Market=="Africa")


#(2)APAC
APAC_Consumer<- subset(Global_Superstore, Global_Superstore$Segment=="Consumer" & Global_Superstore$Market=="APAC")
APAC_Corporate<- subset(Global_Superstore, Global_Superstore$Segment=="Corporate" & Global_Superstore$Market=="APAC")
APAC_Home_Office<- subset(Global_Superstore, Global_Superstore$Segment=="Home Office" & Global_Superstore$Market=="APAC")

#(3) CANADA
Canada_Consumer<- subset(Global_Superstore, Global_Superstore$Segment=="Consumer" & Global_Superstore$Market=="Canada")
Canada_Corporate<- subset(Global_Superstore, Global_Superstore$Segment=="Corporate" & Global_Superstore$Market=="Canada")
Canada_Home_Office<- subset(Global_Superstore, Global_Superstore$Segment=="Home Office" & Global_Superstore$Market=="Canada")

#(4)EMEA
EMEA_Consumer<- subset(Global_Superstore, Global_Superstore$Segment=="Consumer" & Global_Superstore$Market=="EMEA")
EMEA_Corporate<- subset(Global_Superstore, Global_Superstore$Segment=="Corporate" & Global_Superstore$Market=="EMEA")
EMEA_Home_Office<- subset(Global_Superstore, Global_Superstore$Segment=="Home Office" & Global_Superstore$Market=="EMEA")

#(5)EU
Eu_Consumer<- subset(Global_Superstore, Global_Superstore$Segment=="Consumer" & Global_Superstore$Market=="EU")
Eu_Corporate<- subset(Global_Superstore, Global_Superstore$Segment=="Corporate" & Global_Superstore$Market=="EU")
Eu_Home_Office<- subset(Global_Superstore, Global_Superstore$Segment=="Home Office" & Global_Superstore$Market=="EU")

#(6)LATAM
LATAM_Consumer<- subset(Global_Superstore, Global_Superstore$Segment=="Consumer" & Global_Superstore$Market=="LATAM")
LATAM_Corporate<- subset(Global_Superstore, Global_Superstore$Segment=="Corporate" & Global_Superstore$Market=="LATAM")
LATAM_Home_Office<- subset(Global_Superstore, Global_Superstore$Segment=="Home Office" & Global_Superstore$Market=="LATAM")

#(7)US
Us_Consumer<- subset(Global_Superstore, Global_Superstore$Segment=="Consumer" & Global_Superstore$Market=="US")
Us_Corporate<- subset(Global_Superstore, Global_Superstore$Segment=="Corporate" & Global_Superstore$Market=="US")
Us_Home_Office<- subset(Global_Superstore, Global_Superstore$Segment=="Home Office" & Global_Superstore$Market=="US")

## TO get Time Series, converting granular data into aggregated items

## Africa
TS_Afr_Consumer <-
  data.frame(aggregate(cbind(Africa_Consumer$Sales, Africa_Consumer$Profit, Africa_Consumer$Quantity), 
                       by = list(Month=Africa_Consumer$Monthly_Order), FUN = sum))
TS_Afr_Corp <-
  data.frame(aggregate(cbind(Africa_Corporate$Sales, Africa_Corporate$Profit, Africa_Corporate$Quantity), 
                       by = list(Month=Africa_Corporate$Monthly_Order), FUN = sum))
TS_Afr_HO <- 
  data.frame(aggregate(cbind(Africa_Home_Office$Sales, Africa_Home_Office$Profit, Africa_Home_Office$Quantity), 
                       by = list(Month=Africa_Home_Office$Monthly_Order), FUN = sum))
## Asia
TS_APAC_Consumer <- 
  data.frame(aggregate(cbind(APAC_Consumer$Sales, APAC_Consumer$Profit, APAC_Consumer$Quantity), 
                       by = list(Month=APAC_Consumer$Monthly_Order), FUN = sum))
TS_APAC_Corp <- 
  data.frame(aggregate(cbind(APAC_Corporate$Sales, APAC_Corporate$Profit, APAC_Corporate$Quantity), 
                       by = list(Month=APAC_Corporate$Monthly_Order), FUN = sum))
TS_APAC_HO <-
  data.frame(aggregate(cbind(APAC_Home_Office$Sales, APAC_Home_Office$Profit, APAC_Home_Office$Quantity), 
                       by = list(Month=APAC_Home_Office$Monthly_Order), FUN = sum))

## Canada
TS_Can_Consumer <-
  data.frame(aggregate(cbind(Canada_Consumer$Sales, Canada_Consumer$Profit, Canada_Consumer$Quantity), 
                       by = list(Month=Canada_Consumer$Monthly_Order), FUN = sum))
TS_Can_Corp <-
  data.frame(aggregate(cbind(Canada_Corporate$Sales, Canada_Corporate$Profit, Canada_Corporate$Quantity), 
                       by = list(Month=Canada_Corporate$Monthly_Order), FUN = sum))
TS_Can_HO <-
  data.frame(aggregate(cbind(Canada_Home_Office$Sales, Canada_Home_Office$Profit, Canada_Home_Office$Quantity), 
                       by = list(Month=Canada_Home_Office$Monthly_Order), FUN = sum))

## EMEA
TS_EMEA_Consumer <-
  data.frame(aggregate(cbind(EMEA_Consumer$Sales, EMEA_Consumer$Profit, EMEA_Consumer$Quantity), 
                       by = list(Month=EMEA_Consumer$Monthly_Order), FUN = sum))
TS_EMEA_Corp <-
  data.frame(aggregate(cbind(EMEA_Corporate$Sales, EMEA_Corporate$Profit, EMEA_Corporate$Quantity), 
                       by = list(Month=EMEA_Corporate$Monthly_Order), FUN = sum))
TS_EMEA_HO <-
  data.frame(aggregate(cbind(EMEA_Home_Office$Sales, EMEA_Home_Office$Profit, EMEA_Home_Office$Quantity), 
                       by = list(Month=EMEA_Home_Office$Monthly_Order), FUN = sum))

## EU
TS_EU_Consumer <- data.frame(aggregate(cbind(Eu_Consumer$Sales, Eu_Consumer$Profit, Eu_Consumer$Quantity), 
                                       by = list(Month=Eu_Consumer$Monthly_Order), FUN = sum))
TS_EU_Corp <- data.frame(aggregate(cbind(Eu_Corporate$Sales, Eu_Corporate$Profit, Eu_Corporate$Quantity), 
                                   by = list(Month=Eu_Corporate$Monthly_Order), FUN = sum))  
TS_EU_HO <- data.frame(aggregate(cbind(Eu_Home_Office$Sales, Eu_Home_Office$Profit, Eu_Home_Office$Quantity), 
                                 by = list(Month=Eu_Home_Office$Monthly_Order), FUN = sum))

## LATAM
TS_LAT_Consumer <- data.frame(aggregate(cbind(LATAM_Consumer$Sales, LATAM_Consumer$Profit, LATAM_Consumer$Quantity), 
                                        by = list(Month=LATAM_Consumer$Monthly_Order), FUN = sum))
TS_LAT_Corp <- data.frame(aggregate(cbind(LATAM_Corporate$Sales, LATAM_Corporate$Profit, LATAM_Corporate$Quantity), 
                                    by = list(Month=LATAM_Corporate$Monthly_Order), FUN = sum))
TS_LAT_HO <- data.frame(aggregate(cbind(LATAM_Home_Office$Sales, LATAM_Home_Office$Profit, LATAM_Home_Office$Quantity), 
                                  by = list(Month=LATAM_Home_Office$Monthly_Order), FUN = sum))

## USA
TS_US_Consumer <- data.frame(aggregate(cbind(Us_Consumer$Sales, Us_Consumer$Profit, Us_Consumer$Quantity), 
                                       by = list(Month=Us_Consumer$Monthly_Order), FUN = sum))
TS_US_Corp <- data.frame(aggregate(cbind(Us_Corporate$Sales, Us_Corporate$Profit, Us_Corporate$Quantity), 
                                   by = list(Month=Us_Corporate$Monthly_Order), FUN = sum))
TS_US_HO <- data.frame(aggregate(cbind(Us_Home_Office$Sales, Us_Home_Office$Profit, Us_Home_Office$Quantity), 
                                 by = list(Month=Us_Home_Office$Monthly_Order), FUN = sum))


## Converting the segregated segments across markets into Time series plots
## plotting the aggregated data into plots

## Segment Africa Plots


plot(TS_Afr_Consumer$Month, TS_Afr_Consumer$V1)
plot(TS_Afr_Consumer$Month, TS_Afr_Consumer$V2)
plot(TS_Afr_Consumer$Month, TS_Afr_Consumer$V3)

plot(TS_Afr_Corp$Month, TS_Afr_Corp$V1)
plot(TS_Afr_Corp$Month, TS_Afr_Corp$V2)
plot(TS_Afr_Corp$Month, TS_Afr_Corp$V3)


plot(TS_Afr_HO$Month, TS_Afr_HO$V3)
plot(TS_Afr_HO$Month, TS_Afr_HO$V2)
plot(TS_Afr_HO$Month, TS_Afr_HO$V1)

## Segment Asia Plots

plot(TS_APAC_Consumer$Month, TS_APAC_Consumer$V1)
plot(TS_APAC_Consumer$Month, TS_APAC_Consumer$V2)
plot(TS_APAC_Consumer$Month, TS_APAC_Consumer$V3)

plot(TS_APAC_Corp$Month, TS_APAC_Corp$V1)
plot(TS_APAC_Corp$Month, TS_APAC_Corp$V2)
plot(TS_APAC_Corp$Month, TS_APAC_Corp$V3)

plot(TS_APAC_HO$Month, TS_APAC_HO$V1)
plot(TS_APAC_HO$Month, TS_APAC_HO$V2)
plot(TS_APAC_HO$Month, TS_APAC_HO$V3)

## SEgment EMEA

plot(TS_EMEA_Consumer$Month, TS_EMEA_Consumer$V1)
plot(TS_EMEA_Consumer$Month, TS_EMEA_Consumer$V2)
plot(TS_EMEA_Consumer$Month, TS_EMEA_Consumer$V3)

plot(TS_EMEA_Corp$Month, TS_EMEA_Corp$V1)
plot(TS_EMEA_Corp$Month, TS_EMEA_Corp$V2)
plot(TS_EMEA_Corp$Month, TS_EMEA_Corp$V3)

plot(TS_EMEA_HO$Month, TS_EMEA_HO$V1)
plot(TS_EMEA_HO$Month, TS_EMEA_HO$V2)
plot(TS_EMEA_HO$Month, TS_EMEA_HO$V3)

## Segment LATAM

plot(TS_LAT_Corp$Month, TS_LAT_Corp$V1)
plot(TS_LAT_Corp$Month, TS_LAT_Corp$V2)
plot(TS_LAT_Corp$Month, TS_LAT_Corp$V3)

plot(TS_LAT_Consumer$Month, TS_LAT_Consumer$V1)
plot(TS_LAT_Consumer$Month, TS_LAT_Consumer$V2)
plot(TS_LAT_Consumer$Month, TS_LAT_Consumer$V3)


plot(TS_LAT_HO$Month, TS_LAT_HO$V1)
plot(TS_LAT_HO$Month, TS_LAT_HO$V2)
plot(TS_LAT_HO$Month, TS_LAT_HO$V3)

## Segment EU
plot(TS_EU_Consumer$Month, TS_EU_Consumer$V1)
plot(TS_EU_Consumer$Month, TS_EU_Consumer$V2)
plot(TS_EU_Consumer$Month, TS_EU_Consumer$V3)

plot(TS_EU_Corp$Month, TS_EU_Corp$V1)
plot(TS_EU_Corp$Month, TS_EU_Corp$V2)
plot(TS_EU_Corp$Month, TS_EU_Corp$V3)

plot(TS_EU_HO$Month, TS_EU_HO$V1)
plot(TS_EU_HO$Month, TS_EU_HO$V2)
plot(TS_EU_HO$Month, TS_EU_HO$V3)


## Segment Canada Plots
plot(TS_Can_Consumer$Month, TS_Can_Consumer$V1)
plot(TS_Can_Consumer$Month, TS_Can_Consumer$V2)
plot(TS_Can_Consumer$Month, TS_Can_Consumer$V3)

plot(TS_Can_Corp$Month, TS_Can_Corp$V1)
plot(TS_Can_Corp$Month, TS_Can_Corp$V2)
plot(TS_Can_Corp$Month, TS_Can_Corp$V3)

plot(TS_Can_HO$Month, TS_Can_HO$V1)
plot(TS_Can_HO$Month, TS_Can_HO$V2)
plot(TS_Can_HO$Month, TS_Can_HO$V3)

## sEGMENT US
plot(TS_US_Consumer$Month, TS_US_Consumer$V1)
plot(TS_US_Consumer$Month, TS_US_Consumer$V2)
plot(TS_US_Consumer$Month, TS_US_Consumer$V3)

plot(TS_US_Corp$Month, TS_US_Corp$V1)
plot(TS_US_Corp$Month, TS_US_Corp$V2)
plot(TS_US_Corp$Month, TS_US_Corp$V3)

plot(TS_US_HO$Month, TS_US_HO$V1)
plot(TS_US_HO$Month, TS_US_HO$V2)
plot(TS_US_HO$Month, TS_US_HO$V3)



## Finding the most profitable segment amongst these using Coefficient of Profit: ratio of 
## Std Deviation to Average

TS_Afr_Consumer_indata <- TS_Afr_Consumer[1:42,]

TS_Afr_Corp_indata <- TS_Afr_Corp[1:42,]

TS_Afr_HO_indata <- TS_Afr_HO[1:42,]

TS_APAC_Consumer_indata <- TS_APAC_Consumer[1:42,]

TS_APAC_Corp_indata <- TS_APAC_Corp[1:42,]

TS_APAC_HO_indata <- TS_APAC_HO[1:42,]

TS_EMEA_Consumer_indata <- TS_EMEA_Consumer[1:42,]

TS_EMEA_Corp_indata <- TS_EMEA_Corp[1:42,]

TS_EMEA_HO_indata <- TS_EMEA_HO[1:42,]

TS_LAT_Corp_indata <- TS_LAT_Corp[1:42,]

TS_LAT_Consumer_indata <- TS_LAT_Consumer[1:42,]

TS_LAT_HO_indata <- TS_LAT_HO[1:42,]

TS_EU_Consumer_indata <- TS_EU_Consumer[1:42,]

TS_EU_Corp_indata <- TS_EU_Corp[1:42,]

TS_EU_HO_indata <- TS_EU_HO[1:42,]

TS_Can_Consumer_indata <- TS_Can_Consumer[1:42,]

TS_Can_Corp_indata <- TS_Can_Corp[1:42,]

TS_Can_HO_indata <- TS_Can_HO[1:42,]

TS_US_Consumer_indata <- TS_US_Consumer[1:42,]

TS_US_Corp_indata <- TS_US_Corp[1:42,]

TS_US_HO_indata <- TS_US_HO[1:42,]

CV_Af_Consumer <- sd(TS_Afr_Consumer_indata$V2)/mean(TS_Afr_Consumer_indata$V2) #1.446
CV_Af_Corp <- sd(TS_Afr_Corp_indata$V2)/mean(TS_Afr_Corp_indata$V2) #1.685
CV_Af_HO <- sd(TS_Afr_HO_indata$V2)/mean(TS_Afr_HO_indata$V2) #2.014

CV_APAC_Cons <- sd(TS_APAC_Consumer_indata$V2)/mean(TS_APAC_Consumer_indata$V2) #0.603
CV_APAC_Corp <- sd(TS_APAC_Corp_indata$V2)/mean(TS_APAC_Corp_indata$V2) #0.741
CV_APAC_HO <- sd(TS_APAC_HO_indata$V2)/mean(TS_APAC_HO_indata$V2) #1.0615

CV_EMEA_Cons <- sd(TS_EMEA_Consumer_indata$V2)/mean(TS_EMEA_Consumer_indata$V2) #2.7499
CV_EMEA_Corp <- sd(TS_EMEA_Corp_indata$V2)/mean(TS_EMEA_Corp_indata$V2) #6.861
CV_EMEA_HO <- sd(TS_EMEA_HO_indata$V2)/mean(TS_EMEA_HO_indata$V2) #6.140

CV_CAN_Cons <- sd(TS_Can_Consumer_indata$V2)/mean(TS_Can_Consumer_indata$V2) # 1.395
CV_CAN_CorP <- sd(TS_Can_Corp_indata$V2)/mean(TS_Can_Corp_indata$V2) #NA
CV_CAN_HO <- sd(TS_Can_HO_indata$V2)/mean(TS_Can_HO_indata$V2) #NA

CV_LATAM_Cons <- sd(TS_LAT_Consumer_indata$V2)/mean(TS_LAT_Consumer_indata$V2) #0.6889
CV_LATAM_Corp <- sd(TS_LAT_Corp_indata$V2)/mean(TS_LAT_Corp_indata$V2) #0.8909
CV_LATAM_HO <- sd(TS_LAT_HO_indata$V2)/mean(TS_LAT_HO_indata$V2) #1.3599

CV_EU_HO <- sd(TS_EU_HO_indata$V2)/mean(TS_EU_HO_indata$V2) #1.128
CV_EU_Cons <- sd(TS_EU_Consumer_indata$V2)/mean(TS_EU_Consumer_indata$V2) #0.655
CV_EU_Corp <- sd(TS_EU_Corp_indata$V2)/mean(TS_EU_Corp_indata$V2) #0.6977

CV_US_Cons <- sd(TS_US_Consumer_indata$V2)/mean(TS_US_Consumer_indata$V2) #1.108
CV_US_Corp <- sd(TS_US_Corp_indata$V2)/mean(TS_US_Corp_indata$V2) #1.039
CV_US_HO <- sd(TS_US_HO_indata$V2)/mean(TS_US_HO_indata$V2) #1.231

## Examining the Co-Efficients, we fidn that lowest CV is that of APAC Consumer and 
## EU COnsumer market segment

## Data PReparation Complete

## Model Building
## Smoothing the data

total_TS_APAC_Consumer <- ts(TS_APAC_Consumer$V1)

timeser_APAC_Cons <- ts(TS_APAC_Consumer_indata$V1)
plot(timeser_APAC_Cons)

total_TS_EU_Consumer <- ts(TS_EU_Consumer$V1)

timeser_EU_Cons <- ts(TS_EU_Consumer_indata$V1)
plot(timeser_EU_Cons)

w <-1
smoothedseries_APAC_Cons <- stats::filter(timeser_APAC_Cons,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)

#Smoothing left end of the time series

diff_APAC_Cons <- smoothedseries_APAC_Cons[w+2] - smoothedseries_APAC_Cons[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_APAC_Cons[i] <- smoothedseries_APAC_Cons[i+1] - diff_APAC_Cons
}


#Smoothing right end of the time series

n <- length(smoothedseries_APAC_Cons)
diff_APAC_Cons <- smoothedseries_APAC_Cons[n-w] - smoothedseries_APAC_Cons[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_APAC_Cons[i] <- smoothedseries_APAC_Cons[i-1] + diff_APAC_Cons
}

#Plot the smoothed time series

timevals_in <- TS_APAC_Consumer_indata$Month
lines(smoothedseries_APAC_Cons, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, lets convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_APAC_Cons)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, lets fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, lets look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_APAC_Cons-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#Well check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, lets evaluate the model using MAPE
#First, lets make a prediction for the last 6 months

outdata <- TS_APAC_Consumer[43:48,]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, lets compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#Lets also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_TS_APAC_Consumer, col = "black")
lines(class_dec_pred, col = "red")
lines(class_dec_pred[1:42], col = "blue")

#So, that was classical decomposition, now lets do an ARIMA fit

autoarima <- auto.arima(timeser_APAC_Cons)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, lets check if the residual series is white noise

resi_auto_arima <- timeser_APAC_Cons - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, lets evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

#Lastly, lets plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_TS_APAC_Consumer, col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(autoarima), col = "blue", lwd = 2)

# Forecating APAC Consumer Quantity 

total_TS_APAC_Consumer <- ts(TS_APAC_Consumer$V3)

timeser_APAC_Cons <- ts(TS_APAC_Consumer_indata$V3)
plot(timeser_APAC_Cons)


w <-1
smoothedseries_APAC_Cons <- stats::filter(timeser_APAC_Cons,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)

#Smoothing left end of the time series

diff_APAC_Cons <- smoothedseries_APAC_Cons[w+2] - smoothedseries_APAC_Cons[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_APAC_Cons[i] <- smoothedseries_APAC_Cons[i+1] - diff_APAC_Cons
}


#Smoothing right end of the time series

n <- length(smoothedseries_APAC_Cons)
diff_APAC_Cons <- smoothedseries_APAC_Cons[n-w] - smoothedseries_APAC_Cons[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_APAC_Cons[i] <- smoothedseries_APAC_Cons[i-1] + diff_APAC_Cons
}

#Plot the smoothed time series

timevals_in <- TS_APAC_Consumer_indata$Month
lines(smoothedseries_APAC_Cons, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, lets convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_APAC_Cons)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, lets fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, lets look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_APAC_Cons-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#Well check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, lets evaluate the model using MAPE
#First, lets make a prediction for the last 6 months

outdata <- TS_APAC_Consumer[43:48,]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, lets compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#Lets also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_TS_APAC_Consumer, col = "black")
lines(class_dec_pred, col = "red")
lines(class_dec_pred[1:42], col = "blue")

#So, that was classical decomposition, now lets do an ARIMA fit

autoarima <- auto.arima(timeser_APAC_Cons)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, lets check if the residual series is white noise

resi_auto_arima <- timeser_APAC_Cons - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, lets evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

#Lastly, lets plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_TS_APAC_Consumer, col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(autoarima), col = "blue", lwd = 2)


# Forecast EU consumer Sales 

total_TS_EU_Consumer <- ts(TS_EU_Consumer$V1)

timeser_EU_Cons <- ts(TS_EU_Consumer_indata$V1)
plot(timeser_EU_Cons)


w <-1
smoothedseries_EU_Cons <- stats::filter(timeser_EU_Cons,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)

#Smoothing left end of the time series

diff_EU_Cons <- smoothedseries_EU_Cons[w+2] - smoothedseries_EU_Cons[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_EU_Cons[i] <- smoothedseries_EU_Cons[i+1] - diff_EU_Cons
}


#Smoothing right end of the time series

n <- length(smoothedseries_EU_Cons)
diff_EU_Cons <- smoothedseries_EU_Cons[n-w] - smoothedseries_EU_Cons[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_EU_Cons[i] <- smoothedseries_EU_Cons[i-1] + diff_EU_Cons
}

#Plot the smoothed time series

timevals_in <- TS_EU_Consumer_indata$Month
lines(smoothedseries_EU_Cons, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, lets convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_EU_Cons)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, lets fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, lets look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_EU_Cons-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#Well check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, lets evaluate the model using MAPE
#First, lets make a prediction for the last 6 months

outdata <- TS_EU_Consumer[43:48,]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, lets compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#Lets also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_TS_EU_Consumer, col = "black")
lines(class_dec_pred, col = "red")
lines(class_dec_pred[1:42], col = "blue")

#So, that was classical decomposition, now lets do an ARIMA fit

autoarima <- auto.arima(timeser_EU_Cons)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, lets check if the residual series is white noise

resi_auto_arima <- timeser_EU_Cons - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, lets evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

#Lastly, lets plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_TS_EU_Consumer, col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(autoarima), col = "blue", lwd = 2)


# Forecast EU consumer Quantity 

total_TS_EU_Consumer <- ts(TS_EU_Consumer$V3)

timeser_EU_Cons <- ts(TS_EU_Consumer_indata$V3)
plot(timeser_EU_Cons)


w <-1
smoothedseries_EU_Cons <- stats::filter(timeser_EU_Cons,filter=rep(1/(2*w+1),(2*w+1)),method='convolution', sides=2)

#Smoothing left end of the time series

diff_EU_Cons <- smoothedseries_EU_Cons[w+2] - smoothedseries_EU_Cons[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_EU_Cons[i] <- smoothedseries_EU_Cons[i+1] - diff_EU_Cons
}


#Smoothing right end of the time series

n <- length(smoothedseries_EU_Cons)
diff_EU_Cons <- smoothedseries_EU_Cons[n-w] - smoothedseries_EU_Cons[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_EU_Cons[i] <- smoothedseries_EU_Cons[i-1] + diff_EU_Cons
}

#Plot the smoothed time series

timevals_in <- TS_EU_Consumer_indata$Month
lines(smoothedseries_EU_Cons, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, lets convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_EU_Cons)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, lets fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, lets look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser_EU_Cons-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#Well check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, lets evaluate the model using MAPE
#First, lets make a prediction for the last 6 months

outdata <- TS_EU_Consumer[43:48,]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, lets compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
MAPE_class_dec

#Lets also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_TS_EU_Consumer, col = "black")
lines(class_dec_pred, col = "red")
lines(class_dec_pred[1:42], col = "blue")

#So, that was classical decomposition, now lets do an ARIMA fit

autoarima <- auto.arima(timeser_EU_Cons)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, lets check if the residual series is white noise

resi_auto_arima <- timeser_EU_Cons - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, lets evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,2])[5]
MAPE_auto_arima

#Lastly, lets plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_TS_EU_Consumer, col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(autoarima), col = "blue", lwd = 2)