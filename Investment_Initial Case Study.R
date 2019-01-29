#Checkpoints - Part 1

# Load packages

library(ggplot2)

library(dplyr)

library(tidyr)

library(stringr)

rounds2 <- read.csv("rounds2.csv", stringsAsFactors = F)

companies <- read.delim("companies.txt", stringsAsFactors = F)

#How many unique companies are present in rounds2?
Unique_companies <- unique(rounds2 $ company_permalink)
length(Unique_companies)


#How many unique companies are present in companies?
Unique_companies <- unique(companies $ permalink)
length(Unique_companies)


#Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame. How many observations are present in master_frame?

master_frame <- merge(x = companies, y = rounds2, by = "permalink")


# Replacing all NA values in raised amount column to 0

master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)==T] <- 0


# because # 17% missing values of total recor

#checkpoint 2

#Average funding amount of venture type

venture <- subset(master_frame, funding_round_type=="venture")
mean(venture$raised_amount_usd)

# Average funding amount of angel type

angel <- subset(master_frame,funding_round_type=="angel")
mean(angel$raised_amount_usd)



#Average funding amount of seed type

seed <- subset(master_frame,funding_round_type=="seed")
mean(seed$raised_amount_usd)


# Average funding amount of private equity type

private_equity <- subset(master_frame,funding_round_type=="private_equity")
mean(private_equity$raised_amount_usd)



#checkpoint 3

# 1. Top English-speaking country

summary(factor(master_frame$country_code))

# 2. Second English-speaking country

  #some of the country code are balnk

length(which(master_frame$country_code == ""))
length(which(master_frame$country_code == ""))/nrow(master_frame)
  #replace the balnk cells by empty
master_frame$country_code[which(master_frame$country_code=="")]="Empty"


highest_funding_countries <- aggregate(raised_amount_usd~country_code,venture, sum)

#Third English-speaking country
  #first find top 9 countries
top9 <- head(highest_funding_countries_sorted, 9)

  #3rd highest english speaking country from top 9
View(top9)


#checkpoint 5
#D1<-USA,D2<-GBR, D3<-IND









