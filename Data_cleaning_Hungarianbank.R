rm(list=ls())

library(ggplot2)
library(GGally)
library(VIM)
library(readr)
library(translate)
library(data.table)
library(dplyr)
library(compare)
library(reshape2)
library(mice)
library(caret)
library(Boruta)

# Load data properly
setwd("C:/Users/RobbieBroughton/Documents/MC2BIS")
data<-fread("Hungarian_Data_Cleaned_shit_updated.csv")
original_data<-data[1:57383,]

# Make one new variable - if customer branch and account branch are equal then 1 and if not then 0, otherwise NA
original_data$Customer_Equals_Account<-ifelse(original_data$CUSTOMER_BRANCH==original_data$ACCOUNT_BRANCH,0,1)

data<- subset(original_data, select=-c(EqualTo,Compound_ID,Unique,REASON_FOR_CLOSURE,CUSTOMER_REGION,DESCRIPTION,CREATION_DATE,CREATION_DATE,LAST_UPDATED,EVENT_DATE,AGE_IN_DAYS,ACTION_NUM, TRANSACTION_ID,CREDIT_DEBIT_CODE, TXN_TYPE_DESC, ASSIGNED_TO,ASSIGNED_BY,ORIGINAL_CURRENCY_AMOUNT,ORIGINAL_CURRENCY,POSTAL_CODE,BRANCH_ID,BUSINESS_TYPE,COUNTRY_OF_RESIDENCE,ACCOUNT_BALANCE,HOLDING_BANK_NAME,CUSTOMER_BRANCH,ACCOUNT_BRANCH,BRANCH_CODE,CASE_KEY, CASE_IDENTIFIER,NUM_ALERTS,NUM_CUSTOMERS,CASE_CREATION_DATE,INVESTIGATION_LENGTH,NORKOM_SCORE,TRANSACTION_DATE))
attach(data)

# Response Variable (SAR)
data$SAR<- ifelse(data$CASE_STATUS=="Reported/Closed",1,ifelse(data$CASE_STATUS=="Closed",0,NA))


data$SAR[with(data,is.na(data$SAR) & data$STATUS_NAME=="Closed" )]=0
data$SAR[with(data,is.na(data$SAR) & data$STATUS_NAME=="Linked Closed" )]=0

#count the balance of resoponse
table(data$SAR)
summary(data$SAR)

# Delete NA values from SAR variable
datanew<-data[with(data,!is.na(data$SAR))]

# Descriptive Statistics
### Exploratory Analysis ####
datanew_Q1=datanew[CAL_QUARTER==1,]
ggpairs(datanew=datanew,columns=c('SAR','AGE_IN_DAYS','NUM_CASES','CUSTOMER_FOR_DAYS','NUM_ALERTS'))
ggpairs(datanew=datanew_Q1,columns=c('SAR','AGE_IN_DAYS','NUM_CASES','CUSTOMER_FOR_DAYS','NUM_ALERTS'))

ggplot(datanew, aes(x = CAL_QUARTER, fill = SAR)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(datanew, aes(x = EVENT_MONTH, fill = SAR)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

aggr_plot <- aggr(datanew, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(datanew), cex.axis=.7, gap=3, 
                  ylab=c("Histogram of missing data","Pattern"))


# Missing Values

# Feature reduction






