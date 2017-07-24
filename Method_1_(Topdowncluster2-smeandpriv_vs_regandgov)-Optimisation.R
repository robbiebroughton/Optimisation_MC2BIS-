rm(list=ls())

library(ggplot2)
library(GGally)
library(VIM)
library(readr)
library(translate)
library(data.table)
library(dplyr)
library(car)
library(compare)
library(reshape2)
library(mice)
library(VIF)
library(caret)
library(Boruta)

# Load data properly
setwd("C:/Users/RobbieBroughton/Documents/MC2BIS")
data<-fread("Hungarian_Data_Cleaned_shit_updated.csv",na.strings=c(""," ","NA")) # so that it interprets blank spaces as NA's
original_data<-data[1:57383,]

# Make one new variable - if customer branch and account branch are equal then 1 and if not then 0, otherwise NA
original_data$Customer_Equals_Account<-ifelse(original_data$CUSTOMER_BRANCH==original_data$ACCOUNT_BRANCH,0,1)

data<- subset(original_data, select=-c(EqualTo,Compound_ID,ALERT_CUSTOMER_SEGMENT,Unique,REASON_FOR_CLOSURE,CUSTOMER_REGION,DESCRIPTION,CREATION_DATE,CREATION_DATE,LAST_UPDATED,EVENT_DATE,AGE_IN_DAYS,ACTION_NUM, TRANSACTION_ID,CREDIT_DEBIT, TXN_TYPE_DESC, ASSIGNED_TO,ASSIGNED_BY,ORIGINAL_CURRENCY_AMOUNT,ORIGINAL_CURRENCY,POSTAL_CODE,BRANCH_ID,BUSINESS_TYPE,COUNTRY_OF_RESIDENCE,ACCOUNT_BALANCE,HOLDING_BANK_NAME,CUSTOMER_BRANCH,ACCOUNT_BRANCH,BRANCH_CODE,CASE_KEY, CASE_IDENTIFIER,NUM_ALERTS,NUM_CUSTOMERS,CASE_CREATION_DATE,INVESTIGATION_LENGTH,NORKOM_SCORE,TRANSACTION_DATE))
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

# After creation of SAR I can now delete STATUS_NAME and CASE_STATUS
datanew<-subset(datanew,select=-c(CASE_STATUS,STATUS_NAME))

# Check is it necessary to make 4 different data sets or can we get away with two (similar behaviour for the companies?)
# Visualisation to see similar average values for a subset of variables!
# get rid of NA's so i can scale and visualise BASE_CURRENCY_AMOUNT
nona<- datanew[complete.cases(datanew$BASE_CURRENCY_AMOUNT), ] # only 21,000 points now
nona1<-subset(nona,select=c(CUSTOMER_SEGMENT,BASE_CURRENCY_AMOUNT))
dfNormZ <- data.frame(scale(nona1))
nona1[, 2] <- scale(nona1[,2])

# Note: Can try to do for other continuous variables! Tried for SC04_avg_cash_deposit but the range was too high
# Come back to this later if want to explore more different top down cluster solutions - maybe ask Tom about some visualisation in SAS VA for this? might be easier in dealing with NA's

par(mfrow=c(1,2))
boxplot(NUM_CASES~CUSTOMER_SEGMENT, horizontal=F, data=datanew, col = "gray",ylab='Base Currenct Amount')
boxplot(NUM_ACCOUNTS~CUSTOMER_SEGMENT, horizontal=F, data=datanew, col = "gray",ylab='Base Currenct Amount')
boxplot(BASE_CURRENCY_AMOUNT~CUSTOMER_SEGMENT, horizontal=F, data=nona1, col = "gray",ylab='Base Currenct Amount', ylim = c(-1, 1)) #here we see small med and priv are similar and others are similar (possible top down clusters)
boxplot(SC04_AVG_Cash_Deposit~CUSTOMER_SEGMENT, horizontal=F, data=datanew, col = "gray",ylab='Base Currenct Amount')

# Conclusion is that different segments have diff behaviour for dif vars so just try different top down cluster solutions and compare
# Ask Tom to show how to visualise better in SAS
# Number 1 - Gov,Reg and Sme against Priv
# Number 2 - Gov, Reg Vs Sme and Priv (because in terms of transaction amounts these groups tended to be similar)
# Number 3 - Gov Vs Reg Vs Priv Vs Sme

### Exploratory Analysis -  Number 2 ####

# Split up the newdata
bigdata<-datanew[which(CUSTOMER_SEGMENT=="GOV" | CUSTOMER_SEGMENT== "SME"),]
smalldata<-datanew[which(CUSTOMER_SEGMENT=="REG"|CUSTOMER_SEGMENT=="PRIV"),]

## Company Data ##

hi<-bigdata[which(is.na(bigdata$COUNTRY_OF_ORIGIN))]

## Investigate effect of categoricals on response (to see if important) ##

ggplot(bigdata, aes(x = COUNTRY_OF_ORIGIN, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

sum(is.na(smalldata$COUNTRY_OF_ORIGIN))

ggplot(bigdata, aes(x = SCENARIO, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90)) # we see SC28 has quite a lot of SAR's (ranking how good each scenario is)
# I think this can even be good as a predictor in model as if transaction was alerted by an accurate scenario then it can be marked as more risky
# SC28 very hih percentage of SAR - will need to recode this categorical and lots others

ggplot(bigdata, aes(x = EVENT_MONTH, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#more SAR's in May, June July (possibly August also) - no good reason for this so no real need to make new model for different time periods

ggplot(bigdata, aes(x = CAL_QUARTER, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not so much difference (may june july however is quite different)
#maybe remove this and keep event month!

ggplot(bigdata, aes(x = TXN_TYPE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#informative - much variation between types - need to recode

ggplot(bigdata, aes(x = INSTRUMENT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# not so different - maybe not such a good variable - binary

ggplot(bigdata, aes(x = SCOPE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#much variation - binary

ggplot(bigdata, aes(x = REGION, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#much variation - binary

ggplot(bigdata, aes(x = RAISED_ON, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not so different - binary

ggplot(bigdata, aes(x = PEP_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#More SAR's for Y's - binary

ggplot(bigdata, aes(x = SPECIAL_ATTENTION_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#all Y's are not SAR's but some N's were SAR's (strange) - may be still informative - binary

ggplot(bigdata, aes(x = COUNTRY_OF_RESIDENCE_abbrev, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#should be informative but will need to convert

ggplot(bigdata, aes(x = BUSINESS_TYPE2, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#informative - will need to convert - non financial businesses have more SAR's it seems

ggplot(bigdata, aes(x = CUSTOMER_STATUS, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not so different - binary

ggplot(bigdata, aes(x = STATUS, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# a little informative (if reopened no SAR's) - will need to code

ggplot(bigdata, aes(x = CUSTOMER_SEGMENT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# like one SAR in GOV so very informative (GOV companies unlikely to have a SAR)

ggplot(bigdata, aes(x = TIME_PERIOD, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not super informative

ggplot(bigdata, aes(x = CUSTOMER_REGION, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#if in region X (23 obs) then we have a SAR - will need to recode but quite informative

ggplot(bigdata, aes(x = DISTRICT_OF_BUDAPEST, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# other (31 obs) has lots of SAR's so can be informative

ggplot(bigdata, aes(x = RISK_SCORE_2, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# what we find here is counter intuitive - more risky second party countries have less SAR's, one explanation is that because scenarios alert based on country then lots of alerts generated but not that many money laundering
#therefore maybe we should give different risk score here

ggplot(bigdata, aes(x = RISK_SCORE_1, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# equal proportion of SAR's for all groups here - maybe this risk scoring isnt that good and we should group differently

ggplot(bigdata, aes(x = Customer_Equals_Account, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#maybe not so informative - binary

ggplot(bigdata, aes(x = `SC01_Amount_Exceding_250000_(HUF)`, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not so much difference - binary

# To remove = Customer_TYPE (all are corporate in this case), CAL_quarter (because it doesnt contain info of event_month)
bigdata1<-subset(bigdata,select=-c(CUSTOMER_TYPE,CAL_QUARTER))

## Investigate effect of continuous vars on each other - if plot against response its not informative ##
ggpairs(data=bigdata1,columns=c('SAR','BASE_CURRENCY_AMOUNT','NUM_CASES','NUM_ACCOUNTS','CUSTOMER_FOR_DAYS'))
#correlations here indicate not much multicolin and also not highly correlated to SAR
ggpairs(data=bigdata1,columns=c())

#continue from here - get more info about rest of categoricals and also relationship with scenario description vars
#.....#




# Missing Values

# Feature reduction