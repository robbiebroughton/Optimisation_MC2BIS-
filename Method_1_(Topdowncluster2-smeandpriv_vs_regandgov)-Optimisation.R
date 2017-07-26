rm(list=ls())

library(ggplot2)
library(GGally)
library(Ridit)
library(VIM)
library(readr)
library(translate)
library(FSelector)
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
data<-fread("Hungaria_Data_ excel_for_VA_Cleaned_Corrected_Risk_scores.csv",na.strings=c(""," ","NA")) # so that it interprets blank spaces as NA's

# Make one new variable - if customer branch and account branch are equal then 1 and if not then 0, otherwise NA
data$Customer_Equals_Account<-ifelse(data$CUSTOMER_BRANCH==data$ACCOUNT_BRANCH,0,1)

data<- subset(data, select=-c(EqualTo,Compound_ID,ALERT_CUSTOMER_SEGMENT,Unique,REASON_FOR_CLOSURE,CUSTOMER_REGION,DESCRIPTION,CREATION_DATE,CREATION_DATE,LAST_UPDATED,EVENT_DATE,AGE_IN_DAYS,ACTION_NUM, TRANSACTION_ID, TXN_TYPE_DESC, ASSIGNED_TO,ASSIGNED_BY,ORIGINAL_CURRENCY_AMOUNT,ORIGINAL_CURRENCY,POSTAL_CODE,BRANCH_ID,BUSINESS_TYPE,COUNTRY_OF_RESIDENCE,ACCOUNT_BALANCE,HOLDING_BANK_NAME,CUSTOMER_BRANCH,ACCOUNT_BRANCH,BRANCH_CODE,CASE_KEY, CASE_IDENTIFIER,NUM_ALERTS,NUM_CUSTOMERS,CASE_CREATION_DATE,INVESTIGATION_LENGTH,NORKOM_SCORE,TRANSACTION_DATE))
attach(data)

#Credit_Debit_code or Credit_Debit- which to delete?
table(original_data$CREDIT_DEBIT_CODE)
table(original_data$CREDIT_DEBIT) # this one
data<- subset(data, select=-c(CREDIT_DEBIT))

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

# Merge 2 similar Credit_Debit variables : Sc16,17,18 and the original one 
#Replace in SC16 variable with D and C to check with the main credit debit code variable

##check
datanew$SC16_17_18_Type_Transaction__Credit_Debit_[datanew$SC16_17_18_Type_Transaction__Credit_Debit_=='Debit']<-"D" 
datanew$SC16_17_18_Type_Transaction__Credit_Debit_[datanew$SC16_17_18_Type_Transaction__Credit_Debit_=='Credit']<-"C"
merger<-subset(datanew,select=c(CREDIT_DEBIT_CODE,SC16_17_18_Type_Transaction__Credit_Debit_))
na1<-sum(is.na(merger$CREDIT_DEBIT_CODE));na1
na2<-sum(is.na(merger$SC16_17_18_Type_Transaction__Credit_Debit_));na2
new<-merger[complete.cases(merger$SC16_17_18_Type_Transaction__Credit_Debit_),]
##check

#no observations in sc161718 CD code that arent in the main credit debit code variable, so delete sc16_17_18!
datanew<-subset(datanew,select=-c(SC16_17_18_Type_Transaction__Credit_Debit_))


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
boxplot(NUM_CASES~CUSTOMER_SEGMENT, horizontal=F, data=datanew, col = "gray",ylab='No of cases')
boxplot(NUM_ACCOUNTS~CUSTOMER_SEGMENT, horizontal=F, data=datanew, col = "gray",ylab='No of accounts')

boxplot(BASE_CURRENCY_AMOUNT~CUSTOMER_SEGMENT, horizontal=F, data=nona1, col = "gray",ylab='Base Currenct Amount', ylim = c(-1, 1)) #here we see small med and priv are similar and others are similar (possible top down clusters)

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

## Investigate effect of categoricals on response (to see if important) ##

ggplot(bigdata, aes(x = COUNTRY_OF_ORIGIN, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

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
#equal proportion of SAR's for all groups here - maybe this risk scoring isnt that good and we should group differently

ggplot(bigdata, aes(x = Customer_Equals_Account, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#maybe not so informative - binary

ggplot(bigdata, aes(x = `SC01_Amount_Exceding_250000_(HUF)`, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not so much difference - binary

ggplot(bigdata, aes(x = CREDIT_DEBIT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

# To remove = Customer_TYPE (all are corporate in this case), CAL_quarter (because it doesnt contain info of event_month)
bigdata1<-subset(bigdata,select=-c(CUSTOMER_TYPE,CAL_QUARTER))

## Investigate effect of continuous vars on each other - if plot against response its not informative ##
ggpairs(data=bigdata1,columns=c('SAR','BASE_CURRENCY_AMOUNT','NUM_CASES','NUM_ACCOUNTS','CUSTOMER_FOR_DAYS'))
#correlations here indicate not much multicolin and also not highly correlated to SAR

# get more info about rest of categoricals and also relationship with scenario description vars
# tried to find relationship between scenario description vars but either get error saying to adjust cardinality threshold or trying pairs function cant deal with NA's well
# best to decide what i should remove by hand first in these variables

## Feature Selection ##

# we will remove all vars with greater than 15% missing values (too unreliable to impute acc to reports)
rankmissing<-data.frame(sapply(bigdata1,function(x) mean(is.na(x))) %>%
                          sort())# gets vers percentage of missing vals and the order

varnames<-data.frame(row.names(rankmissing))
workingvars<-varnames[1:22,]

refineddata<-subset(bigdata1,select=c(SCENARIO,EVENT_MONTH, RAISED_ON, PEP_FLAG, SPECIAL_ATTENTION_FLAG, RISK_SCORE_1,BUSINESS_TYPE2, CUSTOMER_STATUS, NUM_CASES, NUM_ACCOUNTS, CUSTOMER_FOR_DAYS,CUSTOMER_SEGMENT, TIME_PERIOD, CUSTOMER_REGION, RISK_SCORE_2, SC01_Amount_Exceding_250000__HUF_, SAR, COUNTRY_OF_ORIGIN,COUNTRY_OF_RESIDENCE_abbrev, ORIGINAL_ACCOUNT_CURRENCY,Customer_Equals_Account,STATUS))
# subset of 21 explan variables (with more possibly to introduce)
# note that now I have removed some seemingly good variables (because they have lots of NA's) but if they NA's are not actually missing values then I can reintroduce these guys possibly, revisit later
# possible vars to consider include: 

categors<-subset(bigdata1,select=c(as.factor(Customer_Equals_Account),EVENT_MONTH,                                                                                                                
                                   TXN_TYPE,                                                                                                          
                                   INSTRUMENT,                                                                                                            
                                   SCOPE,                                                                                                                    
                                   REGION,                                                                                                                    
                                   RAISED_ON,                                                                                                         
                                   PEP_FLAG,                                                                                                                   
                                   SPECIAL_ATTENTION_FLAG,                                                                                                    
                                   COUNTRY_OF_ORIGIN,                                                                                                          
                                   COUNTRY_OF_RESIDENCE_abbrev,                                                                                                
                                   BUSINESS_TYPE2,                                                                                                             
                                   CUSTOMER_STATUS,
                                   ORIGINAL_ACCOUNT_CURRENCY,                                                                                                  
                                   STATUS,                                                                                                                     
                                   CUSTOMER_SEGMENT,                                                                                                           
                                   TIME_PERIOD,                                                                                                                
                                   CUSTOMER_REGION,                                                                                                            
                                   DISTRICT_OF_BUDAPEST,                                                                                                       
                                   SECOND_PARTY_COUNTRY,SAR))
#all above are now categoricals
# first convert categoricals to factors and leave continuous as they are
categoricals<-chi.squared(SAR~., categors)



#Remove Redundant Features (with low correlation with response)
# calculate correlation matrix
correlationMatrix <- cor(bigdata1[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

## Missing Values ##

rankmissing<-data.frame(sapply(bigdata1,function(x) mean(is.na(x))) %>%
                          sort())# gets vers percentage of missing vals and the order
#
varnames<-data.frame(row.names(hey))
workingvars<-hi[1:22,]

refineddata<-subset(bigdata1,select=c(SCENARIO,EVENT_MONTH, RAISED_ON, PEP_FLAG, SPECIAL_ATTENTION_FLAG, RISK_SCORE_1,BUSINESS_TYPE2, CUSTOMER_STATUS, NUM_CASES, NUM_ACCOUNTS, CUSTOMER_FOR_DAYS,CUSTOMER_SEGMENT, TIME_PERIOD, CUSTOMER_REGION, RISK_SCORE_2, SC01_Amount_Exceding_250000_(HUF), SAR, COUNTRY_OF_ORIGIN,COUNTRY_OF_RESIDENCE_abbrev, ORIGINAL_ACCOUNT_CURRENCY,Customer_Equals_Account,STATUS))
# note that now I have removed some seemingly good variables (because they have lots of NA's) but if they NA's are not actually missing values then I can reintroduce these guys possibly, revisit later

data_missing=mice(data,m=5,meth='pmm',maxit=0,seed=500)
summary(data_missing)
densityplot(data_missing,~.) #put here a variable of interest #
stripplot(data_missing,pch=20,cex=1.2)

## Standardising (categorizing) variables (attempt) ##
#convert categoricals to ridit score (less categoricals to deal with)
tab1<-table(bigdata1$SAR,bigdata1$SCENARIO)
tab2<-table(bigdata1$SAR,bigdata1$EVENT_MONTH)
tab3<-table(bigdata1$SAR,bigdata1$BUSINESS_TYPE2)
tab4<-table(bigdata1$SAR,bigdata1$COUNTRY_OF_RESIDENCE_abbrev)
tab5<-table(bigdata1$SAR,bigdata1$TXN_TYPE)
tab6<-table(bigdata1$SAR,bigdata1$COUNTRY_OF_ORIGIN)
tab7<-table(bigdata1$SAR,bigdata1$CUSTOMER_REGION)
tab8<-table(bigdata1$SAR,bigdata1$TIME_PERIOD)
tab9<-table(bigdata1$SAR,bigdata1$STATUS)
tab10<-table(bigdata1$SAR,bigdata1$DISTRICT_OF_BUDAPEST)
tab11<-table(bigdata1$SAR,bigdata1$SECOND_PARTY_COUNTRY)

bigdata1$SCENARIO_2<-ridit(tab1,g=2)
bigdata1$EVENT_MONTH_2<-ridit(tab2,g=2)
bigdata1$BUSINESS_TYPE2_2<-ridit(tab3,g=2)
bigdata1$COUNTRY_OF_RESIDENCE_abbrev_2<-ridit(tab4,g=2)
bigdata1$TXN_TYPE_2<-ridit(tab5,g=2)
bigdata1$COUNTRY_OF_ORIGIN_2<-ridit(tab6,g=2)
bigdata1$CUSTOMER_REGION_2<-ridit(tab7,g=2)
bigdata1$TIME_PERIOD_2<-ridit(tab8,g=2)
bigdata1$STATUS<-ridit(tab9,g=2)
bigdata1$DISTRICT_OF_BUDAPEST<-ridit(tab10,g=2)
bigdata1$SECOND_PARTY_COUNTRY<-ridit(tab11,g=2)
#interpretted as an estimate of the probability a random observation from that group will be greater than or equal to a random observation from the reference group
