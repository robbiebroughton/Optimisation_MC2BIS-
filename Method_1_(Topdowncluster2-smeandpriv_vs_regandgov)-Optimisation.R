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
data<-fread("Hungarian_Data_CSV_for_R_updated_col_names_ converted_all_cont_to_num_type_and_filled_missing_values_with_dots.csv",na.strings=c(""," ",".","NA")) # so that it interprets blank spaces as NA's
data<-data[1:57384,]

# Make one new variable - if customer branch and account branch are equal then 1 and if not then 0, otherwise NA
data$Customer_Equals_Account<-ifelse(data$CUSTOMER_BRANCH==data$ACCOUNT_BRANCH,0,1)

data<- subset(data, select=-c(EqualTo,SC16_17_18_2nd_party_Country,Compound_ID,ALERT_CUSTOMER_SEGMENT,CREDIT_DEBIT,Unique,REASON_FOR_CLOSURE,DESCRIPTION,CREATION_DATE,CREATION_DATE,LAST_UPDATED,EVENT_DATE,AGE_IN_DAYS,ACTION_NUM, TRANSACTION_ID, TXN_TYPE_DESC, ASSIGNED_TO,ASSIGNED_BY,ORIGINAL_CURRENCY_AMOUNT,ORIGINAL_CURRENCY,POSTAL_CODE,BRANCH_ID,BUSINESS_TYPE,COUNTRY_OF_RESIDENCE,ACCOUNT_BALANCE,HOLDING_BANK_NAME,CUSTOMER_BRANCH,ACCOUNT_BRANCH,BRANCH_CODE,CASE_KEY, CASE_IDENTIFIER,NUM_ALERTS,NUM_CUSTOMERS,CASE_CREATION_DATE,INVESTIGATION_LENGTH,NORKOM_SCORE,TRANSACTION_DATE))
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
boxplot(NUM_CASES~CUSTOMER_SEGMENT, horizontal=F, data=datanew, col = "gray",ylab='No of cases')
boxplot(NUM_ACCOUNTS~CUSTOMER_SEGMENT, horizontal=F, data=datanew, col = "gray",ylab='No of accounts')
boxplot(BASE_CURRENCY_AMOUNT~CUSTOMER_SEGMENT, horizontal=F, data=nona1, col = "gray",ylab='Base Currenct Amount', ylim = c(-1, 1)) #here we see small med and priv are similar and others are similar (possible top down clusters)

# Conclusion is that different segments have diff behaviour for dif vars so just try different top down cluster solutions and compare
# Ask Tom to show how to visualise better in SAS
# Number 1 - Gov,Reg and Sme against Priv
# Number 2 - Gov, Reg Vs Sme and Priv (because in terms of transaction amounts these groups tended to be similar)
# Number 3 - Gov Vs Reg Vs Priv Vs Sme

###########################################################
############ Data Split - Top Down Clustering #############
###########################################################

# Split up the newdata
bigdata<-datanew[which(CUSTOMER_SEGMENT=="GOV" | CUSTOMER_SEGMENT== "REG"),]
smalldata<-datanew[which(CUSTOMER_SEGMENT=="SME"|CUSTOMER_SEGMENT=="PRIV"),]

##################
#### Big Data ####
##################

# To remove = Customer_TYPE (all are corporate in this case), CAL_quarter (because it doesnt contain info of event_month)
bigdata1<-subset(bigdata,select=-c(CUSTOMER_TYPE,CAL_QUARTER))

## Investigate effect of continuous vars on each other - if plot against response its not informative ##
ggpairs(data=bigdata1,columns=c('SAR','BASE_CURRENCY_AMOUNT','NUM_CASES','NUM_ACCOUNTS','CUSTOMER_FOR_DAYS'))
#correlations here indicate not much multicolin and also not highly correlated to SAR

# get more info about rest of categoricals and also relationship with scenario description vars
# tried to find relationship between scenario description vars but either get error saying to adjust cardinality threshold or trying pairs function cant deal with NA's well
# best to decide what i should remove by hand first in these variables
# first step is to reduce number of features with too many missing values

################################
## Feature Selection - Step 1 ##
################################

# we will remove all vars with greater than 15% missing values (too unreliable to impute acc to reports)
rankmissing<-data.frame(sapply(bigdata1,function(x) mean(is.na(x))) %>%
                          sort())# gets vers percentage of missing vals and the order

varnames<-data.frame(row.names(rankmissing))
workingvars<-varnames[1:22,]

refineddata<-subset(bigdata1,select=c(SCENARIO,SC01_Amount_Exceding_250000__HUF_,NEW_ACCOUNT_BALANCE,EVENT_MONTH, RAISED_ON, PEP_FLAG,NEW_ACCOUNT_BALANCE,SPECIAL_ATTENTION_FLAG,COUNTRY_OF_RESIDENCE_abbrev, RISK_SCORE_1,BUSINESS_TYPE2, CUSTOMER_STATUS, NUM_CASES, NUM_ACCOUNTS, CUSTOMER_FOR_DAYS,CUSTOMER_SEGMENT, TIME_PERIOD, CUSTOMER_REGION, RISK_SCORE_2, SAR, COUNTRY_OF_ORIGIN,STATUS,ORIGINAL_ACCOUNT_CURRENCY,Customer_Equals_Account))
# note that now I have removed some seemingly good variables (because they have lots of NA's) but if they NA's are not actually missing values then I can reintroduce these guys possibly, revisit later
# possible vars to consider include: district_ofbudapest,credit/debitcode,txn_type,scope,region,base_currency_amount
# it looks as though all these have same amount of missing vals (most likely because the NA's are actually valid, instances are not transactions but just inquiries)
# to check, take subset of data set where base_currency_amount is involved and all these vars and they should all match

allsamenas<-subset(bigdata1,select=c(BASE_CURRENCY_AMOUNT,CREDIT_DEBIT_CODE,TXN_TYPE,INSTRUMENT,SCOPE,REGION))
allsamenas1<-allsamenas[complete.cases(allsamenas$BASE_CURRENCY_AMOUNT),]
#conclude all the NA values in these variables are informative as they tell us that these instances are not transactions, so we should convert wherever it says NA to character value
#so put in these extra vars to refined data
#new refined data
newrefineddata<-subset(bigdata1,select=c(SCENARIO,EVENT_MONTH, RAISED_ON, PEP_FLAG, SPECIAL_ATTENTION_FLAG,COUNTRY_OF_RESIDENCE_abbrev, RISK_SCORE_1,BUSINESS_TYPE2, CUSTOMER_STATUS, NUM_CASES, NUM_ACCOUNTS, CUSTOMER_FOR_DAYS,CUSTOMER_SEGMENT, TIME_PERIOD, CUSTOMER_REGION, RISK_SCORE_2, SAR, COUNTRY_OF_ORIGIN,STATUS,ORIGINAL_ACCOUNT_CURRENCY,Customer_Equals_Account,NEW_ACCOUNT_BALANCE,DISTRICT_OF_BUDAPEST,CREDIT_DEBIT_CODE,TXN_TYPE,INSTRUMENT,SCOPE,REGION,BASE_CURRENCY_AMOUNT,SC01_Amount_Exceding_250000__HUF_))

#need to change NA values in the last 6 vars in above data set to character (dont think this is correct - try extract values from description vars)
newrefineddata$DISTRICT_OF_BUDAPEST[is.na(newrefineddata$DISTRICT_OF_BUDAPEST)]<-"OUTSIDE_BUDAPEST"
newrefineddata$CREDIT_DEBIT_CODE[is.na(newrefineddata$CREDIT_DEBIT_CODE)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$TXN_TYPE[is.na(newrefineddata$TXN_TYPE)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$INSTRUMENT[is.na(newrefineddata$INSTRUMENT)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$SCOPE[is.na(newrefineddata$SCOPE)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$REGION[is.na(newrefineddata$REGION)]<-"NOT_A_SINGLE_TRANSACTION"
# because these variables are defined by one transaction so any alert that is alerted because of multiple transactions should be put as not applicable

# dealing with BASE_CURRENCY_AMOUNT variable
newrefineddata$BASE_CURRENCY_AMOUNT=as.numeric(newrefineddata$BASE_CURRENCY_AMOUNT)
newrefineddata$BASE_CURRENCY_AMOUNT=cut(newrefineddata$BASE_CURRENCY_AMOUNT,c(quantile(newrefineddata$BASE_CURRENCY_AMOUNT,na.rm = T)))
newrefineddata$BASE_CURRENCY_AMOUNT=addNA(newrefineddata$BASE_CURRENCY_AMOUNT)
newrefineddata$BASE_CURRENCY_AMOUNT<-as.character(newrefineddata$BASE_CURRENCY_AMOUNT)
newrefineddata$BASE_CURRENCY_AMOUNT[is.na(newrefineddata$BASE_CURRENCY_AMOUNT)]<-"NOT_A_SINGLE_TRANSACTION"



#################################
## Dealing with missing values ##
#################################
rankmissing<-data.frame(sapply(newrefineddata,function(x) mean(is.na(x))) %>%
                          sort())# gets vers percentage of missing vals and the order

# Go through each var and the missing vals and decide what to do with each

## Country_of_origin and COUNTRY_OF_RESIDENCE_abbrev ##
countryorigin<-newrefineddata[which(newrefineddata$COUNTRY_OF_RESIDENCE_abbrev=="HU"),]
countryres<-newrefineddata[which(is.na(newrefineddata$COUNTRY_OF_RESIDENCE_abbrev)),]
countryres2<-subset(countryres,select=c(COUNTRY_OF_ORIGIN,COUNTRY_OF_RESIDENCE_abbrev))
# since all of these NA's have acc currency of HUF's and also customer_region is in Hungary so put HU for all missing vals
countryorig<-countryorigin[which(countryorigin$COUNTRY_OF_ORIGIN=="MAGYARORSZÁG"),]
#so we found there are only 59 out of 10797 obs where country of res is hungary where the country of origin ISNT hungary therefore for my 3 missing vals here I have Hungary as country of res, so i will impute hungary for country or origin for these guys
newrefineddata$COUNTRY_OF_ORIGIN[is.na(newrefineddata$COUNTRY_OF_ORIGIN)]<-"MAGYARORSZÁG"
newrefineddata$COUNTRY_OF_RESIDENCE_abbrev[is.na(newrefineddata$COUNTRY_OF_RESIDENCE_abbrev)]<-"HU"


## STATUS and ORIGINAL_ACC_CURRENCY and CUSTOMER_EQUALS_ACCOUNT and NEW_ACCOUNT_BALANCE ##

# inspect reason for missingness
missing<-subset(newrefineddata,select=c(ORIGINAL_ACCOUNT_CURRENCY,NEW_ACCOUNT_BALANCE,Customer_Equals_Account,STATUS))
missing1<-newrefineddata[which(is.na(newrefineddata$ORIGINAL_ACCOUNT_CURRENCY))]
#so these guys all had alerts based on client but doesnt mean they dont have an account
# i think still impute because these guys have accounts still

#function to calculate the mode of a variables
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#impute new account BALANCE, status, orig acc currency and customer equals account
newrefineddata[is.na(newrefineddata$NEW_ACCOUNT_BALANCE) == TRUE, c("NEW_ACCOUNT_BALANCE")]  <- mean(as.numeric(newrefineddata[['NEW_ACCOUNT_BALANCE']]), na.rm = T)
# imputed by 12455331
newrefineddata[is.na(newrefineddata$STATUS) == TRUE, "STATUS"]  <- Mode(newrefineddata[['STATUS']])
# imputed by "OPENED"
newrefineddata[is.na(newrefineddata$ORIGINAL_ACCOUNT_CURRENCY) == TRUE, "ORIGINAL_ACCOUNT_CURRENCY"]  <- Mode(newrefineddata[['ORIGINAL_ACCOUNT_CURRENCY']])
# imputed by "HUF"
newrefineddata[is.na(newrefineddata$Customer_Equals_Account) == TRUE, "Customer_Equals_Account"]  <- Mode(newrefineddata[['Customer_Equals_Account']])
# imputed by "0"

# now convert all character vars to factors and all continuous variables to numeric or int
newrefineddata$SCENARIO<-as.factor(newrefineddata$SCENARIO)
newrefineddata$EVENT_MONTH<-as.factor(newrefineddata$EVENT_MONTH)
newrefineddata$RAISED_ON<-as.factor(newrefineddata$RAISED_ON)
newrefineddata$PEP_FLAG<-as.factor(newrefineddata$SPECIAL_ATTENTION_FLAG)
newrefineddata$COUNTRY_OF_RESIDENCE_abbrev<-as.factor(newrefineddata$COUNTRY_OF_RESIDENCE_abbrev)
newrefineddata$RISK_SCORE_1<-as.factor(newrefineddata$RISK_SCORE_1)
newrefineddata$BUSINESS_TYPE2<-as.factor(newrefineddata$BUSINESS_TYPE2)
newrefineddata$CUSTOMER_STATUS<-as.factor(newrefineddata$CUSTOMER_STATUS)
newrefineddata$CUSTOMER_SEGMENT<-as.factor(newrefineddata$CUSTOMER_SEGMENT)
newrefineddata$TIME_PERIOD<-as.factor(newrefineddata$TIME_PERIOD)
newrefineddata$CUSTOMER_REGION<-as.factor(newrefineddata$CUSTOMER_REGION)
newrefineddata$RISK_SCORE_2<-as.factor(newrefineddata$RISK_SCORE_2)
newrefineddata$SAR<-as.factor(newrefineddata$SAR)
newrefineddata$ORIGINAL_ACCOUNT_CURRENCY<-as.factor(newrefineddata$ORIGINAL_ACCOUNT_CURRENCY)
newrefineddata$Customer_Equals_Account<-as.factor(newrefineddata$Customer_Equals_Account)
newrefineddata$DISTRICT_OF_BUDAPEST<-as.factor(newrefineddata$DISTRICT_OF_BUDAPEST)
newrefineddata$CREDIT_DEBIT_CODE<-as.factor(newrefineddata$CREDIT_DEBIT_CODE)
newrefineddata$TXN_TYPE<-as.factor(newrefineddata$TXN_TYPE)
newrefineddata$SCOPE<-as.factor(newrefineddata$SCOPE)
newrefineddata$INSTRUMENT<-as.factor(newrefineddata$INSTRUMENT)
newrefineddata$REGION<-as.factor(newrefineddata$REGION)
newrefineddata$BASE_CURRENCY_AMOUNT<-as.factor(newrefineddata$BASE_CURRENCY_AMOUNT)
newrefineddata$COUNTRY_OF_ORIGIN<-as.factor(newrefineddata$COUNTRY_OF_ORIGIN)
newrefineddata$SPECIAL_ATTENTION_FLAG<-as.factor(newrefineddata$SPECIAL_ATTENTION_FLAG)
newrefineddata$STATUS<-as.factor(newrefineddata$STATUS)
newrefineddata$SC01_Amount_Exceding_250000__HUF_<-as.factor(newrefineddata$SC01_Amount_Exceding_250000__HUF_)
str(newrefineddata)

# recheck missingness
rankmissing<-data.frame(sapply(newrefineddata,function(x) mean(is.na(x))) %>%
                          sort())# gets vers percentage of missing vals and the order
#all good

################################
## Feature Selection - Step 2 ##
################################

# seperate continuous and categorical vars 
# contin = Fisher Score, cat = info value or entropy

contin<-subset(newrefineddata,select=c(NUM_CASES,NUM_ACCOUNTS,CUSTOMER_FOR_DAYS,NEW_ACCOUNT_BALANCE))
categor<-subset(newrefineddata,select=c(SCENARIO,SC01_Amount_Exceding_250000__HUF_, EVENT_MONTH, RAISED_ON, PEP_FLAG, SPECIAL_ATTENTION_FLAG, BASE_CURRENCY_AMOUNT,COUNTRY_OF_RESIDENCE_abbrev, RISK_SCORE_1, BUSINESS_TYPE2, CUSTOMER_STATUS, CUSTOMER_SEGMENT, TIME_PERIOD, CUSTOMER_REGION, RISK_SCORE_2, SAR, COUNTRY_OF_ORIGIN, STATUS, ORIGINAL_ACCOUNT_CURRENCY, Customer_Equals_Account, DISTRICT_OF_BUDAPEST, CREDIT_DEBIT_CODE, TXN_TYPE, INSTRUMENT, SCOPE, REGION))

## Continuous ##

# correlation 
cor(contin) # no major correlations so all is okay

## Categorical ##

#info value/ entropy
infovalue <- gain.ratio(SAR~., categor)
varnames<-data.frame(row.names(infovalue))
infovalue$varnames<-data.frame(row.names(infovalue))
sortedinfogain<-infovalue[order(infovalue$attr_importance,decreasing=TRUE),c(1,2)]
sortedinfogain<-subset(sortedinfogain,select=c(attr_importance))
# we see that the 3 worst vars here are SC01_, raises on, and instrument

# chi square variable importance
weights <- chi.squared(SAR~., categor)
varnames<-data.frame(row.names(weights))
weights$varnames<-data.frame(row.names(weights))
sortedchi<-weights[order(weights$attr_importance,decreasing=TRUE),c(1,2)]
sortedchi<-subset(sortedinfogain,select=c(attr_importance))
# same as above

##############################################################################################
## Exploratory Stuff - Investigate effect of categoricals on response (to see if important) ##
##############################################################################################

## How good are the categoricals as predictors? ##

ggplot(newrefineddata, aes(x = SCENARIO, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#SC28 pretty good but not so many obs

ggplot(newrefineddata, aes(x = SC01_Amount_Exceding_250000__HUF_, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#really bad as predictor - can remove I think

ggplot(newrefineddata, aes(x = EVENT_MONTH, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# pretty okay

ggplot(newrefineddata, aes(x = RAISED_ON, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# realy bad

ggplot(newrefineddata, aes(x = PEP_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not so informative 

ggplot(newrefineddata, aes(x = SPECIAL_ATTENTION_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
datarep<-newrefineddata[which(newrefineddata$SPECIAL_ATTENTION_FLAG=="Y"),]
# exactly the same Y's and N's as PEP flag in this subset data set but not in original, therefore remove one from this analysis (remove SPEcial_attention_flag)

ggplot(newrefineddata, aes(x = BASE_CURRENCY_AMOUNT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# really bad var

ggplot(newrefineddata, aes(x = COUNTRY_OF_RESIDENCE_abbrev, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# might be a good idea to group some groups here with very few obs into one group! - good var

ggplot(newrefineddata, aes(x = RISK_SCORE_1, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# quite informative - unsure to include both risk score 1 and country of res

ggplot(newrefineddata, aes(x = BUSINESS_TYPE2, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# good

ggplot(newrefineddata, aes(x = CUSTOMER_STATUS, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# kind of informative

ggplot(newrefineddata, aes(x = CUSTOMER_SEGMENT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#very good

ggplot(newrefineddata, aes(x = TIME_PERIOD, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# good var 

ggplot(newrefineddata, aes(x = CUSTOMER_REGION, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# quite good 

ggplot(newrefineddata, aes(x = RISK_SCORE_2, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not so good

ggplot(newrefineddata, aes(x = COUNTRY_OF_ORIGIN, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#pretty good

ggplot(newrefineddata, aes(x = STATUS, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# good

ggplot(newrefineddata, aes(x = ORIGINAL_ACCOUNT_CURRENCY, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#good

ggplot(newrefineddata, aes(x = Customer_Equals_Account, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not so great

ggplot(newrefineddata, aes(x = DISTRICT_OF_BUDAPEST, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#prett okay

ggplot(newrefineddata, aes(x = CREDIT_DEBIT_CODE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not very good

ggplot(newrefineddata, aes(x = TXN_TYPE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not great

ggplot(newrefineddata, aes(x = INSTRUMENT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#bad

ggplot(newrefineddata, aes(x = SCOPE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# okay

ggplot(newrefineddata, aes(x = REGION, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
# bad 

# so remove special attention flag but not the bad vars - remember they might be good when we split data set again into different bottom up clusters!

######################################################
## Standardising (categorizing) variables (attempt) ##
######################################################

#convert categoricals to ridit score (less categoricals to deal with)
tab1<-table(newrefineddata$SAR,newrefineddata$SCENARIO)
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

hi<-ridit(tab1,g=2,ref="SC01")$MeanRidit

bigdata1$EVENT_MONTH_2<-ridit(tab2,g=2,ref=)
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


## next time ...finish ridit score and try to figure out how to make a var with mean ridit score and also see how much this affects information!
## also do cluster analysis now and decide optimal number of clusters, then build model for each cluster!


