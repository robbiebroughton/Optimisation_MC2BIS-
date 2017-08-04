rm(list=ls())

library(ggplot2)
library(GGally)
library(Ridit)
library(sparcl)
library(VIM)
library(readr)
library(translate)
library(FSelector)
library(sm)
library(NbClust)
library(bindrcpp)
library(data.table)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(factoextra)
library(dplyr)
library(car)
library(compare)
library(reshape2)
library(mice)
library(VIF)
library(caret)
library(Boruta)
set.seed(123)
## Ridit Function
ridit_scores=function(y){
  x = vector('numeric')
  for (i in 1:length(y)) {
    
    x[i]=(sum(y[1:i])-sum(y[length(y):(i)]))
  }
  return(x)
}

# Load data properly
setwd("C:/Users/RobbieBroughton/Documents/MC2BIS")
data<-fread("Hungarian_Data_CSV_for_R_updated_col_names_ converted_all_cont_to_num_type_and_filled_missing_values_with_dots.csv",na.strings=c(""," ",".","NA","#N/A")) # so that it interprets blank spaces as NA's
data<-data[1:57383,]

# Make one new variable - if customer branch and account branch are equal then 1 and if not then 0, otherwise NA
data$Customer_Equals_Account<-ifelse(data$CUSTOMER_BRANCH==data$ACCOUNT_BRANCH,0,1)

data<- subset(data, select=-c(EqualTo,SC16_17_18_2nd_party_Country,Compound_ID,ALERT_CUSTOMER_SEGMENT,CREDIT_DEBIT,Unique,REASON_FOR_CLOSURE,DESCRIPTION,CREATION_DATE,LAST_UPDATED,EVENT_DATE,AGE_IN_DAYS,ACTION_NUM, TRANSACTION_ID, TXN_TYPE_DESC, ASSIGNED_TO,ASSIGNED_BY,ORIGINAL_CURRENCY_AMOUNT,ORIGINAL_CURRENCY,POSTAL_CODE,BRANCH_ID,BUSINESS_TYPE,COUNTRY_OF_RESIDENCE,ACCOUNT_BALANCE,HOLDING_BANK_NAME,CUSTOMER_BRANCH,ACCOUNT_BRANCH,BRANCH_CODE,CASE_KEY, CASE_IDENTIFIER,NUM_ALERTS,NUM_CUSTOMERS,CASE_CREATION_DATE,INVESTIGATION_LENGTH,NORKOM_SCORE,TRANSACTION_DATE))
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
newrefineddata<-subset(bigdata1,select=c(SCENARIO,EVENT_MONTH, RAISED_ON, PEP_FLAG, SPECIAL_ATTENTION_FLAG,COUNTRY_OF_RESIDENCE_abbrev, SAR,RISK_SCORE_1,BUSINESS_TYPE2, CUSTOMER_STATUS, NUM_CASES, NUM_ACCOUNTS, CUSTOMER_FOR_DAYS,CUSTOMER_SEGMENT, TIME_PERIOD, CUSTOMER_REGION, RISK_SCORE_2, COUNTRY_OF_ORIGIN,STATUS,ORIGINAL_ACCOUNT_CURRENCY,Customer_Equals_Account,NEW_ACCOUNT_BALANCE,DISTRICT_OF_BUDAPEST,CREDIT_DEBIT_CODE,TXN_TYPE,INSTRUMENT,SCOPE,REGION,BASE_CURRENCY_AMOUNT,SC01_Amount_Exceding_250000__HUF_))

#need to change NA values in the last 6 vars in above data set to character (dont think this is correct - try extract values from description vars)
newrefineddata$DISTRICT_OF_BUDAPEST[is.na(newrefineddata$DISTRICT_OF_BUDAPEST)]<-"OUTSIDE_BUDAPEST"
newrefineddata$CREDIT_DEBIT_CODE[is.na(newrefineddata$CREDIT_DEBIT_CODE)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$TXN_TYPE[is.na(newrefineddata$TXN_TYPE)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$INSTRUMENT[is.na(newrefineddata$INSTRUMENT)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$SCOPE[is.na(newrefineddata$SCOPE)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$REGION[is.na(newrefineddata$REGION)]<-"NOT_A_SINGLE_TRANSACTION"
# because these variables are defined by one transaction so any alert that is alerted because of multiple transactions should be put as not applicable

# deciding on a good binning strategy for BASE_CURRENCY_AMOUNT

bigdat<-bigdata[complete.cases(newrefineddata$BASE_CURRENCY_AMOUNT),]
range(bigdat$BASE_CURRENCY_AMOUNT)
par(mfrow=c(1,1))
hist(newrefineddata$BASE_CURRENCY_AMOUNT,col="blue") #very right skewed - take log to get better look at it 
bigdat$BASE_CURRENCY_AMOUNT<-log(bigdat$BASE_CURRENCY_AMOUNT)
hist(bigdat$BASE_CURRENCY_AMOUNT) # more normal (easier to pick categories - should I use this instead?)
#0 to 10, 10 to 15, 15 to 20, 20 to 25, greater than 25 (5 categories - quite concice)

# dealing with BASE_CURRENCY_AMOUNT variable
newrefineddata$BASE_CURRENCY_AMOUNT<-log(newrefineddata$BASE_CURRENCY_AMOUNT)
colnames(newrefineddata)[29] <- "LOG_BASE_CURRENCY_AMOUNT"
newrefineddata$LOG_BASE_CURRENCY_AMOUNT <- cut(newrefineddata$LOG_BASE_CURRENCY_AMOUNT, c(0, 17, 19, Inf))
newrefineddata$LOG_BASE_CURRENCY_AMOUNT<-as.factor(newrefineddata$LOG_BASE_CURRENCY_AMOUNT)
newrefineddata$LOG_BASE_CURRENCY_AMOUNT=addNA(newrefineddata$LOG_BASE_CURRENCY_AMOUNT)
newrefineddata$LOG_BASE_CURRENCY_AMOUNT<-as.character(newrefineddata$LOG_BASE_CURRENCY_AMOUNT)
newrefineddata$LOG_BASE_CURRENCY_AMOUNT[is.na(newrefineddata$LOG_BASE_CURRENCY_AMOUNT)]<-"NOT_A_SINGLE_TRANSACTION"
newrefineddata$LOG_BASE_CURRENCY_AMOUNT<-as.factor(newrefineddata$LOG_BASE_CURRENCY_AMOUNT)
str(newrefineddata$LOG_BASE_CURRENCY_AMOUNT)
table(newrefineddata$SAR,newrefineddata$LOG_BASE_CURRENCY_AMOUNT)
# I checked was it now an informative variable and its not great but, you never know it might be good after the split into dif clusters, for now leave it 

#################################
## Dealing with missing values ##
#################################

rankmissing<-data.frame(sapply(newrefineddata,function(x) mean(is.na(x))) %>%
                          sort())# gets vers percentage of missing vals and the order

# Go through each var and the missing vals and decide what to do with each

## Country_of_origin ##
countryorigin<-newrefineddata[which(newrefineddata$COUNTRY_OF_RESIDENCE_abbrev=="HU"),]
countryres<-newrefineddata[which(is.na(newrefineddata$COUNTRY_OF_RESIDENCE_abbrev)),]
countryres2<-subset(countryres,select=c(COUNTRY_OF_ORIGIN,COUNTRY_OF_RESIDENCE_abbrev))
# since all of these NA's have acc currency of HUF's and also customer_region is in Hungary so put HU for all missing vals
countryorig<-countryorigin[which(countryorigin$COUNTRY_OF_ORIGIN=="MAGYARORSZÁG"),]
#so we found there are only 59 out of 10797 obs where country of res is hungary where the country of origin ISNT hungary therefore for my 3 missing vals here I have Hungary as country of res, so i will impute hungary for country or origin for these guys
newrefineddata$COUNTRY_OF_ORIGIN[is.na(newrefineddata$COUNTRY_OF_ORIGIN)]<-"MAGYARORSZÁG"

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
# imputed by 3159960
newrefineddata[is.na(newrefineddata$STATUS) == TRUE, "STATUS"]  <- Mode(newrefineddata[['STATUS']])
# imputed by "OPENED"
newrefineddata[is.na(newrefineddata$ORIGINAL_ACCOUNT_CURRENCY) == TRUE, "ORIGINAL_ACCOUNT_CURRENCY"]  <- Mode(newrefineddata[['ORIGINAL_ACCOUNT_CURRENCY']])
# imputed by "HUF"
newrefineddata[is.na(newrefineddata$Customer_Equals_Account) == TRUE, "Customer_Equals_Account"]  <- Mode(newrefineddata[['Customer_Equals_Account']])
# imputed by "0"

# impute Customer_region (first to see what pattern is there in the NA's)
custreg<-newrefineddata[which(is.na(newrefineddata$CUSTOMER_REGION)),] #call these guys "outside Hungary" except for the guys who are inside inside impute these as mode
custregHU<-custreg[which(custreg$COUNTRY_OF_RESIDENCE_abbrev=="HU"),]

#impute customer_region where country of res is HU as mode of Customer_region 
newrefineddata[is.na(newrefineddata$CUSTOMER_REGION) == TRUE & (newrefineddata$COUNTRY_OF_RESIDENCE_abbrev=="HU"), "CUSTOMER_REGION"]  <- Mode(newrefineddata[['CUSTOMER_REGION']])
Mode(newrefineddata[['CUSTOMER_REGION']]) # imputed as Budapest

# call the other NA's "OUTSIDE_HUNGARY"
newrefineddata$CUSTOMER_REGION[is.na(newrefineddata$CUSTOMER_REGION)]<-"OUTSIDE_HUNGARY"

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
newrefineddata$LOG_BASE_CURRENCY_AMOUNT<-as.factor(newrefineddata$LOG_BASE_CURRENCY_AMOUNT)
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
## Continuous ##

# correlation 
cor(contin) # no major correlations so all is okay

#info value/ entropy
infovalue <- gain.ratio(SAR~., newrefineddata)
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
#SC11 pretty good but not so many obs

ggplot(newrefineddata, aes(x = SC01_Amount_Exceding_250000__HUF_, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#really bad as predictor

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

newrefineddata<-subset(newrefineddata,select=-c(SPECIAL_ATTENTION_FLAG))

###########################################################
## Standardising (categorizing) variables (Ridit Scores) ##
###########################################################

# Do it even for binary - can then use in clustering and shouldnt technically change relationship of the var

# SCENARIO
tab1=prop.table(table(newrefineddata$SAR,newrefineddata$SCENARIO),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab1) = c("Var1", "SCENARIO", "Freq_SAR")  

tab1_2=prop.table(table(newrefineddata$SCENARIO)) %>%
  data.frame()
colnames(tab1_2) = c('SCENARIO', 'Freq')
joined_tab1=left_join(tab1,tab1_2, by = 'SCENARIO')

ridit_tab1=ridit_scores(joined_tab1$Freq) %>% data.frame()
colnames(ridit_tab1)='Ridit_Scores'
ridit_tab1=bind_cols(joined_tab1,ridit_tab1) %>% select(c('SCENARIO','Ridit_Scores'))
colnames(ridit_tab1)=c('SCENARIO','RS_SCENARIO')
newrefineddata=left_join(newrefineddata,ridit_tab1,by = 'SCENARIO')
newrefineddata<-subset(newrefineddata,select=-c(SCENARIO))# remove original

# EVENT_MONTH
tab2=prop.table(table(newrefineddata$SAR,newrefineddata$EVENT_MONTH),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab2) = c("Var1", "EVENT_MONTH", "Freq_SAR")  

tab2_2=prop.table(table(newrefineddata$EVENT_MONTH)) %>%
  data.frame()
colnames(tab2_2) = c('EVENT_MONTH', 'Freq')
joined_tab2=left_join(tab2,tab2_2, by = 'EVENT_MONTH')

ridit_tab2=ridit_scores(joined_tab2$Freq) %>% data.frame()
colnames(ridit_tab2)='Ridit_Scores'
ridit_tab2=bind_cols(joined_tab2,ridit_tab2) %>% select(c('EVENT_MONTH','Ridit_Scores'))
colnames(ridit_tab2)=c('EVENT_MONTH','RS_EVENT_MONTH')
newrefineddata=left_join(newrefineddata,ridit_tab2,by = 'EVENT_MONTH')
newrefineddata<-subset(newrefineddata,select=-c(EVENT_MONTH))# remove original

# RAISED_ON
tab3=prop.table(table(newrefineddata$SAR,newrefineddata$RAISED_ON),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab3) = c("Var1", "RAISED_ON", "Freq_SAR")  

tab3_2=prop.table(table(newrefineddata$RAISED_ON)) %>%
  data.frame()
colnames(tab3_2) = c('RAISED_ON', 'Freq')
joined_tab3=left_join(tab3,tab3_2, by = 'RAISED_ON')

ridit_tab3=ridit_scores(joined_tab3$Freq) %>% data.frame()
colnames(ridit_tab3)='Ridit_Scores'
ridit_tab3=bind_cols(joined_tab3,ridit_tab3) %>% select(c('RAISED_ON','Ridit_Scores'))
colnames(ridit_tab3)=c('RAISED_ON','RS_RAISED_ON')
newrefineddata=left_join(newrefineddata,ridit_tab3,by = 'RAISED_ON')
newrefineddata<-subset(newrefineddata,select=-c(RAISED_ON))# remove original

# PEP_FLAG
tab4=prop.table(table(newrefineddata$SAR,newrefineddata$PEP_FLAG),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab4) = c("Var1", "PEP_FLAG", "Freq_SAR")  

tab4_2=prop.table(table(newrefineddata$PEP_FLAG)) %>%
  data.frame()
colnames(tab4_2) = c('PEP_FLAG', 'Freq')
joined_tab4=left_join(tab4,tab4_2, by = 'PEP_FLAG')

ridit_tab4=ridit_scores(joined_tab4$Freq) %>% data.frame()
colnames(ridit_tab4)='Ridit_Scores'
ridit_tab4=bind_cols(joined_tab4,ridit_tab4) %>% select(c('PEP_FLAG','Ridit_Scores'))
colnames(ridit_tab4)=c('PEP_FLAG','RS_PEP_FLAG')
newrefineddata=left_join(newrefineddata,ridit_tab4,by = 'PEP_FLAG')
newrefineddata<-subset(newrefineddata,select=-c(PEP_FLAG))# remove original


# COUNTRY_OF_RESIDENCE_abbrev
tab5=prop.table(table(newrefineddata$SAR,newrefineddata$COUNTRY_OF_RESIDENCE_abbrev),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab5) = c("Var1", "COUNTRY_OF_RESIDENCE_abbrev", "Freq_SAR")  

tab5_2=prop.table(table(newrefineddata$COUNTRY_OF_RESIDENCE_abbrev)) %>%
  data.frame()
colnames(tab5_2) = c('COUNTRY_OF_RESIDENCE_abbrev', 'Freq')
joined_tab5=left_join(tab5,tab5_2, by = 'COUNTRY_OF_RESIDENCE_abbrev')

ridit_tab5=ridit_scores(joined_tab5$Freq) %>% data.frame()
colnames(ridit_tab5)='Ridit_Scores'
ridit_tab5=bind_cols(joined_tab5,ridit_tab5) %>% select(c('COUNTRY_OF_RESIDENCE_abbrev','Ridit_Scores'))
colnames(ridit_tab5)=c('COUNTRY_OF_RESIDENCE_abbrev','RS_COUNTRY_OF_RESIDENCE_abbrev')
newrefineddata=left_join(newrefineddata,ridit_tab5,by = 'COUNTRY_OF_RESIDENCE_abbrev')
newrefineddata<-subset(newrefineddata,select=-c(COUNTRY_OF_RESIDENCE_abbrev))# remove original

# BUSINESS_TYPE2
tab6=prop.table(table(newrefineddata$SAR,newrefineddata$BUSINESS_TYPE2),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab6) = c("Var1", "BUSINESS_TYPE2", "Freq_SAR")  

tab6_2=prop.table(table(newrefineddata$BUSINESS_TYPE2)) %>%
  data.frame()
colnames(tab6_2) = c('BUSINESS_TYPE2', 'Freq')
joined_tab6=left_join(tab6,tab6_2, by = 'BUSINESS_TYPE2')

ridit_tab6=ridit_scores(joined_tab6$Freq) %>% data.frame()
colnames(ridit_tab6)='Ridit_Scores'
ridit_tab6=bind_cols(joined_tab6,ridit_tab6) %>% select(c('BUSINESS_TYPE2','Ridit_Scores'))
colnames(ridit_tab6)=c('BUSINESS_TYPE2','RS_BUSINESS_TYPE2')
newrefineddata=left_join(newrefineddata,ridit_tab6,by = 'BUSINESS_TYPE2')
newrefineddata<-subset(newrefineddata,select=-c(BUSINESS_TYPE2))# remove original

# CUSTOMER_STATUS
tab7=prop.table(table(newrefineddata$SAR,newrefineddata$CUSTOMER_STATUS),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab7) = c("Var1", "CUSTOMER_STATUS", "Freq_SAR")  

tab7_2=prop.table(table(newrefineddata$CUSTOMER_STATUS)) %>%
  data.frame()
colnames(tab7_2) = c('CUSTOMER_STATUS', 'Freq')
joined_tab7=left_join(tab7,tab7_2, by = 'CUSTOMER_STATUS')

ridit_tab7=ridit_scores(joined_tab7$Freq) %>% data.frame()
colnames(ridit_tab7)='Ridit_Scores'
ridit_tab7=bind_cols(joined_tab7,ridit_tab7) %>% select(c('CUSTOMER_STATUS','Ridit_Scores'))
colnames(ridit_tab7)=c('CUSTOMER_STATUS','RS_CUSTOMER_STATUS')
newrefineddata=left_join(newrefineddata,ridit_tab7,by = 'CUSTOMER_STATUS')
newrefineddata<-subset(newrefineddata,select=-c(CUSTOMER_STATUS))# remove original

# CUSTOMER_SEGMENT
tab8=prop.table(table(newrefineddata$SAR,newrefineddata$CUSTOMER_SEGMENT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab8) = c("Var1", "CUSTOMER_SEGMENT", "Freq_SAR")  

tab8_2=prop.table(table(newrefineddata$CUSTOMER_SEGMENT)) %>%
  data.frame()
colnames(tab8_2) = c('CUSTOMER_SEGMENT', 'Freq')
joined_tab8=left_join(tab8,tab8_2, by = 'CUSTOMER_SEGMENT')

ridit_tab8=ridit_scores(joined_tab8$Freq) %>% data.frame()
colnames(ridit_tab8)='Ridit_Scores'
ridit_tab8=bind_cols(joined_tab8,ridit_tab8) %>% select(c('CUSTOMER_SEGMENT','Ridit_Scores'))
colnames(ridit_tab8)=c('CUSTOMER_SEGMENT','RS_CUSTOMER_SEGMENT')
newrefineddata=left_join(newrefineddata,ridit_tab8,by = 'CUSTOMER_SEGMENT')
newrefineddata<-subset(newrefineddata,select=-c(CUSTOMER_SEGMENT))# remove original

# TIME_PERIOD
tab9=prop.table(table(newrefineddata$SAR,newrefineddata$TIME_PERIOD),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab9) = c("Var1", "TIME_PERIOD", "Freq_SAR")  

tab9_2=prop.table(table(newrefineddata$TIME_PERIOD)) %>%
  data.frame()
colnames(tab9_2) = c('TIME_PERIOD', 'Freq')
joined_tab9=left_join(tab9,tab9_2, by = 'TIME_PERIOD')

ridit_tab9=ridit_scores(joined_tab9$Freq) %>% data.frame()
colnames(ridit_tab9)='Ridit_Scores'
ridit_tab9=bind_cols(joined_tab9,ridit_tab9) %>% select(c('TIME_PERIOD','Ridit_Scores'))
colnames(ridit_tab9)=c('TIME_PERIOD','RS_TIME_PERIOD')
newrefineddata=left_join(newrefineddata,ridit_tab9,by = 'TIME_PERIOD')
newrefineddata<-subset(newrefineddata,select=-c(TIME_PERIOD))# remove original

# CUSTOMER_REGION
tab10=prop.table(table(newrefineddata$SAR,newrefineddata$CUSTOMER_REGION),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab10) = c("Var1", "CUSTOMER_REGION", "Freq_SAR")  

tab10_2=prop.table(table(newrefineddata$CUSTOMER_REGION)) %>%
  data.frame()
colnames(tab10_2) = c('CUSTOMER_REGION', 'Freq')
joined_tab10=left_join(tab10,tab10_2, by = 'CUSTOMER_REGION')

ridit_tab10=ridit_scores(joined_tab10$Freq) %>% data.frame()
colnames(ridit_tab10)='Ridit_Scores'
ridit_tab10=bind_cols(joined_tab10,ridit_tab10) %>% select(c('CUSTOMER_REGION','Ridit_Scores'))
colnames(ridit_tab10)=c('CUSTOMER_REGION','RS_CUSTOMER_REGION')
newrefineddata=left_join(newrefineddata,ridit_tab10,by = 'CUSTOMER_REGION')
newrefineddata<-subset(newrefineddata,select=-c(CUSTOMER_REGION))# remove original

# COUNTRY_OF_ORIGIN
tab11=prop.table(table(newrefineddata$SAR,newrefineddata$COUNTRY_OF_ORIGIN),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab11) = c("Var1", "COUNTRY_OF_ORIGIN", "Freq_SAR")  

tab11_2=prop.table(table(newrefineddata$COUNTRY_OF_ORIGIN)) %>%
  data.frame()
colnames(tab11_2) = c('COUNTRY_OF_ORIGIN', 'Freq')
joined_tab11=left_join(tab11,tab11_2, by = 'COUNTRY_OF_ORIGIN')

ridit_tab11=ridit_scores(joined_tab11$Freq) %>% data.frame()
colnames(ridit_tab11)='Ridit_Scores'
ridit_tab11=bind_cols(joined_tab11,ridit_tab11) %>% select(c('COUNTRY_OF_ORIGIN','Ridit_Scores'))
colnames(ridit_tab11)=c('COUNTRY_OF_ORIGIN','RS_COUNTRY_OF_ORIGIN')
newrefineddata=left_join(newrefineddata,ridit_tab11,by = 'COUNTRY_OF_ORIGIN')
newrefineddata<-subset(newrefineddata,select=-c(COUNTRY_OF_ORIGIN))# remove original

# STATUS
tab12=prop.table(table(newrefineddata$SAR,newrefineddata$STATUS),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab12) = c("Var1", "STATUS", "Freq_SAR")  

tab12_2=prop.table(table(newrefineddata$STATUS)) %>%
  data.frame()
colnames(tab12_2) = c('STATUS', 'Freq')
joined_tab12=left_join(tab12,tab12_2, by = 'STATUS')

ridit_tab12=ridit_scores(joined_tab12$Freq) %>% data.frame()
colnames(ridit_tab12)='Ridit_Scores'
ridit_tab12=bind_cols(joined_tab12,ridit_tab12) %>% select(c('STATUS','Ridit_Scores'))
colnames(ridit_tab12)=c('STATUS','RS_STATUS')
newrefineddata=left_join(newrefineddata,ridit_tab12,by = 'STATUS')
newrefineddata<-subset(newrefineddata,select=-c(STATUS))# remove original

# ORIGINAL_ACCOUNT_CURRENCY
tab13=prop.table(table(newrefineddata$SAR,newrefineddata$ORIGINAL_ACCOUNT_CURRENCY),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab13) = c("Var1", "ORIGINAL_ACCOUNT_CURRENCY", "Freq_SAR")  

tab13_2=prop.table(table(newrefineddata$ORIGINAL_ACCOUNT_CURRENCY)) %>%
  data.frame()
colnames(tab13_2) = c('ORIGINAL_ACCOUNT_CURRENCY', 'Freq')
joined_tab13=left_join(tab13,tab13_2, by = 'ORIGINAL_ACCOUNT_CURRENCY')

ridit_tab13=ridit_scores(joined_tab13$Freq) %>% data.frame()
colnames(ridit_tab13)='Ridit_Scores'
ridit_tab13=bind_cols(joined_tab13,ridit_tab13) %>% select(c('ORIGINAL_ACCOUNT_CURRENCY','Ridit_Scores'))
colnames(ridit_tab13)=c('ORIGINAL_ACCOUNT_CURRENCY','RS_ORIGINAL_ACCOUNT_CURRENCY')
newrefineddata=left_join(newrefineddata,ridit_tab13,by = 'ORIGINAL_ACCOUNT_CURRENCY')
newrefineddata<-subset(newrefineddata,select=-c(ORIGINAL_ACCOUNT_CURRENCY))# remove original

# CUSTOMER_EQUALS_ACCOUNT
tab14=prop.table(table(newrefineddata$SAR,newrefineddata$Customer_Equals_Account),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab14) = c("Var1", "Customer_Equals_Account", "Freq_SAR")  

tab14_2=prop.table(table(newrefineddata$Customer_Equals_Account)) %>%
  data.frame()
colnames(tab14_2) = c('Customer_Equals_Account', 'Freq')
joined_tab14=left_join(tab14,tab14_2, by = 'Customer_Equals_Account')

ridit_tab14=ridit_scores(joined_tab14$Freq) %>% data.frame()
colnames(ridit_tab14)='Ridit_Scores'
ridit_tab14=bind_cols(joined_tab14,ridit_tab14) %>% select(c('Customer_Equals_Account','Ridit_Scores'))
colnames(ridit_tab14)=c('Customer_Equals_Account','RS_Customer_Equals_Account')
newrefineddata=left_join(newrefineddata,ridit_tab14,by = 'Customer_Equals_Account')
newrefineddata<-subset(newrefineddata,select=-c(Customer_Equals_Account))# remove original

# DISTRICT_OF_BUDAPEST
tab15=prop.table(table(newrefineddata$DISTRICT_OF_BUDAPEST,newrefineddata$DISTRICT_OF_BUDAPEST),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab15) = c("Var1", "DISTRICT_OF_BUDAPEST", "Freq_SAR")  

tab15_2=prop.table(table(newrefineddata$DISTRICT_OF_BUDAPEST)) %>%
  data.frame()
colnames(tab15_2) = c('DISTRICT_OF_BUDAPEST', 'Freq')
joined_tab15=left_join(tab15,tab15_2, by = 'DISTRICT_OF_BUDAPEST')

ridit_tab15=ridit_scores(joined_tab15$Freq) %>% data.frame()
colnames(ridit_tab15)='Ridit_Scores'
ridit_tab15=bind_cols(joined_tab15,ridit_tab15) %>% select(c('DISTRICT_OF_BUDAPEST','Ridit_Scores'))
colnames(ridit_tab15)=c('DISTRICT_OF_BUDAPEST','RS_DISTRICT_OF_BUDAPEST')
newrefineddata=left_join(newrefineddata,ridit_tab15,by = 'DISTRICT_OF_BUDAPEST')
newrefineddata<-subset(newrefineddata,select=-c(DISTRICT_OF_BUDAPEST))# remove original

# CREDI_DEBIT_CODE
tab16=prop.table(table(newrefineddata$SAR,newrefineddata$CREDIT_DEBIT_CODE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab16) = c("Var1", "CREDIT_DEBIT_CODE", "Freq_SAR")  

tab16_2=prop.table(table(newrefineddata$CREDIT_DEBIT_CODE)) %>%
  data.frame()
colnames(tab16_2) = c('CREDIT_DEBIT_CODE', 'Freq')
joined_tab16=left_join(tab16,tab16_2, by = 'CREDIT_DEBIT_CODE')

ridit_tab16=ridit_scores(joined_tab16$Freq) %>% data.frame()
colnames(ridit_tab16)='Ridit_Scores'
ridit_tab16=bind_cols(joined_tab16,ridit_tab16) %>% select(c('CREDIT_DEBIT_CODE','Ridit_Scores'))
colnames(ridit_tab16)=c('CREDIT_DEBIT_CODE','RS_CREDIT_DEBIT_CODE')
newrefineddata=left_join(newrefineddata,ridit_tab16,by = 'CREDIT_DEBIT_CODE')
newrefineddata<-subset(newrefineddata,select=-c(CREDIT_DEBIT_CODE))# remove original

# TXN_TYPE
tab17=prop.table(table(newrefineddata$SAR,newrefineddata$TXN_TYPE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab17) = c("Var1", "TXN_TYPE", "Freq_SAR")  

tab17_2=prop.table(table(newrefineddata$TXN_TYPE)) %>%
  data.frame()
colnames(tab17_2) = c('TXN_TYPE', 'Freq')
joined_tab17=left_join(tab17,tab17_2, by = 'TXN_TYPE')

ridit_tab17=ridit_scores(joined_tab17$Freq) %>% data.frame()
colnames(ridit_tab17)='Ridit_Scores'
ridit_tab17=bind_cols(joined_tab17,ridit_tab17) %>% select(c('TXN_TYPE','Ridit_Scores'))
colnames(ridit_tab17)=c('TXN_TYPE','RS_TXN_TYPE')
newrefineddata=left_join(newrefineddata,ridit_tab17,by = 'TXN_TYPE')
newrefineddata<-subset(newrefineddata,select=-c(TXN_TYPE))# remove original

# INSTRUMENT
tab18=prop.table(table(newrefineddata$SAR,newrefineddata$INSTRUMENT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab18) = c("Var1", "INSTRUMENT", "Freq_SAR")  

tab18_2=prop.table(table(newrefineddata$INSTRUMENT)) %>%
  data.frame()
colnames(tab18_2) = c('INSTRUMENT', 'Freq')
joined_tab18=left_join(tab18,tab18_2, by = 'INSTRUMENT')

ridit_tab18=ridit_scores(joined_tab18$Freq) %>% data.frame()
colnames(ridit_tab18)='Ridit_Scores'
ridit_tab18=bind_cols(joined_tab18,ridit_tab18) %>% select(c('INSTRUMENT','Ridit_Scores'))
colnames(ridit_tab18)=c('INSTRUMENT','RS_INSTRUMENT')
newrefineddata=left_join(newrefineddata,ridit_tab18,by = 'INSTRUMENT')
newrefineddata<-subset(newrefineddata,select=-c(INSTRUMENT))# remove original

# SCOPE
tab19=prop.table(table(newrefineddata$SAR,newrefineddata$SCOPE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab19) = c("Var1", "SCOPE", "Freq_SAR")  

tab19_2=prop.table(table(newrefineddata$SCOPE)) %>%
  data.frame()
colnames(tab19_2) = c('SCOPE', 'Freq')
joined_tab19=left_join(tab19,tab19_2, by = 'SCOPE')

ridit_tab19=ridit_scores(joined_tab19$Freq) %>% data.frame()
colnames(ridit_tab19)='Ridit_Scores'
ridit_tab19=bind_cols(joined_tab19,ridit_tab19) %>% select(c('SCOPE','Ridit_Scores'))
colnames(ridit_tab19)=c('SCOPE','RS_SCOPE')
newrefineddata=left_join(newrefineddata,ridit_tab19,by = 'SCOPE')
newrefineddata<-subset(newrefineddata,select=-c(SCOPE))# remove original

# REGION
tab20=prop.table(table(newrefineddata$SAR,newrefineddata$REGION),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab20) = c("Var1", "REGION", "Freq_SAR")  

tab20_2=prop.table(table(newrefineddata$REGION)) %>%
  data.frame()
colnames(tab20_2) = c('REGION', 'Freq')
joined_tab20=left_join(tab20,tab20_2, by = 'REGION')

ridit_tab20=ridit_scores(joined_tab20$Freq) %>% data.frame()
colnames(ridit_tab20)='Ridit_Scores'
ridit_tab20=bind_cols(joined_tab20,ridit_tab20) %>% select(c('REGION','Ridit_Scores'))
colnames(ridit_tab20)=c('REGION','RS_REGION')
newrefineddata=left_join(newrefineddata,ridit_tab20,by = 'REGION')
newrefineddata<-subset(newrefineddata,select=-c(REGION))# remove original

# LOG_BASE_CURRENCY_AMOUNT
tab21=prop.table(table(newrefineddata$SAR,newrefineddata$LOG_BASE_CURRENCY_AMOUNT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab21) = c("Var1", "LOG_BASE_CURRENCY_AMOUNT", "Freq_SAR")  

tab21_2=prop.table(table(newrefineddata$LOG_BASE_CURRENCY_AMOUNT)) %>%
  data.frame()
colnames(tab21_2) = c('LOG_BASE_CURRENCY_AMOUNT', 'Freq')
joined_tab21=left_join(tab21,tab21_2, by = 'LOG_BASE_CURRENCY_AMOUNT')

ridit_tab21=ridit_scores(joined_tab21$Freq) %>% data.frame()
colnames(ridit_tab21)='Ridit_Scores'
ridit_tab21=bind_cols(joined_tab21,ridit_tab21) %>% select(c('LOG_BASE_CURRENCY_AMOUNT','Ridit_Scores'))
colnames(ridit_tab21)=c('LOG_BASE_CURRENCY_AMOUNT','RS_LOG_BASE_CURRENCY_AMOUNT')
newrefineddata=left_join(newrefineddata,ridit_tab21,by = 'LOG_BASE_CURRENCY_AMOUNT')
newrefineddata<-subset(newrefineddata,select=-c(LOG_BASE_CURRENCY_AMOUNT))# remove original

# SC01
tab22=prop.table(table(newrefineddata$SAR,newrefineddata$SC01_Amount_Exceding_250000__HUF_),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab22) = c("Var1", "SC01_Amount_Exceding_250000__HUF_", "Freq_SAR")  

tab22_2=prop.table(table(newrefineddata$SC01_Amount_Exceding_250000__HUF_)) %>%
  data.frame()
colnames(tab22_2) = c('SC01_Amount_Exceding_250000__HUF_', 'Freq')
joined_tab22=left_join(tab22,tab22_2, by = 'SC01_Amount_Exceding_250000__HUF_')

ridit_tab22=ridit_scores(joined_tab22$Freq) %>% data.frame()
colnames(ridit_tab22)='Ridit_Scores'
ridit_tab22=bind_cols(joined_tab22,ridit_tab22) %>% select(c('SC01_Amount_Exceding_250000__HUF_','Ridit_Scores'))
colnames(ridit_tab22)=c('SC01_Amount_Exceding_250000__HUF_','RS_SC01_Amount_Exceding_250000__HUF_')
newrefineddata=left_join(newrefineddata,ridit_tab22,by = 'SC01_Amount_Exceding_250000__HUF_')
newrefineddata<-subset(newrefineddata,select=-c(SC01_Amount_Exceding_250000__HUF_))# remove original

str(newrefineddata)

# should I use RISK_SCORE_1 and RISK_SCORE_2?
# note that if second party country is missing there is still a value for risk score 2. how is this ?
# find out info off bharat and decide after

## Categorical ##

#info value/ entropy
infovalue <- gain.ratio(SAR~., newrefineddata)
varnames<-data.frame(row.names(infovalue))
infovalue$varnames<-data.frame(row.names(infovalue))
sortedinfogain2<-infovalue[order(infovalue$attr_importance,decreasing=TRUE),c(1,2)]
sortedinfogain2<-subset(sortedinfogain2,select=c(attr_importance))

pairs(newrefineddata)

######################################
######## Bottom Up Clustering ########
######################################

mrefined_num_per=select_if(mrefined_per1, is.numeric)
mrefined_num_per=scale(mrefined_num_per) %>% data.frame() #scale all variables before clustering by Euclidean dist

# K=5

cl5_per=clara(mrefined_num_per,5, samples = 50, rngR = T) #sampsize = 40 + 2*5= 50
cluster_df=mrefined_num_per
cluster_df$labels_k5=cl5_per$clustering
ggplot(cluster_df, aes(x = as.factor(cluster_df$labels_k5), fill = mrefined_per1$SAR)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))


#so 5 clusters have decreasing values of SAR

######trials###########
#try with higher sampling size

cl5_per2=clara(mrefined_num_per,5, samples = 50, sampsize = 250,rngR = T) #sampsize = 40 + 2*5= 50
cluster_df2=mrefined_num_per
cluster_df2$labels_k5=cl5_per2$clustering
ggplot(cluster_df2, aes(x = as.factor(cluster_df2$labels_k5), fill = mrefined_per1$SAR)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
###########trials didnt work###########

summary(cl5_per)

cl5_per$clusinfo

cluster_df_tNA<-data.frame(t(na.omit(t(cluster_df))))
fviz_cluster(cl5_per,stand = FALSE, geom = "point",
             ellipse.type = "norm")
cl5_per$medoids
summary(cl5_per)
summary(silhouette(cl5_per))

# K=4
cl4_per=clara(mrefined_num_per, 4 , samples = 50, rngR = T)
cluster_df$labels_k4=cl4_per$clustering
ggplot(cluster_df, aes(x = as.factor(cluster_df$labels_k4), fill = mrefined_per1$SAR)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
fviz_cluster(cl4_per,stand = FALSE, geom = "point",
             frame.type = "norm")
cl4_per$clusinfo
summary(cl4_per)
summary(silhouette(cl4_per))

# CLARA - K=3
cl3_per=clara(mrefined_num_per, 3 , samples = 50, rngR = T)
cluster_df$labels_k3=cl3_per$clustering
ggplot(cluster_df, aes(x = as.factor(cluster_df$labels_k3), fill = mrefined_per1$SAR)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
cl3_per$clusinfo
profile_cl3=cl3_per$medoids

summary(cl3_per)
summary(silhouette(cl3_per))
fviz_cluster(cl3_per,stand = FALSE, geom = "point",
             frame.type = "norm")

# Kmeans Sparse 


#

table_k3 = table(mrefined_per1$SAR,cluster_df$labels_k3)
prop.table(table_k3,2)
table_k4 = table(mrefined_per1$SAR,cluster_df$labels_k4)
prop.table(table_k4,2)
table_k5 = table(mrefined_per1$SAR,cluster_df$labels_k5)
prop.table(table_k5,2)
perf_measures_per=list(cl3_per$objective,cl4_per$objective,cl5_per$objective)
perf_measures_per
#5clusters seems optimal by objective function (minimized) 

# !!!!!!! Optimal number of Clusters !!! CAREFUL it takes a lot time
fviz_nbclust(mrefined_num_per, clara, method = "wss", k.max = 6) +
  +   geom_vline(xintercept = 3, linetype = 2)
#check sparse
clusGap(mrefined_num_per, clara, method = "gap_stat", k.max = 4, B = 10) +
  geom_vline(xintercept = 3, linetype = 2)
fviz_gap_stat(gap_stat)



