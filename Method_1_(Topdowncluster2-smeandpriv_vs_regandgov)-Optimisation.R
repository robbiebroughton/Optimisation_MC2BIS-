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
data<-fread("Hungarian_Data_updated_col_names_CSV_new_acc_bal_incl.csv",na.strings=c(""," ","NA")) # so that it interprets blank spaces as NA's
data<-data[1:57384,]

# Make one new variable - if customer branch and account branch are equal then 1 and if not then 0, otherwise NA
data$Customer_Equals_Account<-ifelse(data$CUSTOMER_BRANCH==data$ACCOUNT_BRANCH,0,1)

data<- subset(data, select=-c(EqualTo,Compound_ID,ALERT_CUSTOMER_SEGMENT,CREDIT_DEBIT,Unique,REASON_FOR_CLOSURE,CUSTOMER_REGION,DESCRIPTION,CREATION_DATE,CREATION_DATE,LAST_UPDATED,EVENT_DATE,AGE_IN_DAYS,ACTION_NUM, TRANSACTION_ID, TXN_TYPE_DESC, ASSIGNED_TO,ASSIGNED_BY,ORIGINAL_CURRENCY_AMOUNT,ORIGINAL_CURRENCY,POSTAL_CODE,BRANCH_ID,BUSINESS_TYPE,COUNTRY_OF_RESIDENCE,ACCOUNT_BALANCE,HOLDING_BANK_NAME,CUSTOMER_BRANCH,ACCOUNT_BRANCH,BRANCH_CODE,CASE_KEY, CASE_IDENTIFIER,NUM_ALERTS,NUM_CUSTOMERS,CASE_CREATION_DATE,INVESTIGATION_LENGTH,NORKOM_SCORE,TRANSACTION_DATE))
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
#Replace in SC16 variable with D and C to check with the main credit debit code variable - we find no extra info from SC16_17_18 var
datanew<-subset(datanew,select=-c(SC16_17_18_Type_Transaction__Credit_Debit_))

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

###########################################################
############ Data Split - Top Down Clustering #############
###########################################################

# Split up the newdata
bigdata<-datanew[which(CUSTOMER_SEGMENT=="GOV" | CUSTOMER_SEGMENT== "SME"),]
smalldata<-datanew[which(CUSTOMER_SEGMENT=="REG"|CUSTOMER_SEGMENT=="PRIV"),]

##################
## Company Data ##
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

refineddata<-subset(bigdata1,select=c(SCENARIO,NEW_ACCOUNT_BALANCE,EVENT_MONTH, RAISED_ON, PEP_FLAG,NEW_ACCOUNT_BALANCE,SPECIAL_ATTENTION_FLAG,COUNTRY_OF_RESIDENCE_abbrev, RISK_SCORE_1,BUSINESS_TYPE2, CUSTOMER_STATUS, NUM_CASES, NUM_ACCOUNTS, CUSTOMER_FOR_DAYS,CUSTOMER_SEGMENT, TIME_PERIOD, CUSTOMER_REGION, RISK_SCORE_2, SC01_Amount_Exceding_250000__HUF_, SAR, COUNTRY_OF_ORIGIN,STATUS,ORIGINAL_ACCOUNT_CURRENCY,Customer_Equals_Account))
# note that now I have removed some seemingly good variables (because they have lots of NA's) but if they NA's are not actually missing values then I can reintroduce these guys possibly, revisit later
# possible vars to consider include: district_ofbudapest,credit/debitcode,txn_type,scope,region,base_currency_amount
# it looks as though all these have same amount of missing vals (most likely because the NA's are actually valid, instances are not transactions but just inquiries)
# to check, take subset of data set where base_currency_amount is involved and all these vars and they should all match

allsamenas<-subset(bigdata1,select=c(BASE_CURRENCY_AMOUNT,CREDIT_DEBIT_CODE,TXN_TYPE,INSTRUMENT,SCOPE,REGION))
allsamenas1<-allsamenas[complete.cases(allsamenas$BASE_CURRENCY_AMOUNT),]
#conclude all the NA values in these variables are informative as they tell us that these instances are not transactions, so we should convert wherever it says NA to character value
#so put in these extra vars to refined data
#new refined data
newrefineddata<-subset(bigdata1,select=c(SCENARIO,EVENT_MONTH, RAISED_ON, PEP_FLAG, SPECIAL_ATTENTION_FLAG,COUNTRY_OF_RESIDENCE_abbrev, RISK_SCORE_1,BUSINESS_TYPE2, CUSTOMER_STATUS, NUM_CASES, NUM_ACCOUNTS, CUSTOMER_FOR_DAYS,CUSTOMER_SEGMENT, TIME_PERIOD, CUSTOMER_REGION, RISK_SCORE_2, SC01_Amount_Exceding_250000__HUF_, SAR, COUNTRY_OF_ORIGIN,STATUS,ORIGINAL_ACCOUNT_CURRENCY,Customer_Equals_Account,NEW_ACCOUNT_BALANCE,DISTRICT_OF_BUDAPEST,CREDIT_DEBIT_CODE,TXN_TYPE,INSTRUMENT,SCOPE,REGION,BASE_CURRENCY_AMOUNT))

#need to change NA values in the last 7 vars in above data set to character (dont think this is correct - try extract values from description vars)
newrefineddata$DISTRICT_OF_BUDAPEST[is.na(newrefineddata$DISTRICT_OF_BUDAPEST)]<-"OUTSIDE_BUDAPEST"
newrefineddata$CREDIT_DEBIT_CODE[is.na(newrefineddata$CREDIT_DEBIT_CODE)]<-"NOT_A_TRANSACTION"
newrefineddata$TXN_TYPE[is.na(newrefineddata$TXN_TYPE)]<-"NOT_A_TRANSACTION"
newrefineddata$INSTRUMENT[is.na(newrefineddata$INSTRUMENT)]<-"NOT_A_TRANSACTION"
newrefineddata$SCOPE[is.na(newrefineddata$SCOPE)]<-"NOT_A_TRANSACTION"
newrefineddata$REGION[is.na(newrefineddata$REGION)]<-"NOT_A_TRANSACTION"
newrefineddata$BASE_CURRENCY_AMOUNT[is.na(newrefineddata$BASE_CURRENCY_AMOUNT)]<-"NOT_A_TRANSACTION"
                        
#################################
## Dealing with missing values ##
#################################
rankmissing<-data.frame(sapply(newrefineddata,function(x) mean(is.na(x))) %>%
                          sort())# gets vers percentage of missing vals and the order

# Go through each var and the missing vals and decide what to do with each

## Country_of_origin ##
countryorigin<-newrefineddata[which(newrefineddata$COUNTRY_OF_RESIDENCE_abbrev=="HU"),]
countryorig<-countryorigin[which(countryorigin$COUNTRY_OF_ORIGIN=="MAGYARORSZÁG"),]
#so we found there are only 59 out of 10797 obs where country of res is hungary where the country of origin ISNT hungary therefore for my 3 missing vals here I have Hungary as country of res, so i will impute hungary for country or origin for these guys
newrefineddata$COUNTRY_OF_ORIGIN[is.na(newrefineddata$COUNTRY_OF_ORIGIN)]<-"MAGYARORSZÁG"

## STATUS and ORIGINAL_ACC_CURRENCY and CUSTOMER_EQUALS_ACCOUNT and NEW_ACCOUNT_BALANCE ##
# inspect reason for missingness
missing<-subset(newrefineddata,select=c(ORIGINAL_ACCOUNT_CURRENCY,NEW_ACCOUNT_BALANCE,Customer_Equals_Account,STATUS))
missing1<-newrefineddata[which(is.na(newrefineddata$ORIGINAL_ACCOUNT_CURRENCY))]
#so these guys all had alerts based on client but doesnt mean they dont have an account
# i think still impute because these guys have accounts still

imputedata1 <- subset(newrefineddata,select=c(STATUS))
imputedata2 <- subset(newrefineddata,select=c(ORIGINAL_ACCOUNT_CURRENCY))
imputedata3 <- subset(newrefineddata,select=c(Customer_Equals_Account))
imputedata4 <- subset(newrefineddata,select=c(NEW_ACCOUNT_BALANCE))
#account currency and status by mode function defined as below
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
                        
##############################################################################################
## Exploratory Stuff - Investigate effect of categoricals on response (to see if important) ##
##############################################################################################
                        
ggplot(bigdata, aes(x = COUNTRY_OF_ORIGIN, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

######################################################
## Standardising (categorizing) variables (attempt) ##
######################################################
                        
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

################################
## Feature Selection - Step 2 ##
################################
                        
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


                        
         
