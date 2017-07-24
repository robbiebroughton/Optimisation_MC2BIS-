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

data<- subset(original_data, select=-c(EqualTo,Compound_ID,Unique,REASON_FOR_CLOSURE,DESCRIPTION,AGE_IN_DAYS,CREATION_DATE,LAST_UPDATED,EVENT_DATE,ACTION_NUM, TRANSACTION_ID, TXN_TYPE_DESC, ASSIGNED_TO,ASSIGNED_BY,ORIGINAL_CURRENCY_AMOUNT,ORIGINAL_CURRENCY,CUSTOMER_REGION,POSTAL_CODE,BRANCH_ID,CUSTOMER_BRANCH,COUNTRY_OF_RESIDENCE,BUSINESS_TYPE,ACCOUNT_BALANCE,CREDIT_DEBIT,HOLDING_BANK_NAME,BRANCH_CODE,ACCOUNT_BRANCH,CASE_KEY, CASE_IDENTIFIER,NUM_ALERTS,NUM_CUSTOMERS,CASE_CREATION_DATE,INVESTIGATION_LENGTH,NORKOM_SCORE,TRANSACTION_DATE))
attach(data)

# Response Variable (SAR)
data$SAR<- ifelse(data$CASE_STATUS=="Reported/Closed",1,ifelse(data$CASE_STATUS=="Closed",0,NA))
data$SAR[with(data,is.na(data$SAR) & data$STATUS_NAME=="Closed" )]=0
data$SAR[with(data,is.na(data$SAR) & data$STATUS_NAME=="Linked Closed" )]=0
data$SAR=as.factor(data$SAR)
data$CUSTOMER_SEGMENT=as.factor(data$CUSTOMER_SEGMENT)
data$EVENT_MONTH=as.factor(data$EVENT_MONTH)
data=data[!is.na(data$SAR),]
data$EVENT_MONTH = factor(data$EVENT_MONTH,levels(data$EVENT_MONTH)[c(5, 4, 8, 1, 9, 7, 6,
                                                                      2, 12, 11, 10, 3)])

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
data_missing=mice(data,m=5,meth='pmm',maxit=0,seed=500)
summary(data_missing)
densityplot(data_missing,~.) #put here a variable of interest #
stripplot(data_missing,pch=20,cex=1.2)

# Feature reduction - Boruta

data2=data[complete.cases(data),]
index_train=createDataPartition(data2$SAR,p=.70)
data2_train=data2[unlist(index_train),]
  #Use a meaningful set of explanatory vars
data2_train=data2_train[c()]
set.seed(123)
boruta.train=Boruta(data2_train$SAR~., data = data2_train, doTrace = 2)
print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)





