+rm(list=ls())
+
+library(ggplot2)
+library(GGally)
+library(VIM)
+library(readr)
+library(translate)
+library(data.table)
+library(dplyr)
+library(car)
+library(compare)
+library(reshape2)
+library(mice)
+library(VIF)
+library(caret)
+library(Boruta)
+
+# Load data properly
+setwd("C:/Users/RobbieBroughton/Documents/MC2BIS")
+data<-fread("Hungarian_Data_Cleaned_shit_updated.csv",na.strings=c(""," ","NA")) # so that it interprets blank spaces as NA's
+original_data<-data[1:57383,]
+
+# Make one new variable - if customer branch and account branch are equal then 1 and if not then 0, otherwise NA
+original_data$Customer_Equals_Account<-ifelse(original_data$CUSTOMER_BRANCH==original_data$ACCOUNT_BRANCH,0,1)
+
+data<- subset(original_data, select=-c(EqualTo,Compound_ID,ALERT_CUSTOMER_SEGMENT,Unique,REASON_FOR_CLOSURE,CUSTOMER_REGION,DESCRIPTION,CREATION_DATE,CREATION_DATE,LAST_UPDATED,EVENT_DATE,AGE_IN_DAYS,ACTION_NUM, TRANSACTION_ID,CREDIT_DEBIT, TXN_TYPE_DESC, ASSIGNED_TO,ASSIGNED_BY,ORIGINAL_CURRENCY_AMOUNT,ORIGINAL_CURRENCY,POSTAL_CODE,BRANCH_ID,BUSINESS_TYPE,COUNTRY_OF_RESIDENCE,ACCOUNT_BALANCE,HOLDING_BANK_NAME,CUSTOMER_BRANCH,ACCOUNT_BRANCH,BRANCH_CODE,CASE_KEY, CASE_IDENTIFIER,NUM_ALERTS,NUM_CUSTOMERS,CASE_CREATION_DATE,INVESTIGATION_LENGTH,NORKOM_SCORE,TRANSACTION_DATE))
+attach(data)
+
+# Response Variable (SAR)
+data$SAR<- ifelse(data$CASE_STATUS=="Reported/Closed",1,ifelse(data$CASE_STATUS=="Closed",0,NA))
+
+
+data$SAR[with(data,is.na(data$SAR) & data$STATUS_NAME=="Closed" )]=0
+data$SAR[with(data,is.na(data$SAR) & data$STATUS_NAME=="Linked Closed" )]=0
+
+#count the balance of resoponse
+table(data$SAR)
+summary(data$SAR)
+
+# Delete NA values from SAR variable
+datanew<-data[with(data,!is.na(data$SAR))]
+
+# After creation of SAR I can now delete STATUS_NAME and CASE_STATUS
+datanew<-subset(datanew,select=-c(CASE_STATUS,STATUS_NAME))
+
+# Check is it necessary to make 4 different data sets or can we get away with two (similar behaviour for the companies?)
+# Visualisation to see similar average values for a subset of variables!
+# get rid of NA's so i can scale and visualise BASE_CURRENCY_AMOUNT
+nona<- datanew[complete.cases(datanew$BASE_CURRENCY_AMOUNT), ] # only 21,000 points now
+nona1<-subset(nona,select=c(CUSTOMER_SEGMENT,BASE_CURRENCY_AMOUNT))
+dfNormZ <- data.frame(scale(nona1))
+nona1[, 2] <- scale(nona1[,2])
+
+# Note: Can try to do for other continuous variables! Tried for SC04_avg_cash_deposit but the range was too high
+# Come back to this later if want to explore more different top down cluster solutions - maybe ask Tom about some visualisation in SAS VA for this? might be easier in dealing with NA's
+
+par(mfrow=c(1,2))
+boxplot(NUM_CASES~CUSTOMER_SEGMENT, horizontal=F, data=datanew, col = "gray",ylab='Base Currenct Amount')
+boxplot(NUM_ACCOUNTS~CUSTOMER_SEGMENT, horizontal=F, data=datanew, col = "gray",ylab='Base Currenct Amount')
+boxplot(BASE_CURRENCY_AMOUNT~CUSTOMER_SEGMENT, horizontal=F, data=nona1, col = "gray",ylab='Base Currenct Amount', ylim = c(-1, 1)) #here we see small med and priv are similar and others are similar (possible top down clusters)
+boxplot(SC04_AVG_Cash_Deposit~CUSTOMER_SEGMENT, horizontal=F, data=datanew, col = "gray",ylab='Base Currenct Amount')
+
+# Conclusion is that different segments have diff behaviour for dif vars so just try different top down cluster solutions and compare
+# Ask Tom to show how to visualise better in SAS
+# Number 1 - Gov,Reg and Sme against Priv
+# Number 2 - Gov, Reg Vs Sme and Priv (because in terms of transaction amounts these groups tended to be similar)
+# Number 3 - Gov Vs Reg Vs Priv Vs Sme
+


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





