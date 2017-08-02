library(sparcl)
library(cluster)
library(NbClust)
library(ggplot2)
library(Ridit)
library(GGally)
library(VIM)
library(readr)
library(translate)
library(data.table)
library(plyr)
library(dplyr)
library(compare)
library(reshape2)
library(mice)
library(caret)
library(Boruta)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library(factoextra)
#*********************
#**** Personal Units = Small data for Cluster type 1  *****
#*********************

# Ranking of Missing Values
rankmissing = (sapply(smalldata,function(x) mean(is.na(x))) %>%
                 sort())
names_miss=names(rankmissing[which(rankmissing < 0.15)])
# some variables' NA are actually informative, for example these actually below aren't missing information, they are indicating values..
smalldata$DISTRICT_OF_BUDAPEST[is.na(smalldata$DISTRICT_OF_BUDAPEST)] = "OUTSIDE_BUDAPEST"
smalldata$TXN_TYPE[is.na(smalldata$TXN_TYPE)] = "NOT_A_TRANSACTION"
smalldata$INSTRUMENT[is.na(smalldata$INSTRUMENT)] = "NOT_A_TRANSACTION"
smalldata$SCOPE[is.na(smalldata$SCOPE)] = "NOT_A_TRANSACTION"
smalldata$REGION[is.na(smalldata$REGION)] = "NOT_A_TRANSACTION"
smalldata$BASE_CURRENCY_AMOUNT[is.na(smalldata$BASE_CURRENCY_AMOUNT)] = "NOT_A_TRANSACTION"

# Making Factor some variables


# Again Ranking of Missing Values
rankmissing = (sapply(smalldata,function(x) mean(is.na(x))) %>%
                 sort())
names_miss=names(rankmissing[which(rankmissing < 0.15)])
refined_per= subset(smalldata, select = names_miss) #maaki kirikiri, was so easy!
# UPS - The var should be numeric; 6446 is numerical version of 'NOT_A_TRANSACTION'

refined_per$BASE_CURRENCY_AMOUNT[which(refined_per$BASE_CURRENCY_AMOUNT=='NOT_A_TRANSACTION')]=NA
refined_per$BASE_CURRENCY_AMOUNT=as.numeric(refined_per$BASE_CURRENCY_AMOUNT)

refined_per$BASE_CURRENCY_AMOUNT=cut(refined_per$BASE_CURRENCY_AMOUNT,c(quantile(refined_per$BASE_CURRENCY_AMOUNT,prob = seq(0,1, length.out = 11), na.rm=T,type = 5)))
              

#cut numeric variable into intervals based on quantiles,
refined_per$BASE_CURRENCY_AMOUNT=addNA(refined_per$BASE_CURRENCY_AMOUNT)
sum(is.na(refined_per$BASE_CURRENCY_AMOUNT)) #No NA's, perfect!
class(refined_per$BASE_CURRENCY_AMOUNT) #converting NA's into Not a transaction
levels(refined_per$BASE_CURRENCY_AMOUNT)[is.na(levels(refined_per$BASE_CURRENCY_AMOUNT))] = "NOT_A_TRANSACTION" 



#################Missing values = Imputations ########################
#remove CUstomer_segment for personal customers as it doesn't have any levels and is not informative
mrefined_per=refined_per[,-"CUSTOMER_SEGMENT"]


allsamenas<-subset(mrefined_per,select=c(BASE_CURRENCY_AMOUNT,TXN_TYPE,INSTRUMENT,SCOPE,REGION))


##COUNTRY_OF_RESIDENCE_abbrev ##
countryres<-mrefined_per[which(is.na(mrefined_per$COUNTRY_OF_RESIDENCE_abbrev)),] #1580 Na's out of 25k for country of residence

#So both are NA's simultaneously


mrefined_per[which(is.na(mrefined_per$COUNTRY_OF_RESIDENCE_abbrev)),'COUNTRY_OF_RESIDENCE_abbrev']<- 'HU'




## STATUS and ORIGINAL_ACC_CURRENCY and CUSTOMER_EQUALS_ACCOUNT and NEW_ACCOUNT_BALANCE ##

# inspect reason for missingness
missing<-subset(mrefined_per,select=c(ORIGINAL_ACCOUNT_CURRENCY,NEW_ACCOUNT_BALANCE,Customer_Equals_Account,STATUS))
missing1<-mrefined_per[which(is.na(mrefined_per$ORIGINAL_ACCOUNT_CURRENCY))]
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
mrefined_per[is.na(mrefined_per$NEW_ACCOUNT_BALANCE) == TRUE, c("NEW_ACCOUNT_BALANCE")]  <- mean(as.numeric(mrefined_per[['NEW_ACCOUNT_BALANCE']]), na.rm = T)
# imputed by 16834997
mrefined_per[is.na(mrefined_per$STATUS) == TRUE, "STATUS"]  <- Mode(mrefined_per[['STATUS']])
# imputed by "OPENED"
mrefined_per[is.na(mrefined_per$ORIGINAL_ACCOUNT_CURRENCY) == TRUE, "ORIGINAL_ACCOUNT_CURRENCY"]  <- Mode(mrefined_per[['ORIGINAL_ACCOUNT_CURRENCY']])
# imputed by "HUF"
mrefined_per[is.na(mrefined_per$Customer_Equals_Account) == TRUE, "Customer_Equals_Account"]  <- Mode(mrefined_per[['Customer_Equals_Account']])
# imputed by "0"

## Impute customer region####
sum(is.na(mrefined_per$CUSTOMER_REGION)) #1897 missing values

customerreg<- mrefined_per[which(is.na(mrefined_per$CUSTOMER_REGION)),] #1897missingvalues
table(customerreg$COUNTRY_OF_RESIDENCE_abbrev) #233 HU residents, impute with mode of customer region

mrefined_per[which((is.na(mrefined_per$CUSTOMER_REGION)) & (mrefined_per$COUNTRY_OF_RESIDENCE_abbrev=="HU")),'CUSTOMER_REGION']  <- Mode(mrefined_per[['CUSTOMER_REGION']])
sum(is.na(mrefined_per$CUSTOMER_REGION))#now 1663
table(mrefined_per[which(is.na(mrefined_per$CUSTOMER_REGION)),'COUNTRY_OF_RESIDENCE_abbrev'] ) #no HU residents among missing cust regions!

# call the other NA's "OUTSIDE_HUNGARY"
mrefined_per$CUSTOMER_REGION[is.na(mrefined_per$CUSTOMER_REGION)]<-"OUTSIDE_HUNGARY"


sum(is.na(mrefined_per$CUSTOMER_REGION))#now 0

# # now convert all character vars to factors and all continuous variables to numeric or int

lapply(mrefined_per, class)#obtained all classes, now check what we need to be factors and numeric!
colnames(mrefined_per) #see column indices for specifying class types



mrefined_per[,c(1,2,4,5,6,7,8,9,10,11,13,14,15,19,20,22,23,24,25,27,28,29)]<-as.data.frame(sapply(mrefined_per[,c(1,2,4,5,6,7,8,9,10,11,13,14,15,19,20,22,23,24,25,27,28,29)], as.factor))
mrefined_per[,c(3,12,16,17,18,21,26)] <- as.data.frame(sapply(mrefined_per[,c(3,12,16,17,18,22,27)], as.numeric))
#base currency (variable no.8) has missing values, so didnt convert into numeric yet- converted into factor type!



classvars<- as.data.frame(lapply(mrefined_per, class))

#plot amount of missing values in each variable
aggr_plot = aggr(mrefined_per, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                 labels=names(mrefined_per), cex.axis=.7, gap=3, 
                 ylab=c("Histogram of missing data","Pattern"))

#all good

#Feature reduction at this stage may not worthy as we have 30 variables which we can use to build a model by converting them to ridit scores
mrefined_per1<-mrefined_per
sum(is.na(mrefined_per)) #0 missing values!

#################
# RIDIT SCORES ##
#################

## Ridit Function
ridit_scores=function(y){
  x = vector('numeric')
  for (i in 1:length(y)) {
    
    x[i]=(sum(y[1:i])-sum(y[length(y):(i)]))
  }
  return(x)
}

#sample
# (sum(joined_tab1$Freq[1:i])-sum(joined_tab1$Freq[length(joined_tab1$Freq):(i)]))

#all char variables

# SCENARIO Ridit Scores for tab1

tab1=prop.table(table(mrefined_per1$SAR,mrefined_per1$SCENARIO),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
#table gives 2 way table of SAR w.r.t scenario with margin 2. 
#Store all these values in a data variable, it creates var 1= SAR, var 2= Scenario and Freq= proportion of SAR
#Filter where SAR=1, note that we don't need proportion of SAR's by scenario (Freq_SAR) though
#sort by descending frequency of obs in each scenario which are SAR's (actually Freq_SAR) 
colnames(tab1) = c("Var1", "SCENARIO", "Freq_SAR")  
tab1
tab1_2=prop.table(table(mrefined_per1$SCENARIO)) %>%
  data.frame()
#tab1_2 : freq table by scenario 
colnames(tab1_2) = c('SCENARIO', 'Freq')
joined_tab1=left_join(tab1,tab1_2, by = 'SCENARIO')
joined_tab1 #when joined, order by desc freq_SAR is retained



ridit_tab1=ridit_scores(joined_tab1$Freq) %>% data.frame()
colnames(ridit_tab1)='Ridit_Scores'
ridit_tab1=bind_cols(joined_tab1,ridit_tab1) %>% select(c('SCENARIO','Ridit_Scores')) #joined riditscores and joined scores but selected only 2 columns- scenarios and Ridit scores
colnames(ridit_tab1)=c('SCENARIO','RS_SCENARIO')
mrefined_per1<-left_join(mrefined_per1,ridit_tab1,by = 'SCENARIO')



# 2.EVENT_ MONTH Ridit Scores for tab2
tab2=prop.table(table(mrefined_per1$SAR,mrefined_per1$EVENT_MONTH),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab2) = c("Var1", "EVENT_MONTH", "Freq_SAR")  

tab2_2=prop.table(table(mrefined_per1$EVENT_MONTH)) %>%
  data.frame()
colnames(tab2_2) = c('EVENT_MONTH', 'Freq')
joined_tab2=left_join(tab2,tab2_2, by = 'EVENT_MONTH')

ridit_tab2=ridit_scores(joined_tab2$Freq) %>% data.frame()
colnames(ridit_tab2)='Ridit_Scores'
ridit_tab2=bind_cols(joined_tab2,ridit_tab2) %>% select(c('EVENT_MONTH','Ridit_Scores'))
colnames(ridit_tab2)=c('EVENT_MONTH','RS_EVENT_MONTH')
mrefined_per1<-left_join(mrefined_per1,ridit_tab2,by = 'EVENT_MONTH')



# 3.TXN_TYPE Ridit Scores for tab3
tab3=prop.table(table(mrefined_per1$SAR,mrefined_per1$TXN_TYPE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab3) = c("Var1", "TXN_TYPE", "Freq_SAR")  

tab3_2=prop.table(table(mrefined_per1$TXN_TYPE)) %>%
  data.frame()
colnames(tab3_2) = c('TXN_TYPE', 'Freq')
joined_tab3=left_join(tab3,tab3_2, by = 'TXN_TYPE')
tab3
tab3_2
ridit_tab3=ridit_scores(joined_tab3$Freq) %>% data.frame()
colnames(ridit_tab3)='Ridit_Scores'
ridit_tab3=bind_cols(joined_tab3,ridit_tab3) %>% select(c('TXN_TYPE','Ridit_Scores'))
colnames(ridit_tab3)=c('TXN_TYPE','RS_TXN_TYPE')
mrefined_per1<-left_join(mrefined_per1,ridit_tab3,by = 'TXN_TYPE')


# 4.INSTRUMENT Ridit Scores for tab4
tab4=prop.table(table(mrefined_per1$SAR,mrefined_per1$INSTRUMENT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab4) = c("Var1", "INSTRUMENT", "Freq_SAR")  

tab4_2=prop.table(table(mrefined_per1$INSTRUMENT)) %>%
  data.frame()
colnames(tab4_2) = c('INSTRUMENT', 'Freq')
joined_tab4=left_join(tab4,tab4_2, by = 'INSTRUMENT')
tab4
tab4_2
ridit_tab4=ridit_scores(joined_tab4$Freq) %>% data.frame()
colnames(ridit_tab4)='Ridit_Scores'
ridit_tab4=bind_cols(joined_tab4,ridit_tab4) %>% select(c('INSTRUMENT','Ridit_Scores'))
colnames(ridit_tab4)=c('INSTRUMENT','RS_INSTRUMENT')
mrefined_per1<-left_join(mrefined_per1,ridit_tab4,by = 'INSTRUMENT')


# 5.SCOPE Ridit Scores for tab5
tab5=prop.table(table(mrefined_per1$SAR,mrefined_per1$SCOPE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab5) = c("Var1", "SCOPE", "Freq_SAR")  

tab5_2=prop.table(table(mrefined_per1$SCOPE)) %>%
  data.frame()
colnames(tab5_2) = c('SCOPE', 'Freq')
joined_tab5=left_join(tab5,tab5_2, by = 'SCOPE')
tab5
tab5_2
ridit_tab5=ridit_scores(joined_tab5$Freq) %>% data.frame()
colnames(ridit_tab5)='Ridit_Scores'
ridit_tab5=bind_cols(joined_tab5,ridit_tab5) %>% select(c('SCOPE','Ridit_Scores'))
colnames(ridit_tab5)=c('SCOPE','RS_SCOPE')
mrefined_per1<-left_join(mrefined_per1,ridit_tab5,by = 'SCOPE')



# 6.REGION Ridit Scores for tab6
tab6=prop.table(table(mrefined_per1$SAR,mrefined_per1$REGION),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab6) = c("Var1", "REGION", "Freq_SAR")  

tab6_2=prop.table(table(mrefined_per1$REGION)) %>%
  data.frame()
colnames(tab6_2) = c('REGION', 'Freq')
joined_tab6=left_join(tab6,tab6_2, by = 'REGION')
tab6
tab6_2
ridit_tab6=ridit_scores(joined_tab6$Freq) %>% data.frame()
colnames(ridit_tab6)='Ridit_Scores'
ridit_tab6=bind_cols(joined_tab6,ridit_tab6) %>% select(c('REGION','Ridit_Scores'))
colnames(ridit_tab6)=c('REGION','RS_REGION')
mrefined_per1<-left_join(mrefined_per1,ridit_tab6,by = 'REGION')


# 7.BASE_CURRENCY_AMOUNT Ridit Scores for tab7
tab7=prop.table(table(mrefined_per1$SAR,mrefined_per1$BASE_CURRENCY_AMOUNT),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab7) = c("Var1", "BASE_CURRENCY_AMOUNT", "Freq_SAR")  

tab7_2=prop.table(table(mrefined_per1$BASE_CURRENCY_AMOUNT)) %>%
  data.frame()
colnames(tab7_2) = c('BASE_CURRENCY_AMOUNT', 'Freq')
joined_tab7=left_join(tab7,tab7_2, by = 'BASE_CURRENCY_AMOUNT')
tab7
tab7_2
ridit_tab7=ridit_scores(joined_tab7$Freq) %>% data.frame()
colnames(ridit_tab7)='Ridit_Scores'
ridit_tab7=bind_cols(joined_tab7,ridit_tab7) %>% select(c('BASE_CURRENCY_AMOUNT','Ridit_Scores'))
colnames(ridit_tab7)=c('BASE_CURRENCY_AMOUNT','RS_BASE_CURRENCY_AMOUNT')
mrefined_per1<-left_join(mrefined_per1,ridit_tab7,by = 'BASE_CURRENCY_AMOUNT')



# 8.RAISED_ON Ridit Scores for tab8
tab8=prop.table(table(mrefined_per1$SAR,mrefined_per1$RAISED_ON),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab8) = c("Var1", "RAISED_ON", "Freq_SAR")  

tab8_2=prop.table(table(mrefined_per1$RAISED_ON)) %>%
  data.frame()
colnames(tab8_2) = c('RAISED_ON', 'Freq')
joined_tab8=left_join(tab8,tab8_2, by = 'RAISED_ON')
tab8
tab8_2
ridit_tab8=ridit_scores(joined_tab8$Freq) %>% data.frame()
colnames(ridit_tab8)='Ridit_Scores'
ridit_tab8=bind_cols(joined_tab8,ridit_tab8) %>% select(c('RAISED_ON','Ridit_Scores'))
colnames(ridit_tab8)=c('RAISED_ON','RS_RAISED_ON')
mrefined_per1<-left_join(mrefined_per1,ridit_tab8,by = 'RAISED_ON')

# 9.PEP_FLAG Ridit Scores for tab9
tab9=prop.table(table(mrefined_per1$SAR,mrefined_per1$PEP_FLAG),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab9) = c("Var1", "PEP_FLAG", "Freq_SAR")  

tab9_2=prop.table(table(mrefined_per1$PEP_FLAG)) %>%
  data.frame()
colnames(tab9_2) = c('PEP_FLAG', 'Freq')
joined_tab9=left_join(tab9,tab9_2, by = 'PEP_FLAG')
tab9
tab9_2
ridit_tab9=ridit_scores(joined_tab9$Freq) %>% data.frame()
colnames(ridit_tab9)='Ridit_Scores'
ridit_tab9=bind_cols(joined_tab9,ridit_tab9) %>% select(c('PEP_FLAG','Ridit_Scores'))
colnames(ridit_tab9)=c('PEP_FLAG','RS_PEP_FLAG')
mrefined_per1<-left_join(mrefined_per1,ridit_tab9,by = 'PEP_FLAG')


# 10.SPECIAL_ATTENTION_FLAG Ridit Scores for tab10
tab10=prop.table(table(mrefined_per1$SAR,mrefined_per1$SPECIAL_ATTENTION_FLAG),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab10) = c("Var1", "SPECIAL_ATTENTION_FLAG", "Freq_SAR")  

tab10_2=prop.table(table(mrefined_per1$SPECIAL_ATTENTION_FLAG)) %>%
  data.frame()
colnames(tab10_2) = c('SPECIAL_ATTENTION_FLAG', 'Freq')
joined_tab10=left_join(tab10,tab10_2, by = 'SPECIAL_ATTENTION_FLAG')
tab10
tab10_2
ridit_tab10=ridit_scores(joined_tab10$Freq) %>% data.frame()
colnames(ridit_tab10)='Ridit_Scores'
ridit_tab10=bind_cols(joined_tab10,ridit_tab10) %>% select(c('SPECIAL_ATTENTION_FLAG','Ridit_Scores'))
colnames(ridit_tab10)=c('SPECIAL_ATTENTION_FLAG','RS_SPECIAL_ATTENTION_FLAG')
mrefined_per1<-left_join(mrefined_per1,ridit_tab10,by = 'SPECIAL_ATTENTION_FLAG')

# 11.BUSINESS_TYPE2 Ridit Scores for tab11
tab11=prop.table(table(mrefined_per1$SAR,mrefined_per1$BUSINESS_TYPE2),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab11) = c("Var1", "BUSINESS_TYPE2", "Freq_SAR")  

tab11_2=prop.table(table(mrefined_per1$BUSINESS_TYPE2)) %>%
  data.frame()
colnames(tab11_2) = c('BUSINESS_TYPE2', 'Freq')
joined_tab11=left_join(tab11,tab11_2, by = 'BUSINESS_TYPE2')
tab11
tab11_2
ridit_tab11=ridit_scores(joined_tab11$Freq) %>% data.frame()
colnames(ridit_tab11)='Ridit_Scores'
ridit_tab11=bind_cols(joined_tab11,ridit_tab11) %>% select(c('BUSINESS_TYPE2','Ridit_Scores'))
colnames(ridit_tab11)=c('BUSINESS_TYPE2','RS_BUSINESS_TYPE2')
mrefined_per1<-left_join(mrefined_per1,ridit_tab11,by = 'BUSINESS_TYPE2')



# 12.CUSTOMER_TYPE Ridit Scores for tab12
tab12=prop.table(table(mrefined_per1$SAR,mrefined_per1$CUSTOMER_TYPE),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab12) = c("Var1", "CUSTOMER_TYPE", "Freq_SAR")  

tab12_2=prop.table(table(mrefined_per1$CUSTOMER_TYPE)) %>%
  data.frame()
colnames(tab12_2) = c('CUSTOMER_TYPE', 'Freq')
joined_tab12=left_join(tab12,tab12_2, by = 'CUSTOMER_TYPE')
tab12
tab12_2
ridit_tab12=ridit_scores(joined_tab12$Freq) %>% data.frame()
colnames(ridit_tab12)='Ridit_Scores'
ridit_tab12=bind_cols(joined_tab12,ridit_tab12) %>% select(c('CUSTOMER_TYPE','Ridit_Scores'))
colnames(ridit_tab12)=c('CUSTOMER_TYPE','RS_CUSTOMER_TYPE')
mrefined_per1<-left_join(mrefined_per1,ridit_tab12,by = 'CUSTOMER_TYPE')



# 13.CUSTOMER_STATUS Ridit Scores for tab13
tab13=prop.table(table(mrefined_per1$SAR,mrefined_per1$CUSTOMER_STATUS),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab13) = c("Var1", "CUSTOMER_STATUS", "Freq_SAR")  

tab13_2=prop.table(table(mrefined_per1$CUSTOMER_STATUS)) %>%
  data.frame()
colnames(tab13_2) = c('CUSTOMER_STATUS', 'Freq')
joined_tab13=left_join(tab13,tab13_2, by = 'CUSTOMER_STATUS')
tab13
tab13_2
ridit_tab13=ridit_scores(joined_tab13$Freq) %>% data.frame()
colnames(ridit_tab13)='Ridit_Scores'
ridit_tab13=bind_cols(joined_tab13,ridit_tab13) %>% select(c('CUSTOMER_STATUS','Ridit_Scores'))
colnames(ridit_tab13)=c('CUSTOMER_STATUS','RS_CUSTOMER_STATUS')
mrefined_per1<-left_join(mrefined_per1,ridit_tab13,by = 'CUSTOMER_STATUS')

#14 was customer segment, later deleted as it is not informative for the subset of personal customers


# 15.TIME_PERIOD Ridit Scores for tab15
tab15=prop.table(table(mrefined_per1$SAR,mrefined_per1$TIME_PERIOD),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab15) = c("Var1", "TIME_PERIOD", "Freq_SAR")  

tab15_2=prop.table(table(mrefined_per1$TIME_PERIOD)) %>%
  data.frame()
colnames(tab15_2) = c('TIME_PERIOD', 'Freq')
joined_tab15=left_join(tab15,tab15_2, by = 'TIME_PERIOD')
tab15
tab15_2
ridit_tab15=ridit_scores(joined_tab15$Freq) %>% data.frame()
colnames(ridit_tab15)='Ridit_Scores'
ridit_tab15=bind_cols(joined_tab15,ridit_tab15) %>% select(c('TIME_PERIOD','Ridit_Scores'))
colnames(ridit_tab15)=c('TIME_PERIOD','RS_TIME_PERIOD')
mrefined_per1<-left_join(mrefined_per1,ridit_tab15,by = 'TIME_PERIOD')


# 16.DISTRICT_OF_BUDAPEST Ridit Scores for tab16
tab16=prop.table(table(mrefined_per1$SAR,mrefined_per1$DISTRICT_OF_BUDAPEST),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab16) = c("Var1", "DISTRICT_OF_BUDAPEST", "Freq_SAR")  

tab16_2=prop.table(table(mrefined_per1$DISTRICT_OF_BUDAPEST)) %>%
  data.frame()
colnames(tab16_2) = c('DISTRICT_OF_BUDAPEST', 'Freq')
joined_tab16=left_join(tab16,tab16_2, by = 'DISTRICT_OF_BUDAPEST')
tab16
tab16_2
ridit_tab16=ridit_scores(joined_tab16$Freq) %>% data.frame()
colnames(ridit_tab16)='Ridit_Scores'
ridit_tab16=bind_cols(joined_tab16,ridit_tab16) %>% select(c('DISTRICT_OF_BUDAPEST','Ridit_Scores'))
colnames(ridit_tab16)=c('DISTRICT_OF_BUDAPEST','RS_DISTRICT_OF_BUDAPEST')
mrefined_per1<-left_join(mrefined_per1,ridit_tab16,by = 'DISTRICT_OF_BUDAPEST')


# 17.SC01_Amount_Exceding_250000__HUF_ Ridit Scores for tab17
tab17=prop.table(table(mrefined_per1$SAR,mrefined_per1$SC01_Amount_Exceding_250000__HUF_),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab17) = c("Var1", "SC01_Amount_Exceding_250000__HUF_", "Freq_SAR")  

tab17_2=prop.table(table(mrefined_per1$SC01_Amount_Exceding_250000__HUF_)) %>%
  data.frame()
colnames(tab17_2) = c('SC01_Amount_Exceding_250000__HUF_', 'Freq')
joined_tab17=left_join(tab17,tab17_2, by = 'SC01_Amount_Exceding_250000__HUF_')
tab17
tab17_2
ridit_tab17=ridit_scores(joined_tab17$Freq) %>% data.frame()
colnames(ridit_tab17)='Ridit_Scores'
ridit_tab17=bind_cols(joined_tab17,ridit_tab17) %>% select(c('SC01_Amount_Exceding_250000__HUF_','Ridit_Scores'))
colnames(ridit_tab17)=c('SC01_Amount_Exceding_250000__HUF_','RS_SC01_Amount_Exceding_250000__HUF_')
mrefined_per1<-left_join(mrefined_per1,ridit_tab17,by = 'SC01_Amount_Exceding_250000__HUF_')


# 18.COUNTRY_OF_RESIDENCE_abbrev Ridit Scores for tab18
tab18=prop.table(table(mrefined_per1$SAR,mrefined_per1$COUNTRY_OF_RESIDENCE_abbrev),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab18) = c("Var1", "COUNTRY_OF_RESIDENCE_abbrev", "Freq_SAR")  

tab18_2=prop.table(table(mrefined_per1$COUNTRY_OF_RESIDENCE_abbrev)) %>%
  data.frame()
colnames(tab18_2) = c('COUNTRY_OF_RESIDENCE_abbrev', 'Freq')
joined_tab18=left_join(tab18,tab18_2, by = 'COUNTRY_OF_RESIDENCE_abbrev')
tab18
tab18_2
ridit_tab18=ridit_scores(joined_tab18$Freq) %>% data.frame()
colnames(ridit_tab18)='Ridit_Scores'
ridit_tab18=bind_cols(joined_tab18,ridit_tab18) %>% select(c('COUNTRY_OF_RESIDENCE_abbrev','Ridit_Scores'))
colnames(ridit_tab18)=c('COUNTRY_OF_RESIDENCE_abbrev','RS_COUNTRY_OF_RESIDENCE_abbrev')
mrefined_per1<-left_join(mrefined_per1,ridit_tab18,by = 'COUNTRY_OF_RESIDENCE_abbrev')


# 19.CUSTOMER_REGION Ridit Scores for tab19
tab19=prop.table(table(mrefined_per1$SAR,mrefined_per1$CUSTOMER_REGION),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab19) = c("Var1", "CUSTOMER_REGION", "Freq_SAR")  

tab19_2=prop.table(table(mrefined_per1$CUSTOMER_REGION)) %>%
  data.frame()
colnames(tab19_2) = c('CUSTOMER_REGION', 'Freq')
joined_tab19=left_join(tab19,tab19_2, by = 'CUSTOMER_REGION')
tab19
tab19_2
ridit_tab19=ridit_scores(joined_tab19$Freq) %>% data.frame()
colnames(ridit_tab19)='Ridit_Scores'
ridit_tab19=bind_cols(joined_tab19,ridit_tab19) %>% select(c('CUSTOMER_REGION','Ridit_Scores'))
colnames(ridit_tab19)=c('CUSTOMER_REGION','RS_CUSTOMER_REGION')
mrefined_per1<-left_join(mrefined_per1,ridit_tab19,by = 'CUSTOMER_REGION')


# 20.ORIGINAL_ACCOUNT_CURRENCY Ridit Scores for tab20
tab20=prop.table(table(mrefined_per1$SAR,mrefined_per1$ORIGINAL_ACCOUNT_CURRENCY),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab20) = c("Var1", "ORIGINAL_ACCOUNT_CURRENCY", "Freq_SAR")  

tab20_2=prop.table(table(mrefined_per1$ORIGINAL_ACCOUNT_CURRENCY)) %>%
  data.frame()
colnames(tab20_2) = c('ORIGINAL_ACCOUNT_CURRENCY', 'Freq')
joined_tab20=left_join(tab20,tab20_2, by = 'ORIGINAL_ACCOUNT_CURRENCY')
tab20
tab20_2
ridit_tab20=ridit_scores(joined_tab20$Freq) %>% data.frame()
colnames(ridit_tab20)='Ridit_Scores'
ridit_tab20=bind_cols(joined_tab20,ridit_tab20) %>% select(c('ORIGINAL_ACCOUNT_CURRENCY','Ridit_Scores'))
colnames(ridit_tab20)=c('ORIGINAL_ACCOUNT_CURRENCY','RS_ORIGINAL_ACCOUNT_CURRENCY')
mrefined_per1<-left_join(mrefined_per1,ridit_tab20,by = 'ORIGINAL_ACCOUNT_CURRENCY')


# 21.Customer_Equals_Account Ridit Scores for tab21
tab21=prop.table(table(mrefined_per1$SAR,mrefined_per1$Customer_Equals_Account),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab21) = c("Var1", "Customer_Equals_Account", "Freq_SAR")  

tab21_2=prop.table(table(mrefined_per1$Customer_Equals_Account)) %>%
  data.frame()
colnames(tab21_2) = c('Customer_Equals_Account', 'Freq')
joined_tab21=left_join(tab21,tab21_2, by = 'Customer_Equals_Account')
tab21
tab21_2
ridit_tab21=ridit_scores(joined_tab21$Freq) %>% data.frame()
colnames(ridit_tab21)='Ridit_Scores'
ridit_tab21=bind_cols(joined_tab21,ridit_tab21) %>% select(c('Customer_Equals_Account','Ridit_Scores'))
colnames(ridit_tab21)=c('Customer_Equals_Account','RS_Customer_Equals_Account')
mrefined_per1<-left_join(mrefined_per1,ridit_tab21,by = 'Customer_Equals_Account')


# 22.STATUS Ridit Scores for tab22
tab22=prop.table(table(mrefined_per1$SAR,mrefined_per1$STATUS),2) %>%
  data.frame() %>% 
  filter(Var1==1) %>% 
  arrange(desc(Freq))
colnames(tab22) = c("Var1", "STATUS", "Freq_SAR")  

tab22_2=prop.table(table(mrefined_per1$STATUS)) %>%
  data.frame()
colnames(tab22_2) = c('STATUS', 'Freq')
joined_tab22=left_join(tab22,tab22_2, by = 'STATUS')
tab22
tab22_2
ridit_tab22=ridit_scores(joined_tab22$Freq) %>% data.frame()
colnames(ridit_tab22)='Ridit_Scores'
ridit_tab22=bind_cols(joined_tab22,ridit_tab22) %>% select(c('STATUS','Ridit_Scores'))
colnames(ridit_tab22)=c('STATUS','RS_STATUS')
mrefined_per1<-left_join(mrefined_per1,ridit_tab22,by = 'STATUS')

#Note that tab14 was customer segment, which was removed.
mrefined_per1
sum(is.na(mrefined_per1))




#####################################
### CLUSTER NONHIERARCHICAL - per ###
#####################################

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


















