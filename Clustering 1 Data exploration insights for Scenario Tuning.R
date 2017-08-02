###  Step1 :Exploratory analysis by each variables for this splitted datasets: more for scenario tuning

#1, Calendar quarter
ggplot(smalldata, aes(x = smalldata$CAL_QUARTER, fill = as.factor(smalldata$SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(bigdata, aes(x = bigdata$CAL_QUARTER, fill = as.factor(bigdata$SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

#No clustering recommended by the difference perceived by the categorical variable

#2. Calendar Month
ggplot(smalldata, aes(x = (smalldata$EVENT_MONTH), fill = as.factor(smalldata$SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(bigdata, aes(x = bigdata$EVENT_MONTH, fill = as.factor(bigdata$SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#No clustering recommended by the difference perceived by the categorical variable


#3. Risk score 1 and Risk score 2

ggplot(smalldata, aes(x = (smalldata$RISK_SCORE_1), fill = as.factor(smalldata$SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#For personal, risk score 1 yes, makes a difference

ggplot(bigdata, aes(x = bigdata$RISK_SCORE_1, fill = as.factor(bigdata$SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#what? for corporate customers, country of residence with low risk score seems to have more SAR's than those with high risk score!!


ggplot(smalldata, aes(x = smalldata$RISK_SCORE_2, fill = as.factor(smalldata$SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#For personal, risk score 1 yes, makes a difference

ggplot(bigdata, aes(x = bigdata$RISK_SCORE_2, fill = as.factor(bigdata$SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

#Important interpretation: High Risk score 2nd party country may have generated substantially high number of false positives for corporate customer type
########So check for scenarios 16,17,18 for eg to be tuned for this country risk score rule for Corporate customers?#########
####### also check for clusters more specified by combining risk score 1 and 2 with other categorical variables== to do???
#4. Country of Origin

table(smalldata$COUNTRY_OF_ORIGIN)
sum(is.na(smalldata$COUNTRY_OF_ORIGIN))
# so many missing values for personal clients
ggplot(smalldata, aes(x = (smalldata$COUNTRY_OF_ORIGIN), fill = as.factor(smalldata$SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not useful as just 1 Nagy Britannia


################### analyzing the variable 'Country of Origin'
sort(table(bigdata$COUNTRY_OF_ORIGIN),decreasing = T)
sum(is.na(bigdata$COUNTRY_OF_ORIGIN))
#just 21 missing values for 'Country of Origin' for bigdata- can be imputed?
sort(table(smalldata$COUNTRY_OF_ORIGIN),decreasing = T)



ggplot(bigdata, aes(x = bigdata$COUNTRY_OF_ORIGIN, fill = as.factor(bigdata$SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

Iran_NAcheck <- bigdata[bigdata$COUNTRY_OF_ORIGIN=="IRÁN",]
bigdata[bigdata$COUNTRY_OF_ORIGIN=="IRÁN",]$RISK_SCORE_2 # Iran not used as 2nd party country

bigdata[bigdata$COUNTRY_OF_ORIGIN=="BELIZE",]$RISK_SCORE_1

bigdata[bigdata$COUNTRY_OF_ORIGIN=="BELIZE",]$RISK_SCORE_2

bigdata[bigdata$COUNTRY_OF_ORIGIN=="MALTA",]$RISK_SCORE_1

bigdata[bigdata$COUNTRY_OF_ORIGIN=="MALTA",]$RISK_SCORE_2
#Problem: NA's come as values while subsetting! = problem cleared with colnames updated with _ naming


detach(data)
attach(bigdata)
attach(smalldata)
#Anyway not much opt of fp's possible by this categorical variable
#5.Scenario
ggplot(smalldata, aes(x = SCENARIO, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90)) 
# 31 and 29 least SARs
table(smalldata$SCENARIO)
nrow(smalldata[which(SCENARIO=="SC31")]) #only 3 alerts out of which no SAR, so

# Sc29 very slight SAR success out of 1549 transacs, sent abroad and received abroad - increase thresholds for personal customers

ggplot(bigdata, aes(x = SCENARIO, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

table(bigdata$SCENARIO)
# #Sc10 0/174 completely useless for SAR = Structured cash transac's- maybe Corporate deposit more than alerted thresholds?
# SC41 = 0/19 not very informative for SAR  Also check monitoring list of Hung bank?

#In conclusion, Sc29 for personal customers and Sc10 for corp customers- FP's?


#6.Txn_type
ggplot(smalldata, aes(x = TXN_TYPE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#informative - much variation between types 
table(smalldata$TXN_TYPE) #couldn't find scope for FP's 


ggplot(bigdata, aes(x = TXN_TYPE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
table(bigdata$TXN_TYPE) #couldn't find scope for FP's 
#But ATUTBB (domestic wire within bank) and kp (cash) seem to produce high SAR's for corp customers and not for personal!

#7.Instrument
ggplot(smalldata, aes(x = INSTRUMENT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90)) # not so different for personal customers - maybe not such a good variable - binary

ggplot(bigdata, aes(x = INSTRUMENT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90)) # for corporate, cash more likely than wire? there is noticable difference!
table(bigdata$INSTRUMENT) # cash only 416 while Wire 8837 alerts , so more SAR fraction from cash obviously!


#7.Scope
ggplot(smalldata, aes(x = SCOPE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#not much variation for personal- binary

ggplot(bigdata, aes(x = SCOPE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
table(bigdata$SCOPE)
#for corp customers, more raised external than on client
#Not much useful


#8. Region
ggplot(smalldata, aes(x = REGION, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#much for international than domestic
table(smalldata$REGION) #8000 and 4085

ggplot(bigdata, aes(x = REGION, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#much
table(bigdata$REGION) #5800 and 3500

#Important distinct between big data and small data again: 
# more international transaction are Sar's than domestic from personal customers
# while more domestic SAR's than international from corporate customers!

#9. Raised on
ggplot(smalldata, aes(x = RAISED_ON, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90)) #same for both, so not useful
#not so different - binary
ggplot(bigdata, aes(x = RAISED_ON, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
table((bigdata$RAISED_ON))
#But for corporate, yeah mostly on account, client low raised and low successful as well!


#10.PEP_FLAG
ggplot(smalldata, aes(x = PEP_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(bigdata, aes(x = PEP_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#More SAR's for PEP flag for both clusters- binary

#11. Special attention Flag
ggplot(smalldata, aes(x = SPECIAL_ATTENTION_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
table(smalldata$SPECIAL_ATTENTION_FLAG)

ggplot(bigdata, aes(x = SPECIAL_ATTENTION_FLAG, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
table(bigdata$SPECIAL_ATTENTION_FLAG)

#wtf- Special attention flag has lesser success than without the attention for personal customers


# Important:all Y's are not SAR's for priv and small medium! but some N's were SAR's (strange) - may be still informative - binary
#anyway, this variable not used in scenario as well. So not so useful unless used in scenario!


#11.Country of Residence
ggplot(smalldata, aes(x = COUNTRY_OF_RESIDENCE_abbrev, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(smalldata$COUNTRY_OF_RESIDENCE_abbrev), decreasing =T)

#should be informative but will need to convert


ggplot(bigdata, aes(x = COUNTRY_OF_RESIDENCE_abbrev, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(bigdata$COUNTRY_OF_RESIDENCE_abbrev), decreasing =T)


sum(ifelse(bigdata[bigdata$COUNTRY_OF_RESIDENCE_abbrev=="CY",]$SAR==1,1,0))#13 out of 332 alerts are SAR's for Cyprus.
#So maybe not so informative


#12. Business Type2
ggplot(smalldata, aes(x = BUSINESS_TYPE2, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(smalldata$BUSINESS_TYPE2), decreasing =T)
ggplot(bigdata, aes(x = BUSINESS_TYPE2, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(bigdata$BUSINESS_TYPE2), decreasing =T)
#ImportantLocal Gov : 0/950 fp, maybe use these transactions to check for easy autoclosing
LocalGovFP<-bigdata[bigdata$BUSINESS_TYPE2=="Local Gov",]



#informative - will need to convert - non financial businesses have more SAR's it seems


#13. Customer status
ggplot(smalldata, aes(x = CUSTOMER_STATUS, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#Not so useful for personal customers

ggplot(bigdata, aes(x = CUSTOMER_STATUS, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
#Important: difference between closed>Active pronounced for Corp customers 


#14.Status
ggplot(smalldata, aes(x = STATUS, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(smalldata$STATUS), decreasing = T)
# applicable for both personal and SME customer- a little informative (if reopened no SAR's 0/330!) - will need to code

ggplot(bigdata, aes(x = STATUS, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(bigdata$STATUS), decreasing = T)
#not so useful for Corporate customers


#15. Customer Segment- only for corporate customers
ggplot(bigdata, aes(x = CUSTOMER_SEGMENT, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(bigdata$CUSTOMER_SEGMENT), decreasing = T)
datanew[which(datanew$CUSTOMER_SEGMENT!= "GOV" && datanew$BUSINESS_TYPE2== "Local Gov"),]-> LocalGovbutnotGov
# like one SAR in GOV so very informative (GOV companies unlikely to have a SAR- especially combined with Bus Type2 Local Gov = 0/950!)
#local gov and Gov dont fully overlap, so treat them as separate variables. local Govt. (Business Type2) comes from Transaction system
# and Customer segment comes from Norkom system..


#16. Time_Period
ggplot(smalldata, aes(x = TIME_PERIOD, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(smalldata$TIME_PERIOD), decreasing = T)

ggplot(bigdata, aes(x = TIME_PERIOD, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(bigdata$TIME_PERIOD), decreasing = T)
#not super informative: small remark that Monthly scenarios more productive for Corp than Personal custoemrs


#17. Customer Region
ggplot(smalldata, aes(x = smalldata$CUSTOMER_REGION, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(smalldata$CUSTOMER_REGION), decreasing = T)

ggplot(bigdata, aes(x = bigdata$CUSTOMER_REGION, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(bigdata$CUSTOMER_REGION), decreasing = T)
#Not so informative


#18. District of Budapest
ggplot(smalldata, aes(x = DISTRICT_OF_BUDAPEST, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(smalldata$DISTRICT_OF_BUDAPEST), decreasing = T)

ggplot(bigdata, aes(x = DISTRICT_OF_BUDAPEST, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(bigdata$DISTRICT_OF_BUDAPEST), decreasing = T)
#not so informative for both



#19 Customer_Equals_Account
ggplot(smalldata, aes(x = as.factor(Customer_Equals_Account), fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
ggplot(bigdata, aes(x = as.factor(Customer_Equals_Account), fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
sort(table(smalldata$Customer_Equals_Account), decreasing = T)
#maybe not so informative - binary



#20 Amount exceeding 25k
ggplot(smalldata, aes(x = SC01_Amount_Exceding_250000__HUF_, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

ggplot(bigdata, aes(x = SC01_Amount_Exceding_250000__HUF_, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

#For personal,  amount exceding has more sars
#For corp, not exceeding more sars (important: inverted trends for both customer segments)
#not so much difference 

#21. Credit_Debit_Code
ggplot(smalldata, aes(x = CREDIT_DEBIT_CODE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))
ggplot(bigdata, aes(x = CREDIT_DEBIT_CODE, fill = as.factor(SAR))) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

#more credit than debit sars among corp alerts!