dir()
setwd("~/SchoolWork/Stat702/final")
churn.data = read.csv("churn_data.csv", header = T)


#####################Exploratory Data Analysis############################
dim(churn.data)
names(churn.data)
# cc = credit card
# [1] "user"                    "churn"                   "age"                     "housing"                
# [5] "credit_score"            "deposits"                "withdrawal"              "purchases_partners"     
# [9] "purchases"               "cc_taken"                "cc_recommended"          "cc_disliked"            
# [13] "cc_liked"                "cc_application_begin"    "app_downloaded"          "web_user"               
# [17] "app_web_user"            "ios_user"                "android_user"            "registered_phones"      
# [21] "payment_type"            "waiting_4_loan"          "cancelled_loan"          "received_loan"          
# [25] "rejected_loan"           "zodiac_sign"             "left_for_two_month_plus" "left_for_one_month"     
# [29] "rewards_earned"          "reward_rate"             "is_referred"
sum(is.na(churn.data)) #11262
attach(churn.data)
housing = replace(housing, housing=='na', 'NA')
housing = as.factor(housing)
zodiac_sign = replace(zodiac_sign, zodiac_sign == 'na', 'NA')
zodiac_sign = as.factor(zodiac_sign)
sum(is.na(zodiac_sign)) #2159
sum(is.na(housing)) #13860
sum(is.na(age)) #4
sum(is.na(credit_score)) #8031
sum(is.na(rewards_earned)) #3227
sum(is.na(churn)) #0
churn = as.numeric(churn)
dim(churn.data)
#response is churn
hist(churn, breaks = 2);table(churn)
# churn
# 0     1 
# 15826 11174 
sapply(churn.data, function(x) sum(is.na(x)))
#####replace na with NA in the variable housing
hist(age);table(age); #max = 91
#hist(log(log(age))) looks better, but costs another log
hist(log(age)) # "good"
table(churn.data$housing)
# housing
# na     O     R 
# 13860  2171 10969
hist(credit_score); table(credit_score); #max =838
hist(deposits);max(deposits); #65 
hist(log(deposits))
# deposits
# 0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17 
# 18156  2461  1085   679   454   401   328   266   207   193   175   173   130   134   125   117    98   101 
# 18    19    20    21    22    23    24    25    26    27    28    29    30    31    32    33    34    35 
# 106    99    79    52    52    43    61    67    52    50    56    38    39    48    39    41    40    43 
# 36    37    38    39    40    41    42    43    44    45    46    47    48    49    50    51    52    53 
# 38    44    35    41    31    38    38    25    33    20    15    24    24    18    24    16    29    31 
# 54    55    56    57    58    59    60    61    62    63    64    65 
# 27    32    22    26    19    20    26     4     7     2     2     1
table(deposits);dim(table(deposits)) #66
hist(withdrawal);max(withdrawal)# 29
hist(log(withdrawal))
table(withdrawal)
# withdrawal
# 0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17 
# 22995  2203   853   434   205   108    65    39    33    18     9    13     6     5     1     4     3     1 
# 19    20    24    28    29 
# 1     1     1     1     1 
hist(purchases_partners);max(purchases_partners); #1067 
hist(log(purchases_partners))
table(purchases_partners); dim(table(purchases_partners))# 294
hist(purchases); max(purchases); # 63 
hist(log(purchases))
table(purchases); dim(table(table(deposits)))# 52
# purchases
# 0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16    17 
# 18295  2388  1059   678   445   403   327   262   210   196   179   168   129   134   122   119    94   109 
# 18    19    20    21    22    23    24    25    26    27    28    29    30    31    32    33    34    35 
# 110    90    71    47    64    45    57    64    48    59    51    38    54    37    42    38    42    39 
# 36    37    38    39    40    41    42    43    44    45    46    47    48    49    50    51    52    53 
# 38    42    43    40    29    31    40    18    31    25    17    29    16    23    30    26    20    31 
# 54    55    56    57    58    59    60    61    62    63 
# 21    43    19    19    23    13    15     3     1     1
hist(cc_taken); max(cc_taken);#29 
hist(log(cc_taken))
table(cc_taken); dim(table(cc_taken)) # 12
# cc_taken
# 0     1     2     3     4     5     6     7     8    10    11    29 
# 25705   923   218    75    45    16    11     2     1     2     1     1
hist(cc_recommended);max(cc_recommended); #522 
hist(log(cc_recommended))
#####lgrowr<-ifelse(growr[set2]>0, log(growr[set2]+1), -log(-growr[set2]+1)) solve the log NaNs problem
table(cc_recommended); dim(table(cc_recommended)) #325
hist(cc_disliked); max(cc_disliked); #65 
hist(log(cc_disliked))
table(cc_disliked); dim(table(cc_disliked)); #20 sum(cc_disliked == 0)
# cc_disliked
# 0     1     2     3     4     5     6     7     8     9    10    11    12    13    15    23    25    59 
# 26444   363    78    34    36    11     9     2     3     3     2     3     2     1     3     1     1     2 
# 62    65 
# 1     1 
hist(cc_liked); max(cc_liked); #27
hist(log(cc_liked))
table(cc_liked); dim(table(cc_liked)) #9
# cc_liked
# 0     1     2     3     4     8     9    10    27 
# 26770   182    26    11     6     1     2     1     1 
hist(cc_application_begin); max(cc_application_begin)#263 
table(cc_application_begin); dim(table(cc_application_begin)) #128
hist(app_downloaded, breaks = 2); table(app_downloaded);
# app_downloaded
# 0     1 
# 1283 25717
hist(web_user, breaks = 2); table(web_user)
# web_user
# 0     1 
# 10636 16364
hist(app_web_user, breaks = 2); table(app_web_user);
# app_web_user
# 0     1 
# 11833 15167
hist(ios_user, breaks = 2); table(ios_user);
# ios_user
# 0     1 
# 16364 10636 
hist(android_user, breaks = 2); table(android_user);
# android_user
# 0     1 
# 11144 15856
hist(registered_phones, breaks = 2); table(registered_phones);
# registered_phones
# 0     2     3     4     5 
# 21960  4048   754   183    55
table(payment_type)
# payment_type
# Bi-Weekly      Monthly           na Semi-Monthly       Weekly 
# 12716         2656         3899         2440         5289 
hist(waiting_4_loan); table(waiting_4_loan)
# waiting_4_loan
# 0     1 
# 26965    35
hist(cancelled_loan, breaks = 2); table(cancelled_loan);
# cancelled_loan
# 0     1 
# 26492   508 
hist(received_loan, breaks = 2); table(received_loan);
# received_loan
# 0     1 
# 26509   491 
hist(rejected_loan, breaks = 2); table(rejected_loan);
# rejected_loan
# 0     1 
# 26868   132 
table(zodiac_sign)
hist(as.numeric(zodiac_sign), breaks = 20)

# zodiac_sign
# Aquarius       Aries      Cancer   Capricorn      Gemini         Leo       Libra          na      Pisces 
# 2117        2001        2424         682        2168        2374        2128        2159        2127 
# Sagittarius     Scorpio      Taurus       Virgo 
# 2056        2118        2236        2410
hist(left_for_two_month_plus, breaks = 2); table(left_for_two_month_plus);
# left_for_two_month_plus
# 0     1 
# 22317  4683 
hist(left_for_one_month, breaks = 2); table(left_for_one_month);
# left_for_one_month
# 0     1 
# 26512   488
hist(rewards_earned); table(rewards_earned) #max = 114
hist(reward_rate); table(reward_rate) #max 4
hist(is_referred, breaks = 2); table(is_referred) 
# is_referred
# 0     1 
# 18413  8587 
################EDA#########33
#Take log transform of age
churn.data$age<-log(churn.data$age)

#Set age as numerical
churn.data$age<-as.numeric(churn.data$age)

#Set response variable as a factor and add labels
churn.data$churn <- factor(churn.data$churn, levels=0:1, labels=c("!churn", "churn"))

#Set housing as factor
churn.data$housing <- factor(churn.data$housing)

#app_web_user correlated to web_user
#remove app web_user for now we will modify based on step wise model building
cor.test(~app_web_user+web_user, data=churn.data)
churn.data<-churn.data[,-16]

#ios_user correlated to android_user
#remove android user for now, we will modify based on step wise model building
cor.test(~ios_user+android_user, data=churn.data)
churn.data<-churn.data[,-16]

#Set zodiac_sign as factor
churn.data$zodiac_sign<-as.factor(churn.data$zodiac_sign)

#Deposits feature is highly skewed and log transform not helping
#set to 0 if no deposits made, otherwise if at least one deposit made set to 1
#set withdrawal as factor
churn.data[,5]=sapply(churn.data[,5],function(x) replace(x,x>0,1))
churn.data$deposits<-as.factor(churn.data$deposits)


###########
#Change features to factor (5:9) and (11:13)
#churn.data[,5:9]=sapply(churn.data[,5:9],function(x) replace(x,x>0,1))
#churn.data[,11:13]=sapply(churn.data[,11:13],function(x) replace(x,x>0,1))
###########

#Withdrawal feature is highly skewed and log transform not helping
#set to 0 if no withdrawal made, otherwise if at least one withdrawal made set to 1
#set withdrawal as factor
churn.data$withdrawal<-as.factor(churn.data$withdrawal)

#set purchases as factor
churn.data$purchases<-as.factor(churn.data$purchases)

#set purchases_partners as factor
churn.data$purchases_partners<-as.factor(churn.data$purchases_partners)

#set cc taken as factor
churn.data$cc_taken<-as.factor(churn.data$cc_taken)

#set cc_disliked as factor
churn.data$cc_disliked<-as.factor(churn.data$cc_disliked)
#set cc_liked as factor
churn.data$cc_liked<-as.factor(churn.data$cc_liked)

#set cc_application_begin as factor
churn.data$cc_application_begin<-as.factor(churn.data$cc_application_begin)
#######
###Correlation plot
numeric.var <- sapply(churn.data, is.numeric)
corr.matrix <- cor(churn.data[,numeric.var])
library(corrplot)
corrplot(corr.matrix, main = '\n\nCorrelation Plot for Numerical Variables')
housing = replace(housing, housing== c('NA', 'O', 'R'), c(0,1,2))
table(housing)

#Looking at missing values in all the features
sapply(churn.data, function(x) sum(is.na(x)))
sum(is.na(churn.data))
age = !is.na(age)
sum(is.na(age))
numeric.var <- sapply(churn.data, is.numeric)
corr.matrix <- cor(churn.data[,numeric.var])
corrplot(corr.matrix, main = '\n\nCorrelation Plot for Numerical Variables')

#features that are skewed towards 0- deposits, withdrawls, purchases, purchase partners, 
#Cc application bein, cc liked, cc disliked, cc taken, 
#features that need log transform: age, cc recommended, registered phones

#CORRELATION PLOT ON CAT VAR  
# cat.var <- sapply(churn.data, is.factor)
# cat.matrix <- cor(churn.data[,cat.var])
# corrplot()

###Logistic Regression
library(caret)
set.seed(2019)
intrain<- createDataPartition(churn,p=0.7,list=FALSE)
training<- churn.data[intrain,]
testing<- churn.data[-intrain,]


LogModel <- glm(churn ~.,family=binomial(link="logit"),data=training)
print(summary(LogModel))
plot(LogModel)
anova(LogModel, test="Chisq")

LogModel1 <- glm(churn ~ user+age+housing + credit_score + purchases_partners+
                   cc_taken + cc_recommended + registered_phones + payment_type + cancelled_loan + received_loan +
                   rejected_loan + zodiac_sign + left_for_one_month +
                   rewards_earned + reward_rate
                   ,family=binomial(link="logit"),data=training)
summary(LogModel1)
LogModel2 <- glm(churn ~ user+age+housing + credit_score + purchases_partners+
                   cc_taken + cc_recommended + registered_phones + payment_type + cancelled_loan + received_loan +
                   rejected_loan  + left_for_one_month +
                   rewards_earned + reward_rate
                 ,family=binomial(link="logit"),data=training)
summary(LogModel2)
LogModel3 <- glm(churn ~ user+age + credit_score + purchases_partners+
                   cc_taken + cc_recommended + registered_phones + payment_type + cancelled_loan + received_loan +
                   rejected_loan  + left_for_one_month +
                   rewards_earned + reward_rate
                 ,family=binomial(link="logit"),data=training)
summary(LogModel3)
LogTest <- glm(churn ~ user+age+housing + credit_score + purchases_partners+
                 cc_taken + cc_recommended + registered_phones + payment_type + cancelled_loan + received_loan +
                 rejected_loan  + left_for_one_month +
                 rewards_earned + reward_rate
               ,family=binomial(link="logit"),data=testing)

summary(LogTest)
LogTest1 <- glm(churn ~ user+age+ credit_score + purchases_partners+
                 cc_taken + cc_recommended + registered_phones + payment_type + cancelled_loan + received_loan +
                 rejected_loan  + left_for_one_month +
                 rewards_earned + reward_rate
               ,family=binomial(link="logit"),data=testing)

summary(LogTest1)
anova(LogModel, LogModel2)
anova(LogModel, LogModel1)
anova(LogModel1, LogModel2)
anova(LogTest, LogTest1)
library(MASS)
exp(cbind(OR=coef(LogModel), confint(LogModel)))
exp(cbind(OR=coef(LogModel1), confint(LogModel1)))
exp(cbind(OR=coef(LogModel2), confint(LogModel2)))
exp(cbind(OR=coef(LogModel3), confint(LogModel3)))
exp(cbind(OR=coef(LogTest), confint(LogTest)))
exp(cbind(OR=coef(LogTest1), confint(LogTest1)))

###########Random Forest############
library(randomForest)
#churn = replace(churn, churn=='na', 'NA')
#which(is.na(churn.data))
nona.data = na.omit(churn.data)
dim(nona.data) #17779
dim(churn.data) #27000
#17779/27000 = .658
rfModel <- randomForest(churn ~., data = nona.data)
dim(nona.data)
print(rfModel) #31.6% var explained, msr .164
rfModel1 <- randomForest(churn ~ user+age+ credit_score + purchases_partners+
                           cc_taken + cc_recommended + registered_phones + payment_type + cancelled_loan + received_loan +
                           rejected_loan  + left_for_one_month +
                           rewards_earned + reward_rate, data = nona.data)
print(rfModel1) #30.7 % var explained, .166 msr
pred_rf <- predict(rfModel, churn.data[-nona.data])
caret::confusionMatrix(pred_rf, churn.data[-nona.data])


#### support vector machine ####

library(e1071)
svmfit = svm(churn~., data = nona.data, kernel = "linear", cost = 10, scale = F)
print(svmfit)
summary(svmfit)

dat = data.frame(nona.data[-churn], y = as.factor(nona.data[churn]))
svmfit = svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)

plot(svmfit, nona.data)
plot(svmfit)
