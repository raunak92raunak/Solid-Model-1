getwd()
setwd("C:/Users/raunakr/Documents/Jigsaw")
getwd()
telecom-read.csv("telecom.csv",stringsAsFactors = F)
cdvdcdelecom)
library(dplyr)
library(irr)
library(gains)
library(caret)
head(telecom)
##Missing values in the sample dataset
cbind(colSums(is.na(telecom)))
##Y = Dependent variable = Churn 
## 0 = Is the customer who have not cancelled the service 
## 1 = People who have cancelled the service
plot(telecom$churn)
unique(telecom$churn)
table(telecom$churn)
# 0's  = 10140 , 1's = 3119
# 0's - 76 % and 1's - 23 %

##Removing variables which is having lot of missing values - more than 30 % of whole sample data
telecom<-telecom[-div_type]
cbind(colSums(is.na(telecom)))
telecom1<-subset(telecom,select = -c(occu1,retdays,wrkwoman,numbcars,solflag,proptype,mailresp,cartype,
                                     children,div_type,mailordr,dwllsize))
cbind(colSums(is.na(telecom1)))
                    
##Removing range as we are considering only means and also removing 3 months and 6 months data as we are taking lifetime data
telecom3<-subset(telecom2,select = -c(mou_Range,drop_vce_Range,owylis_vce_Range,mou_opkv_Range,callwait_Range,ccrndmou_Range,avg3mou,avg3qty,avg6mou,avg6qty,da_Range,datovr_Range))
cbind(colSums(is.na(telecom2)))
dim(telecom2)
#54 variable left
boxplot(telecom2$mou_Mean)

##Removing rows frmo data where variables contains low number of NA's i.e., l2ss than 3 %
mou_Mean_NA<-which(is.na(telecom2$mou_Mean))
mou_Mean_NA
telecom2<-telecom2[-mou_Mean_NA,]
cbind(colSums(is.na(telecom2)))
change_mou_NA<-which(is.na(telecom2$change_mou))
telecom2<-telecom2[-change_mou_NA,]
csa_NA<-which(is.na(telecom2$csa))
telecom2<-telecom2[-csa_NA,]
cbind(colSums(is.na(telecom2)))

##Checking poutliers using Boxplot
boxplot(telecom2$mou_Mean)
telecom2%>%filter(mou_Mean>=4500)%>%nrow()
telecom2$mou_Mean<-ifelse(telecom2$mou_Mean>=4500,NA,telecom2$mou_Mean)
summary(telecom2$mou_Mean)
telecom2$mou_Mean<-ifelse(is.na(telecom2$mou_Mean),median(telecom2$mou_Mean,na.rm = T),telecom2$mou_Mean)
boxplot(telecom2$change_mou)
telecom2%>%filter(change_mou>=3500)%>%nrow()
telecom2$change_mou<-ifelse(telecom2$change_mou>=3500,NA,telecom2$change_mou)
telecom2$change_mou<-ifelse(is.na(telecom2$change_mou),median(telecom2$change_mou,na.rm = T),telecom2$change_mou)
boxplot(telecom2$drop_blk_Mean)
telecom2%>%filter(drop_blk_Mean>=250)%>%nrow()
telecom2$drop_blk_Mean<-ifelse(telecom2$drop_blk_Mean>=250,NA,telecom2$drop_blk_Mean)
telecom2$drop_blk_Mean<-ifelse(is.na(telecom2$drop_blk_Mean),median(telecom2$drop_blk_Mean,na.rm = T),telecom2$drop_blk_Mean)
cbind(colSums(is.na(telecom2)))
##  Removing the variables not required as reference from Data Dictionary
telecom2<-subset(telecom2,select = -c(prizm_social_one,Customer_ID,asl_flag,datovr_Mean,drop_dat_Mean,
                                      drop_vce_Mean,blck_dat_Mean,refurb_new))
cbind(colSums(is.na(telecom2)))
telecom2<-subset(telecom2,select = -c(hnd_price,mtrcycle,truck,car_buy,dwlltype,csa,ethnic))

##Imputing missing values with unknown - categorical variable
unique(telecom2$hnd_webcap)
telecom2$hnd_webcap1<-ifelse(is.na(telecom2$hnd_webcap),"UNKN",as.character(telecom2$hnd_webcap))
summary(telecom2$hnd_webcap1)
class(telecom2$hnd_webcap1)
unique(telecom2$hnd_webcap1)
#drop older column
telecom2<-subset(telecom2,select = -c(hnd_webcap))
#converting to factor
telecom2$hnd_webcap1<-as.factor(telecom2$hnd_webcap1)
unique(telecom2$hnd_webcap1)
telecom2$marital1<-ifelse(is.na(telecom2$marital),"Missing",as.character(telecom2$marital))
class(telecom2$marital1)
telecom2$marital1<-as.factor(telecom2$marital1)
telecom2<-subset(telecom2,select = -c(marital))
unique(telecom2$forgntvl)  
telecom2$forgntvl1<-ifelse(is.na(telecom2$forgntvl),"Missing",as.character(telecom2$forgntvl))
telecom2$forgntvl1<-as.factor(telecom2$forgntvl1)
telecom2<-subset(telecom2,select = -c(forgntvl))
cbind(colSums(is.na(telecom2)))

##For imputing age we can impute using lm model
unique(telecom2$age1)
telecom2$age1<-ifelse(is.na(telecom2$age1),median(telecom2$age1,na.rm = T),telecom2$age1)
telecom2$age2<-ifelse(is.na(telecom2$age2),median(telecom2$age2,na.rm = T),telecom2$age2)

##Imputing median for income
telecom2$income<-ifelse(is.na(telecom2$income),median(telecom2$income,na.rm = T),telecom2$income)
cbind(colSums(is.na(telecom2)))

## Change dependant variable to factor
lapply(telecom2,class)
telecom2$churn<-as.factor(telecom2$churn)
unique(telecom2$churn)

##Model Building

model1<-glm(churn~.,data = telecom2,family = "binomial")
summary(model1)

##Running stepwise regression
step(object = model1,direction = "both")

model2<-glm(churn ~ mou_Mean + totmrc_Mean + change_mou + drop_blk_Mean + 
              months + totcalls + income + eqpdays + custcare_Mean + callwait_Mean + 
              iwylis_vce_Mean + adjqty + ovrrev_Mean + rev_Mean + ovrmou_Mean + 
              comp_vce_Mean + plcd_vce_Mean + avgmou + avgqty + crclscod + 
              area + age1 + age2 + models + actvsubs + uniqsubs + opk_dat_Mean + 
              roam_Mean + recv_sms_Mean + mou_pead_Mean + da_Mean + adjmou + 
              totrev + adjrev + avgrev + hnd_webcap1 + marital1 + forgntvl1,data = telecom2,family = "binomial")

summary(model2)

##Splitting data in Test and train samples
set.seed(200)
index<-sample(nrow(telecom2),0.70*nrow(telecom2),replace = F)
train<-telecom2[index,]
test<-telecom2[-index,]

#Model on train dataset sample
model3<-glm(churn ~ mou_Mean + totmrc_Mean + change_mou + drop_blk_Mean + 
              months + totcalls + income + eqpdays + custcare_Mean + callwait_Mean + 
              iwylis_vce_Mean + adjqty + ovrrev_Mean + rev_Mean + ovrmou_Mean + 
              comp_vce_Mean + plcd_vce_Mean + avgmou + avgqty + crclscod + 
              area + age1 + age2 + models + actvsubs + uniqsubs + opk_dat_Mean + 
              roam_Mean + recv_sms_Mean + mou_pead_Mean + da_Mean + adjmou + 
              totrev + adjrev + avgrev + hnd_webcap1 + marital1 + forgntvl1,data = train,family = "binomial")
summary(model3)
model_final<-glm(churn ~ mou_Mean + totmrc_Mean + change_mou + drop_blk_Mean + 
                   months + eqpdays + ovrrev_Mean + comp_vce_Mean + age1 + models + actvsubs +
                   uniqsubs + adjmou + totrev + adjrev,data = train,family = "binomial")
summary(model_final)

##Creating dummies  
train$crclscodA2_d<-ifelse(train$crclscod=="A2",1,0)
train$crclscodAA_d<-ifelse(train$crclscod=="AA",1,0)
train$crclscodE4_d<-ifelse(train$crclscod=="E4",1,0)
train$crclscodEA_d<-ifelse(train$crclscod=="EA",1,0)
train$crclscodK_d<-ifelse(train$crclscod=="K",1,0)
trains$areaNORTHWESTROCKYMOUNTAINAREA_d<-ifelse(train$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
train$areaSOUTHFLORIDAAREA_d<-ifelse(train$area=="SOUTH FLORIDA AREA",1,0)
test$crclscodA2_d<-ifelse(test$crclscod=="A2",1,0)
test$crclscodAA_d<-ifelse(test$crclscod=="AA",1,0)
test$crclscodE4_d<-ifelse(test$crclscod=="E4",1,0)
test$crclscodEA_d<-ifelse(test$crclscod=="EA",1,0)
test$crclscodK_d<-ifelse(test$crclscod=="K",1,0)
test$areaNORTHWESTROCKYMOUNTAINAREA_d<-ifelse(test$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
test$areaSOUTHFLORIDAAREA_d<-ifelse(test$area=="SOUTH FLORIDA AREA",1,0)

Model_final1<-glm(churn ~ mou_Mean + totmrc_Mean + change_mou + drop_blk_Mean + 
                   months + eqpdays + ovrrev_Mean + comp_vce_Mean + age1 + models + actvsubs +
                   uniqsubs + adjmou + totrev + adjrev + crclscodA2_d+ crclscodE4_d +
                   crclscodEA_d +
                   areaSOUTHFLORIDAAREA_d,data = train,family = "binomial")
summary(Model_final1)


##Prediction
pred<-predict(Model_final1,type = "response",newdata = test)
head(pred)
table(telecom2$churn)/nrow(telecom2)
pred<-ifelse(pred>0.2341475,1,0)
table(pred)

##Performance
##confusion metrics
cbind(colSums(is.na(test)))
unique(test[31])
conf<-table(test[,31],pred)
conf

library(car)
#AUC
roc_obj<-roc(test[,31],pred)
auc(roc_obj)


##VIF
car::vif(Model_final1)

##Will remove variable one by one having VIF > 5
Model_final2<-glm(churn ~ mou_Mean + totmrc_Mean + change_mou + drop_blk_Mean + 
                    months + eqpdays + ovrrev_Mean + comp_vce_Mean + age1 + models + actvsubs +
                    uniqsubs + adjmou + crclscodA2_d+ crclscodE4_d +
                    crclscodEA_d +
                    areaSOUTHFLORIDAAREA_d,data = train,family = "binomial")

summary(Model_final2)

## The drop in residual deviance from Null deviance and .1 drop in AUC too
