getwd()
setwd("C:/Users/raunakr/Documents/Jigsaw")
getwd()
direct<-read.csv("DirectMarketing.csv")
View(direct)
library(dplyr)
library(irr)
library(gains)
library(caret)
direct%>%mutate(Target=ifelse(AmountSpent>mean(direct$AmountSpent),1,0))->direct
head(direct)
direct%>%select(-AmountSpent)->direct
head(direct)
##to check missing values
summary(direct)
direct$History1<-ifelse(is.na(direct$History),"Missing",as.character(direct$History))
head(direct)
class(direct)
class(direct$History1)
direct$History1<-as.factor(direct$History1)
summary(direct$History1)
lapply(direct, class) ##to check all datatypesof all variables
##Now - Will consider Children and Catalogue as categorical variable and for that will
##change its datatype from 
##integer to factor

direct$Children<-as.factor(direct$Children)
direct$Catalogs<-as.factor(direct$Catalogs)
lapply(direct, class)
direct<-direct[,-8] ##Remove the older History col


##Splitting dataset in training and test samples
set.seed(200)
index<-sample(nrow(direct),0.70*nrow(direct),replace = F)
train<-direct[index,]
test<-direct[-index,]

head(train)
head(test)
 
##Build Model using 'glm'
##1st model using all vaiables
model<-glm(Target~.,data = direct,family = "binomial")
summary(model)

##Stepwise automatic
step(model,direction = "both")

##Follow the model and significant variables suggested by Stepwise process
model1<-glm(formula = Target~ Age + Location + Salary + Children + Catalogs + 
              History1, family = "binomial", data = train)
summary(model1)
##Will create dummy variables fo those factor type categorical variable for which
#Model is showing some significance

##Creating Dummies
train$AgeYoung_d<-ifelse(train$Age=="Young",1,0)
train$Children2_d<-ifelse(train$Children=="2",1,0)
train$Children3_d<-ifelse(train$Children=="3",1,0)
train$History1.Med_d<-ifelse(train$History1=="Medium",1,0)
test$AgeYoung_d<-ifelse(test$Age=="Young",1,0)
test$Children2_d<-ifelse(test$Children=="2",1,0)
test$Children3_d<-ifelse(test$Children=="3",1,0)
test$History1.Med_d<-ifelse(test$History1=="Medium",1,0)

model2<-glm(formula = Target~ AgeYoung_d + Location + Salary + Children2_d + Catalogs + 
              History1.Med_d, family = "binomial", data = direct)

##Checking the Performance metrics of this model - Kappa and Confusion Metric
##Prediction
pred<-predict(model1,type = "response",newdata = test) ##Response will make sure that return will begiven as probability of event
head(pred)

table(direct$Target)/nrow(direct)
##Proportion of good customers in our data is .399 ,so will set a cutoff of .399
pred<-ifelse(pred>=0.399,1,0)

##Kappa Metric - Ideally for a good model kappa metric should be more than 0.60
kappa(data.frame(test$Target,pred))

#Confusion Metrics - using package caret
confusionMatrix(pred,test$Target,positive="1")

##Gains chart in R to see the prediction in probability of their occurences
##gains package not installed so gains will  not work

