rm(list=ls())
gc()
setwd("C:\\Users\\Yixuan\\Desktop\\research\\weather_bike\\data")
load("JN_weather_term_added.rdata")
load("SW_weather_term_added.rdata")
library(caret)

#creat folds
JN_folds=createFolds(y=JN_weather[,1],k=10)
SW_folds=createFolds(y=SW_weather[,1],k=10)

#OLS model with lagged term and interaction term

#current and L1
#JN path
JN_OLS_list=list()
JN_OLS_prd=list()
JN_OLS_dev=c()
for(i in 1:10){
  fold_test=JN_weather[JN_folds[[i]],]
  fold_train=JN_weather[-JN_folds[[i]],]
  JN_OLS_list[[i]]=lm(volume~Average.Temperature...F.+
                        l1.mean_temp+l1.pre+
                        Humidity....+tempxpre+tempxwind+
                        Rainfall..inch.+Snowfall..inch.+Wind..mph.+
                        weekday+holiday+session,data=fold_train)
  JN_OLS_prd[[i]]=predict(JN_OLS_list[[i]],fold_test)
  JN_OLS_dev[i]=mean((JN_OLS_prd[[i]]-fold_test$volume)^2)
}
summary(JN_OLS_list[[which.min(JN_OLS_dev)]])

#SW path
SW_OLS_list=list()
SW_OLS_prd=list()
SW_OLS_dev=c()
for(i in 1:10){
  fold_test=SW_weather[SW_folds[[i]],]
  fold_train=SW_weather[-SW_folds[[i]],]
  SW_OLS_list[[i]]=lm(volume~Average.Temperature...F.+
                        l1.mean_temp+l1.pre+
                        Humidity....+tempxpre+tempxwind+
                        Rainfall..inch.+Snowfall..inch.+Wind..mph.+
                        weekday+holiday+session,data=fold_train)
  SW_OLS_prd[[i]]=predict(SW_OLS_list[[i]],fold_test)
  SW_OLS_dev[i]=mean((SW_OLS_prd[[i]]-fold_test$volume)^2)
}
summary(SW_OLS_list[[which.min(SW_OLS_dev)]])

#only L1
#JN path
JN_OLS_list=list()
JN_OLS_prd=list()
JN_OLS_dev=c()
for(i in 1:10){
  fold_test=JN_weather[JN_folds[[i]],]
  fold_train=JN_weather[-JN_folds[[i]],]
  JN_OLS_list[[i]]=lm(volume~
                        l1.mean_temp+l1.pre+
                        Humidity....+tempxpre+tempxwind+
                        Wind..mph.+
                        weekday+holiday+session,data=fold_train)
  JN_OLS_prd[[i]]=predict(JN_OLS_list[[i]],fold_test)
  JN_OLS_dev[i]=mean((JN_OLS_prd[[i]]-fold_test$volume)^2)
}
summary(JN_OLS_list[[which.min(JN_OLS_dev)]])

#SW path
SW_OLS_list=list()
SW_OLS_prd=list()
SW_OLS_dev=c()
for(i in 1:10){
  fold_test=SW_weather[SW_folds[[i]],]
  fold_train=SW_weather[-SW_folds[[i]],]
  SW_OLS_list[[i]]=lm(volume~
                        l1.mean_temp+l1.pre+
                        Humidity....+tempxpre+tempxwind+
                        Wind..mph.+
                        weekday+holiday+session,data=fold_train)
  SW_OLS_prd[[i]]=predict(SW_OLS_list[[i]],fold_test)
  SW_OLS_dev[i]=mean((SW_OLS_prd[[i]]-fold_test$volume)^2)
}
summary(SW_OLS_list[[which.min(SW_OLS_dev)]])

#only current
#JN path
JN_OLS_list=list()
JN_OLS_prd=list()
JN_OLS_dev=c()
for(i in 1:10){
  fold_test=JN_weather[JN_folds[[i]],]
  fold_train=JN_weather[-JN_folds[[i]],]
  JN_OLS_list[[i]]=lm(volume~Average.Temperature...F.+
                        Humidity....+tempxpre+tempxwind+
                        Rainfall..inch.+Snowfall..inch.+Wind..mph.+
                        weekday+holiday+session,data=fold_train)
  JN_OLS_prd[[i]]=predict(JN_OLS_list[[i]],fold_test)
  JN_OLS_dev[i]=mean((JN_OLS_prd[[i]]-fold_test$volume)^2)
}
summary(JN_OLS_list[[which.min(JN_OLS_dev)]])

#SW path
SW_OLS_list=list()
SW_OLS_prd=list()
SW_OLS_dev=c()
for(i in 1:10){
  fold_test=SW_weather[SW_folds[[i]],]
  fold_train=SW_weather[-SW_folds[[i]],]
  SW_OLS_list[[i]]=lm(volume~Average.Temperature...F.+
                        Humidity....+tempxpre+tempxwind+
                        Rainfall..inch.+Snowfall..inch.+Wind..mph.+
                        weekday+holiday+session,data=fold_train)
  SW_OLS_prd[[i]]=predict(SW_OLS_list[[i]],fold_test)
  SW_OLS_dev[i]=mean((SW_OLS_prd[[i]]-fold_test$volume)^2)
}
summary(SW_OLS_list[[which.min(SW_OLS_dev)]])