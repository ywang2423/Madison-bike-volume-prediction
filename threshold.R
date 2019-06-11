rm(list=ls())
gc()
setwd("C:\\Users\\Yixuan\\Desktop\\research\\weather_bike\\data")
load("JN_weather_term_added.rdata")
load("SW_weather_term_added.rdata")
library(caret)
library(car)

#divide sample
JN_weather_high=JN_weather[which(JN_weather$Average.Temperature...F.>=39.2),]
SW_weather_high=SW_weather[which(SW_weather$Average.Temperature...F.>=39.2),]
JN_weather_low=JN_weather[which(JN_weather$Average.Temperature...F.<39.2),]
SW_weather_low=SW_weather[which(SW_weather$Average.Temperature...F.<39.2),]

#original model

#creat folds
JN_folds_high=createFolds(y=JN_weather_high[,1],k=10)
SW_folds_high=createFolds(y=SW_weather_high[,1],k=10)
JN_folds_low=createFolds(y=JN_weather_low[,1],k=10)
SW_folds_low=createFolds(y=SW_weather_low[,1],k=10)

#JN path high temperature
JN_OLS_list=list()
JN_OLS_prd=list()
JN_OLS_dev=c()
for(i in 1:10){
  fold_test=JN_weather[JN_folds_high[[i]],]
  fold_train=JN_weather[-JN_folds_high[[i]],]
  JN_OLS_list[[i]]=lm(volume~Average.Temperature...F.+temp2+
                        Humidity....+tempxpre+tempxwind+
                        Rainfall..inch.+Wind..mph.+
                        weekday+holiday+session,data=fold_train)
  JN_OLS_prd[[i]]=predict(JN_OLS_list[[i]],fold_test)
  JN_OLS_dev[i]=mean((JN_OLS_prd[[i]]-fold_test$volume)^2)
}
summary(JN_OLS_list[[which.min(JN_OLS_dev)]])
vif(JN_OLS_list[[which.min(JN_OLS_dev)]])
plot(JN_OLS_list[[which.min(JN_OLS_dev)]])

#JN path low temperature
JN_OLS_list=list()
JN_OLS_prd=list()
JN_OLS_dev=c()
for(i in 1:10){
  fold_test=JN_weather[JN_folds_low[[i]],]
  fold_train=JN_weather[-JN_folds_low[[i]],]
  JN_OLS_list[[i]]=lm(volume~Average.Temperature...F.+temp2+
                        Humidity....+tempxwind+
                        Snowfall..inch.+Wind..mph.+
                        weekday+holiday+session,data=fold_train)
  JN_OLS_prd[[i]]=predict(JN_OLS_list[[i]],fold_test)
  JN_OLS_dev[i]=mean((JN_OLS_prd[[i]]-fold_test$volume)^2)
}
summary(JN_OLS_list[[which.min(JN_OLS_dev)]])
vif(JN_OLS_list[[which.min(JN_OLS_dev)]])
plot(JN_OLS_list[[which.min(JN_OLS_dev)]])

#SW path high temperature
SW_OLS_list=list()
SW_OLS_prd=list()
SW_OLS_dev=c()
for(i in 1:10){
  fold_test=SW_weather[SW_folds_high[[i]],]
  fold_train=SW_weather[-SW_folds_high[[i]],]
  SW_OLS_list[[i]]=lm(volume~Average.Temperature...F.+temp2+
                        Humidity....+tempxpre+tempxwind+
                        Rainfall..inch.+Wind..mph.+
                        weekday+holiday+session,data=fold_train)
  SW_OLS_prd[[i]]=predict(SW_OLS_list[[i]],fold_test)
  SW_OLS_dev[i]=mean((SW_OLS_prd[[i]]-fold_test$volume)^2)
}
summary(SW_OLS_list[[which.min(SW_OLS_dev)]])
vif(SW_OLS_list[[which.min(SW_OLS_dev)]])
plot(SW_OLS_list[[which.min(SW_OLS_dev)]])

#SW path low temperature
SW_OLS_list=list()
SW_OLS_prd=list()
SW_OLS_dev=c()
for(i in 1:10){
  fold_test=SW_weather[SW_folds_low[[i]],]
  fold_train=SW_weather[-SW_folds_low[[i]],]
  SW_OLS_list[[i]]=lm(volume~Average.Temperature...F.+temp2+
                        Humidity....+tempxwind+
                        Snowfall..inch.+Wind..mph.+
                        weekday+holiday+session,data=fold_train)
  SW_OLS_prd[[i]]=predict(SW_OLS_list[[i]],fold_test)
  SW_OLS_dev[i]=mean((SW_OLS_prd[[i]]-fold_test$volume)^2)
}
summary(SW_OLS_list[[which.min(SW_OLS_dev)]])
vif(SW_OLS_list[[which.min(SW_OLS_dev)]])
plot(SW_OLS_list[[which.min(SW_OLS_dev)]])