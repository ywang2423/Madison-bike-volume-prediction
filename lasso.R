rm(list=ls())
gc()
setwd("D:\\research\\weather_bike\\data")
load("JN_weather_term_added.rdata")
load("SW_weather_term_added.rdata")
library(lars)

#divide sample
JN_weather_high=JN_weather[which(JN_weather$Average.Temperature...F.>=39.2),]
SW_weather_high=SW_weather[which(SW_weather$Average.Temperature...F.>=39.2),]
JN_weather_low=JN_weather[which(JN_weather$Average.Temperature...F.<39.2),]
SW_weather_low=SW_weather[which(SW_weather$Average.Temperature...F.<39.2),]

#matrix preparation
#JN high temperature
JN_high_x=as.matrix(JN_weather_high[,c(4,5,6,8,25,26,61,58,63,68,69,70,71,72,73)])
JN_high_y=as.matrix(JN_weather_high[,2])
#JN low temperature
JN_low_x=as.matrix(JN_weather_low[,c(4,5,7,8,25,26,61,58,63,68,69,70,71,72,73)])
JN_low_y=as.matrix(JN_weather_low[,2])
#SW high temperature
SW_high_x=as.matrix(SW_weather_high[,c(4,5,6,8,25,26,61,58,63,68,69,70,71,72,73)])
SW_high_y=as.matrix(SW_weather_high[,2])
#SW low temperature
SW_low_x=as.matrix(SW_weather_low[,c(4,5,7,8,25,26,61,58,63,68,69,70,71,72,73)])
SW_low_y=as.matrix(SW_weather_low[,2])

#lasso: variable selection
JN_model_high=lars(JN_high_x,JN_high_y,type="lasso")
JN_model_high
summary(JN_model_high)
plot(JN_model_high)
JN_model_high$beta[which.min(JN_model_high$Cp),]

JN_model_low=lars(JN_low_x,JN_low_y,type="lasso")
JN_model_low
summary(JN_model_low)
plot(JN_model_low)
JN_model_low$beta[which.min(JN_model_low$Cp),]

SW_model_high=lars(SW_high_x,SW_high_y,type="lar")
SW_model_high
summary(SW_model_high)
plot(SW_model_high)
SW_model_high$beta[which.min(SW_model_high$Cp),]

SW_model_low=lars(SW_low_x,SW_low_y,type="lar")
SW_model_low
summary(SW_model_low)
plot(SW_model_low)
SW_model_low$beta[which.min(SW_model_low$Cp),]
