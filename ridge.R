rm(list=ls())
gc()
setwd("D:\\research\\weather_bike\\data")
load("JN_weather_term_added.rdata")
load("SW_weather_term_added.rdata")

#divide sample
JN_weather_high=JN_weather[which(JN_weather$Average.Temperature...F.>=39.2),]
SW_weather_high=SW_weather[which(SW_weather$Average.Temperature...F.>=39.2),]
JN_weather_low=JN_weather[which(JN_weather$Average.Temperature...F.<39.2),]
SW_weather_low=SW_weather[which(SW_weather$Average.Temperature...F.<39.2),]

#ridge: deal with collinearity
library(ridge)

#JN path high temperature
JN_model_high=linearRidge(volume~Average.Temperature...F.+temp2+
                       Humidity....+tempxpre+tempxwind+
                       Rainfall..inch.+Wind..mph.+
                       weekday+holiday+session,data=JN_weather_high)
summary(JN_model_high)
#JN path low temperature
JN_model_low=linearRidge(volume~Average.Temperature...F.+temp2+
                            Humidity....+tempxwind+
                            Snowfall..inch.+Wind..mph.+
                            weekday+holiday+session,data=JN_weather_low)
summary(JN_model_low)

#SW path high temperature
SW_model_high=linearRidge(volume~Average.Temperature...F.+temp2+
                       Humidity....+tempxpre+tempxwind+
                       Rainfall..inch.+Wind..mph.+
                       weekday+holiday+session,data=SW_weather_high)
summary(SW_model_high)
#SW path low temperature
SW_model_low=linearRidge(volume~Average.Temperature...F.+temp2+
                           Humidity....+tempxwind+
                           Snowfall..inch.+Wind..mph.+
                           weekday+holiday+session,data=SW_weather_low)
summary(SW_model_low)