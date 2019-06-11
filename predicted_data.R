#Powered by Dark Sky
rm(list=ls())
gc()
setwd("D:\\research\\weather_bike\\data")
load("JN_weather_term_added.rdata")
load("SW_weather_term_added.rdata")
library(darksky)
library(tidyverse)

JN_weather=JN_weather[,c(1,2,24,25,26)]
SW_weather=SW_weather[,c(1,2,24,25,26)]
JN_weather$Time=paste(JN_weather$Time,"T12:00:00",sep="")
SW_weather$Time=paste(SW_weather$Time,"T12:00:00",sep="")

JN_weather$temp.high=0
JN_weather$preci.intensity=0
JN_weather$humidity=0
JN_weather$pressure=0
JN_weather$windSpeed=0
JN_weather$uvindex=0
JN_weather$visibility=0
SW_weather$temp.high=0
SW_weather$preci.intensity=0
SW_weather$humidity=0
SW_weather$pressure=0
SW_weather$windSpeed=0
SW_weather$uvindex=0
SW_weather$visibility=0

for(i in 1:nrow(JN_weather)){
  p=get_forecast_for(43.0731,-89.4012,JN_weather$Time[i])
  JN_weather$temp.high[i] = p$daily$temperatureHigh
  JN_weather$preci.intensity[i] = p$daily$precipIntensity
  JN_weather$humidity[i] = p$daily$humidity
  JN_weather$pressure[i] = p$daily$pressure
  JN_weather$windSpeed[i] = p$daily$windSpeed
  JN_weather$uvindex[i] = p$daily$uvIndex
  JN_weather$visibility[i] = p$daily$visibility
}

for(i in 1:nrow(SW_weather)){
  p=get_forecast_for(43.0731,-89.4012,SW_weather$Time[i])
  SW_weather$temp.high[i] = p$daily$temperatureHigh
  SW_weather$preci.intensity[i] = p$daily$precipIntensity
  SW_weather$humidity[i] = p$daily$humidity
  SW_weather$pressure[i] = p$daily$pressure
  SW_weather$windSpeed[i] = p$daily$windSpeed
  SW_weather$uvindex[i] = p$daily$uvIndex
  SW_weather$visibility[i] = p$daily$visibility
}

write.csv(JN_weather,file="JN_predict.csv")
write.csv(SW_weather,file="SW_predict.csv")
save(JN_weather,file="JN_predict.rdata")
save(SW_weather,file="SW_predict.rdata")
