rm(list=ls())
gc()
setwd("C:\\Users\\Yixuan\\Desktop\\weather_bike\\data") #set working directory
library(openxlsx)
library(lubridate)
library(stringr)
library(dplyr)
library(chron)
library(timeDate)
weather=read.csv("weather.csv",header = T,sep=",")
JNPath=read.xlsx("JNPath.xlsx",colNames=T,rowNames=F)
SWPath=read.xlsx("SWPath.xlsx",colNames=T,rowNames=F)

#unify date format
names(weather)[names(weather)=="sampledate"]="Time"
JNPath$Time=str_sub(JNPath$Time,5,-1)
SWPath$Time=str_sub(SWPath$Time,5,-1)
JNPath$Time=mdy(JNPath$Time)  
SWPath$Time=mdy(SWPath$Time)
weather$Time=as_date(weather$Time)

#merge data
JNPath$location=c("JNPath")
SWPath$location=c("SWPath")
names(JNPath)[names(JNPath)=="JNPath"]="volume"
names(SWPath)[names(SWPath)=="SWPath"]="volume"

JN_weather=inner_join(JNPath,weather,by="Time")
SW_weather=inner_join(SWPath,weather,by="Time")

Sys.setlocale("LC_TIME", "English")  #decide weekdays & weekends
JN_weather$weekday=weekdays(JN_weather$Time)
SW_weather$weekday=weekdays(SW_weather$Time)

hlist=c("USChristmasDay","USMLKingsBirthday","USIndependenceDay",
        "USLaborDay","USVeteransDay","USNewYearsDay","USWashingtonsBirthday",
        "USThanksgivingDay","USColumbusDay") #decide holidays
myholidays=dates(as.character(holiday(2014:2017,hlist)),format="Y-M-D")
JN_weather$holiday=as.numeric(is.holiday(JN_weather$Time,myholidays))
SW_weather$holiday=as.numeric(is.holiday(SW_weather$Time,myholidays))

#UW-Madison school sessions
JN_weather$session=0
for (i in 1:length(JN_weather$Time)) {
  if(JN_weather$Time[i]>=as.Date("2014-1-21") & JN_weather$Time[i]<=as.Date("2014-5-17")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2014-5-17") & JN_weather$Time[i]<=as.Date("2014-8-8")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2014-9-2") & JN_weather$Time[i]<=as.Date("2014-12-20")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2015-1-20") & JN_weather$Time[i]<=as.Date("2015-5-16")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2015-5-26") & JN_weather$Time[i]<=as.Date("2015-8-7")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2015-9-2") & JN_weather$Time[i]<=as.Date("2015-12-23")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2016-1-19") & JN_weather$Time[i]<=as.Date("2016-5-14")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2016-5-23") & JN_weather$Time[i]<=as.Date("2016-8-5")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2016-9-6") & JN_weather$Time[i]<=as.Date("2016-12-23")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2017-1-17") & JN_weather$Time[i]<=as.Date("2017-5-12")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2017-5-30") & JN_weather$Time[i]<=as.Date("2017-8-11")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2017-9-6") & JN_weather$Time[i]<=as.Date("2017-12-21")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2018-1-23") & JN_weather$Time[i]<=as.Date("2018-5-11")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2018-5-29") & JN_weather$Time[i]<=as.Date("2018-8-10")){
    JN_weather$session[i]=1
  }
  else if(JN_weather$Time[i]>=as.Date("2018-9-5") & JN_weather$Time[i]<=as.Date("2018-12-20")){
    JN_weather$session[i]=1
  }
  else(JN_weather$session[i]=0)
}

SW_weather$session=0
for (i in 1:length(SW_weather$Time)) {
  if(SW_weather$Time[i]>=as.Date("2014-1-21") & SW_weather$Time[i]<=as.Date("2014-5-17")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2014-5-17") & SW_weather$Time[i]<=as.Date("2014-8-8")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2014-9-2") & SW_weather$Time[i]<=as.Date("2014-12-20")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2015-1-20") & SW_weather$Time[i]<=as.Date("2015-5-16")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2015-5-26") & SW_weather$Time[i]<=as.Date("2015-8-7")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2015-9-2") & SW_weather$Time[i]<=as.Date("2015-12-23")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2016-1-19") & SW_weather$Time[i]<=as.Date("2016-5-14")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2016-5-23") & SW_weather$Time[i]<=as.Date("2016-8-5")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2016-9-6") & SW_weather$Time[i]<=as.Date("2016-12-23")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2017-1-17") & SW_weather$Time[i]<=as.Date("2017-5-12")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2017-5-30") & SW_weather$Time[i]<=as.Date("2017-8-11")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2017-9-6") & SW_weather$Time[i]<=as.Date("2017-12-21")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2018-1-23") & SW_weather$Time[i]<=as.Date("2018-5-11")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2018-5-29") & SW_weather$Time[i]<=as.Date("2018-8-10")){
    SW_weather$session[i]=1
  }
  else if(SW_weather$Time[i]>=as.Date("2018-9-5") & SW_weather$Time[i]<=as.Date("2018-12-20")){
    SW_weather$session[i]=1
  }
  else(SW_weather$session[i]=0)
}
aggregate_data=rbind(JN_weather,SW_weather) #merge together

#save data
write.xlsx(aggregate_data,"aggregate.xlsx")
save(aggregate_data,file="aggregate.rdata")
write.xlsx(JN_weather,"JN_weather.xlsx")
save(JN_weather,file="JN_weather.rdata")
write.xlsx(SW_weather,"SW_weather.xlsx")
save(SW_weather,file="SW_weather.rdata")
