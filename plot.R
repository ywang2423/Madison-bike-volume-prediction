setwd("C:\\Users\\Yixuan\\Desktop\\weather_bike\\data")
load("aggregate.rdata")
load("JN_weather.rdata")
load("SW_weather.rdata")
library(ggplot2)
library(plotrix)

#volume and average temperature
twoord.plot(lx=JN_weather$Time,ly=JN_weather$volume,rx=JN_weather$Time,
            ry=JN_weather$`Average.Temperature.(°F)`,
            main="JNPath average temperature and volume",xlab="date"
            ,ylab="volume",rylab="temperature (F)",type=c("line","line"),
            xtickpos=as.numeric(JN_weather$Time),
            xticklab = as.character(JN_weather$Time)) #JN Path

twoord.plot(lx=SW_weather$Time,ly=SW_weather$volume,rx=SW_weather$Time,
            ry=SW_weather$`Average.Temperature.(°F)`,
            main="SWPath average temperature and volume",xlab="date"
            ,ylab="volume",rylab="temperature (F)",type=c("line","line"),
            xtickpos=as.numeric(SW_weather$Time),
            xticklab = as.character(SW_weather$Time)) #SW Path

#volume and average precipitation
JN_weather$preci=JN_weather$`Rainfall.(inch)`+JN_weather$`Snowfall.(inch)`
SW_weather$preci=SW_weather$`Rainfall.(inch)`+SW_weather$`Snowfall.(inch)`

twoord.plot(lx=JN_weather$Time,ly=JN_weather$volume,rx=JN_weather$Time,
            ry=JN_weather$preci,
            main="JNPath precipitation and volume",xlab="date"
            ,ylab="volume",rylab="precipitation (inch)",type=c("line","line"),
            xtickpos=as.numeric(JN_weather$Time),
            xticklab = as.character(JN_weather$Time)) #JN Path

twoord.plot(lx=SW_weather$Time,ly=SW_weather$volume,rx=SW_weather$Time,
            ry=SW_weather$preci,
            main="SWPath precipitation and volume",xlab="date"
            ,ylab="volume",rylab="precipitation (inch)",type=c("line","line"),
            xtickpos=as.numeric(SW_weather$Time),
            xticklab = as.character(SW_weather$Time)) #SW Path

#volume and wind speed
twoord.plot(lx=JN_weather$Time,ly=JN_weather$volume,rx=JN_weather$Time,
            ry=JN_weather$`Wind.(mph)`,
            main="JNPath wind speed and volume",xlab="date"
            ,ylab="volume",rylab="wind (mph)",type=c("line","line"),
            xtickpos=as.numeric(JN_weather$Time),
            xticklab = as.character(JN_weather$Time)) #JN Path

twoord.plot(lx=SW_weather$Time,ly=SW_weather$volume,rx=SW_weather$Time,
            ry=SW_weather$`Wind.(mph)`,
            main="SWPath wind speed and volume",xlab="date"
            ,ylab="volume",rylab="wind (mph)",type=c("line","line"),
            xtickpos=as.numeric(SW_weather$Time),
            xticklab = as.character(SW_weather$Time)) #SW Path

#volume and weekday
weekday=factor(c("Monday","Tuesday","Wednesday","Thursday",
        "Friday","Saturday","Sunday"),levels=c("Monday","Tuesday",
        "Wednesday","Thursday","Friday","Saturday","Sunday"))
#JN path
mon_volume_JN=mean(JN_weather[JN_weather$weekday=="Monday",]$volume)
tue_volume_JN=mean(JN_weather[JN_weather$weekday=="Tuesday",]$volume)
wed_volume_JN=mean(JN_weather[JN_weather$weekday=="Wednesday",]$volume)
thu_volume_JN=mean(JN_weather[JN_weather$weekday=="Thursday",]$volume)
fri_volume_JN=mean(JN_weather[JN_weather$weekday=="Friday",]$volume)
sat_volume_JN=mean(JN_weather[JN_weather$weekday=="Saturday",]$volume)
sun_volume_JN=mean(JN_weather[JN_weather$weekday=="Sunday",]$volume)
JN_weekday=c(mon_volume_JN,tue_volume_JN,wed_volume_JN,thu_volume_JN,
             fri_volume_JN,sat_volume_JN,sun_volume_JN)

ggplot(data=data.frame(weekday,JN_weekday))+
  geom_col(mapping=aes(x=weekday,y=JN_weekday),fill="navy")+
  xlab("day of the week")+ylab("average volume")+ggtitle("JN Path")
#SW path
mon_volume_SW=mean(SW_weather[SW_weather$weekday=="Monday",]$volume)
tue_volume_SW=mean(SW_weather[SW_weather$weekday=="Tuesday",]$volume)
wed_volume_SW=mean(SW_weather[SW_weather$weekday=="Wednesday",]$volume)
thu_volume_SW=mean(SW_weather[SW_weather$weekday=="Thursday",]$volume)
fri_volume_SW=mean(SW_weather[SW_weather$weekday=="Friday",]$volume)
sat_volume_SW=mean(SW_weather[SW_weather$weekday=="Saturday",]$volume)
sun_volume_SW=mean(SW_weather[SW_weather$weekday=="Sunday",]$volume)
SW_weekday=c(mon_volume_SW,tue_volume_SW,wed_volume_SW,thu_volume_SW,
             fri_volume_SW,sat_volume_SW,sun_volume_SW)

ggplot(data=data.frame(weekday,SW_weekday))+
  geom_col(mapping=aes(x=weekday,y=SW_weekday),fill="darkgreen")+
  xlab("day of the week")+ylab("average volume")+ggtitle("SW Path")

#volume and holiday
holiday=factor(c("business day","weekends","holiday"),
               levels=c("business day","weekends","holiday"))
#JN path
weekend_volume_JN=mean(JN_weather[JN_weather$weekday=="Saturday"|
                                  JN_weather$weekday=="Sunday",]$volume)
holiday_volume_JN=mean(JN_weather[JN_weather$holiday==1,]$volume)
business_volue_JN=mean(JN_weather[JN_weather$holiday!=1 &
                                  JN_weather$weekday!="Saturday" & 
                                  JN_weather$weekday!="Sunday",]$volume)
JN_holiday=c(business_volue_JN,weekend_volume_JN,holiday_volume_JN)

ggplot(data=data.frame(holiday,JN_holiday))+
  geom_col(mapping=aes(x=holiday,y=JN_holiday),fill="navy")+
  xlab("type of the day")+ylab("average volume")+ggtitle("JN Path")
#SW path
weekend_volume_SW=mean(SW_weather[SW_weather$weekday=="Saturday"|
                                  SW_weather$weekday=="Sunday",]$volume)
holiday_volume_SW=mean(SW_weather[SW_weather$holiday==1,]$volume)
business_volue_SW=mean(SW_weather[SW_weather$holiday!=1 &
                                  SW_weather$weekday!="Saturday" & 
                                  SW_weather$weekday!="Sunday",]$volume)
SW_holiday=c(business_volue_SW,weekend_volume_SW,holiday_volume_SW)

ggplot(data=data.frame(holiday,SW_holiday))+
  geom_col(mapping=aes(x=holiday,y=SW_holiday),fill="darkgreen")+
  xlab("type of the day")+ylab("average volume")+ggtitle("SW Path")

#UW-Madison session
session=factor(c("UW-Madison session","not UW-Madsion session"),
               levels=c("UW-Madison session","not UW-Madsion session"))
#JN path
session_volume_JN=mean(JN_weather[JN_weather$session==1,]$volume)
nosession_volume_JN=mean(JN_weather[JN_weather$session==0,]$volume)
JN_session=c(session_volume_JN,nosession_volume_JN)

ggplot(data=data.frame(session,JN_session))+
  geom_col(mapping=aes(x=session,y=JN_session),fill="navy")+
  xlab("UW-Madison session")+ylab("average volume")+ggtitle("JN Path")

#SW Path
session_volume_SW=mean(SW_weather[SW_weather$session==1,]$volume)
nosession_volume_SW=mean(SW_weather[SW_weather$session==0,]$volume)
SW_session=c(session_volume_SW,nosession_volume_SW)

ggplot(data=data.frame(session,SW_session))+
  geom_col(mapping=aes(x=session,y=SW_session),fill="darkgreen")+
  xlab("UW-Madison session")+ylab("average volume")+ggtitle("SW Path")
