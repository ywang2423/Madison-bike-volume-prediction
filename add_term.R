setwd("D:\\research\\weather_bike\\data")
load("JN_weather.rdata")
load("SW_weather.rdata")
library(xts)
library(magrittr)

#generating precipitation data
JN_weather$precipitation=JN_weather$`Rainfall.(inch)`+JN_weather$`Snowfall.(inch)`
SW_weather$precipitation=SW_weather$`Rainfall.(inch)`+SW_weather$`Snowfall.(inch)`

#defining time series
JN_weather=xts(JN_weather[,-1],order.by = JN_weather$Time)
SW_weather=xts(SW_weather[,-1],order.by = SW_weather$Time)

#generating lagged term for mean temp
JN_weather$l1.mean_temp=lag(JN_weather$`Average.Temperature.(°F)`,k=1)%>%as.numeric()
JN_weather$l2.mean_temp=lag(JN_weather$l1.mean_temp,k=1)
JN_weather$l3.mean_temp=lag(JN_weather$l2.mean_temp,k=1)
JN_weather$l4.mean_temp=lag(JN_weather$l3.mean_temp,k=1)
JN_weather$l5.mean_temp=lag(JN_weather$l4.mean_temp,k=1)

SW_weather$l1.mean_temp=lag(SW_weather$`Average.Temperature.(°F)`,k=1)%>%as.numeric()
SW_weather$l2.mean_temp=lag(SW_weather$l1.mean_temp,k=1)
SW_weather$l3.mean_temp=lag(SW_weather$l2.mean_temp,k=1)
SW_weather$l4.mean_temp=lag(SW_weather$l3.mean_temp,k=1)
SW_weather$l5.mean_temp=lag(SW_weather$l4.mean_temp,k=1)

#generating lagged term for precipitation
JN_weather$l1.pre=lag(JN_weather$precipitation,k=1)
JN_weather$l2.pre=lag(JN_weather$l1.pre,k=1)
JN_weather$l3.pre=lag(JN_weather$l2.pre,k=1)
JN_weather$l4.pre=lag(JN_weather$l3.pre,k=1)
JN_weather$l5.pre=lag(JN_weather$l4.pre,k=1)

SW_weather$l1.pre=lag(SW_weather$precipitation,k=1)
SW_weather$l2.pre=lag(SW_weather$l1.pre,k=1)
SW_weather$l3.pre=lag(SW_weather$l2.pre,k=1)
SW_weather$l4.pre=lag(SW_weather$l3.pre,k=1)
SW_weather$l5.pre=lag(SW_weather$l4.pre,k=1)
#generating lagged term for rainfall
JN_weather$l1.rainfall=lag(JN_weather$Rainfall..inch.,k=1)
JN_weather$l2.rainfall=lag(JN_weather$l1.rainfall,k=1)
JN_weather$l3.rainfall=lag(JN_weather$l2.rainfall,k=1)
JN_weather$l4.rainfall=lag(JN_weather$l3.rainfall,k=1)
JN_weather$l5.rainfall=lag(JN_weather$l4.rainfall,k=1)

SW_weather$l1.rainfall=lag(SW_weather$Rainfall..inch.,k=1)
SW_weather$l2.rainfall=lag(SW_weather$l1.rainfall,k=1)
SW_weather$l3.rainfall=lag(SW_weather$l2.rainfall,k=1)
SW_weather$l4.rainfall=lag(SW_weather$l3.rainfall,k=1)
SW_weather$l5.rainfall=lag(SW_weather$l4.rainfall,k=1)
#generating lagged term for snow
JN_weather$l1.snow=lag(JN_weather$Snowfall..inch.,k=1)
JN_weather$l2.snow=lag(JN_weather$l1.snow,k=1)
JN_weather$l3.snow=lag(JN_weather$l2.snow,k=1)
JN_weather$l4.snow=lag(JN_weather$l3.snow,k=1)
JN_weather$l5.snow=lag(JN_weather$l4.snow,k=1)

SW_weather$l1.snow=lag(SW_weather$Snowfall..inch.,k=1)
SW_weather$l2.snow=lag(SW_weather$l1.snow,k=1)
SW_weather$l3.snow=lag(SW_weather$l2.snow,k=1)
SW_weather$l4.snow=lag(SW_weather$l3.snow,k=1)
SW_weather$l5.snow=lag(SW_weather$l4.snow,k=1)
#generating lagged term for wind speed
JN_weather$l1.windspeed=lag(JN_weather$Wind..mph.,k=1)
JN_weather$l2.windspeed=lag(JN_weather$l1.windspeed,k=1)
JN_weather$l3.windspeed=lag(JN_weather$l2.windspeed,k=1)
JN_weather$l4.windspeed=lag(JN_weather$l3.windspeed,k=1)
JN_weather$l5.windspeed=lag(JN_weather$l4.windspeed,k=1)

SW_weather$l1.windspeed=lag(SW_weather$Wind..mph.,k=1)
SW_weather$l2.windspeed=lag(SW_weather$l1.windspeed,k=1)
SW_weather$l3.windspeed=lag(SW_weather$l2.windspeed,k=1)
SW_weather$l4.windspeed=lag(SW_weather$l3.windspeed,k=1)
SW_weather$l5.windspeed=lag(SW_weather$l4.windspeed,k=1)
#generating lagged term for humidity
JN_weather$l1.humidity=lag(JN_weather$Humidity....,k=1)
JN_weather$l2.humidity=lag(JN_weather$l1.humidity,k=1)
JN_weather$l3.humidity=lag(JN_weather$l2.humidity,k=1)
JN_weather$l4.humidity=lag(JN_weather$l3.humidity,k=1)
JN_weather$l5.humidity=lag(JN_weather$l4.humidity,k=1)

SW_weather$l1.humidity=lag(SW_weather$Humidity....,k=1)
SW_weather$l2.humidity=lag(SW_weather$l1.humidity,k=1)
SW_weather$l3.humidity=lag(SW_weather$l2.humidity,k=1)
SW_weather$l4.humidity=lag(SW_weather$l3.humidity,k=1)
SW_weather$l5.humidity=lag(SW_weather$l4.humidity,k=1)

#generate interaction term
JN_weather$tempxpre=JN_weather$Average.Temperature...F.*JN_weather$precipitation
SW_weather$tempxpre=SW_weather$Average.Temperature...F.*SW_weather$precipitation

JN_weather$tempxrain=JN_weather$Average.Temperature...F.*JN_weather$Rainfall..inch.
SW_weather$tempxrain=SW_weather$Average.Temperature...F.*SW_weather$Rainfall..inch.

JN_weather$tempxsnow=JN_weather$Average.Temperature...F.*JN_weather$Snowfall..inch.
SW_weather$tempxsnow=SW_weather$Average.Temperature...F.*SW_weather$Snowfall..inch.

JN_weather$tempxwind=JN_weather$Average.Temperature...F.*JN_weather$Wind..mph.
SW_weather$tempxwind=SW_weather$Average.Temperature...F.*SW_weather$Wind..mph.

JN_weather$prexwind=JN_weather$precipitation*JN_weather$Wind..mph.
SW_weather$prexwind=SW_weather$precipitation*SW_weather$Wind..mph.

#generate squared term
JN_weather$temp2=JN_weather$Average.Temperature...F.^2
SW_weather$temp2=SW_weather$Average.Temperature...F.^2

JN_weather$rain2=JN_weather$Rainfall..inch.^2
SW_weather$rain2=SW_weather$Rainfall..inch.^2

JN_weather$snow2=JN_weather$Snowfall..inch.^2
SW_weather$snow2=SW_weather$Snowfall..inch.^2

JN_weather$pre2=JN_weather$precipitation^2
SW_weather$pre2=SW_weather$precipitation^2

JN_weather$wind2=JN_weather$Wind..mph.^2
SW_weather$wind2=SW_weather$Wind..mph.^2

JN_weather_added_term=JN_weather
SW_weather_added_term=SW_weather
#save data
save(JN_weather_added_term,file="JN_weather_term_added.rdata")
save(SW_weather_added_term,file="SW_weather_term_added.rdata")

#replace the NA columns caused by as.numeric
col=c(seq(26,66,1))
load("JN_weather_term_added.rdata")
load("JN_weather_term_added.rdata")
load("JN_weather.rdata")
load("SW_weather.rdata")

name=c("Time",names(JN_weather_added_term))

JN_weather[,col+1]=JN_weather_added_term[,col]
SW_weather[,col+1]=SW_weather_added_term[,col]

names(JN_weather)=name
names(SW_weather)=name

aggregate_data=rbind(JN_weather,SW_weather)

save(JN_weather,file="JN_weather_term_added.rdata")
save(SW_weather,file="SW_weather_term_added.rdata")
save(aggregate_data,file="aggregate_term_added.rdata")