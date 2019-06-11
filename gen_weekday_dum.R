rm(list=ls())
gc()
setwd("C:\\Users\\Yixuan\\Desktop\\research\\weather_bike\\data")
load("JN_weather_term_added.rdata")
load("SW_weather_term_added.rdata")

#generating weekday dummy
JN_weather$i.mon=0
JN_weather$i.tue=0
JN_weather$i.wed=0
JN_weather$i.thu=0
JN_weather$i.fri=0
JN_weather$i.sat=0
JN_weather$i.sun=0
SW_weather$i.mon=0
SW_weather$i.tue=0
SW_weather$i.wed=0
SW_weather$i.thu=0
SW_weather$i.fri=0
SW_weather$i.sat=0
SW_weather$i.sun=0
#JN
for(i in 1:length(JN_weather)){
  if(JN_weather$weekday[i]=="Monday"){JN_weather$i.mon[i]=1}
  else(JN_weather$i.mon[i]=0)
}
for(i in 1:length(JN_weather)){
  if(JN_weather$weekday[i]=="Tuesday"){JN_weather$i.tue[i]=1}
  else(JN_weather$i.tue[i]=0)
}
for(i in 1:length(JN_weather)){
  if(JN_weather$weekday[i]=="Wednesday"){JN_weather$i.wed[i]=1}
  else(JN_weather$i.wed[i]=0)
}
for(i in 1:length(JN_weather)){
  if(JN_weather$weekday[i]=="Thursday"){JN_weather$i.thu[i]=1}
  else(JN_weather$i.thu[i]=0)
}
for(i in 1:length(JN_weather)){
  if(JN_weather$weekday[i]=="Friday"){JN_weather$i.fri[i]=1}
  else(JN_weather$i.fri[i]=0)
}
for(i in 1:length(JN_weather)){
  if(JN_weather$weekday[i]=="Saturday"){JN_weather$i.sat[i]=1}
  else(JN_weather$i.sat[i]=0)
}
for(i in 1:length(JN_weather)){
  if(JN_weather$weekday[i]=="Sunday"){JN_weather$i.sun[i]=1}
  else(JN_weather$i.sun[i]=0)
}
#SW
for(i in 1:length(SW_weather)){
  if(SW_weather$weekday[i]=="Monday"){SW_weather$i.mon[i]=1}
  else(SW_weather$i.mon[i]=0)
}
for(i in 1:length(SW_weather)){
  if(SW_weather$weekday[i]=="Tuesday"){SW_weather$i.tue[i]=1}
  else(SW_weather$i.tue[i]=0)
}
for(i in 1:length(SW_weather)){
  if(SW_weather$weekday[i]=="Wednesday"){SW_weather$i.wed[i]=1}
  else(SW_weather$i.wed[i]=0)
}
for(i in 1:length(SW_weather)){
  if(SW_weather$weekday[i]=="Thursday"){SW_weather$i.thu[i]=1}
  else(SW_weather$i.thu[i]=0)
}
for(i in 1:length(SW_weather)){
  if(SW_weather$weekday[i]=="Friday"){SW_weather$i.fri[i]=1}
  else(SW_weather$i.fri[i]=0)
}
for(i in 1:length(SW_weather)){
  if(SW_weather$weekday[i]=="Saturday"){SW_weather$i.sat[i]=1}
  else(SW_weather$i.sat[i]=0)
}
for(i in 1:length(SW_weather)){
  if(SW_weather$weekday[i]=="Sunday"){SW_weather$i.sun[i]=1}
  else(SW_weather$i.sun[i]=0)
}

save(JN_weather,file="JN_weather_term_added.rdata")
save(SW_weather,file="SW_weather_term_added.rdata")
