rm(list=ls())
gc()
setwd("D:\\research\\weather_bike\\data")
load("JN_predict.rdata")
load("SW_predict.rdata")

#=======================variable preparation======================#
JN_weather$preci.intensity=JN_weather$preci.intensity*1000
SW_weather$preci.intensity=SW_weather$preci.intensity*1000
JN_weather$temp2=JN_weather$temp.high^2
SW_weather$temp2=SW_weather$temp.high^2
JN_weather$tempxwind=JN_weather$temp.high*JN_weather$windSpeed
SW_weather$tempxwind=SW_weather$temp.high*SW_weather$windSpeed
JN_weather$tempxpre=JN_weather$temp.high*JN_weather$preci.intensity
SW_weather$tempxpre=SW_weather$temp.high*SW_weather$preci.intensity

#=============================explore==============================#
summary(JN_weather)
summary(SW_weather)

plot(JN_weather$temp.high,JN_weather$volume,
     xlab="high temperature",ylab="volume",
     main="JN path")

plot(SW_weather$temp.high,SW_weather$volume,
     xlab="high temperature",ylab="volume",
     main="SW path")

plot(JN_weather$preci.intensity,JN_weather$volume,
     xlab="precipitation intensity",ylab="volume",
     main="JN path")

plot(SW_weather$preci.intensity,SW_weather$volume,
     xlab="precipitation intensity",ylab="volume",
     main="SW path")

plot(JN_weather$uvindex,JN_weather$volume,
     xlab="uv index",ylab="volume",
     main="JN path")

plot(SW_weather$uvindex,SW_weather$volume,
     xlab="uv index",ylab="volume",
     main="SW path")

#============================correlation===========================#
library(corrplot)
library(Hmisc)

#JN
options(digits=2)
JN_features=JN_weather[,c(-1,-3,-13,-14,-15)]
cor_JN=cor(JN_features)
cortest_JN=rcorr(as.matrix(JN_features),type="pearson")

corrplot(cor(JN_features),type="upper",
         method="color",tl.pos = "t",tl.col="black")
corrplot(cor(JN_features),add=T,type="lower",method="number",diag=T,
         cl.pos="n",tl.pos="l",tl.col="black")

#SW
SW_features=SW_weather[,c(-1,-3,-13,-14,-15)]
cor_SW=cor(SW_features)
cortest_SW=rcorr(as.matrix(SW_features),type="pearson")

corrplot(cor(SW_features),type="upper",
         method="color",tl.pos = "t",tl.col="black")
corrplot(cor(SW_features),add=T,type="lower",method="number",diag=T,
         cl.pos="n",tl.pos="l",tl.col="black")

#save results
write.csv(cor_SW,"pred.cor_SW.csv")
write.csv(cortest_SW$P,"pred.cortest_SW.csv")
write.csv(cor_JN,"pred.cor_JN.csv")
write.csv(cortest_JN$P,"pred.cortest_JN.csv")

#================================OLS===============================#
#JN
JN_model=lm(volume~temp.high+preci.intensity+humidity+
              pressure+windSpeed+uvindex+visibility+
              weekday+holiday+session,data=JN_weather)
summary(JN_model)

#SW
SW_model=lm(volume~temp.high+preci.intensity+humidity+
              pressure+windSpeed+uvindex+visibility+
              weekday+holiday+session,data=SW_weather)
summary(SW_model)

#===============OLS with squared and interaction term==============#
#JN
JN_model=lm(volume~temp.high+temp2+preci.intensity+humidity+
              pressure+windSpeed+uvindex+visibility+
              tempxpre+tempxwind+
              weekday+holiday+session,data=JN_weather)
summary(JN_model)
p1=predict(JN_model,JN_weather)
mean(abs(p1-JN_weather$volume))
mean(p1-JN_weather$volume)

#SW
SW_model=lm(volume~temp.high+temp2+preci.intensity+humidity+
              pressure+windSpeed+uvindex+visibility+
              tempxpre+tempxwind+
              weekday+holiday+session,data=SW_weather)
summary(SW_model)
p2=predict(SW_model,SW_weather)
mean(abs(p2-SW_weather$volume))
mean(p2-SW_weather$volume)
