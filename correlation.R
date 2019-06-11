setwd("D:\\research\\weather_bike\\data")
load("JN_weather.rdata")
load("SW_weather.rdata")
library(corrplot)
library(Hmisc)
#correlation matrix
JN_weather$precipitation=JN_weather$`Rainfall.(inch)`+JN_weather$`Snowfall.(inch)`
SW_weather$precipitation=SW_weather$`Rainfall.(inch)`+SW_weather$`Snowfall.(inch)`

#JN path
JN_weather$mean_temp=JN_weather$`Average.Temperature.(°F)`
options(digits=2)
JN_features=data.frame(JN_weather$volume,JN_weather$mean_temp,
                       JN_weather$`Humidity.(%)`,JN_weather$`Wind.(mph)`,
                       JN_weather$precipitation,JN_weather$holiday,JN_weather$session)
cor_JN=cor(JN_features)
cortest_JN=rcorr(as.matrix(JN_features),type="pearson")

corrplot(cor(JN_features),type="upper",
         method="color",tl.pos = "t",tl.col="black")
corrplot(cor(JN_features),add=T,type="lower",method="number",diag=T,
         cl.pos="n",tl.pos="l",tl.col="black")

#SW path
SW_weather$mean_temp=SW_weather$`Average.Temperature.(°F)`
SW_features=data.frame(SW_weather$volume,SW_weather$mean_temp,
                       SW_weather$`Humidity.(%)`,SW_weather$`Wind.(mph)`,
                       SW_weather$precipitation,SW_weather$holiday,
                       SW_weather$session)
cor_SW=cor(SW_features)
cortest_SW=rcorr(as.matrix(SW_features),type="pearson")

corrplot(cor(SW_features),type="upper",
         method="color",tl.pos = "t",tl.col="black")
corrplot(cor(SW_features),add=T,type="lower",method="number",diag=T,
         cl.pos="n",tl.pos="l",tl.col="black")

#save correlation matrix and p-value
write.csv(cor_SW,"C:\\Users\\apple\\Desktop\\weather_bike\\results\\cor_SW.csv")
write.csv(cortest_SW$P,"C:\\Users\\apple\\Desktop\\weather_bike\\results\\cortest_SW.csv")
write.csv(cor_JN,"C:\\Users\\apple\\Desktop\\weather_bike\\results\\cor_JN.csv")
write.csv(cortest_JN$P,"C:\\Users\\apple\\Desktop\\weather_bike\\results\\cortest_JN.csv")