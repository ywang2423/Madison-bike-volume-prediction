rm(list=ls())
gc()
setwd("C:\\Users\\Yixuan\\Desktop\\research\\weather_bike\\data")
load("JN_weather_term_added.rdata")
load("SW_weather_term_added.rdata")
library(MASS)

#JN path
JN_weather_high=JN_weather[which(JN_weather$Average.Temperature...F.>=40),]
ridge_JN_high=lm.ridge(volume~Average.Temperature...F.+temp2+
                    Humidity....+tempxpre+tempxwind+
                    Rainfall..inch.+Wind..mph.+
                    weekday+holiday+session,data=JN_weather_high,
                  lambda=seq(0,150,by=0.01),model=T)
ridge_JN_high$lambda[which.min(ridge_JN_high$GCV)] #find lambda which minimize GCV
ridge_JN_high$coef[,which.min(ridge_JN_high$GCV)] #find the corresponding coefficient that has the minimum GCV

par(mfrow = c(1,2))
matplot(ridge_JN_high$lambda,t(ridge_JN_high$coef),
        xlab=expression(lamdba), ylab="Cofficients",
        type="l",lty = 1:20)
abline(v=ridge_JN_high$lambda[which.min(ridge_JN_high$GCV)])
#relationship between GCV and lambda
plot(ridge_JN_high$lambda,ridge_JN_high$GCV,type="l",
     xlab=expression(lambda),ylab="GCV")
abline(v=ridge_JN_high$lambda[which.min(ridge_JN_high$GCV)])

#SW path
SW_weather_high=SW_weather[which(SW_weather$Average.Temperature...F.>=40),]
ridge_SW_high=lm.ridge(volume~Average.Temperature...F.+temp2+
                    Humidity....+tempxpre+tempxwind+
                    Rainfall..inch.+Wind..mph.+
                    weekday+holiday+session,data=SW_weather_high,
                  lambda=seq(0,150,by=0.01),model=T)
ridge_SW_high$lambda[which.min(ridge_SW_high$GCV)] #find lambda which minimize GCV
ridge_SW_high$coef[,which.min(ridge_SW_high$GCV)] #find the corresponding coefficient that has the minimum GCV

par(mfrow = c(1,2))
matplot(ridge_SW_high$lambda,t(ridge_SW_high$coef),
        xlab=expression(lamdba), ylab="Cofficients",
        type="l",lty = 1:20)
abline(v=ridge_SW_high$lambda[which.min(ridge_SW_high$GCV)])
#relationship between GCV and lambda
plot(ridge_SW_high$lambda,ridge_SW_high$GCV,type="l",
     xlab=expression(lambda),ylab="GCV")
abline(v=ridge_SW_high$lambda[which.min(ridge_SW_high$GCV)])
