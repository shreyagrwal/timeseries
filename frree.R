# load data
absentdata<- read.csv("absenteeismatwork.csv")
# apply libraries for analysis
library(stargazer)
library(dplyr)
library(ggplot2)
library(forecast)
library(fpp)
library(ggpubr)
# stats for intial data
summary(absentdata)
stargazer(absentdata, type= text)
reg<-lm(absenttime~ distance+transexp+bmi , data= absentdata)
reg$coefficients
plot(reg$residuals)
plot(reg)
stargazer(reg)
summary(reg)
# convert the data
absentnew <- absentdata %>% group_by( Year, month) %>% summarise(transexp=mean(transexp),	distance=mean(distance),	servtime=mean(servtime),	age=mean(age),	childen=mean(childen),	bmi=mean(bmi),	absenttime=sum(absenttime))
# making timseries and splitting the data
absentts <- ts(absentnew, start= c(2007,7), frequency = 12)
train <-window(absentts, end=c(2009,12))
test <-window(absentts, start=c(2010,1))
# summary stats for train data
summary(train)
boxplot(train[,4:9])
str(train)
stargazer(train)
ggdensity(train[,9], main = "Density plot", xlab = "Absent Time")
plot(absentts[,"absenttime"])
reg1<-lm(absenttime~ distance+transexp+age , data= train)
plot(absenttime~ distance+transexp+age , data= train)
abline(reg1)
reg1$coefficients
summary(reg1)
boxplot(absentnew[,4:9])
train[,"absenttime"]
plot(train[,"absenttime"], type= "l")
# Finding the model
decompose_df <- tslm(absentts[,"absenttime"] ~ trend + fourier(absentts[,"absenttime"], 2))
trend <- coef(decompose_df)[1] + coef(decompose_df)['trend']*seq_along(absentts[,"absenttime"])
components <- cbind(
  data = absentts[,"absenttime"],
  trend = trend,  
  season = absentts[,"absenttime"] - trend - residuals(decompose_df),
  remainder = residuals(decompose_df)
)
autoplot(components, facet=TRUE)
# 1 Trying Arima model
acf(train[,"absenttime"])
pacf(train[,"absenttime"])
lambda<- BoxCox.lambda(train[,"absenttime"])
lambda
timeseries1<- BoxCox(train[,"absenttime"], lambda)
plot(timeseries1, type="l")
acf(timeseries1)
pacf(timeseries1)
adf.test(timeseries1)
kpss.test(timeseries1)
timeseriesD <- diff(timeseries1)
adf.test(timeseriesD)
kpss.test(timeseriesD)
Box.test(timeseriesD, type="Ljung-Box")
bestfit <- auto.arima(timeseries1)
bestfit
bestfit2<- Arima(timeseries1, order=c(2,1,1))
bestfit2
plot(residuals(bestfit),type="p")
acf(residuals(bestfit))
r1 <- residuals(bestfit)
plot(na.omit(r1))
qqnorm(as.vector(r1),main="")
qqline(r1,distribution=qnorm)
hist(r1,main="")
acf(na.omit(r1))
r2 <- residuals(bestfit2)
plot(na.omit(r2))
qqnorm(as.vector(r2),main="")
qqline(r2,distribution=qnorm)
hist(r2,main="")
acf(na.omit(r2))
# 2 Checking for GARCH model
sqres<-residuals(bestfit2)^2
par(mfrow=c(1,3))
plot(sqres)
acf(sqres)
pacf(sqres)
# 3 Applying simple exponential smoothing
bestfit3 <- ses(train[,"absenttime"], alpha=0.3, h=12)
plot(bestfit3, col='red',main="")
par(mfrow=c(2,2))
plot(as.vector(bestfit3$fitted),as.vector(bestfit3$residuals),
     xlab="Fitted values",ylab="Residuals")
hist(bestfit3$residuals,main="")
plot(bestfit3$residuals,ylab="Residuals")
acf(bestfit3$residuals)
plot(absentts[,"absenttime"], col='white',main="")
lines(train[,"absenttime"])
lines(test[,"absenttime"],col="black")
lines(forecast(bestfit3,12)$mean,col="red")
# 4 Applying Neural network
fit<-nnetar(train[,"absenttime"]
plot(forecast(fit,h=20),main="")
checkresiduals(fit)
fit %>%
  forecast(h=7) %>%
  autoplot() + autolayer(test[,"absenttime"])
qqnorm(as.vector(residuals(fit)),main="")
qqline(residuals(fit),distribution=qnorm)