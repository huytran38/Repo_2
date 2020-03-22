library(magrittr)
library(fpp2)
library(astsa)
library(lmtest)
library(forecast)
library(lubridate)
library(tseries)

########################### Monthly data ##############################


###################################################
###         Exploratory Data Analysis           ###
###################################################

#Using aggregated (by month) data with all data for 11 stations
air1 = read.csv("D:/DSC425_TimeSeries/all_jm.csv")
air1 %>% head()

#convert factor to month-date values
air1$just_month = as.yearmon(air1$just_month, "%Y-%m")
measures = c("CO","NO","NO2","NOx","O3","PM10","SO2","TCH","TOL")
measures1 = c("NO2","NOx","O3","PM10","SO2","NMHC")
measures2 = c("CO","NO","TCH")

#all 9 particles 1/2011-5/2018
ggplot() + 
  geom_line(data = air1,aes(x = just_month,y = CO,colour="CO"),size=0.8) + 
  xlab('Years') +
  ylab('CO')

ggplot() + 
  geom_line(data = air1,aes(x = just_month,y = NO,colour="NO"),size=0.8) + 
  xlab('Years') +
  ylab('NO')

ggplot() + 
  geom_line(data = air1,aes(x = just_month,y = NO_2,colour="NO2"),size=0.8) + 
  xlab('Years') +
  ylab('NO2')

ggplot() + 
  geom_line(data = air1,aes(x = just_month,y = NOx,colour="NOx"),size=0.8) + 
  xlab('Years') +
  ylab('NOx')

ggplot() + 
  geom_line(data = air1,aes(x = just_month,y = O_3,colour="O3"),size=0.8) + 
  labs(title= "O3 measurements 2001 - 2018") +
  xlab('Years') +
  ylab('O3')

ggplot() + 
  geom_line(data = air1,aes(x = just_month,y = PM10,colour="PM10"),size=0.8) + 
  xlab('Years') +
  ylab('PM10')

ggplot() + 
  geom_line(data = air1,aes(x = just_month,y = SO_2,colour="SO2"),size=0.8) + 
  xlab('Years') +
  ylab('SO2')

ggplot() + 
  geom_line(data = air1,aes(x = just_month,y = TCH,colour="TCH"),size=0.8) + 
  xlab('Years') +
  ylab('TCH')

ggplot() + 
  geom_line(data = air1,aes(x = just_month,y = NMHC,colour="NMHC"),size=0.8) + 
  xlab('Years') +
  ylab('NMHC')

#Using aggregated (by month) data with train set(50%)
#air2 = read.csv("D:/DSC425_TimeSeries/train_jm.csv")
#air2 %>% head()
#air2 %>% tail()
#str(air2)

#plot of 3 measurements: NO2, NOx, O3
par(mfrow=c(1,1))
par(mfrow=c(3,1), mar=c(1,1,1,1), oma=c(2,2,2,2))
plot(air1$NO_2, type = 'l', main = "NO2")
plot(air1$NOx, type = 'l', main = "NOx")
plot(air1$O_3, type = 'l', main = "O3")


###################################################
###         Explore time series data            ###
###################################################

#convert to time-series data for O3
O3 = ts(air1$O_3, start=c(2001,1), frequency=12)
frequency(O3)

#plots, log, diff, and ACF/PACF
autoplot(O3)
O3 %>% acf(lag.max = 60, main="O3 ACF Plot, 60 lags")
O3 %>% pacf(lag.max = 60,main="O3 PACF Plot, 60 lags")
O3 %>% acf2()

O3 %>% log() %>% autoplot(main="O3 Log")
O3 %>% log() %>% acf2(main="O3 Log ACF/PACF")

O3 %>% diff(12) %>% autoplot(main="O3 diff(12)")
O3 %>% diff(12) %>% acf2(main="O3 diff(12) ACF/PACF") #diff shows stationary

O3 %>% log() %>% diff(12) %>% autoplot(main="O3 Log & 1st order diff(12)")
O3 %>% log() %>% diff(12) %>% acf2(main="O3 Log & 1st order diff(12) ACF/PACF")
O3 %>% log() %>% diff(12) %>% acf2(max.lag=60,main="O3 Log & 1st order diff(12) ACF/PACF")

#O3 %>% log %>% diff(12) %>% diff() %>% autoplot()
#O3 %>% log %>% diff(12) %>% diff() %>% acf2()
#O3 %>% log() %>% diff() %>% diff(12) %>% autoplot()
#O3 %>% log() %>% diff() %>% diff(12) %>% acf2()

#reset graphics
dev.off() 

#histogram, QQ-plot
O3 %>% hist(main='O3 Distribution', xlab="O3", border="blue", col="green")
qqnorm(O3)
qqline(O3)


###################################################
###           Test time-series models           ###
###################################################

######## Check AR models ########
AR1 = Arima(O3, order=c(1,0,0)) 
plot(residuals(AR1), main="AR1 residuals")
AR1_fit = O3 - residuals(AR1) 

AR2 = Arima(O3, order=c(2,0,0)) 
plot(residuals(AR2), main="AR2 residuals")
AR2_fit = O3 - residuals(AR2) 

plot(O3, main="Fitting AR1 model")
points(AR1_fit, col=2, type="l")  
plot(O3, main="Fitting AR2 model")
points(AR2_fit, col=4, type="l")

AR1 
AR2 #AR2 performs better

#Check residuals for AR2
AR2$residuals %>% autoplot(main="AR2 residuals")
Box.test(AR2$residuals, lag=3, type= 'Ljung-Box')  
Box.test(AR2$residuals, lag=5, type= 'Ljung-Box')  
Box.test(AR2$residuals, lag=15, type= 'Ljung-Box') 
Box.test(AR2$residuals, lag=50, type= 'Ljung-Box') 

AR2$residuals %>% hist(main='AR2 residuals distribution', xlab="residuals", border="blue", col="green")
qqnorm(AR2$residuals, main="AR2 residuals Normal Q-Q Plot")
qqline(AR2$residuals)
AR2$residuals %>% acf2(main="AR2 residuals ACF/PACF")


######## Check MA models ########
MA1 = Arima(O3, order=c(0,0,1)) 
plot(residuals(MA1),main="MA1 residuals")
MA1_fit = O3 - residuals(MA1)

MA2 = Arima(O3, order=c(0,0,2)) 
plot(residuals(MA2),main="MA2 residuals")
MA2_fit = O3 - residuals(MA2)

plot(O3, main="Fitting MA1 model")
points(MA1_fit, col=2, type="l") 
plot(O3, main="Fitting MA2 model")
points(MA2_fit, col=4, type="l") #MA2 seems to fit better

MA1
MA2 #MA2 performs better

#Check residuals for MA2
MA2$residuals %>% autoplot(main="MA2 residuals")
Box.test(MA2$residuals, lag=3, type= 'Ljung-Box')  
Box.test(MA2$residuals, lag=5, type= 'Ljung-Box')  
Box.test(MA2$residuals, lag=15, type= 'Ljung-Box')
Box.test(MA2$residuals, lag=50, type= 'Ljung-Box') 

MA2$residuals %>% hist(main='MA2 residuals distribution', xlab="residuals", border="blue", col="green")
qqnorm(MA2$residuals, main="MA2 residuals Normal Q-Q Plot")
qqline(MA2$residuals)
MA2$residuals %>% acf2(main="MA2 residuals ACF/PACF")

######## Use auto.arima() ########
m = auto.arima(O3,ic=c('aic'))
m
coeftest(m)

m = auto.arima(O3,ic=c('aic'), seasonal = F) #suppress seasonality
m
coeftest(m)

### OBSSERVATION: model with seasonality performs better (lower AIC score)
#Fit SARIMA model
sarima(O3, p=1, d=0, q=0, P=1, D=0, Q=0, S = 12) #Pure SAR: aic = 1229.02
sarima(O3, p=0, d=0, q=1, P=0, D=0, Q=1, S = 12) #Pure SAM: aic = 1404.42 
sarima(O3, p=1, d=0, q=1, P=1, D=0, Q=1, S = 12) #SARIMA1: aic = 1161.28
sarima(O3, p=1, d=0, q=1, P=0, D=1, Q=1, S = 12) #SARIMA2: aic = 1074.72 (option 1)
sarima(O3, p=1, d=0, q=2, P=0, D=1, Q=1, S = 12) #SARIMA3: aic = 1076.57
sarima(O3, p=1, d=0, q=1, P=1, D=1, Q=1, S = 12) #SARIMA4: aic = 1076.72
sarima(O3, p=1, d=1, q=1, P=1, D=1, Q=1, S = 12) #SARIMA5: aic = 1075.35 (option 2)
sarima(O3, p=1, d=0, q=2, P=1, D=1, Q=1, S = 12)

m1 = Arima(O3, order = c(1,0,1), seasonal = c(0,1,1))
m1
coeftest(m1)

m2 = Arima(O3, order = c(1,1,1), seasonal = c(1,1,1))
m2
coeftest(m2)

##### Model m1 #####
#Since ma1 is not significantZero out ma1 coefficient when fitting ARIMA model
m1 = Arima(O3, order = c(1,0,1), seasonal = c(0,1,1), fixed=c(NA,0,NA))
m1
coeftest(m1)

#Check residuals for model m1
m1$residuals %>% autoplot()
Box.test(m1$residuals, lag=3, type= 'Ljung-Box') 
Box.test(m1$residuals, lag=5, type= 'Ljung-Box')
Box.test(m1$residuals, lag=15, type= 'Ljung-Box')
Box.test(m1$residuals, lag=50, type= 'Ljung-Box')

m1$residuals %>% hist(main='m1 Residuals distribution', xlab="residuals", border="blue", col="green")
qqnorm(m1$residuals, main='m1 Residuals Normal Q-Q Plot')
qqline(m1$residuals)
m1$residuals %>% acf2(main='m1 Residuals ACF/PACF')

##### Model m2 #####
#Since sar1 and sam1 are not significant, Zero sar1/ma1 coefficients when fitting ARIMA model
m2 = Arima(O3, order = c(1,1,1), seasonal = c(1,1,1), fixed=c(NA,NA,0,NA))
m2
coeftest(m2)

#Check residuals for model m2
m2$residuals %>% autoplot()
Box.test(m2$residuals, lag=3, type= 'Ljung-Box') 
Box.test(m2$residuals, lag=5, type= 'Ljung-Box')
Box.test(m2$residuals, lag=15, type= 'Ljung-Box')
Box.test(m2$residuals, lag=50, type= 'Ljung-Box')

m2$residuals %>% hist(main='m2 Residuals distribution', xlab="residuals", border="blue", col="green")
qqnorm(m2$residuals, main='m2 Residuals Normal Q-Q Plot')
qqline(m2$residuals)
m2$residuals %>% acf2(main='m2 Residuals ACF/PACF')


###################################################
###           Apply test set                    ###
###################################################

#get test data
#air3 = read.csv("D:/DSC425_TimeSeries/valid_jm.csv")
#air3 %>% head()
#air3 %>% tail()
#str(air3)

#convert to time-series data for O3
#O3t = ts(air3$O_3, start=c(2012,1), frequency=12)
#frequency(O3t)

#fitting m1 and m2 with train set/testing with test set
train = window(O3, start=2001, end=c(2012,12))
test = window(O3, start=c(2013,1))

m1 = Arima(train, order = c(1,0,1), seasonal = c(0,1,1))
m1
coeftest(m1)
m1 %>% forecast(h=65)
test %>% head(65)
forecast(m1, h=65) %>% plot()
lines(test, col=2)

m2 = Arima(train, order = c(1,1,1), seasonal = c(1,1,1))
m2
coeftest(m2)
m2 %>% forecast(h=36)
test %>% head(36)
forecast(m2, h=36) %>% plot()
lines(test, col=2)

#RMSE
RMSE1 = sqrt(mean(m1$residuals^2))
MAE1 = mean(abs(m1$residuals))
RMSE1
MAE1

RMSE2 = sqrt(mean(m2$residuals^2))
MAE2 = mean(abs(m2$residuals))
RMSE2
MAE2


###################################################
###         Trend + Quadratic                   ###
###################################################

lm = tslm(O3 ~ trend + I(trend^2))
fcast = forecast(lm, h=36)

autoplot(O3) + 
  autolayer(lm$fitted.values) +
  autolayer(fcast)


###################################################
###              Trend + Seasonality            ###
###################################################

lm = tslm(train ~ trend + season)
summary(lm)

fcast = forecast(lm, h=65)

autoplot(O3) + 
  autolayer(lm$fitted.values) +
  autolayer(fcast) +
  autolayer(test)


###################################################
###      Trend + Quadratic + Seasonality        ###
###################################################

lm = tslm(train ~ trend + I(trend^2) + season) 
summary(lm)

fcast = forecast(lm, h=65)

autoplot(O3) + 
  autolayer(lm$fitted.values) +
  autolayer(fcast) +
  autolayer(test)


###################################################
###           Use backtest function             ###
###################################################

source("D:/DSC425_TimeSeries/Assignments/backtest_v2.R")
tr = backtest(m1,O3,144,1)
tt = backtest(m1,O3,65,1)

tr = backtest(m2,O3,144,1) 
tt = backtest(m2,O3,65,1) 
