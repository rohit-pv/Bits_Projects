library(astsa)
library(forecast)
library(smooth)
library(fpp2)
library(tseries)
library(FitAR)
library(TTR)

# Reading the Data
alcohol_data<-read.csv("C:/BITS/Sem2/TSAF_MPBA_G512/Project/Alcohol_Sales.csv")
alcohol_data

#converting to time series Data
a_ts<-ts(alcohol_data$Sales,frequency = 12,start=c(1992,1))
plot.ts(a_ts)

#Simple Exponential Smoothing
asforecasts<-stats::HoltWinters(a_ts, beta=FALSE, gamma=FALSE)
asforecasts
asforecasts$fitted
plot(asforecasts)
asforecasts$SSE
m<-stats::HoltWinters(a_ts, beta=F, gamma=F, start=3459.000)
asforecasts2<-forecast(m)
asforecasts2
plot(asforecasts2$residuals)

#acf(asforecasts2$residuals, lag.max=20)
acf(asforecasts2$residuals, lag.max=20, na.action = na.pass)#na.action is to address the missing values
Box.test(asforecasts2$residuals, lag=20, type="Ljung-Box")
#further improvements are possible

#Simple Moving Average(SMA)
#Smoothing the data using simple moving avereage
ATS_SMA<-SMA(a_ts, N=3)
plot.ts(ATS_SMA)

#Decomposition
ATS_SMA %>% decompose(type="multiplicative")%>%autoplot()+xlab("Year")+ggtitle("Classical multiplicative decomposition of alcohol sales")
#X11 Decomposition

#seasonal dataset
library(seasonal)
ATS_SMA%>%seas(x11="")->fit#seas returns seasonally adjusted data by removing seasonal components
fit
autoplot(fit)+ggtitle("X11 decomposition of alcohol sales")
autoplot(ATS_SMA, series='Original data')+autolayer(trendcycle(fit),series='Trend')+autolayer(seasadj(fit),series="Seasonally adjusted")+xlab("Year")+ylab("New Sales order")+ggtitle("Alcohol Sales")+scale_color_manual(values=c("grey", "blue", "red"), breaks=c("Original data","Trend","Seasonally adjusted"))
SeaA<-seasadj(fit)
plot.ts(SeaA)
ndiffs(SeaA)
nsdiffs(SeaA)

#Differencing Data to remove non-stationarity
a_ts_d1<-diff(SeaA)
plot(a_ts_d1)
acf2(a_ts_d1)

#Differencing Data again to make it stationary
ndiffs(a_ts_d1)
a_ts_d2<-diff(a_ts_d1,differences = 1)
plot(a_ts_d2) # stationary

#ADF test to check stationarity
adf.test(a_ts_d2)
#P<0.05, so we reject null hypothesis.Thus, data is stationary.

#KPSS test to check stationarity
kpss.test(a_ts_d2) #trend stationary

#auto.arima(ABC)
acf2(a_ts_d2)
as_arima<-arima(a_ts,order=c(0,2,1))
as_arima
as_forecast<-forecast:::forecast.Arima(as_arima, h=50)
as_forecast
plot(as_forecast)
acf(as_forecast$residuals,lag.max=20)

Box.test(as_forecast$residuals, lag=20, type="Ljung-Box")
#Test to check the data is stationary)

#SARIMA Model1
sarima(a_ts,3,2,1,0,1,3,12)
sarima.for(a_ts,20,3,2,1,0,1,3,12)

#diff method
nsdiffs(ATS_SMA)
as_d1<-diff(ATS_SMA, lag=12, differences = 1)
plot(as_d1)
acf2(as_d1)
ndiffs(as_d1)
as_dt1<-diff(as_d1)
acf2(as_dt1)
#p=1,q=1,d=1, P=0,D=1,Q=3
#SARIMA Model2
sarima(a_ts,3,1,1,0,1,2,12)
sarima.for(a_ts,20,3,1,1,0,1,2,12)
Box.test(as_dt1, lag=20, type="Ljung-Box")

auto.arima(a_ts, trace = TRUE)
