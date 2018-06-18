#1
data1<-read.table("m-dec19.txt",header=T)
s_return<-data1[,2:3]
acf_d1<-acf(s_return[,1],lag.max = 24)$acf
pacf_d1<-pacf(s_return[,1],lag.max = 24)$acf
list(ACF=as.vector(acf_d1),PACF=as.vector(pacf_d1))
Box.test(s_return[,1],lag = 12,type = "Ljung-Box")

#2
acf_d9<-acf(s_return[,2],lag.max = 12)$acf
list(ACF=as.vector(acf_d9))
Box.test(s_return[,2],lag = 12,type = "Ljung-Box")

#3
data2<-read.table("m-cpileng.txt",header = F)
pg_cpi<-diff(log(data2[,4]))*100
acf_ct<-acf(pg_cpi,lag.max = 12)$acf
pacf_ct<-pacf(pg_cpi,lag.max = 12)$acf
list(ACF=as.vector(acf_ct),PACF=as.vector(pacf_ct))
Box.test(pg_cpi,lag = 12,type = "Ljung-Box")
d1_pg_cpi<-diff(pg_cpi)
acf_d1<-acf(d1_pg_cpi,lag.max = 12)$acf
list(ACF=as.vector(acf_d1))
arma_d1<-arima(pg_cpi,order=c(1,0,5))
arma_d1

#4
data3<-read.table("q-gnprate.txt",header = F)
ar_gnp<-arima(data3,order=c(3,0,0))
z<-c(1,-(ar_gnp$coef[-length(ar_gnp$coef)]))
list(roots=(a<-polyroot(z)))
arg<-Arg(a)/(2*pi)
1/arg
p_ar<-predict(ar_gnp,n.ahead = 4)
list(Forecasts=as.numeric(p_ar$pred),StandardErrors=as.numeric(p_ar$se))

#5
library("TSA")
ma_d9<-arima(s_return[,2],order = c(0,0,1))
res<-as.numeric(ma_d9$residuals)
adf.test(res)
p_d9<-predict(ma_d9,n.ahead = 4)
list(Forecasts=as.numeric(p_d9$pred),StandardErrors=as.numeric(p_d9$se))
