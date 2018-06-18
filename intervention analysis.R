rm(list = ls())
library(TSA)
library(ggplot2)
library(foreign)
library(quantmod)
library(astsa)
library(forecast)
library(aTSA)
library(PerformanceAnalytics)
library(rmgarch)

speech<-read.table("XJPtalk.csv", sep = ",", header = T,stringsAsFactors=F)[,-(2:3)]
ss<-getSymbols("000001.SS", from = '2012-06-01', to = '2018-04-30',auto.assign = F)[,6]
z<-log(ss/Lag(ss,1))[-1]
plot(ss,main="Adjusted Close Price")
#z_all<-weeklyReturn(ss)
#z<-window(z_all,end="2016-12-31");length(z)
#z<-window(z,start="2016-06-01")
plot(z,type='l')
ad_ur<-adf.test(as.vector(z)) #indicates stationary
ggtsdisplay(z,lag.max=400,theme=theme_light(),main = "log-return of SSE Composite Index from Jun 2012 to May 2017")
btv<-c()
for(lag in c(4,8,16,24))
{
  bt<-Box.test(z,lag = lag, type = "Ljung-Box")
  btv<-rbind(btv,c(lag,bt$statistic,bt$p.value))
};colnames(btv)<-c("Lag","X-Squared statistics","p values");btv
auto.arima(z,max.p = 10,max.q = 10,ic="aic")
t<-data.frame(numeric(0))
for(p in 0:5) {
  for(q in 0:5) {
      out<-arima(z,order=c(p,0,q))
      p1<-tseries::adf.test(out$residuals)$p.value
      p2<-Box.test(out$residuals,lag = 6, type="Ljung-Box")$p.value
      t<-rbind(t,c(p,q,out$aic,p1,p2))
  }
};colnames(t)<-c("p","q","AIC","p values for Dickey-Fuller test","p values for Box-Ljung test")
t[which.min(t$AIC),]
ar32<-arima(z,order=c(3,0,2),include.mean = F);ar32
library(lmtest)
coeftest(ar32)
# (1-pt(abs(ar32$coef)/sqrt(diag(ar32$var.coef)),df=length(zz[,1])))*2
btv1<-c()
for(lag in c(4,8,16,24))
{
  bt<-Box.test(ar32$residuals,lag = lag, type = "Ljung-Box")
  btv1<-rbind(btv1,c(lag,bt$statistic,bt$p.value))
};colnames(btv1)<-c("Lag","X-Squared statistics","p values");btv1#indicates well fitted 
btv2<-c()
for(lag in c(4,8,16,24))
{
  b<-Box.test((ar32$residuals)^2,lag=lag,type="Ljung-Box")
  btv2<-rbind(btv2,c(lag,b$statistic,b$p.value))
};colnames(btv2)<-c("lag","X-Squared statistics","p value for Box-Ljung test");btv2 #indicates ARCH effect
arch.test(ar32) #indicates ARCH effect
# arma32<-Arima(z,order=c(3,0,2));arma32
# f_arma32<-Rolling_Forecast(ar32,rbind(z),200)
# plot(as.vector(z_test),type='l')
# lines(as.vector(f_arma32),col="red")


garch22<-ugarchspec(mean.model = list(armaOrder=c(3,2)),#fixed.pars=list(alpha1=0),
                    variance.model = list(garchOrder=c(2,2),model="sGARCH",variance.targeting=T),
                    distribution.model = "std")
garch22_fit<-ugarchfit(garch22,data=z);garch22_fit
#alpha1 not significant, drop from model
garch22<-ugarchspec(mean.model = list(armaOrder=c(3,2)),fixed.pars=list(alpha1=0),
                    variance.model = list(garchOrder=c(2,2),model="sGARCH",variance.targeting=T),
                    distribution.model = "std")
garch22_fit<-ugarchfit(garch22,data=z);garch22_fit


zz<-data.frame(z)
zz["date"]<-as.Date(rownames(zz))
speech1<-speech[speech$category=="党建",]
xjp<-data.frame(date=as.Date(unique(speech1$date)),ind=rep(1,length(unique(speech1$date))))
mtb<-merge(zz,xjp,by="date",all=T)

xjp_effect<-function(data,type,length=5,value)
{
  eff<-numeric(length(data[,1]))
  if(type=="const")
  {
    for(i in which(data$ind!=0))
    {
      eff[i:(i+length-1)]<-rep(value,length)
    }
  }
  if(type=="jump")
  {
    for(i in which(data$ind!=0))
    {
      eff[i]<-value
    }
  }
  if(type=="gradually")
  {
    for(i in which(data$ind!=0))
    {
      eff[i:(i+length-1)]<-(-exp((0:(length-1))/(length-1)*log(2))+2)*value
    }
  }
  return(eff[-which(is.na(data[,2]))])
}
xr1<-xjp_effect(mtb,type="const",21,mean(z))
xr3<-xjp_effect(mtb,type="gradually",21,mean(z))
xr2<-xjp_effect(mtb,type="jump",21,mean(z))

cv<-c()
for (i in seq(3,21,2))
{
  
  xr1<-xjp_effect(mtb,type="const",i,mean(z))
  garch_xjp<-ugarchspec(mean.model = list(armaOrder=c(3,2)#,external.regressors=as.matrix(xr1)
                                          ),fixed.pars=list(alpha1=0),
                      variance.model = list(garchOrder=c(2,2),model="sGARCH",variance.targeting=T
                                            ,external.regressors=as.matrix(xr1)
                                          ),
                      distribution.model = "std")
  garch_xjpfit<-ugarchfit(garch_xjp,data=z);garch_xjpfit
  cv <- rbind(cv, c(i,garch_xjpfit@fit$tval, infocriteria(garch_xjpfit)[1]))
}



#all#mean:const 21
#economics#mean:none #volitility:const 21 #volitility:gradually 13
#politics#mean:const 5 #volitility:const 21 #volitility:gradually 19
#culture#mean:none
#society#mean:none
#ecology#mean:none
#party#mean:none #volitility:const 21
#defense#mean:none
#diplomacy#mean:none
