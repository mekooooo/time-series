library("astsa")
library("TSA")
library("forecast")
#1
data<-read.table("m-mortg.txt",header=F)
data<-log(data[,4])
data_1<-diff(data)
adf.test(data_1)
acf2(data_1)
model1<-arima(data,order=c(2,1,1))
res_1<-model1$residuals
adf.test(res_1)
Box.test(res_1,lag=12,type="Ljung-Box")
p_1<-predict(model1,n.ahead = 4)

#2
dec1<-read.table("m-dec1-8006.txt",header=F)
dec1<-dec1[,2]
smodel_12<-arima(dec1,order=c(0,0,1),seasonal=list(order=c(1,0,1),period=12))
res_12<-smodel_12$residuals
Box.test(res_12,type="Ljung-Box",lag = 24)

#3
earn<-read.table("q-aa-earn.txt",header=F)
earn<-earn[,4]
earn1<-diff(earn)
earn1_4<-diff(earn1,4)
plot.ts(cbind(earn,earn1,diff(earn1,4)),main='')
adf.test(earn1)
earn4<-diff(earn1,lag=4)
acf2(earn4)
model1<-arima(earn,order=c(0,1,1))
t<-numeric(0)
for(p in 0:2) {
  for(q in 0:2) {
    for(P in 0:2) {
      for(Q in 0:2)
      {out<-arima(earn,order=c(p,1,q),seasonal=list(order=c(P,1,Q),period=4))
      p1<-adf.test(out$residuals)$p.value
      p2<-Box.test(out$residuals,lag = 12, type="Ljung-Box")$p.value
        t<-rbind(t,c(p,q,P,Q,out$aic,p1,p2))
      }
    } }
}
model2<-arima(earn,order=c(0,1,0),seasonal = list(order=c(2,1,2),period=4))
model2
res_2<-model2$residuals
adf.test(res_2)
Box.test(res_2,lag = 12,type="Ljung-Box")
predict(model2,n.ahead = 4)
acf2(earn2)
