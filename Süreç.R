#MA SÜRECİ
set.seed(12)
n<-10
e<-rnorm(n)
x.ma1<-e[2:n]-0.8*e[1:(n-1)]
x.ma1


plot(x.ma1, type="l", xlab="Time", ylab="Series")

covariance<-acf(x.ma1, type = "covariance", lag.max = NULL, plot = FALSE)
covariance

oto1<-acf(x.ma1, xlab="Lag", ylab="ACF", main="")
print(oto1)
ot1<-pacf(x.ma1, xlab="Lag", ylab="PACF", main="")
print(ot1)

set.seed(23)
n<-10
e<-rnorm(n)
x.ma2<-e[3:n]+0.7*e[2:(n-1)]-0.2*e[1:(n-2)]
x.ma2
covariance1<-acf(x.ma2, type = "covariance", lag.max = NULL, plot = FALSE)
covariance1



plot(x.ma2, type="l", xlab="Time", ylab="Series")

oto2<-acf(x.ma2, xlab="Lag", ylab="ACF", main="")
print(oto2)

ot2<-pacf(x.ma2, xlab="Lag", ylab="PACF", main="")
print(ot2)



#AR SÜRECİ

set.seed(2)
alfa1 <- 1.3  
alfa2 <- -0.42 
ar2_sureci <- arima.sim(n = 10, list(ar = c(alfa1, alfa2)))
ar2_sureci 
plot(ar2_sureci, main = "AR(2)", ylab = "Degerler", xlab = "Zaman")

a<-acf(ar2_sureci)
print(a)

a1<-pacf(ar2_sureci)
print(a1)




