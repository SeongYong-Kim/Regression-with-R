load(file="C:/Users/pc/Desktop/dataQ1.RData")
#1
#a
model1<-lm(Y~x1)
summary(model1)
#Answer : Y(hat) = 40652.81 - 515.93x1

#b
#Answer : R-squared = 0.9734

#c?
par(mfrow=c(2,2))
plot(x1, scale(model1$residual),pch=16, ylab="Std Residuals")
plot(Y, scale(model1$residual),pch=16, ylab="Std Residuals")
plot(x1, model1$residual,pch=16, ylab="Residuals")
plot(Y, model1$residual,pch=16, ylab="Residuals")

#possible problem
#위의 linear model을 생성할때 기본적인 가정은 데이터가 iid라는 것이다.
#생성된 그래프에서 residual이 normal분포처럼 무작위로 분산되어 있지않고 일정한 경향성을 보인다.
#특히 0을 기준으로 + - + 순으로 순차적으로 redisual이 계산되는 경우 실제 model이 non linear 모델인데 linear model로 fitting했을 수 있다는 문제점이 생긴다.
#또한 밀접한 데이터의 redisual이 차이가 크지 않고 크기가 비슷한 것으로 보아 residual이 서로 dependent일 수 있고 이는 Y가 dependent한 데이터라는 것을 의미한다.
#이는 데이터가 independent라는 기본 가정에 위배된다.

#d
library(tseries)
binarized<-sign(model1$resid)
runs.test(factor(binarized))
#Null hypothesis -> H0 : rho = 0
#Alternative hypothesis -> H1 : rho != 0
#test statistic = -6.149
#p-value = 7.799e-10
#p-value < alpha 이므로 reject null hypothesis

#e
library(lmtest)
DWtest<- dwtest(model1)
DWtest
#Null hypothesis -> H0 : rho = 0
#Alternative hypothesis -> H1 : rho > 0
#test statistic = 0.11683
#p-value = 2.2e-16
#p-value < alpha 이므로 reject null hypothesis


#2
#a
load(file="C:/Users/pc/Desktop/dataQ2.RData")
model2<-lm(y~n)
summary(model2)
#intercept는 p-value가 0.379로 significant하지않다.
#Answer : y(hat) = 1.0606n

#b?
par(mfrow=c(1,1))
y.hat = 1.0606*n
plot(y.hat, scale(model2$resid), pch=16, ylab="Std Residuals" )
#possible problem
#위의 linear model을 생성할때 기본적인 가정은 데이터가 iid라는 것이다.
#생성된 그래프에서 residual이 normal분포처럼 무작위로 분산되어 있지않고 일정한 경향성을 보인다.
#특히 y.hat이 커짐에 따라 residual이 커지는 경향을 보이는데 이는 데이터가 등분산성 가정에 위배될 수 있다는 문제가 생긴다.
#independent는 알 수 없나?

#c
y.sqrt = sqrt(y)
model2.sqrt<-lm(y.sqrt~n)
summary(model2.sqrt)

#d?
#Answer : root(yhat) = 2.612745 + 0.081519n or yhat = (2.612745 + 0.081519n)^2 뭐가 맞을까?

#e
fitted.value = (2.612745 + 0.081519*40)^2
fitted.value
#Answer : fitted value = 34.49806

#f
y.log = log(y)
model2.log.sq<-lm(y.log~poly(n,2))
summary(model2.log.sq)

#g
#Answer : y(hat) = expo(3.62444 + 11.78652n - 4.02615n^2)

