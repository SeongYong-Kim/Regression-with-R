#1
x <- c(1, 2, 3, 3, 4, 5, 5)
y <- c(3, 7, 5, 8, 11, 14, 12)
#(a)
model.x.y <- lm(y~x)
model.x.y$coef
#Answer : y(hat) = 0.5319 + 2.4468x

#(b)
sigma_square <- sum(model.x.y$re^2) / (length(x) - 2)
sigma <- sqrt(sigma_square)
s.eBeta1 <- sigma * (1 / sqrt(sum((x-mean(x))^2)))
t1 <- (model.x.y$coef[[2]] - 2) / s.eBeta1
p_value <- pt(t1, length(x) - 2)
p_value
#Answer : p-value = 0.8190909

#(c)
s.eBeta0 <- sqrt(sigma_square * (1/length(x) + (mean(x)^2)/sum((x-mean(x))^2)))
U <- model.x.y$coef[[1]] + qt(0.975, length(x)-2)*s.eBeta0
L <- model.x.y$coef[[1]] - qt(0.975, length(x)-2)*s.eBeta0
c(L,U)
#Answer : C.I = (-3.550487  4.614317), confidence interval이 1을 포함하므로 do not reject H0.

#(d)
summary(model.x.y)
model.reduced <- lm(y~1)
SSE.RM <- sum(model.reduced$residuals^2)
SSE.FM <- sum(model.x.y$residuals^2)
F = ((SSE.RM - SSE.FM)/(1+1-1))/(SSE.FM/(length(x)-1-1))
p_value_f <- 1 - pf(F, 1, length(x)-2)
p_value_f
#Answer : p-value = 0.002729

#(e)
#x가 4일때 y의 confidence interval을 구해야한다.
predict(model.x.y,interval="confidence",newdata=data.frame(x=c(4)), level=0.9)
#Answer :        fit      lwr      upr
#           10.31915 8.920531 11.71777

#2
load(file="C:/Users/pc/Desktop/HW2(data).RData")
#(a)
model.gas <- lm(mpg~engine+hp+weight, data.HW2)
model.gas$coef
#Answer : mpg(hat) = 35.1805 - 0.002568engine + 0.0154hp - 0.0018weight

#(b)
#Answer : engine, hp가 fixed 되어있을때 weight가 한단위 올라가면 mpg는 평균적으로 0.0018감소한다.

#(c)
anova(model.gas)$F
#F statistics : (engine, hp, weight) = (33.0076, 0.0592, 9.9575)
anova(model.gas)$Pr
#p-value of each variable : (engine, hp, weight) = (0.0007018494, 0.8147244889, 0.0160272985)
#모든 변수가 fitted된 regression model에서 각각의 변수값이 가지는 p-value는 위와 같다.
#이 p-value는 각각의 변수가 regression model을 얼마나 잘 설명하느냐를 나타내는 척도로 쓸 수 있다.
#이는 각 변수의 계수가 0이 될 확률이라고도 해석할 수 있으며 현재 engine과 weight는 model을 잘 나타내고 계수가 0일 확률이 낮으며
#hp는 model을 잘 설명하지 못하고 있으며 계수가 0일 확률이 높음을 알 수 있다.

#(d)
model.engine <- lm(mpg~engine, data.HW2)
anova(model.engine)$Pr
summary(model.engine)
#p-value : 0.002382
#R-squared : 0.6598

model.hp <- lm(mpg~hp, data.HW2)
anova(model.hp)$Pr
summary(model.hp)
#p-value : 0.4969
#R-squared : 0.05274

model.weight <- lm(mpg~weight, data.HW2)
anova(model.weight)$Pr
summary(model.weight)
#p-value : 0.1381
#R-squared : 0.2273

#Answer : 3개 모델의 p-value와 R-squared를 구한 결과가 위와 같다. p-value와 R-squared를 통해
#         hp, weight로 만든 모델들의 경우 regression model을 잘 설명하고 있지 못한 것을 파악할 수 있다.
#         hp, weight variable을 제거하는 것이 좋겠다.

#(e)
model.engine$coef
#Answer : mpg(hat) = 30.5736 - 0.0025engine

#(f)
anova(model.engine, model.gas)$Pr
#Answer : F-test 결과 p-value값이 0.0446임을 알 수 있다. 이는 꽤나 작은 숫자로,
#         reduced model과 full model이 크게 성능차이가 없다고 말할 수 있다.

#3
#(a)
#regression의 자유도는 변수의 개수로 나타난다. 현재 변수가 X1 한개이므로 df = 1이다.
#Answer : 1

#(b)
SSR <- 1848.76
MSR <- SSR / 1 #MSR은 SSR의 평균값으로 SSR에서 df를 나눈다.
MSR
#Answer : MSR = 1848.76

#(c)
#변수가 1개인 regression model에서는 beta1에 대한 t의 제곱값이 F-statistic의 값과 동일하다.
t1 <- 8.32
F_statistic <- t1^2
F_statistic
#Answer : 69.2224

#(d)
#F_statistic = MSR / MSE로 표현된다.
MSE <- MSR / F_statistic
#MSE = SSE / df 에서 residual의 df는 n-p-1로 표시되고 이는 18이다.
SSE <- MSE * 18
SSE
#Answer : 480.7357

#(e)
#redisual의 df는 n-p-1로 표시되고, p=1에서 df =18이 된다.
#Answer : 18

#(f)
#(d)번에서 MSE를 계산했다.
MSE
#Answer : 26.70754

#(g)
#beta0에 대한 p-value가 0.084이고 이에대한 자유도는 p 즉, 1이다.
t0 <- -qt(0.0824/2, 1)
t0
#Answer : 7.682775

#(h)
#t1 = Beta1/s.eBeta1 에서 Beta1 = t1 * s.eBeta1이다.
s.eBeta1 <- 0.1528
t1 <- 8.32
Beta1 = t1 * s.eBeta1
Beta1
#Answer : 1.271296

#(i)
#n은 sample의 개수이므로 20이다.
#Answer : 20

#(j)
R2 <- 1 - SSE/(SSE + SSR)
R2
#Answer : 0.793631

#(k)
SST <- SSE + SSR
Ra2 <- 1 -(SSE/(20-1-1))/(SST/(20-1))
Ra2
#Answer : 0.782166

#(l)
sigma_square <- SSE / (20 - 2)
sigma <- sqrt(sigma_square)
sigma
#Answer : 5.167934

#(m)
#residual의 자유도가 표기된다. 이는 n-p-1이다.
df <- 20-1-1
df
#Answer : 18

