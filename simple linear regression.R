#1
#(a)
x <- c(2,3,3,3,4,5)
y <- c(0,1,1,2,2,3)
model.a <- lm(y~x)
model.a$coefficients

#(b)
x <- c(2,3,3,3,4,5)
y <- c(0,1,1,2,2,3)
model.b <- lm(y~x -1)
model.b$coefficients

#2
#(a)
EDU <- c(12,20,20,14,16,16,18,14,12,16,15,10)
Income <- c(35,80,78,45,57,65,59,63,57,66,73,23)
plot(EDU, Income)
cor(EDU, Income)

#(b)
model.b <- lm(Income~EDU)
abline(model.b, lwd=2)
model.b$coefficients

#(c)
cov(EDU, Income)
sd(EDU)
sd(Income)

#(d)

#3
#(a)
Length <- c(5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, 4.9, 5.4, 4.8)
Width <- c(3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1, 3.7, 3.4)

mean(Length)
mean(Width)
var(Length)
var(Width)
cor(Length, Width)
#correlation의 값이 0.8로 꽤나 강한 선형관계를 갖는다는 것을 알 수 있음.

#(b)
model.b <- lm(Width~Length)
model.b$coefficients
plot(Length,Width)
abline(model.b, lwd=2)

#(c)
sigma_square <- sum(model.b$residuals^2)/(length(Width)-2)
sigma <- sqrt(sigma_square)
sigma

#(d)
sigma_square <- sum(model.b$residuals^2)/(length(Width)-2)
sigma <- sqrt(sigma_square)
s.e <- sigma*sqrt(1/sum((Length-mean(Length))^2))
t1 <- model.b$coefficients[2]/s.e
t1

p_value <- 2*(1-pt(t1, length(Width)-2))
p_value
#귀무가설 기각

#(e)
predict(model.b, interval = "confidence", newdata = data.frame(Length = c(4.8), level = 0.95))

#(f)
predict(model.b, interval = "predict", newdata = data.frame(Length = c(4.8), level = 0.95))

