
Gasoline<- read.table("C:/Users/pc/Desktop/Gasoline.txt",header=T)

#1
attach(Gasoline)
corr.matrix <- cor(Gasoline[,-1])
corr.matrix
pairs(Gasoline[,-1])
#(x1, x2), (x2,x3) 등과 같이 correlation pairwise scatter plot에서 상당히 강한 선형적인 relationship을 가진 변수들이 있다는 것을 확인할 수 있다.
#또한 correation matrix에서도 0.9 이상의 correlation을 가지는 변수들이 확인된다.
#따라서 multicollinearity를 가질 확률이 매우 높다.

#2
R.mat <- corr.matrix

eigen.values<- eigen(R.mat)$values
eigen.values
sum(eigen.values) #eigen value의 합이 변수의 개수인 11이 되는지 확인

sum(eigen.values[1])/sum(eigen.values)
sum(eigen.values[1:2])/sum(eigen.values)
sum(eigen.values[1:3])/sum(eigen.values)
#변수 3개를 사용했을 때, 85%를 넘었기 때문에 3개를 사용해야함

#3
library(car)
model.full <- lm(Y~.,data=Gasoline)
vif(model.full)
#일반적으로 VIF값이 10이 넘어가면 multicollinearity를 일으키는 변수라고 판단한다.
#따라서 X1, X2, X3, X7, X8, X10에 의해 multicollinearity가 발생했다고 예상할 수 있다.

#4
X <- scale(as.matrix(Gasoline[,-1]))
U1<- as.matrix(eigen(R.mat)$vectors[,1],nrow=3)
U2<- as.matrix(eigen(R.mat)$vectors[,2],nrow=3)

Z1<- X%*%U1
Z2<- X%*%U2
Z1
Z2
cbind(Z1,Z2)[1:3,]
# Z1 : -1.7710783 -1.7118429  0.1762204
# Z2 : 0.1463517 -0.8936548  1.7017057

#5
null=lm(Y~X6)
full=lm(Y~.-X11,data=Gasoline)

n = length(Y)

step(null,scope=list(lower=null,upper=full),direction="forward", k=log(n))

#fitted regression equaion : Y(hat) = 32.74244-0.05209X1+0.85052

#6
library(leaps)
best = regsubsets(Y~.,data=Gasoline, force.in=6, force.out=11)
plot(best, scale="adjr2")
#adjr2가 클수록 좋다 X6랑 X5 X8 X10을 사용하는게 좋네, 근데 intercept은 언급 안해줘도 되나?

