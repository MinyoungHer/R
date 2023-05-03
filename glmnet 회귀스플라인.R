rm(list=ls())

#데이터 생성
set.seed(25)

n=200
x=seq(-2,2,length=n)
f=cos(pi*x)
y=f+rnorm(n,sd=0.3)
par(mfrow=c(1,1))
plot(x,y, col='grey', bty='n')
lines(x,f)

#relu 함수
plus=function(x)
{
  x_plus=x
  x_plus[x<0]=0
  return(x_plus)
}

plus_constant=function(x)
{
  x_plus=rep(1,length(x))
  x_plus[x<0]=0
  return(x_plus)
}


#knot의 개수
knots=seq(x[2],x[n-1], length=50)

#기저함수
B=rep(1,n)
for(k in 1:length(knots))
{
  B=cbind(B, plus_constant(x-knots[k]))
}


library(glmnet)
fit=glmnet(B[,-1],y)
printed=print(fit)
coe=coef(fit, s=0.1384)
fit1=lm(y~(B[,-1]))
fit1$coefficients
lambda= fit$lambda
predict(fit, newx=B[,-1], s=0.000298)


par(mfrow=c(1,1))

for(k in 1:length(lambda))
{
  
  plot(x,y, col='grey', bty='n')
  title(printed$Df[k])
  fitted_values=predict(fit, newx=B[,-1], s=lambda[k])

  lines(x, fitted_values)
 
  coe=coef(fit, s=lambda[k])
  coe=as.matrix(coe)
  wh=c(which(coe!=0)[!which(coe!=0)%in% c(1,2)])
  
  point=c()
  for(i in 1:length(wh))
  {
    a=wh[i]-1
    point=cbind(point, knots[a])
  }
  
  abline(v=point, lty=2, col='grey')
  
   Sys.sleep(1)
  
}

