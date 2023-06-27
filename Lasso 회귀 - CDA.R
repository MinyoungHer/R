#데이터 생성
rm(list=ls())

library(glmnet)
data("QuickStartExample")
x=QuickStartExample$x
y=QuickStartExample$y
x
y

B=cbind(1,x)
fit=cv.glmnet(B-1,y)

soft_thresholding=function(c, lambda_val)
{
  if(c>lambda_val)
   
    return(c-lambda_val)
  
  if(abs(c)>lambda_val)
    
    return(c+lambda_val)
    
    return(0)
  
}


#CDA_lasso
epsilon=1e-5
max_iter=10000
lambda_val=0
CDA_lasso=function(B, y, epsilon, max_iter, lambda_val=0)
{
  #J는 행렬 B의 열의 개수
  
  J=ncol(B)
  
  #초기값
  beta=rep(0,J)
  residual=y
  l_one=lambda_val*sum(abs(beta))
  RSS_old=0.5*sum(residual^2)+l_one
  BB=colSums(B^2)
  
    for (r in 1:max_iter)
   {
      partial_residual=residual +beta[1]*B[,1]
      beta[1]=sum(partial_residual*B[,1])/BB[1]
      residual=partial_residual-beta[1]*B[,1]
      
    for(j in 2:J)
    {
      
      partial_residual=residual +beta[j]*B[,j]
      c=sum(partial_residual*B[,j])/BB[j]
     
  
      beta[j]=soft_thresholding(c=c, lambda_val=lambda_val)      
      
      
      residual=partial_residual-beta[j]*B[,j]
      
      cat(r, "번째바퀴", j, "번쨰좌표 업데이트 진행중","\n")
      cat("beta=", beta,"\n")
    }
      l_one=sum(abs(beta[-1]))
    #stopping rule
      
      RSS_new=sum(residual^2)*0.5+l_one
    if(abs(RSS_new-RSS_old)< epsilon)
      break
    RSS_old=RSS_new
  }
  #정보 저장 후 출력
  result=list()
  result$coefficient=beta
  result$fitted.values=y-residual-l_one
  return(result)
  
}


fit2=CDA_lasso(B,y, epsilon=epsilon, max_iter=max_iter)
fit2$coefficient

coef(fit, s=0)
