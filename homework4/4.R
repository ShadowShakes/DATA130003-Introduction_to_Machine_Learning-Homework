# Problem 2 

# N-R algorithm for R=200 rounds
NR = function(N){
  beta = c(0.5, 1.2, -1)
  R = 200
  result = matrix(0,0,3)
  for (i in 1:R){
    X1 = rnorm(N)
    X2 = rnorm(N)
    X = cbind(1, X1, X2)  
    Y = exp(X%*%beta)/(1+exp(X%*%beta))
    for (j in 1: length(Y)){
      judge=runif(1,min=0,max=1)
      if (judge>Y[j]){
        Y[j]=0
      }
      if (judge<=Y[j]){
        Y[j]=1
      }
    }
    beta_old = c(-1, -1, -1) 
    beta_new = c(0.5, 0.5, 0.5)
    while(max(abs(beta_old-beta_new))>1e-5)
    {
      P = as.numeric(exp(X %*% beta_new)/(1+exp(X %*% beta_new)))  
      W = diag(P*(1-P))
      partial1 = t(X)%*%(Y-P)
      partial2 = t(X)%*%W%*%X
      beta_old = beta_new
      beta_new = beta_old+solve(partial2)%*%partial1
    }
    result=rbind(result,t(beta_new)) 
  }
  return(result)  
}

# calculate result for N=200,500,800,1000
NL=c(200,500,800,1000)
result1 = NR(NL[1]) 
result2 = NR(NL[2]) 
result3 = NR(NL[3]) 
result4 = NR(NL[4])

# do boxplot (recall that beta=c(0.5,1.2,-1))
boxplot(result1[,1]-0.5, result2[,1]-0.5, result3[,1]-0.5, result4[,1]-0.5, col="coral1", border="dimgray",
main="各轮次计算的beta_0与实际值差值的分布箱线图", ylab="hat(beta_0)-beta_0", names=c('N=200','N=500','N=800','N=1000'))

boxplot(result1[,2]-1.2, result2[,2]-1.2, result3[,2]-1.2, result4[,2]-1.2, col="dodgerblue", border="dimgray",
main="各轮次计算的beta_1与实际值差值的分布箱线图", ylab="hat(beta_1)-beta_1", names=c('N=200','N=500','N=800','N=1000'))

boxplot(result1[,3]+1, result2[,3]+1, result3[,3]+1, result4[,3]+1, col="mediumspringgreen",border="dimgray",
main="各轮次计算的beta_2与实际值差值的分布箱线图", ylab="hat(beta_2)-beta_2", names=c('N=200','N=500','N=800','N=1000'))
