
n=30
N=100

lambda=1
X=rep(0,n*N)
X=rexp(n*N,rate=lambda)
Xmax=rep(0,N)
#hist(X)
B=rep(0,n)
for (i in 1:N){
  for (j in 1:n){
    B[j]=X[n*(i-1)+j]
  }
  Xmax[i]=max(B)
 }
Xmax
hist(Xmax-log(n))

f_gumbel <- function(x) {
  exp(-(x + exp(-x)))
}

y=f_gumbel(Xmax-log(n))
hist(y)

library(evd)
library(evir)
library(evmix)
library(extRemes)
ks.test(Xmax-log(n),"pgumbel")