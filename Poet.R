## load library
library(flare)
library(POET)
library(huge)
df = read.csv('hsi_1701-3.csv')
new_df = na.omit(df)
cov=cov(new_df)
# cov
# e = eigen(cov, symmetric = TRUE)
# e
# POETKhat(cov)
# #gives 1
# Sy<-POET(cov,1,0.5,'soft','vad')$SigmaY
# Su<-POET(cov,1,0.5,'soft','vad')$SigmaU
v1 = c(rep(1,50))
x = data.matrix(new_df)
nx = huge.npn(x, npn.func = "shrinkage", npn.thresh = NULL, verbose = TRUE) #convert to normal scores
tiger = sugm(nx,  method = "tiger")
plot(tiger)
#lambda 1
invs = tiger$icov[[1]]
w1 = invs%*%v1
w2 = (v1%*%invs%*%v1)
w = w1*(1/w2[1,])
w
hist(w)
for (i in 1:50){
  if (w[i]<0.02){
    w[i]=0
  }
}
w = (1/sum(w)) *w
sum(w)
hist(w)
p2r <- function(x)  diff(x, lag =20)
ret_m = t(apply(x,2,function(x){p2r(x)})) #daily return
# The following expressions compute the same
# thing as that t(apply(...)) does:

# 100 * (log(a[,-1]) - log(a[,-ncol(a)]))
# or
# 100 * log(a[,-1]/a[,-ncol(a)])
# 
# The [,-1] (all but the first column) and
# [,-ncol(a)] (all but the last column) is
# a way to work with lagged data.
ret = c(rep(NA, 50))
for( i in 1:50){
  ret[i] = ret_m[i]/x[1,i]
}
ret = ret %*% w
ret
# settingthreshold from 0 -> 0.02 increase our return from 0.07385182 to 0.08145335
#2
invs = tiger$icov[[2]]
w1 = invs%*%v1
w2 = (v1%*%invs%*%v1)
w = w1*(1/w2[1,])
for (i in 1:99){
  if (w[i]<0.01){
    w[i]=0
  }
}
w = (1/sum(w)) *w
sum(w)
hist(w)
p2r <- function(x)  diff(x, lag =20)
ret_m = t(apply(x,2,function(x){p2r(x)})) #daily return
for( i in 1:99){
  ret[i] = ret_m[i]/x[1,i]
}
ret = ret %*% w
ret

#3
invs = tiger$icov[[3]]
w1 = invs%*%v1
w2 = (v1%*%invs%*%v1)
w = w1*(1/w2[1,])
plot(w)
for (i in 1:99){
  if (w[i]<0.02){
    w[i]=0
  }
}
w = (1/sum(w)) *w
w
sum(w)
hist(w)
p2r <- function(x)  diff(x, lag =20)
ret_m = t(apply(x,2,function(x){p2r(x)})) #daily return
for( i in 1:99){
  ret[i] = ret_m[i]/x[1,i]
}
ret = ret %*% w
ret

#4
invs = tiger$icov[[4]]
w1 = invs%*%v1
w2 = (v1%*%invs%*%v1)
w = w1*(1/w2[1,])
for (i in 1:99){
  if (w[i]<0.01){
    w[i]=0
  }
}
w = (1/sum(w)) *w
sum(w)
hist(w)
p2r <- function(x)  diff(x, lag =20)
ret_m = t(apply(x,2,function(x){p2r(x)})) #daily return
for( i in 1:99){
  ret[i] = ret_m[i]/x[1,i]
}
ret = ret %*% w
ret


#5
invs = tiger$icov[[5]]
w1 = invs%*%v1
w2 = (v1%*%invs%*%v1)
w = w1*(1/w2[1,])
for (i in 1:99){
  if (w[i]<0.01){
    w[i]=0
  }
}
w = (1/sum(w)) *w
sum(w)
hist(w)
p2r <- function(x)  diff(x, lag =20)
ret_m = t(apply(x,2,function(x){p2r(x)})) #daily return
for( i in 1:99){
  ret[i] = ret_m[i]/x[1,i]
}
ret = ret %*% w
ret
