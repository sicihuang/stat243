########2(c)########
####overwrite####
library(pryr)

#function to compute cholesky decomposition for n by n matrix X; overwrite X with upper 
#triangular matrix U
cholOverwrite <- function(i){
  n <- i*1000
  x <- crossprod( matrix( rnorm(n^2), n ) )
  x <- chol(x)
}

dimen <- seq(from=1000, to=9000, by=1000)
#initialize
maxMemo <- rep(0,9)
procTime <- rep(0,9)

#get maximum memory use and processing time separately so system.time function doesn't add 
#to memory use 
gc(reset = TRUE)
for (i in 1:9){
  gc(reset = TRUE)
  cholOverwrite(i)
  #2nd row 6th column of gc is max memory used in Mb
  maxMemo[i] <- gc()[2,6] 
}

#3rd column of system.time is elapsed time
for (i in 1:9){
  gc(reset = TRUE)
  procTime[i] <- as.double( system.time(cholOverwrite(i))[3] )
}

dimen  
maxMemo
procTime

par(mfrow=c(1,2), pty="s")
plot(dimen,maxMemo)
plot(dimen,procTime)

####not overwrite####
rm(list=ls())

#function to compute cholesky decomposition for n by n matrix X; keep both X and upper 
#triangular matrix U
cholKeep <- function(i){
  n <- i*1000
  x <- crossprod( matrix( rnorm(n^2),n ) )
  u <- chol(x)
}

dimen <- seq(from=1000, to=9000, by=1000)
#initialize
maxMemo <- rep(0,9)
procTime <- rep(0,9)

gc(reset = TRUE)
for (i in 1:9){
  gc(reset = TRUE)
  cholKeep(i)
  maxMemo[i] <- gc()[2,6] 
}

for (i in 1:9){
  gc(reset = TRUE)
  procTime[i] <- as.double( system.time(cholKeep(i))[3] )
}

dimen
maxMemo 
procTime 

par(mfrow=c(1,2), pty="s")
plot(dimen,maxMemo)
plot(dimen,procTime)

########3(a)########
x <- crossprod( matrix( rnorm(5000^2), 5000 ) )
y <- matrix( rnorm(5000), ncol=1 )

#approach a
system.time(b1 <- solve(x)%*%y)

#approach b
system.time(b2 <- solve(x,y))

#approach c
system.time(b3 <- backsolve( chol(x), backsolve(chol(x),y,transpose=TRUE) ))

########3(b)########
all.equal(b1,b2,tolerance=.Machine$double.eps)
all.equal(b1,b3,tolerance=.Machine$double.eps)
all.equal(b2,b3,tolerance=.Machine$double.eps)

max(abs(b1-b2),abs(b1-b3),abs(b2-b3))

#compute condition number
norm(x)*norm(solve(x))

########4########
gls <- function(x,y,v){
  u <- chol(v)
  x1 <- backsolve(u,x,transpose=TRUE)
  y1 <- backsolve(u,y,transpose=TRUE)
  b <- qr.solve(x1,y1)
  return(b)
}

x <- matrix( rnorm(1000*100), 1000 )
#make v positive definite
v <- crossprod( matrix( rnorm(1000^2), 1000 ) ) 
y <- matrix( rnorm(1000), ncol=1 )

head(gls(x,y,v))