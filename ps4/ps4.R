set.seed(0) 
save(.Random.seed, file = 'tmp.Rda')

tmp <- function() { 
  browser()
  load('tmp.Rda')
  browser()
  runif(1)
} 
tmp()


tmp <- function() { 
  load('tmp.Rda', env = .GlobalEnv) 
  runif(1)
}
tmp()
tmp()

sumDenom <- function( n, p, phi ){
  logDenom <- function( k ){
    #special case k=0, returns NaN if computed using the generic formula
    if( k==0 ){
      exp( (n*phi)*log(1-p) )
    }
    #special case k=n, returns NaN if computed using the generic formula
    else if( k==n ){
      exp( (n*phi)*log(p) )
    }
    else{
      exp( lchoose(n,k)+k*log(k)+(n-k)*log(n-k)-n*log(n)+phi*(n*log(n)-k*log(k)-(n-k)*
           log(n-k))+(k*phi)*log(p)+((n-k)*phi)*log(1-p) )
    }
  }
  return( sum( sapply(0:n, logDenom) ) )
}

sumDenom(10,0.3,0.5)


sumDenomVec <- function( n, p, phi ){
  k <- 0:n
  denom <- exp( lchoose(n,k)+k*log(k)+(n-k)*log(n-k)-n*log(n)+phi*(n*log(n)-k*log(k)-(n-k)*
                log(n-k))+(k*phi)*log(p)+((n-k)*phi)*log(1-p) )
  #substitute in results for special cases when k=0 and k=n
  denom[1] <- exp( (n*phi)*log(1-p) )
  denom[n+1] <- exp( (n*phi)*log(p) )
  return( sum(denom) )
}

sumDenomVec(10,0.3,0.5)

system.time( sumDenom(20,0.3,0.5) )
system.time( sumDenomVec(20,0.3,0.5) )
system.time( sumDenom(200,0.3,0.5) )
system.time( sumDenomVec(200,0.3,0.5) )
system.time( sumDenom(2000,0.3,0.5) )
system.time( sumDenomVec(2000,0.3,0.5) )


load("/Users/Sici/Documents/Cal/stat243/units/mixedMember.Rda")
wgtSumA <- sapply(1:100000, function(x){ muA[ IDsA[[x]] ]%*%wgtsA[[x]] })
wgtSumB <- sapply(1:100000, function(x){ wgtsB[[x]]%*%muB[ IDsB[[x]] ] })


#determine number of rows for matrices
maxLengthA <- max( sapply(IDsA, length) )
#set up mu matrix with columns being the appropriate mu's for each individual
muIdA <- matrix( 0, nrow=maxLengthA, ncol=100000 )
for( i in 1:ncol(muIdA) ){
  muIdA[,i] <- c( muA[ IDsA[[i]] ], rep( 0, maxLengthA-length(IDsA[[i]]) ) )
}
#construct matrix weightA from list wgtsA; each column is one element of the list 
weightA <- matrix( 0, nrow=maxLengthA, ncol=100000 )
for( i in 1:ncol(weightA) ){
  weightA[,i] <- c( wgtsA[[i]], rep( 0, maxLengthA-length(wgtsA[[i]]) ) )
}

weightSumA <- colSums( weightA*muIdA )


maxLengthB <- max( sapply(IDsB, length) )
muIdB <- matrix( 0, nrow=maxLengthB, ncol=100000 )
for( i in 1:ncol(muIdB) ){
  muIdB[,i] <- c( muB[ IDsB[[i]] ], rep( 0, maxLengthB-length(IDsB[[i]]) ) )
}
weightB <- matrix( 0, nrow=maxLengthB, ncol=100000 )
for( i in 1:ncol(weightB) ){
  weightB[,i] <- c( wgtsB[[i]], rep( 0, maxLengthB-length(wgtsB[[i]]) ) )
}

weightSumB <- colSums( weightB*muIdB )


#case A sapply approach
system.time( sapply(1:100000, function(x){ muA[ IDsA[[x]] ]%*%wgtsA[[x]] }) )
#case A data object approach
system.time( colSums( weightA*muIdA ) )

#case B sapply approach
system.time(sapply(1:100000, function(x){ wgtsB[[x]]%*%muB[IDsB[[x]]] }))
#case B data object approach
system.time( colSums( weightB*muIdB ) )


library(pryr)
mem_used()
# 21.6 MB

y <- rnorm(1000000)
x1 <- rnorm(1000000)
x2 <- rnorm(1000000)
x3 <- rnorm(1000000)
mem_used()
#53.7 MB

debug(lm)
lm( y ~ x1 + x2 + x3 )
#step through function; type the follwing command after lm.fit() is called 
mem_used()
#170 MB

undebug(lm)


debug(lm)
lm( y ~ x1 + x2 + x3 )
#step through function; check memory usage after each line
mem_used()

#the following command resulted in 32MB of memory usage; evaluate call in parent environment,
#which is the global environment in this case, save result in object mf
mf <- eval(mf, parent.frame())
object_size(mf)
#32 MB

#the following command resulted in 80MB of memory usage; return the response variable 
#data with type numeric from mf, save in object y
y <- model.response(mf, "numeric")
#note size of y is smaller than total memory used from the command above
object_size(y)
#64 MB

#the following command resulted in 40MB of memory usage; create a model matrix; mt is the 
#terms attribute of mf
x <- model.matrix(mt, mf, contrasts)
#note size of x is larger than total memory used from the command above
object_size(x)
#88 MB

undebug(lm)