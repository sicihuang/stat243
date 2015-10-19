options( digits = 22 )
1.000000000001


x <- c( 1, rep(1e-16, 10000) )
sum( x )


y <- 1
for( i in 1:10000 ){
  y <- y + 1e-16
}
y

z <- 0
for( i in 1:10000 ){
  z <- z + 1e-16
}
z <- z+1
z


library(microbenchmark)

a <- rep(1, 1000000)
b <- rep(as.numeric(1.00000000), 1000000)

microbenchmark(sum(a), sum(b))
microbenchmark(a-10000, b-10000)
microbenchmark(a^10000, b^10000)
microbenchmark(c <- a[1:9999], d <- b[1:9999])