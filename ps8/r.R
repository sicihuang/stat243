########2(b)########

#note: use PtProcess package here because other packages use general pareto distribution
library(PtProcess)

#number of samples for each estimator
m <- 1000 

set.seed(0)
#samples for importance sampler
#sample from g(x), pdf of Pareto(2,3)
x <- rpareto(m, lambda=3, a=2) 
#density of x under f; shifted to the right by 2
f <- dexp(x-2, rate = 1) 
#density of x under g
g <- dpareto(x, lambda=3, a=2)  
#weights
w <- f/g  

est1 <- mean(w*x) 
est1
est2 <- mean(w*(x^2)) 
est2

#h(x)f(x)/g(x) with h(x)=x
estX <- w*x 
#h(x)f(x)/g(x) with h(x)=x^2
estX2 <- w*(x^2)  

par(mfrow = c(2, 2), pty = "s")  
hist(estX) 
hist(estX2) 
hist(w)

var(x)
var(estX)
var(x^2)
var(estX2)

########2(c)########

rm(list=ls())

#number of samples for each estimator
m <- 1000 

set.seed(0)
#samples for importance sampler
#sample from g(x), pdf of exp(1)+2
x <- rexp(m, rate = 1) + 2 
#density of x under f
f <- dpareto(x, lambda=3, a=2)  
#density of x under g
g <- dexp(x-2, rate = 1) 
#weights
w <- f/g  

est1 <- mean(w*x) 
est1
est2 <- mean(w*(x^2)) 
est2

#h(x)f(x)/g(x) with h(x)=x
estX <- w*x 
#h(x)f(x)/g(x) with h(x)=x^2
estX2 <- w*(x^2)

par(mfrow = c(2, 2), pty = "s")  
hist(estX) 
hist(estX2) 
hist(w)

var(x)
var(estX)
var(x^2)
var(estX2)

########3(c)########

#function to solve for EM estimators in probit regression
probitEM <- function(y, x1, x2, x3, intVal=c(1,0.5,0,0), tolerance = .Machine$double.eps^0.5, 
                     maxIteration=10000){   
  n <- length(y)
  #intialization
  b0 <- intVal[1]
  b1 <- intVal[2]
  b2 <- intVal[3]
  b3 <- intVal[4]
  it <- 1
  diff <- Inf
  
  while (diff > tolerance & it < maxIteration){ 
    #save starting values of beta's for later
    intb0 <- b0
    intb1 <- b1
    intb2 <- b2
    intb3 <- b3
    
    #E step
    mu <- b0 + b1*x1 + b2*x2 + b3*x3
    #from formula in (a)
    #notice z is N(mu,1), latent variable 
    z <- ifelse(y==1, mu+dnorm(mu,mean=0,sd=1)/pnorm(mu,mean=0,sd=1), 
                mu-dnorm(mu,mean=0,sd=1)/pnorm(-mu,mean=0,sd=1))    
    
    #M step; estimate betas using lm function     
    b0 <- coef(lm(z ~ x1+x2+x3))[1]
    b1 <- coef(lm(z ~ x1+x2+x3))[2]
    b2 <- coef(lm(z ~ x1+x2+x3))[3]
    b3 <- coef(lm(z ~ x1+x2+x3))[4]
    
    absDiff <- abs(c(b0-intb0, b1-intb1, b2-intb2, b3-intb3))
    diff <- sum(absDiff)/sum(abs(c(intb0, intb1, intb2, intb3)))
    
    it <- it+1
  }
  return(list(b0, b1, b2, b3, it))
}

#function to test different beta values to find the ones that make b1hat/se(b1hat) close to 2
test <- function(b0,b1){
  #assumed in problem
  n <- 100
  b2 <- 0
  b3 <- 0
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  XTb <- b0 + b1*x1 + b2*x2 + b3*x3
  
  set.seed(0)
  y <- rbinom(n, 1, prob = pnorm(XTb))
  
  #return z value for coefficient of x1, check if close to 2
  summary(glm(y ~ x1+x2+x3, family=binomial(link = "probit")))$coef[2,3]
}

test(0,0)
test(1,1)
test(1,0.5)
#close to 2
test(1,0.3)

#results from the above trials
b0 <- 1
b1 <- 0.3

#assumed in problem
n <- 100
b2 <- 0
b3 <- 0
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
XTb <- b0 + b1*x1 + b2*x2 + b3*x3

set.seed(0)
y <- rbinom(n, 1, prob = pnorm(XTb))

#test choice of starting value of beta in (b)
linearReg <- lm(y ~ x1 + x2 + x3)
intVal <- as.double(coef(linearReg))
probitEM(y, x1, x2, x3, intVal)

########3(d)########

#function to derive log-likelihood
probitLoglik <- function(beta, X, y){
  #result from part (a)
  p <- pnorm(X%*%beta, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
  #log-likelihood of bernoulli
  return(sum(y*log(p)+(1-y)*log(1-p)))
}

#use the same setup as part (c)
n <- 100
b2 <- 0
b3 <- 0
b0 <- 1
b1 <- 0.3
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
X <- cbind(1,x1,x2,x3)
XTb <- b0 + b1*x1 + b2*x2 + b3*x3

set.seed(0)
y <- rbinom(n, 1, prob = pnorm(XTb))

linearReg <- lm(y ~ x1 + x2 + x3)
intVal <- as.double(coef(linearReg))

#estimate betas
#trace: print iterations; maxit: maximum iterations; fnscale=-1: flip log-likelihood function to maximize
result <- optim(intVal, fn=probitLoglik, gr=NULL, y=y, X=X, method="BFGS", 
                control=list(trace=TRUE,maxit=10000,fnscale=-1), hessian=TRUE)
print(result)

#compute standard error
se <- sqrt((-1)*diag(solve(result$hessian)))
print(se)

########4########

library(fields);

#helical valley function
theta <- function(x1,x2) atan2(x2, x1)/(2*pi)

f <- function(x) {
  f1 <- 10*(x[3] - 10*theta(x[1],x[2]))
  f2 <- 10*(sqrt(x[1]^2+x[2]^2)-1)
  f3 <- x[3]
  return(f1^2+f2^2+f3^2)
}

par(mfrow = c(2,2), pty = 's')

x <- seq(-10, 10, length.out=50)
y <- seq(-10, 10, length.out=50)

#heat map and contour lines at z = -10
val <- apply(as.matrix(expand.grid(x, y)), 1, function(x) f(c(x, -10)))
image(x, y, matrix(val, 50, 50), col = heat.colors(100), axes = TRUE, main="Helical at z=-10")
contour(x, y, matrix(val, 50, 50), add = TRUE)

#heat map and contour lines at z = 0
val <- apply(as.matrix(expand.grid(x, y)), 1, function(x) f(c(x, 0)))
image(x, y, matrix(val, 50, 50), col = heat.colors(100), axes = TRUE, main="Helical at z=0")
contour(x, y, matrix(val, 50, 50), add = TRUE)

#heat map and contour lines at z = 5
val <- apply(as.matrix(expand.grid(x, y)), 1, function(x) f(c(x, 5)))
image(x, y, matrix(val, 50, 50), col = heat.colors(100), axes = TRUE, main="Helical at z=5")
contour(x, y, matrix(val, 50, 50), add = TRUE)

#heat map at and contour lines z = 10
val <- apply(as.matrix(expand.grid(x, y)), 1, function(x) f(c(x, 10)))
image(x, y, matrix(val, 50, 50), col = heat.colors(100), axes = TRUE, main="Helical at z=10")
contour(x, y, matrix(val, 50, 50), add = TRUE)

#check that the optimal value changes with starting point
optim(c(0,0,0), f, hessian = TRUE)
optim(c(-1,-4,7), f, hessian = TRUE)
optim(c(1,2,5), f, hessian = TRUE)

nlm(f, c(0,0,0), hessian = TRUE)
nlm(f, c(-1,-4,7), hessian = TRUE)
nlm(f, c(1,2,5), hessian = TRUE)