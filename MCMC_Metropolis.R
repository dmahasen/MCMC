gx <- function(x,a,b)
{
  if(x>0)
    return((x^(a-1))*exp(-x*b))
  else
    return(0)
}


x0 <- 0.5 # initial value 
sigma <- 2
a <- 5
b <- 4
N <- 10000
x <- c()
x[1] <- x0
for(i in 2:N)
{
  y <- rnorm(x[i-1],sigma,n=1)
  u <- runif(0,1,n=1)
  #if(gx(y,a,b)/gx(x[i-1],a,b)>u)
  if(dgamma(y,a,b)/dgamma(x[i-1],a,b)>u)
    x[i] <- y
  else
    x[i] <- x[i-1]
}



fun1 <- function(y)
{
  if(y<0 || y >1)
    return(0)
  else
    return(y^3*sin(y^4)*cos(y^5))
}

x0 <- 0.5 # initial value 
N <- 10000
x <- c()
x[1] <- x0
for(i in 2:N)
{
  y <- rnorm(x[i-1],sigma,n=1)
  u <- runif(0,1,n=1)
  #if(gx(y,a,b)/gx(x[i-1],a,b)>u)
  if(fun1(y)/fun1(x[i-1])>u)
    x[i] <- y
  else
    x[i] <- x[i-1]
}
