n1 <- c(29,30,28,27,30,31,30,29)
n2 <- c(30,30,34,29,33,28,32)
d1 <- c(49.06, 52.99, 56.91, 60.84, 64.76, 68.69, 72.61, 76.54)
d2 <- c(49.06, 52.99, 56.91, 60.84, 64.76, 68.69, 72.61)
p1 <- c(6.9, 23.3, 32.9, 51.9, 76.7, 93.6, 96.7, 100)
p2 <- c(13.3, 20, 26.5, 48.3, 87.9, 85.7, 100)
dose1 <- log(d1)
dose2 <- log(d2)
data <- (data.frame(dose1,p1, n1))
         
         names(data) <- NULL
         
dit <- data.frame(dose2, p2, n2)

         names(dit) <- NULL





  b0 <- NULL
  b1 <- NULL
  lambda <- NULL
  k <- NULL
  n <- NULL
  
loglik <- function(data, par){
  b0 <- par[1]
  b1 <- par[2]
  lambda <- .00001
  x <- data[,1]
  y <- data[,2]/100
  n <- data[,3]
  k <- b0+b1*x
  mu <- 1 - 1/(1 + lambda*exp(k))^(1/lambda)
  j <- -(n*(y * log(mu/(1-mu)) + log(1-mu)) + log(choose(n,round(y*n))))
  return(sum(j))
}
clogmod1 <- optim(c(0,0), loglik, data = data, hessian = T)
clogmod1

clogmod2 <- optim(c(0,0), loglik, data = dit, hessian = T)
clogmod2



loglik <- function(data, par){
  b0 <- par[1]
  b1 <- par[2]
  lambda <- 1
  x <- data[,1]
  y <- data[,2]/100
  n <- data[,3]
  k <- b0+b1*x
  mu <- 1 - 1/(1 + lambda*exp(k))^(1/lambda)
  j <- -(n*(y * log(mu/(1-mu)) + log(1-mu)) + log(choose(n,round(y*n))))
  return(sum(j))
}

logitmod1 <- optim(c(0,0), loglik, data = data, hessian = T)
logitmod1

logitmod2 <- optim(c(0,0), loglik, data = dit, hessian = T)
logitmod2


loglik <- function(data, par){
  b0 <- par[1]
  b1 <- par[2]
  lambda <- par[3]
  x <- data[,1]
  y <- data[,2]/100
  n <- data[,3]
  k <- b0+b1*x
  mu <- 1 - 1/(1 + lambda*exp(k))^(1/lambda)
  j <- -(n*(y * log(mu/(1-mu)) + log(1-mu)) + log(choose(n,round(y*n))))
  l <- sum(j)
  return(l)
}

jj1 <- optim(c(-40,10,1), loglik, data = data, hessian = T)
jj1

jj2 <- optim(c(-40,10,1), loglik, data = dit, hessian = T)
jj2




chi1 <- -2*(jj1$value - logitmod1$value)
chi1
pchisq(chi1, 1, lower.tail = FALSE)

chi2 <- -2*(jj2$value - logitmod2$value)
chi2
pchisq(chi2, 1, lower.tail = FALSE)













