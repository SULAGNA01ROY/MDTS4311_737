################################################
## MLE for logistic regression
## Using gradient ascent
################################################
library(mcmc) #to load a dataset
data(logit)
head(logit)  # y is response and 4 covariates
y <- logit$y
X <- as.matrix(logit[, 2:5])
p <- dim(X)[2]

f.gradient <- function(y, X, beta)
{
  beta <- matrix(beta, ncol = 1)
  pi <- exp(X %*% beta) / (1 + exp(X%*%beta))  
  rtn <- colSums(X* as.numeric(y - pi)) # same as t(X)%*%(y-pi)
  return(rtn)
}


store.beta <- matrix(0, nrow = 1, ncol = p)
store.grads <- NULL
beta_k <- rep(0, p) # start at all 0s
grads <- 100 # large values
t <- .1
tol <- 1e-8
iter <- 0
while((grads > tol) && iter < 1e4)  #not too many iterations
{
  iter <- iter+1
  foo <- f.gradient(y = y, X= X, beta = beta_k)
  grads <- sqrt(sum(foo^2))
  store.grads <- c(store.grads, grads)  ## storing the gradients
  beta_k = beta_k + t* foo
  store.beta <- rbind(store.beta, beta_k)
}
iter # number of iterations
beta_k # last estimate

plot(store.grads, type= "b", pch = 16, ylab = "Norm of gradient")
abline(h = 0, col = "red")


