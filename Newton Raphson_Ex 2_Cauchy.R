rm(list = ls())

# Generate Cauchy data
set.seed(123)
x <- rcauchy(5000, location = 5, scale = 1)
n <- length(x)

# Log-likelihood
LL <- function(theta) {
  -n * log(pi) - sum(log(1 + (x - theta)^2))
}

# Newton-Raphson
theta <- median(x)
tol <- 10^(-6)
iter <- c(theta)

repeat {
  
  score <- sum(2 * (x - theta) / (1 + (x - theta)^2))
  
  hess <- sum(2 * ((x - theta)^2 - 1) /
                (1 + (x - theta)^2)^2)
  
  # Stop if Hessian is too close to zero
  if(abs(hess) < 1e-10) break
  
  theta.new <- theta - score / hess
  
  # Stop if result is not finite
  if(!is.finite(theta.new)) break
  
  iter <- c(iter, theta.new)
  
  if(abs(theta.new - theta) < tol) break
  
  theta <- theta.new
}

MLE <- theta.new;MLE

# Likelihood curve
T <- seq(0, 10, 0.01)
L <- sapply(T, LL)

# Plot
plot(T, L, type = "l", lwd = 1,
     xlab = expression(theta),
     ylab = "Log Likelihood",
     main = "Newton-Raphson Method")

# True value
abline(v = 5, col = "red", lty = 2,lwd=1.5)

# MLE
abline(v = MLE, col = "blue", lty = 2,lwd=1.5)

# Newton-Raphson iterations
points(iter, sapply(iter, LL), pch = 19, cex = 0.7)

# Legend
legend("bottomright",
       legend = c("Log Likelihood", "Truth", "MLE"),
       lty = c(1, 2, 2),lwd=1.5,
       col = c("black", "red", "blue"))