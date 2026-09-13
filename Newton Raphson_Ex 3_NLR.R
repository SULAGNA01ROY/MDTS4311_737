#------------------------------------------
rm(list = ls())
n = 50
set.seed(123)
e = rnorm(n, 0, 0.2)
x = 1:n
m = 1000
theta = matrix(0, ncol = 2, nrow = m)
epsilon = 10^(-6)
theta[1, ] = c(5, 2)
y = (theta[1, 1] * x / (theta[1, 2] + x)) + e
for (k in 2:m)
{
  # First derivative / Gradient vector
  h1 = (-2) * sum((y - (theta[k-1, 1] * x / (theta[k-1, 2] + x))) *(x / (theta[k-1, 2] + x)))
  h2 = (2) * sum((y - (theta[k-1, 1] * x / (theta[k-1, 2] + x))) *(x / ((theta[k-1, 2] + x)^2)))
  
  # Hessian matrix elements
  h11 = 2 * sum((x / (theta[k-1, 2] + x))^2)
  h12 = 2 * sum((x / ((theta[k-1, 2] + x)^2)) *(y - ((2 * theta[k-1, 1] * x) /(theta[k-1, 2] + x))))
  
  h21 = h12
  
  h22 = (-2) * sum(theta[k-1, 1] * x / ((theta[k-1, 2] + x)^3) *((2 * y) - ((3 * theta[k-1, 1] * x) /(theta[k-1, 2] + x))))
  
  # Gradient vector
  H1 = matrix(c(h1, h2), ncol = 1, byrow = TRUE)
  
  # Hessian matrix
  H = matrix(c(h11, h12, h21, h22),byrow = TRUE,nrow = 2,ncol = 2)
  
  # Newton-Raphson update
  theta[k, ] = theta[k-1, ]-matrix(solve(H) %*% H1,ncol = 1,byrow = TRUE)
  
  # Convergence condition
  if (max(abs(theta[k, ] - theta[k-1, ])) < epsilon)
  {
    break
    cat("Converged at iteration:", k, "\n")
  }
}

cat("Number of iterations:", k, "\n")
cat("Estimated parameters:\n")
print(theta[k, ])
#-------------------------------------------