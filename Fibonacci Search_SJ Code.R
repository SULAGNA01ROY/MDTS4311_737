# Objective function
f <- function(x) {
  ((x - 2) * (x - 8))^2
}

# Fibonacci search function
fib_search <- function(f, a, b, tol = 1e-4) {
  
  # Generate Fibonacci numbers
  fib <- c(1, 1)
  while (fib[length(fib)] < (b - a) / tol) {
    fib <- c(fib, fib[length(fib)] + fib[length(fib) - 1])
  }
  
  history <- data.frame(
    a = a,
    b = b
  )
  
  N <- length(fib)
  
  # Initial points
  x1 <- a + fib[N - 2] / fib[N] * (b - a)
  x2 <- a + fib[N - 1] / fib[N] * (b - a)
  
  f1 <- f(x1)
  f2 <- f(x2)
  
  for (k in 1:(N - 2)) {
    
    if (f1 > f2) {
      
      a <- x1
      x1 <- x2
      f1 <- f2
      
      x2 <- a + fib[N - k - 1] / fib[N - k] * (b - a)
      f2 <- f(x2)
      
    } else {
      
      b <- x2
      x2 <- x1
      f2 <- f1
      
      x1 <- a + fib[N - k - 2] / fib[N - k] * (b - a)
      f1 <- f(x1)
    }
    
    history <- rbind(
      history,
      data.frame(a = a, b = b)
    )
  }
  
  list(
    xmin = (a + b) / 2,
    fmin = f((a + b) / 2),
    interval = c(a, b),
    history = history
  )
}

# Run Fibonacci search
result <- fib_search(f, a = 1, b = 5, tol = 1e-4)

print(result)

h = result$history

u = seq(0,6,length = 500)
plot(u,f(u), type = 'l')
for (i in 1:nrow(h)) {
  abline(v = h[i,1], col = 2)
  abline(v = h[i,2], col = 3)
}
