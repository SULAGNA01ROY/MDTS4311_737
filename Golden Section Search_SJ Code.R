# Objective function
f <- function(x) {
  (x - 2)^2 * (x - 8)^2
}

# Golden Section Search
golden_search <- function(f, a, b, tol = 1e-4) {
  
  tau <- (sqrt(5) - 1) / 2  # 0.618...
  
  history <- data.frame(
    iter = 0,
    a = a,
    b = b
  )
  
  x1 <- b - tau * (b - a)
  x2 <- a + tau * (b - a)
  
  f1 <- f(x1)
  f2 <- f(x2)
  
  iter <- 0
  while ((b - a) > tol) {
    iter <- iter + 1
    
    if (f1 > f2) {
      
      a <- x1
      x1 <- x2
      f1 <- f2
      
      x2 <- a + tau * (b - a)
      f2 <- f(x2)
      
    } else {
      
      b <- x2
      x2 <- x1
      f2 <- f1
      
      x1 <- b - tau * (b - a)
      f1 <- f(x1)
    }
    history <- rbind(
      history,
      data.frame(iter = iter, a = a, b = b)
    )
  }
  
  list(
    xmin = (a + b) / 2,
    interval = c(a, b),
    history = history
  )
}

# Run Golden Section Search
result <- golden_search(f, a = 1, b = 5, tol = 1e-4)

print(result)

h = result$history
h



u = seq(0,6,length = 500)
plot(u,f(u), type = 'l')

for (i in 1:nrow(h)) {
  abline(v = h[i,2], col = 2)
  abline(v = h[i,3], col = 3)
  # Sys.sleep(0.5)
}


