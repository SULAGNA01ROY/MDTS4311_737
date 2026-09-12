rm(list = ls())

f <- function(x) (x-2)^2 * (x-8)^2
df <- function(x) 4*(x-2)*(x-5)*(x-8)

a <- 6
b <- 10
e <- 0.01

while ((b-a) > e) {
  m <- (a+b)/2
  
  if (df(m) < 0)
    a <- m
  else
    b <- m
}

x <- (a+b)/2

cat("Minimum x =", x, "\n")
cat("Minimum f(x) =", f(x), "\n")
