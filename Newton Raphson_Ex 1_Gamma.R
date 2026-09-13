rm(list = ls())

# Generate Gamma data
set.seed(123)
x=rgamma(5000, shape = 5, scale = 1)
n=length(x)

# Log-likelihood function
LL=function(a){(a-1)*sum(log(x))-sum(x)-n*lgamma(a)}

# Newton-Raphson
a=1.0
tol=10^(-6)
iter=c(a)

repeat
{
  score=sum(log(x))-n*digamma(a)
  hess=-n*trigamma(a)
  a.new=a-score/hess
  iter=c(iter,a.new)
  if(abs(a.new - a)<tol) break
  a=a.new
}
MLE=a.new;MLE

# Values for likelihood curve
A=seq(0.1, 10, 0.01)
L=sapply(A, LL)

# Plot
plot(A,L,
     type="l",lwd = 1,
     xlab = expression(alpha),
     ylab="Log Likelihood",
     main="Newton-Raphson Method")

# True value
abline(v=5,col="red",lty = 2)

# MLE
abline(v=MLE,col="blue",lty = 2)

# Newton-Raphson iterations
points(iter,sapply(iter,LL),pch=19,cex=0.7)

# Legend
legend("bottomright",
       legend=c("Log Likelihood","Truth","MLE"),
       lty=c(1,2,2),
       col=c("black","red","blue"))
