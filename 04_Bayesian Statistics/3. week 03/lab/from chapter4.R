library(statsr)
data("tapwater")
m_0 <- 35
n_0 <- 25
s2_0 <- 156.25
v_0 <- n_0 - 1

# data 
Y <- tapwater$tthm
ybar <- mean(Y)
s2 <- var(Y)
n <- length(Y)

#posterior 
n_n <- n_0 + n
m_n <- (n*ybar + n_0*m_0)/n_n
v_n <- v_0 + n

s2_n <- ((n-1)*s2 + v_0*s2_0 + n_0*n*(m_0 - ybar)^2/n_n)/v_n


#monte carlo
set.seed(42)
phi <- rgamma(1000, shape = v_n/2, rate = s2_n*v_n/2)
mean(phi)
