#beta distribution

alpha <- 81
beta <- 219

data_beta <- rbeta(100, alpha, beta)
plot(data_beta, dbeta(data_beta, alpha, beta), col='red')


#new 300 n added. success 100, miss 200
plot(data_beta, dbeta(data_beta, 181, 419), col='green')

theta_p <- rbeta(1e6,0.1,14.9)
theta_t <- rbeta(1e6, 1.1,18.9)
mean(theta_t>theta_p)

h <- function(alpha_a, beta_a,
              alpha_b, beta_b) {
    j <- seq.int(0, round(alpha_b) - 1)
    log_vals <- (lbeta(alpha_a + j, beta_a + beta_b) - log(beta_b + j) -
                     lbeta(1 + j, beta_b) - lbeta(alpha_a, beta_a))
    1 - sum(exp(log_vals))
}
h(0.1,14.9,1.1,18.9)
