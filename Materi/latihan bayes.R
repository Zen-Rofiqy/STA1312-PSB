library(stats)
alpha <- 12
beta <- 115
credible_interval <- qbeta(c(0.025, 0.975), shape1 = alpha, shape2 = beta)
credible_interval

#Monte Carlo
alpha <- 12
beta <- 115
n_simulations <- 100000

posterior_samples <- rbeta(n_simulations, shape1 = alpha, shape2 = beta)
credible_interval_MC <- quantile(posterior_samples, c(0.025, 0.975))
credible_interval_MC

library(ggplot2)
df <- data.frame(posterior_samples)
ggplot(df, aes(x = posterior_samples)) +
  geom_density(fill = "skyblue", color = "blue") +
  labs(title = "Posterior Distribution of Pi",
       x = expression(pi),
       y = "Density")

# Install and load necessary packages
install.packages("VGAM")
library(VGAM)

# Fungsi untuk menghitung posterior Gamma
posterior_gamma <- function(mu, alpha, beta, data) {
  likelihood <- dpois(data, lambda = mu)
  prior <- dgamma(mu, shape = alpha, rate = beta)
  posterior <- likelihood * prior
  posterior / sum(posterior)
}

# Fungsi untuk melakukan sampling Monte Carlo
monte_carlo_sampling <- function(n_samples, alpha, beta, data) {
  samples <- rgamma(n_samples, shape = alpha + data, rate = beta + length(data))
  return(samples)
}

# Data
data <- 71 # Jumlah cacat
n_meters <- 100 # Panjang kain dalam meter

# Prior parameters
alpha <- 9
beta <- 3/2

# Jumlah sampel Monte Carlo
n_samples <- 10000

# Sampling Monte Carlo
samples <- monte_carlo_sampling(n_samples, alpha, beta, data)

# Menghitung interval kredibel Bayesian 95%
credible_interval <- quantile(samples, c(0.025, 0.975))

print(credible_interval)
