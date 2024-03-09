library(CircStats)
library(tidyverse)

set.seed(1104)
n_steps <- 20
S_shape <- 2
S_rate <- 1
A_mu <- 0
A_kappa <- 10
x0 <- 0
y0 <- 0
bird <- tibble(
  t = 1:20,
  S = rgamma(n_steps, S_shape, S_rate),
  A = rvm(n_steps, A_mu, A_kappa),
  # bearing
  B = cumsum(A) %% (2 * pi),
  dx = S * cos(B),
  dy = S * sin(B),
  x = x0 + cumsum(dx),
  y = y0 + cumsum(dy)
)

n_rec <- 8
rec_x <- range(bird$x) + c(-0.25, 0.25) * (max(bird$x) - min(bird$x))
rec_y <- range(bird$y) + c(-0.25, 0.25) * (max(bird$y) - min(bird$y))
rec_err_mu <- 0
rec_err_sd <- 0.1
receivers <- tibble(
  i = seq(n_rec),
  x = runif(n_rec, rec_x[1], rec_x[2]),
  y = runif(n_rec, rec_y[1], rec_y[2])
)
# Later: add random effect on receiver for ping error

ggplot(bird, aes(x, y, color = t)) +
  geom_path() +
  geom_point() +
  geom_point(data = receivers, color = "firebrick", size = 2) +
  theme_minimal()

P0 <- 3
P1 <- 0.1
P_sd <- 0.2
pings <- cross_join(bird, receivers, suffix = c("_bird", "_receiver")) %>%
  mutate(D = sqrt((x_bird - x_receiver)^2 + (y_bird - y_receiver)^2),
         P = P0 * exp(-P1 * D) + rnorm(nrow(.), 0, P_sd))

ggplot(pings, aes(D, P)) +
  geom_point(shape = 21) +
  stat_function(fun = \(x) P0 * exp(-P1 * x), color = "blue") +
  theme_minimal()

