barto_likelihood <- function(x, Sd, Bp, Bf, Ud, sigma, n, w) {
  loga  <- w * log(1 + exp(x * Bp))
  logb1 <- (n - w) * x * Bp
  logb2 <- (n - w) * log(1 + exp(x * Bp))
  logc  <- -1 / 2 * ((x - Sd) / (sqrt(2 * Bf ^ 2))) ^ 2
  return(exp(-loga + logb1 - logb2 + logc))
}

proportionalPosterior <- function(Sd, Bp, Bf, Ud, sigma, n, w) {
  l     <- integrate(barto_likelihood, lower = -Inf, upper = Inf, Sd = Sd, Bp = Bp, Bf = Bf, Ud = Ud, sigma = sigma, n = n, w = w) 
  prior <- exp(-1/2 * ((Sd - Ud) / sqrt(2 * sigma ^ 2)) ^ 2)
  -prior * l$value * 10 ^ 20
}

posteriorRatingDiff <- function(Bp, Bf, Ud, sigma, n, w) {
  optim(1, proportionalPosterior, Bp = 1, Bf = 1, Ud = 1, sigma = 0.2, n = 100, w = 60, lower = Ud - 50, upper = Ud + 50, method = "Brent")$par
}