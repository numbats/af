generate_arma <- function(n = 100, c = 0, phi = NULL, theta = NULL, sigma = 1) {
  # Model parameters
  if (is.null(phi)) phi <- numeric()
  if (is.null(theta)) theta <- numeric()
  p <- length(phi)
  q <- length(theta)
  if (any(abs(polyroot(c(1, -phi))) <= 1)) {
    stop("Model is not stationary")
  }
  if (any(abs(polyroot(c(1, theta))) <= 1)) {
    stop("Model is not invertible")
  }

  # Series length
  if (n <= 0) {
    stop("n must be positive")
  }
  # Add 50 for burn-in period
  nplus <- n + max(50, p, q)

  # Generate errors
  error <- rnorm(nplus, mean = 0, sd = sigma)

  # Set up vector for the response with initial values
  # equal to the mean of the process for faster burn-in.
  y <- rep(c / (1 - sum(phi)), nplus)

  # Generate remaining observations
  for (i in max(p, q) + seq_len(nplus - max(p, q))) {
    y[i] <- c + sum(phi * y[seq(i - 1, by = -1, length.out = p)]) + sum(theta * error[seq(i - 1, by = -1, length.out = q)]) + error[i]
  }

  # Return last n observations
  tail(y, n)
}
