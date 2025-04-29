gaussian2d <- function(n, a = 1) {
  if (!is.numeric(n) || n <= 0 || n != round(n)) {
    stop("n must be a positive integer")
  }
  if (!is.numeric(a) || length(a) != 1) {
    stop("a must be a scalar")
  }
  # create grid
  x <- seq(-floor(n / 2), floor(n / 2), length.out = n)
  y <- seq(-floor(n / 2), floor(n / 2), length.out = n)
  # create meshgrid
  grid <- expand.grid(x = x, y = y)
  # calculate the gaussian kernel
  w <- exp(-0.5 * ((grid$x^2 + grid$y^2) / a^2))
  # reshape to matrix
  w <- matrix(w, nrow = n, ncol = n)
  # normalize the kernel
  return(w)
}
