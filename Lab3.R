# libraries
install.packages('dplyr')
install.packages('readRDS')
install.packages('tidyr')

library(dplyr)
library(readRDS)
library(tidyr)

llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}

compute_f_hat = function(z, x, y, omega) {
  Wz = make_weight_matrix(z, x, omega)
  X = make_predictor_matrix(x)
  f_hat = (1 / z) * solve(t(X) %*% Wz %*% X) %*% t(X) %*% Wz %*% y
  return(f_hat)
}

# Define make_weight_matrix
make_weight_matrix = function(z, x, omega) {
  n = length(x)
  Wz = matrix(0, n, n)
  for (i in 1:n) {
    Wz[i, i] = ifelse(abs(x[i] - z) < omega, (1 - abs(x[i] - z) ^ 3) / omega, 0)
  }
  return(diag(Wz))
}

# Define make_predictor_matrix
make_predictor_matrix = function(x) {
  n = length(x)
  X = matrix(0, n, 2)
  X[, 1] = rep(1, n)
  X[, 2] = x
  return(X)
}


