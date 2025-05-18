library(Rcpp)
library(RcppArmadillo)
library(tidyverse)
library(Matrix)
library(MASS)
library(parallel)
sourceCpp("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/GC.cpp")

# Simulation Code for NPGC
npgc <- function(dat, type, hdim, omega, activation, y_select, z_select, x_select, m = 199, k = 10, r = 25) {
  ylags <- seq(15, 1, -1)
  olags <- seq(15, 1, -1)
  L <- nrow(dat)
  Y <- dat[(max(c(ylags, olags)) + 1):L, y_select, drop = FALSE]
  Ylag <- do.call(cbind, lapply(ylags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), y_select, drop = FALSE]))
  Z <- do.call(cbind, lapply(olags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), z_select, drop = FALSE]))
  X <- do.call(cbind, lapply(olags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), x_select, drop = FALSE]))
  output <- NPGC(Y = Y, Ylag = Ylag, Z = Z, X = X, Type = type, Omega = omega, M = m, K = k, R = r, HDim = hdim, Activation = activation)
  return(output)
}

# Simulation Code for GAUSS
gauss <- function(dat, type, hdim, omega, activation, y_select, z_select, x_select, m = 199, k = 10, r = 25) {
  ylags <- seq(15, 1, -1)
  olags <- seq(15, 1, -1)
  L <- nrow(dat)
  Y <- dat[(max(c(ylags, olags)) + 1):L, y_select, drop = FALSE]
  Ylag <- do.call(cbind, lapply(ylags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), y_select, drop = FALSE]))
  Z <- do.call(cbind, lapply(olags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), z_select, drop = FALSE]))
  X <- do.call(cbind, lapply(olags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), x_select, drop = FALSE]))
  output <- GAUSS(Y = Y, Ylag = Ylag, Z = Z, X = X, Type = type, Omega = omega, M = m, K = k, R = r, HDim = hdim, Activation = activation)
  return(output)
}

# Simulation Code for ZERO
zero <- function(dat, type, hdim, omega, activation, y_select, z_select, x_select, k = 10, r = 25) {
  ylags <- seq(15, 1, -1)
  olags <- seq(15, 1, -1)
  L <- nrow(dat)
  Y <- dat[(max(c(ylags, olags)) + 1):L, y_select, drop = FALSE]
  Ylag <- do.call(cbind, lapply(ylags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), y_select, drop = FALSE]))
  Z <- do.call(cbind, lapply(olags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), z_select, drop = FALSE]))
  X <- do.call(cbind, lapply(olags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), x_select, drop = FALSE]))
  output <- ZERO(Y = Y, Ylag = Ylag, Z = Z, X = X, Type = type, Omega = omega, K = k, R = r, HDim = hdim, Activation = activation)
  return(output)
}

# Simulation Code for LASSO
lasso <- function(dat, type, hdim, omega, activation, y_select, z_select, x_select, k = 10, r = 5, penalty, lambdas, chooselambda) {
  ylags <- seq(15, 1, -1)
  olags <- seq(15, 1, -1)
  L <- nrow(dat)
  Y <- dat[(max(c(ylags, olags)) + 1):L, y_select, drop = FALSE]
  Ylag <- do.call(cbind, lapply(ylags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), y_select, drop = FALSE]))
  Z <- do.call(cbind, lapply(olags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), z_select, drop = FALSE]))
  X <- do.call(cbind, lapply(olags, function(arg) dat[arg:(L - max(c(ylags, olags)) + arg - 1), x_select, drop = FALSE]))
  output <- LASSO(Y = Y, Ylag = Ylag, Z = Z, X = X, Type = type, Penalty = penalty, Lambdas = lambdas, Omega = omega, K = k, R = r, HDim = hdim, ChooseLambda = chooselambda, Activation = activation)
  return(output)
}
