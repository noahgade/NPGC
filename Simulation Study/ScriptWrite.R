############################
##### Simulation Study #####
############################
library(tidyverse)
library(data.table)
library(stringr)

##########
## NPGC ##
##########
# ELM Simulations for changing N, lags, activation, and W0
Ns <- c(25, 50, 100)
lags <- c(2, 4, 8)
gs <- c("tanh" = 0, "relu" = 1, "lrelu" = 2)
NPGCkey <- tibble(names = paste0("NPGC", rep(Ns, each = length(lags) * length(gs)), "_",
                             rep(rep(lags, each = length(gs)), length(Ns)), "_",
                             rep(names(gs), length(lags) * length(Ns))),
              N = rep(Ns, each = length(lags) * length(gs)),
              lag = rep(rep(lags, each = length(gs)), length(Ns)),
              g = rep(gs, length(lags) * length(Ns)))
save(NPGCkey, file = "NPGCkey.RData")

set.seed(219)
seeds <- sample(0:10000, nrow(key), replace = TRUE)
for(iter in 1:nrow(NPGCkey)) {
  filename <- paste0('NPGC_', iter, '.R')
  fileConn <- file(filename)
  writeLines(c(paste0('source("GC.R")'),
               paste0('load("data.RData")'),
               paste0('numCores <- detectCores() - 1'),
               paste0('options(mc.cores = numCores)'),
               paste0('RNGkind("L', "'", 'Ecuyer-CMRG")'),
               paste0('set.seed(', seeds[iter], ')'),
               paste0('NPGC_', iter, ' <-  mclapply(data, function(zz) npgc(dat = zz$data, type = 0, hdim = ', NPGCkey$N[iter], ', omega = 1, activation = ', NPGCkey$g[iter], ', y_select = 1, z_select = 2, x_select = 3, max_lag = ', NPGCkey$lag[iter], ', m = 199, k = 10, r = 25), mc.preschedule = TRUE, mc.cores = getOption("mc.cores", numCores))'),
               paste0('save(NPGC_', iter, ', file = "NPGC_', iter, '.RData")'),
               paste0('print("COMPLETE")')),
             fileConn)
  close(fileConn)
}

###########
## GAUSS ##
###########
set.seed(825)
seed <- sample(0:10000, 1, replace = TRUE)
filename <- paste0('GAUSS100.R')
fileConn <- file(filename)
writeLines(c(paste0('source("GC.R")'),
             paste0('load("data.RData")'),
             paste0('numCores <- detectCores() - 1'),
             paste0('options(mc.cores = numCores)'),
             paste0('RNGkind("L', "'", 'Ecuyer-CMRG")'),
             paste0('set.seed(', seed, ')'),
             paste0('GAUSS100 <-  mclapply(data, function(zz) gauss(dat = zz$data, type = 0, hdim = 100, omega = 1, activation = 1, y_select = 1, z_select = 2, x_select = 3, max_lag = 2, m = 199, k = 10, r = 25), mc.preschedule = TRUE, mc.cores = getOption("mc.cores", numCores))'),
             paste0('save(GAUSS100, file = "GAUSS100.RData")'),
             paste0('print("COMPLETE")')),
           fileConn)
close(fileConn)

##########
## ZERO ##
##########
set.seed(829)
seed <- sample(0:10000, 1, replace = TRUE)
filename <- paste0('ZERO100.R')
fileConn <- file(filename)
writeLines(c(paste0('source("/deac/sta/gadeGrp/gaden/NPGC/GC.R")'),
             paste0('load("/deac/sta/gadeGrp/gaden/NPGC/Data/data.RData")'),
             paste0('numCores <- detectCores() - 1'),
             paste0('options(mc.cores = numCores)'),
             paste0('RNGkind("L', "'", 'Ecuyer-CMRG")'),
             paste0('set.seed(', seed, ')'),
             paste0('ZERO100 <-  mclapply(data, function(zz) zero(dat = zz$data, type = 0, hdim = 100, omega = 1, activation = 1, y_select = 1, z_select = 2, x_select = 3, max_lag = 2, k = 10, r = 25), mc.preschedule = TRUE, mc.cores = getOption("mc.cores", numCores))'),
             paste0('save(ZERO100, file = "ZERO100.RData")'),
             paste0('print("COMPLETE")')),
           fileConn)
close(fileConn)

###########
## LASSO ##
###########
set.seed(846)
seeds <- array(sample(0:10000, 5 * 1800, replace = TRUE), dim = c(5, 1800))
for(pen in 1:5) {
  for(iter in 1:1800) {
    filename <- paste0('LASSO', pen, '_', iter, '.R')
    fileConn <- file(filename)
    writeLines(c(paste0('source("GC.R")'),
                 paste0('load("data.RData")'),
                 paste0('RNGkind("L', "'", 'Ecuyer-CMRG")'),
                 paste0('set.seed(', seeds[pen, iter], ')'),
                 paste0('LASSO', pen, '_', iter, ' <-  lasso(dat = data[[', iter, ']]$data, type = 2, hdim = 100, omega = 1, activation = 0, y_select = 1, z_select = 2, x_select = 3, max_lag = 2, k = 10, r = 5, penalty = ', pen, ', lambdas = 10^seq(-1, 2, 0.1), chooselambda = TRUE)'),
                 paste0('save(LASSO', pen, '_', iter, ', file = "LASSO', pen, '_', iter, '.RData")'),
                 paste0('print("COMPLETE")')),
               fileConn)
    close(fileConn)
  }
}

###########
## NPGC2 ##
###########
set.seed(1002)
seeds <- sample(0:10000, 1800, replace = TRUE)
for(iter in 1:1800) {
  filename <- paste0('~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/NPGC2/Files/NPGC2_', iter, '.R')
  fileConn <- file(filename)
  writeLines(c(paste0('source("GC.R")'),
               paste0('load("data.RData")'),
               paste0('RNGkind("L', "'", 'Ecuyer-CMRG")'),
               paste0('set.seed(', seeds[iter], ')'),
               paste0('NPGC2_', iter, ' <-  npgc(dat = data[[', iter, ']]$data, type = 2, hdim = 100, omega = 1, activation = 0, y_select = 1, z_select = 2, x_select = 3, max_lag = 2, m = 199, k = 10, r = 1)'),
               paste0('save(NPGC2_', iter, ', file = "NPGC2_', iter, '.RData")'),
               paste0('print("COMPLETE")')),
             fileConn)
  close(fileConn)
}
