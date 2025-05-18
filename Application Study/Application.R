rm(list = ls())
library(parallel)
source("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/GCApp.R")
load("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/ApplicationData.RData")
RNGkind("L'Ecuyer-CMRG")
set.seed(1058)
seeds <- sample(0:10000, 39)
## NPGC

start <- Sys.time()
start
set.seed(seeds[1])
NPGC1 <- npgc(dat = ApplicationData$data, type = 0, hdim = 200, omega = 1, activation = 0, y_select = 5, z_select = c(3, 4), x_select = 2, m = 199, k = 10, r = 25)
save(NPGC1, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/NPGC1.RData")
Sys.time() - start
mean(rowSums(NPGC1)[1] >= rowSums(NPGC1))


start <- Sys.time() 
start
set.seed(seeds[2])
NPGC2 <- npgc(dat = ApplicationData$data, type = 0, hdim = 200, omega = 1, activation = 0, y_select = 5, z_select = c(2, 4), x_select = 3, m = 199, k = 10, r = 25)
save(NPGC2, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/NPGC2.RData")
Sys.time() - start
mean(rowSums(NPGC2)[1] >= rowSums(NPGC2))

start <- Sys.time()
start
set.seed(seeds[3])
NPGC3 <- npgc(dat = ApplicationData$data, type = 0, hdim = 200, omega = 1, activation = 0, y_select = 5, z_select = c(2, 3), x_select = 4, m = 199, k = 10, r = 25)
save(NPGC3, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/NPGC3.RData")
Sys.time() - start
mean(rowSums(NPGC3)[1] >= rowSums(NPGC3))


## GAUSS
start <- Sys.time()
start
set.seed(seeds[4])
GAUSS1 <- gauss(dat = ApplicationData$data, type = 0, hdim = 200, omega = 1, activation = 0, y_select = 5, z_select = c(3, 4), x_select = 2, m = 199, k = 10, r = 25)
save(GAUSS1, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/GAUSS1.RData")
Sys.time() - start

start <- Sys.time()
start
set.seed(seeds[5])
GAUSS2 <- gauss(dat = ApplicationData$data, type = 0, hdim = 200, omega = 1, activation = 0, y_select = 5, z_select = c(2, 4), x_select = 3, m = 199, k = 10, r = 25)
save(GAUSS2, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/GAUSS2.RData")
Sys.time() - start

start <- Sys.time()
start
set.seed(seeds[6])
GAUSS3 <- gauss(dat = ApplicationData$data, type = 0, hdim = 200, omega = 1, activation = 0, y_select = 5, z_select = c(2, 3), x_select = 4, m = 199, k = 10, r = 25)
save(GAUSS3, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/GAUSS3.RData")
Sys.time() - start


## ZERO
start <- Sys.time()
start
set.seed(seeds[7])
ZERO1 <- zero(dat = ApplicationData$data, type = 0, hdim = 200, omega = 1, activation = 0, y_select = 5, z_select = c(3, 4), x_select = 2, k = 10, r = 25)
save(ZERO1, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/ZERO1.RData")
Sys.time() - start

start <- Sys.time()
start
set.seed(seeds[8])
ZERO2 <- zero(dat = ApplicationData$data, type = 0, hdim = 200, omega = 1, activation = 0, y_select = 5, z_select = c(2, 4), x_select = 3, k = 10, r = 25)
save(ZERO2, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/ZERO2.RData")
Sys.time() - start

start <- Sys.time()
start
set.seed(seeds[9])
ZERO3 <- zero(dat = ApplicationData$data, type = 0, hdim = 200, omega = 1, activation = 0, y_select = 5, z_select = c(2, 3), x_select = 4, k = 10, r = 25)
save(ZERO3, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/ZERO3.RData")
Sys.time() - start


rm(list = ls())
library(parallel)
source("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/GCApp.R")
load("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/ApplicationData.RData")
runLASSO <- function(selection) {
  if(selection %in% 1:5) {
    penalty <- selection
    z_select <- c(3, 4)
    x_select <- 2
    chooselambda <- TRUE
    count <- (penalty - 1) * 3 + 10 
  } else if(selection %in% 6:10) {
    penalty <- selection - 5
    z_select <- c(2, 4)
    x_select <- 3
    count <- (penalty - 1) * 3 + 11
    chooselambda <- TRUE
  } else if(selection %in% 11:15) {
    penalty <- selection - 10
    z_select <- c(2, 3)
    x_select <- 4
    count <- (penalty - 1) * 3 + 12
    chooselambda <- TRUE
  } else if(selection %in% 16:20) {
    penalty <- selection - 15
    z_select <- c(3, 4)
    x_select <- 2
    count <- (penalty - 1) * 3 + 25
    chooselambda <- FALSE
  } else if(selection %in% 21:25) {
    penalty <- selection - 20
    z_select <- c(2, 4)
    x_select <- 3
    count <- (penalty - 1) * 3 + 26
    chooselambda <- FALSE
  } else if(selection %in% 26:30) {
    penalty <- selection - 25
    z_select <- c(2, 3)
    x_select <- 4
    count <- (penalty - 1) * 3 + 27
    chooselambda <- FALSE
  } else {
    print("Enter a valid selection.")
  }
  start <- Sys.time()
  print(paste0(selection, ": ", start))
  set.seed(seeds[count])
  LResult <- lasso(dat = ApplicationData$data, type = 2, hdim = 200, omega = 1, activation = 0, y_select = 5, z_select = z_select, x_select = x_select, k = 10, r = 5, penalty = penalty, lambdas = 10^seq(-1, 2, 0.1), chooselambda = chooselambda)
  save(LResult, file = paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/LResult", selection, ".RData"))
  print(paste0(selection, ": Elapsed ", Sys.time() - start))
  return(LResult)
}

RNGkind("L'Ecuyer-CMRG")
set.seed(1058)
seeds <- sample(0:10000, 39)
LASSOapps <- mclapply(1:15, function(zzz) runLASSO(zzz), mc.preschedule = TRUE, mc.cores = getOption("mc.cores", 18))

RNGkind("L'Ecuyer-CMRG")
set.seed(1058)
seeds <- sample(0:10000, 39)
oLASSOapps <- mclapply(16:30, function(zzz) runLASSO(zzz), mc.preschedule = TRUE, mc.cores = getOption("mc.cores", 18))
