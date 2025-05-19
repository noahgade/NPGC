rm(list = ls())
library(tidyverse)
library(devtools)
library(Rcpp)
library(RcppArmadillo)
library(gridExtra)
load("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/Data/datakey.RData")
load(file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/NPGC/Results/key.RData")
for(iter in 1:27) {
  if(file.exists(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/NPGC/Results/NPGC_", iter, ".RData"))) {
    load(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/NPGC/Results/NPGC_", iter, ".RData"))
  } else {
    print(paste0("NPGC_", iter, ".RData does not exist"))
  }
}
load("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/ZERO/Results/ZERO100.RData")
load("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/GAUSS/Results/GAUSS100.RData")

missing <- vector("list", length = 5)
LASSO1 <- LASSO2 <- LASSO3 <- LASSO4 <- LASSO5 <- vector("list", length = 1800)
for(iter in 1:1800) {
  if(file.exists(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/RawLASSO/Results1/LASSO1_", iter, ".RData"))) {
    load(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/RawLASSO/Results1/LASSO1_", iter, ".RData"))
    eval(parse(text = paste0("LASSO1[[iter]] <- LASSO1_", iter)))
    eval(parse(text = paste0("rm(list = 'LASSO1_", iter, "')")))
  }
  else {
    missing[[1]] <- c(missing[[1]], iter)
  }
  
  if(file.exists(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/RawLASSO/Results2/LASSO2_", iter, ".RData"))) {
    load(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/RawLASSO/Results2/LASSO2_", iter, ".RData"))
    eval(parse(text = paste0("LASSO2[[iter]] <- LASSO2_", iter)))
    eval(parse(text = paste0("rm(list = 'LASSO2_", iter, "')")))
  }
  else {
    missing[[2]] <- c(missing[[2]], iter)
  }
  
  if(file.exists(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/RawLASSO/Results3/LASSO3_", iter, ".RData"))) {
    load(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/RawLASSO/Results3/LASSO3_", iter, ".RData"))
    eval(parse(text = paste0("LASSO3[[iter]] <- LASSO3_", iter)))
    eval(parse(text = paste0("rm(list = 'LASSO3_", iter, "')")))
  }
  else {
    missing[[3]] <- c(missing[[3]], iter)
  }
  
  if(file.exists(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/RawLASSO/Results4/LASSO4_", iter, ".RData"))) {
    load(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/RawLASSO/Results4/LASSO4_", iter, ".RData"))
    eval(parse(text = paste0("LASSO4[[iter]] <- LASSO4_", iter)))
    eval(parse(text = paste0("rm(list = 'LASSO4_", iter, "')")))
  }
  else {
    missing[[4]] <- c(missing[[4]], iter)
  }
  
  if(file.exists(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/RawLASSO/Results5/LASSO5_", iter, ".RData"))) {
    load(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/RawLASSO/Results5/LASSO5_", iter, ".RData"))
    eval(parse(text = paste0("LASSO5[[iter]] <- LASSO5_", iter)))
    eval(parse(text = paste0("rm(list = 'LASSO5_", iter, "')")))
  }
  else {
    missing[[5]] <- c(missing[[5]], iter)
  }
}
save(LASSO1, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/LASSO1/Results/LASSO1.RData")
save(LASSO2, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/LASSO2/Results/LASSO2.RData")
save(LASSO3, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/LASSO3/Results/LASSO3.RData")
save(LASSO4, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/LASSO4/Results/LASSO4.RData")
save(LASSO5, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/LASSO5/Results/LASSO5.RData")

NPGC2 <- vector("list", length = 1800)
for(iter in 1:1800) {
  if(file.exists(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/NPGC2/Results/Raw/NPGC2_", iter, ".RData"))) {
    load(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/NPGC2/Results/Raw/NPGC2_", iter, ".RData"))
    eval(parse(text = paste0("NPGC2[[iter]] <- NPGC2_", iter)))
    eval(parse(text = paste0("rm(list = 'NPGC2_", iter, "')")))
  }
}
save(NPGC2, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Simulations/NPGC2/Results/NPGC2.RData") 
  
source("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/utils.R")
# Table 2
table2 <- produce_tableV1() %>% filter(Model == "TAR", Length == 250)
print(table2, width = 1000)

# Table 3
table3 <- produce_tableV1() %>% filter(Model == "TAR", Length == 500)
print(table3, width = 1000)

# Table 4
table4 <- produce_tableV1() %>% filter(Model == "TAR", Length == 1000)
print(table4, width = 1000)

# Table 5
table5 <- produce_tableV3() %>% filter(Model == "TAR")
print(table5, width = 1000)

# Table 6
table6 <- produce_tableV4() %>% filter(Model == "TAR")
print(table6, width = 1000)

# Table 7
table7 <- produce_tableV5() %>% filter(Model == "TAR")
print(table7, width = 1000)

# Table 8
table8 <- produce_tableV6() %>% filter(Model == "TAR") 
print(table8, width = 1000)

# Supplementary Tables
# Table 9
table9 <- produce_tableV1() %>% filter(Model == "VAR", Length == 250)
print(table9, width = 1000)

# Table 10
table10 <- produce_tableV1() %>% filter(Model == "VAR", Length == 500)
print(table10, width = 1000)

# Table 11
table11 <- produce_tableV1() %>% filter(Model == "VAR", Length == 1000)
print(table11, width = 1000)

# Table 12
table12 <- produce_tableV6() %>% filter(Model == "VAR") 
print(table12, width = 1000)

# Table 13
table13 <- produce_tableV2() %>% filter(Model == "TAR", Length == 250)
print(table13, width = 1000)

# Table 14
table14 <- produce_tableV2() %>% filter(Model == "TAR", Length == 500)
print(table14, width = 1000)

# Table 15
table15 <- produce_tableV2() %>% filter(Model == "TAR", Length == 1000)
print(table15, width = 1000)













