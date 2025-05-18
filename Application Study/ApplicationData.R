rm(list = ls())
library(tidyverse)
library(devtools)
library(Rcpp)
library(RcppArmadillo)
library(gridExtra)
library(grid)
library(ggridges)
library(ggplot2)
library(R.matlab)

# Jaguar mating call stimulus
raw_x01 <- readMat("~/Wake Forest University Dropbox/Noah Gade/Research/crcns-ac1/wehr/Stimuli/fragments/1.mat")
x01sf <- raw_x01$stimulus[2,1,1]$param[,,1]$sf[1,1]
x01key <- raw_x01$stimulus[2,1,1]$param[,,1]$description
x01 <- raw_x01$stimulus[1,1,1]$samples

# Response trial 060
raw <- readMat(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/crcns-ac1/wehr/Results/20020508-mw-003/20020508-mw-003-053.mat"))
y01 <- raw$response[,,1]$scale[1,1] * raw$response[,,1]$trace
y01notes <- raw$param[,,1]
y01sf <- raw$response[,,1]$sf[1,1]

# Humpback whale stimulus
raw_x02 <- readMat("~/Wake Forest University Dropbox/Noah Gade/Research/crcns-ac1/wehr/Stimuli/fragments/5.mat")
x02sf <- raw_x02$stimulus[2,1,1]$param[,,1]$sf[1,1]
x02key <- raw_x02$stimulus[2,1,1]$param[,,1]$description
x02 <- raw_x02$stimulus[1,1,1]$samples

# Knudsen's frog stimulus
raw_x03 <- readMat("~/Wake Forest University Dropbox/Noah Gade/Research/crcns-ac1/wehr/Stimuli/fragments/6.mat")
x03sf <- raw_x03$stimulus[2,1,1]$param[,,1]$sf[1,1]
x03key <- raw_x03$stimulus[2,1,1]$param[,,1]$description
x03 <- raw_x03$stimulus[1,1,1]$samples

# Tibble creation
FullDataX <- tibble(time = 1:nrow(x01), JMC = x01[,1], HBW = x02[,1], KNF = x03[,1])
FullDataX <- FullDataX %>% mutate(time = as.integer(ceiling(y01sf * time / x01sf))) %>% group_by(time) %>% filter(row_number() == n()) %>% ungroup()
FullDataY <- tibble(time = 1:nrow(y01), as_tibble(y01)) %>% rename(JMCR053 = V1)
FullData <- left_join(FullDataX, FullDataY, by = join_by(time)) %>% mutate(time = time / y01sf)

ResampleFreq <- 250
Data <- FullData[((4000 * FullData$time) %% (4000 / ResampleFreq)) == 0,]
ApplicationData <- vector("list")
lower <- 3
upper <- 12
ApplicationData$data <- as.matrix(Data %>% filter(time > lower & time < upper))
ApplicationData$freq <- paste0(ResampleFreq, "Hz")
save(ApplicationData, file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/ApplicationData.RData")

plotx <- ggplot(ApplicationData$data) +
  scale_x_continuous("Experiment Time (s)", expand = c(0, 0), breaks = seq(ceiling(2 * lower) / 2, floor(2 * upper) / 2, 0.5)) +
  scale_y_continuous("Sound Fragment Stimuli", expand = c(0.01, 0.01), limits = c(-24, 18)) +
  geom_line(aes(x = time, y = JMC + 12), linewidth = 0.2) +
  annotate("label", label = "Jaguar mating call", x = lower + 0.1, y = 16, size = 5, hjust = 0) +
  geom_line(aes(x = time, y = HBW), linewidth = 0.2) +
  annotate("label", label = "Humpback whale", x = lower + 0.1, y = 4, size = 5, hjust = 0) +
  geom_line(aes(x = time, y = KNF - 15), linewidth = 0.2) +
  annotate("label", label = "Knudsen's frog", x = lower + 0.1, y = -11, size = 5, hjust = 0) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.title = element_text(size = 20))

ploty60 <- ggplot(ApplicationData$data) +
  geom_line(aes(x = time, y = JMCR053), linewidth = 0.4) +
  scale_x_continuous("Experiment Time (s)", expand = c(0, 0), breaks = seq(11, 15, 0.5)) +
  scale_y_continuous("Trial No. 053", expand = c(0.01, 0.01)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_blank())

quartz(file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/AppFigure053.pdf", type = "pdf", width = 13, height = 9)
grid.arrange(ploty60, plotx, ncol = 1, heights = c(0.36, 1))
dev.off()
