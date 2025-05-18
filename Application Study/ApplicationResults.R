rm(list = ls())
library(tidyverse)
library(devtools)
library(Rcpp)
library(RcppArmadillo)
library(gridExtra)
library(grid)
library(ggridges)
library(ggtext)
library(ggplot2)
library(R.matlab)
source("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/utils.R")
load("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/ApplicationData.RData")
load("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/ApplicationKey.RData")

methods <- c("NPGC", "ZERO", "GAUSS")
for(method in 1:length(methods)) {
  for(item in 1:3) {
    if(file.exists(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/", methods[method], item, ".RData"))) {
      load(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/", methods[method], item, ".RData"))
    } else {
      print(paste0(methods[method], item, ".RData does not exist."))
    }
  }
}

LASSO <- vector("list", length = 30)
for(item in 1:30) {
  if(file.exists(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/LResult", item, ".RData"))) {
    load(paste0("~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/Results/LResult", item, ".RData"))
    LASSO[[item]] <- LResult
  } else {
    print(paste0("LResult", item, ".RData does not exist."))
  }
}

AppResult <- tibble(Methods = rep(c("Actual", "NPGC", "GAUSS", "ZERO", "LASSO", "LASSO", "GR-Lag", "GR-Lag", "GR-Var", "GR-Var", "MIXED", "MIXED", "HIER", "HIER"), 3), 
                    Proc = rep(c("", "", "", "",  rep(c("MV", "ONE"), 5)), 3),
                    Infer = rep(c(1, 0.90, 0.90, 0.50, rep(c(0.50, 0.001), 5)), 3),
                    Stimulus = rep(c("Jaguar\nmating call", "Humpback\nwhale", "Knudsen's\nfrog"), each = 14),
                    Stat = c(1, 1 - npgc_eval(NPGC1)[[1]], 1 - gauss_eval(GAUSS1)[[1]], zero_eval(ZERO1)[[1]], rep(lasso_eval(LASSO[[1]])[[1]], 2), rep(lasso_eval(LASSO[[2]])[[1]], 2), rep(lasso_eval(LASSO[[3]])[[1]], 2), rep(lasso_eval(LASSO[[4]])[[1]], 2), rep(lasso_eval(LASSO[[5]])[[1]], 2),
                             0, 1 - npgc_eval(NPGC2)[[1]], 1 - gauss_eval(GAUSS2)[[1]], zero_eval(ZERO2)[[1]], rep(lasso_eval(LASSO[[6]])[[1]], 2), rep(lasso_eval(LASSO[[7]])[[1]], 2), rep(lasso_eval(LASSO[[8]])[[1]], 2), rep(lasso_eval(LASSO[[9]])[[1]], 2), rep(lasso_eval(LASSO[[10]])[[1]], 2),
                             0, 1 - npgc_eval(NPGC3)[[1]], 1 - gauss_eval(GAUSS3)[[1]], zero_eval(ZERO3)[[1]], rep(lasso_eval(LASSO[[11]])[[1]], 2), rep(lasso_eval(LASSO[[12]])[[1]], 2), rep(lasso_eval(LASSO[[13]])[[1]], 2), rep(lasso_eval(LASSO[[14]])[[1]], 2), rep(lasso_eval(LASSO[[15]])[[1]]), 2))

AppResult <- AppResult %>% mutate(Dec = (AppResult$Stat >= AppResult$Infer)) %>% 
  mutate(Methods = factor(Methods, levels = c("Actual", "NPGC", "GAUSS", "ZERO", "LASSO", "GR-Lag", "GR-Var", "MIXED", "HIER"))) %>% 
  mutate(Proc = factor(Proc, levels = c("", "MV", "ONE"))) %>% 
  mutate(Stimulus = factor(Stimulus, levels = c("Knudsen's\nfrog", "Humpback\nwhale", "Jaguar\nmating call")))
AppResult <- AppResult %>% mutate(Decision = if_else(AppResult$Dec, "Granger causal", "Non-causal"))


AppFigure2 <- ggplot() +
  geom_tile(data = AppResult, aes(x = Proc, y = Stimulus, fill = Decision), color = "black") +
  facet_wrap(~ Methods, nrow = 1, scales = "free_x") +
  scale_x_discrete("", position = "top", expand = c(0, 0)) +
  scale_y_discrete("", expand = c(0, 0)) +
  scale_fill_manual("", values = c("Non-causal" = "white", "Granger causal" = "gray50")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(angle = 45, hjust = 0.7, size = 14),
        axis.title = element_blank(),
        axis.text.x = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key.width = unit(2, "cm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 16),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(linewidth = 1))

gp <- ggplotGrob(AppFigure2)
facet.columns <- gp$layout$l[grepl("panel", gp$layout$name)]
x.var <- sapply(ggplot_build(AppFigure2)$layout$panel_scales_x,
                function(l) length(l$range$range))

# change the relative widths of the facet columns based on
# how many unique x-axis values are in each facet
gp$widths[facet.columns] <- gp$widths[facet.columns] * x.var
# plot result
quartz(file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/AppFigure2.pdf", type = "pdf", width = 14, height = 3.6)
grid::grid.draw(gp)
dev.off()


OracleResult <- tibble(Methods = rep(c("Actual", rep(c("LASSO", "LASSO", "GR-Lag", "GR-Lag", "GR-Var", "GR-Var", "MIXED", "MIXED", "HIER", "HIER"), each = 31)), 3),
                       log10Lambda = rep(c(NA, rep(seq(-1, 2, 0.1), 10)), 3),
                       Proc = rep(c("",  rep(rep(c("MV", "ONE"), each = 31), 5)), 3),
                       Infer = rep(c(1,  rep(rep(c(0.50, 0.001), each = 31), 5)), 3),
                       Stimulus = rep(c("JMC", "HBW", "KNF"), each = 311),
                       Stat = c(1, rep(oracle_lasso_eval(LASSO[[16]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[17]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[18]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[19]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[20]])[[1]], 2),
                                0, rep(oracle_lasso_eval(LASSO[[21]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[22]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[23]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[24]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[25]])[[1]], 2),
                                0, rep(oracle_lasso_eval(LASSO[[26]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[27]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[28]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[29]])[[1]], 2),
                                rep(oracle_lasso_eval(LASSO[[30]])[[1]], 2)))

OracleResult <- OracleResult %>% mutate(Dec = (OracleResult$Stat >= OracleResult$Infer)) %>% 
  mutate(Methods = factor(Methods, levels = c("Actual", "LASSO", "GR-Lag", "GR-Var", "MIXED", "HIER"))) %>% 
  mutate(Proc = factor(Proc, levels = c("", "MV", "ONE"))) %>% 
  mutate(Stimulus = factor(Stimulus, levels = c("KNF", "HBW", "JMC")))
OracleResult <- OracleResult %>% mutate(Decision = if_else(OracleResult$Dec, "Granger causal", "Non-causal"))

AppFigure3_1 <- ggplot() +
  geom_tile(data = OracleResult %>% filter(Methods == "LASSO", Proc == "ONE"), aes(x = log10Lambda, y = Stimulus, fill = Decision, width = 0.1), color = "black") +
  facet_wrap(~ Methods, nrow = 1, scales = "free_x") +
  scale_x_continuous(expression(log[10]*"(\u03bb)"), position = "bottom", expand = c(0, 0), limits = c(-1.05, 2.05), seq(-1, 2, 0.2)) +
  scale_y_discrete("", expand = c(0, 0)) +
  scale_fill_manual("", values = c("Non-causal" = "white", "Granger causal" = "gray50")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.7, size = 14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key.width = unit(2, "cm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 16),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(linewidth = 1))
AppFigure3_2 <- ggplot() +
  geom_tile(data = OracleResult %>% filter(Methods == "GR-Lag", Proc == "ONE"), aes(x = log10Lambda, y = Stimulus, fill = Decision, width = 0.1), color = "black") +
  facet_wrap(~ Methods, nrow = 1, scales = "free_x") +
  scale_x_continuous(expression(log[10]*"(\u03bb)"), position = "bottom", expand = c(0, 0), limits = c(-1.05, 2.05), seq(-1, 2, 0.2)) +
  scale_y_discrete("", expand = c(0, 0)) +
  scale_fill_manual("", values = c("Non-causal" = "white", "Granger causal" = "gray50")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.7, size = 14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key.width = unit(2, "cm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 16),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(linewidth = 1))
AppFigure3_3 <- ggplot() +
  geom_tile(data = OracleResult %>% filter(Methods == "GR-Var", Proc == "ONE"), aes(x = log10Lambda, y = Stimulus, fill = Decision, width = 0.1), color = "black") +
  facet_wrap(~ Methods, nrow = 1, scales = "free_x") +
  scale_x_continuous(expression(log[10]*"(\u03bb)"), position = "bottom", expand = c(0, 0), limits = c(-1.05, 2.05), seq(-1, 2, 0.2)) +
  scale_y_discrete("", expand = c(0, 0)) +
  scale_fill_manual("", values = c("Non-causal" = "white", "Granger causal" = "gray50")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.7, size = 14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key.width = unit(2, "cm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 16),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(linewidth = 1))
AppFigure3_4 <- ggplot() +
  geom_tile(data = OracleResult %>% filter(Methods == "MIXED", Proc == "ONE"), aes(x = log10Lambda, y = Stimulus, fill = Decision, width = 0.1), color = "black") +
  facet_wrap(~ Methods, nrow = 1, scales = "free_x") +
  scale_x_continuous(expression(log[10]*"(\u03bb)"), position = "bottom", expand = c(0, 0), limits = c(-1.05, 2.05), seq(-1, 2, 0.2)) +
  scale_y_discrete("", expand = c(0, 0)) +
  scale_fill_manual("", values = c("Non-causal" = "white", "Granger causal" = "gray50")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.7, size = 14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key.width = unit(2, "cm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 16),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(linewidth = 1))
AppFigure3_5 <- ggplot() +
  geom_tile(data = OracleResult %>% filter(Methods == "HIER", Proc == "ONE"), aes(x = log10Lambda, y = Stimulus, fill = Decision, width = 0.1), color = "black") +
  facet_wrap(~ Methods, nrow = 1, scales = "free_x") +
  scale_x_continuous(expression(log[10]*"(\u03bb)"), position = "bottom", expand = c(0, 0), limits = c(-1.05, 2.05), seq(-1, 2, 0.2)) +
  scale_y_discrete("", expand = c(0, 0)) +
  scale_fill_manual("", values = c("Non-causal" = "white", "Granger causal" = "gray50")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.7, size = 14),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key.width = unit(2, "cm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(size = 16),
        panel.spacing = unit(1, "lines"),
        panel.border = element_rect(linewidth = 1))

g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
leg <- g_legend(AppFigure3_5)

quartz(file = "~/Wake Forest University Dropbox/Noah Gade/Research/NPGC/Application/AppFigure3.pdf", type = "pdf", width = 15, height = 13)
grid.arrange(arrangeGrob(AppFigure3_1 + theme(legend.position = "none"),
                         AppFigure3_2 + theme(legend.position = "none"),
                         AppFigure3_3 + theme(legend.position = "none"),
                         AppFigure3_4 + theme(legend.position = "none"), 
                         AppFigure3_5 + theme(legend.position = "none"),
                         nrow = 5, heights = rep(1, 5)), leg, nrow = 2, heights = c(15, 1))
dev.off()

