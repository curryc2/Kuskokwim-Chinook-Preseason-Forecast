#==================================================================================================
#Project Name: KUSKOKWIM RIVER CHINOOK SALMON FORECAST MODEL - Compare Uncertainty Among Models
#Creator: Curry James Cunningham, College of Fisheries and Ocean Sciences, UAF
#Date: 3.17.20
#
#Purpose: Predict total salmon run size 1-year in advance using current AR-1 model used by ADF&G (but in log space)
#
#
#
#==================================================================================================
#NOTES:
#
#==================================================================================================
require(BEST)
require(rstan)
require(tidybayes)
require(shinystan)
require(tidyverse)
require(ggplot2)
require(ggthemes)
require(dplyr)
require(smooth)
require(Mcomp)
require(loo)
require(reshape2)
require(yardstick)

#CONTROL SECTION ==========================================================

# adfg.range <- c(200, 260)

dir.base <- "AR1_last_year"
dir.emp <- "AR1-empiricalErr_last_year"

# Define Workflow Paths ============================================
# *Assumes you are working from the Sergent_Streamflow R project
wd <- getwd()
dir.output <- file.path(wd, "output")
dir.figs <- file.path(wd, "figs")
dir.data <- file.path(wd,"data")
dir.R <- file.path(wd,"R")

#Create
dir.create(dir.figs, recursive=TRUE)
dir.create(dir.output, recursive=TRUE)

# Load Posterior predictive distributions ===========================================
fit.base <- readRDS(file=file.path(dir.output, dir.base, "fit.rds"))
fit.emp <- readRDS(file=file.path(dir.output, dir.emp, "fit.rds"))

summary(pars.base$post_fcst)
summary(pars.base$post_fcst_unc)

summary(pars.emp$post_fcst)
summary(pars.emp$post_fcst_unc)

pars.base <- rstan::extract(fit.base)
pars.emp <- rstan::extract(fit.emp)

list.base <- data.frame("AR1-Base", pars.base$post_fcst)
list.emp <- data.frame("AR1-Empirical", pars.emp$post_fcst)

names(list.base) <- c("Model", "fcst")
names(list.emp) <- c("Model", "fcst")

list.all <- rbind(list.base, list.emp)

# Load Approximate ADFG Forecast =====================================
appx.adfg <- read.csv(file.path("figs","Last-Year-Fcst-Error","Approximate ADFG Forecast.csv"))

adfg.range <- c(appx.adfg$curr.adfg.range.low, appx.adfg$curr.adfg.range.up)/1e3

# Plot Forecast Comparison ============================================

g <- ggplot(list.all, aes(x=Model, y=fcst/1e3, fill=Model)) +
  theme_dark() +
       scale_fill_tableau() +
       geom_eye(.width=c(0.5,0.95)) +
       # coord_cartesian(ylim=c(0, 500)) +
       coord_flip(ylim=c(0,300)) +
       ylab("Chinook Salmon Abundance (thousands)") +
       ggtitle("Posterior Predictive Distributions for 2021 Forecast") +
       annotate("rect", ymin=adfg.range[1], ymax=adfg.range[2], xmin=-Inf, xmax=Inf, alpha=0.2, fill=rgb(1,0,0, alpha=0.2)) +
       theme(legend.position="none")

g

q.50 <- function(x) { return(quantile(x, probs=c(0.25,0.75))) }
q.95 <- function(x) { return(quantile(x, probs=c(0.025,0.975))) }

g <- ggplot(list.all, aes(x=Model, y=fcst/1e3, fill=Model)) +
       theme_dark() +
       scale_fill_tableau() +
       geom_violin(alpha = 0.5, lwd=0.1, scale='width') +
       stat_summary(fun.y="q.95", colour="black", geom="line", lwd=0.75) +
       stat_summary(fun.y="q.50", colour="black", geom="line", lwd=1.55) +
       stat_summary(fun.y="mean", colour="black", size=3, geom="point", pch=21) +
       coord_flip(ylim=c(0,300)) +
       ylab("Chinook Salmon Abundance (thousands)") +
       ggtitle("Posterior Predictive Distributions for 2021 Forecast") +
       annotate("rect", ymin=adfg.range[1], ymax=adfg.range[2], xmin=-Inf, xmax=Inf, alpha=0.2, fill=rgb(1,0,0, alpha=0.2)) +
       theme(legend.position="none")

g
ggsave(file.path(dir.figs,"Uncertainty Compare.png"), plot=g, height=3, width=8, units='in')


# Figure with AR Empirical only ==========================


g <- list.all %>%  filter(Model=="AR1-Empirical") %>%  ggplot(aes(x=Model, y=fcst/1e3, fill=Model)) +
  theme_dark() +
  scale_fill_tableau() +
  geom_eye(.width=c(0.5,0.95)) +
  # coord_cartesian(ylim=c(0, 500)) +
  coord_flip(ylim=c(0,300)) +
  ylab("Chinook Salmon Abundance (thousands)") +
  ggtitle("Posterior Predictive Distributions for 2021 Forecast") +
  annotate("rect", ymin=adfg.range[1], ymax=adfg.range[2], xmin=-Inf, xmax=Inf, alpha=0.2, fill=rgb(1,0,0, alpha=0.2)) +
  theme(legend.position="none")

g

q.50 <- function(x) { return(quantile(x, probs=c(0.25,0.75))) }
q.95 <- function(x) { return(quantile(x, probs=c(0.025,0.975))) }

g <- list.all %>% filter(Model=="AR1-Empirical") %>% ggplot(aes(x=Model, y=fcst/1e3, fill=Model)) +
  theme_dark() +
  scale_fill_tableau() +
  geom_violin(alpha = 0.5, lwd=0.1, scale='width') +
  stat_summary(fun.y="q.95", colour="black", geom="line", lwd=0.75) +
  stat_summary(fun.y="q.50", colour="black", geom="line", lwd=1.55) +
  stat_summary(fun.y="mean", colour="black", size=3, geom="point", pch=21) +
  coord_flip(ylim=c(0,300)) +
  ylab("Chinook Salmon Abundance (thousands)") +
  ggtitle("Posterior Predictive Distributions for 2021 Forecast") +
  annotate("rect", ymin=adfg.range[1], ymax=adfg.range[2], xmin=-Inf, xmax=Inf, alpha=0.2, fill=rgb(1,0,0, alpha=0.2)) +
  theme(legend.position="none")

g
ggsave(file.path(dir.figs,"Uncertainty Compare_AR1 Only.png"), plot=g, height=3, width=8, units='in')


# Versions without ADFG expectation ===========

g <- list.all %>% filter(Model=="AR1-Empirical") %>% ggplot(aes(x=Model, y=fcst/1e3, fill=Model)) +
  theme_dark() +
  scale_fill_tableau() +
  geom_violin(alpha = 0.5, lwd=0.1, scale='width') +
  stat_summary(fun.y="q.95", colour="black", geom="line", lwd=0.75) +
  stat_summary(fun.y="q.50", colour="black", geom="line", lwd=1.55) +
  stat_summary(fun.y="mean", colour="black", size=3, geom="point", pch=21) +
  coord_flip(ylim=c(0,300)) +
  ylab("Chinook Salmon Abundance (thousands)") +
  ggtitle("Posterior Predictive Distributions for 2021 Forecast") +
  # annotate("rect", ymin=adfg.range[1], ymax=adfg.range[2], xmin=-Inf, xmax=Inf, alpha=0.2, fill=rgb(1,0,0, alpha=0.2)) +
  theme(legend.position="none")

g
ggsave(file.path(dir.figs,"Uncertainty Compare_AR1 Only_noADFG_1.png"), plot=g, height=2, width=6, units='in')

g <- g + theme_linedraw() + theme(legend.position="none")
g
ggsave(file.path(dir.figs,"Uncertainty Compare_AR1 Only_noADFG_2.png"), plot=g, height=2, width=6, units='in')








