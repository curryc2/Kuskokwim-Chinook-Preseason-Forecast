#==================================================================================================
#Project Name: KUSKOKWIM RIVER CHINOOK SALMON FORECAST MODEL - Current autoregressive model
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
do.est <- TRUE

n.chains <- 4
n.iter <- 1e5#2e4
n.thin <- 5
(n.iter/n.thin)*0.5*n.chains
# Model Designations
# model_file <- "Deriso-Schnute3.stan" #Already Run, no AR term
# model_name <- "Deriso-Schnute3"

model_file <- "AR1-empiricalErr.stan" #Already Run, no AR term
model_name <- "AR1-empiricalErr"

# Method for filling in uncertainty
cv.method <- c("last_year","sma")[1]

model_name <- paste(model_name, cv.method, sep="_")

# Define Workflow Paths ============================================
# *Assumes you are working from the Sergent_Streamflow R project
wd <- getwd()
dir.output <- file.path(wd, "output", model_name)
dir.figs <- file.path(wd, "figs", model_name)
dir.data <- file.path(wd,"data")
dir.R <- file.path(wd,"R")

#Create
dir.create(dir.figs, recursive=TRUE)
dir.create(dir.output, recursive=TRUE)


# Read in Data =================================================================
dat <- read.csv(file.path(dir.data, "Kusko-RunSize.csv"), header=TRUE)
head(dat)

# CV Conversion to Logspace ====================================================
dat$ln_est <- log(dat$est)


dat$cv_interp <- NA

# Deal with missing uncertainty estimates
i <- 1
for(i in 1:nrow(dat)) {
  if(is.na(dat$cv[i])) { #CV is missing
    if(cv.method=="last_year") {
      dat$cv_interp[i] <- dat$cv[i-1]
    }
    if(cv.method=="sma") {
      # Fit simple moving average
      sma <- sma(dat$cv[1:(i-1)], h=1)
      # Fitted values and +1 forecast
      sma.fit <- as.vector(sma$fitted[,1])
      sma.fcst <- sma$forecast[1]
      
      # Fill in predicted value
      dat$cv_interp[i] <- sma.fcst
      
      # Trial Plot
      # plot(x=dat$year[1:(i-1)], y=dat$cv[1:(i-1)], type='l', col='gray', xlim=c(min(dat$year), max(dat$year)))
      # points(x=dat$year[1:(i-1)], y=dat$cv[1:(i-1)], pch=21, bg='gray')
      # lines(x=dat$year[1:(i-1)], y=sma.fit, col=rgb(1,0,0, alpha=0.5), lwd=2)
      # # Predicted
      # segments(x0=dat$year[i-1], y0=dat$cv[i-1], x1=dat$year[i], y1=sma.fcst, lwd=2, lty=3)
    }
  } else {
    dat$cv_interp[i] <- dat$cv[i]
  }
}

# Calculate log variance and CV 
dat$ln_var <- log(dat$cv_interp^2 + 1)
dat$ln_sd <- sqrt(dat$ln_var)
dat$ln_cv <- dat$ln_sd/dat$ln_est

dat

# Fit Stan Model ===============================================================
N <- length(dat$year)
# year <- dat$year
# ln_est <- dat$ln_est
# ln_sd <- dat$ln_sd

fit <- NULL

if(do.est==TRUE) {
  
  # Initialization Function
  # init_fn <- function(chain_id=1) {
  #   list("alpha"=runif(S, 0.1,1),
  #        "beta"=runif(S, 0, 1e-5),
  #        "sigma.oe"=runif(S, 0.1, 0.5),
  #        "shape"=runif(S, -0.8,-0.2))
  # }
  # init_fn()
  # Initial List of Lists for Multiple Chains
  # init_ll <- lapply(1:n.chains, function(id) init_fn(chain_id = id))
  
  # Fit Model
  fit <- stan(file=file.path(dir.R, model_file),
              model_name=model_name,
              data=list("N"=N, "year"=dat$year,
                        "ln_est"=dat$ln_est,
                        "ln_sd"=dat$ln_sd,
                        "ln_cv"=dat$ln_cv),
              chains=n.chains, iter=n.iter, thin=n.thin,
              cores=n.chains, verbose=FALSE,
              # chains=1, iter=n.iter, thin=n.thin,
              # cores=1, verbose=FALSE,
              seed=101,
              control=list(adapt_delta=0.99))
  #,
              # init=init_ll)
  plot(fit)
  
  
  #Save Output
  saveRDS(fit, file=file.path(dir.output,"fit.rds"))
  
  
}else {
  fit <- readRDS(file=file.path(dir.output, "fit.rds"))
}

# Save Summary file as a .csv
# write.csv(summary(fit)$summary, file=file.path(dir.figs, paste0(temp.species,"_summary.csv")))


# Extract Parameter Samples
pars <- rstan::extract(fit)

summary(pars$fcst)

# Plot and Write Output ========================================================

# Plot Fit to Data

pdf(file.path(dir.figs, "Point Prediction.pdf"), height=5, width=7)
par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(4,4,4,1))
# Log Space
sum.pred <- apply(pars$ln_pred, 2, quantile, 
                  probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
plot(ln_est ~ year, data=dat, type='l',
     xlab="Return Year", ylab="ln(Return Abundance)",
     main="Kuskokwim River Chinook Salmon: AR1 Model")
points(ln_est ~ year, data=dat, pch=21, bg='gray')
grid(col="gray")
polygon(x=c(dat$year[2:N], rev(dat$year[2:N])),
        y=c(sum.pred[1,], rev(sum.pred[5,])),
        col=rgb(1,0,0, alpha=0.2), border=FALSE)
polygon(x=c(dat$year[2:N], rev(dat$year[2:N])),
        y=c(sum.pred[2,], rev(sum.pred[4,])),
        col=rgb(1,0,0, alpha=0.2), border=FALSE)
lines(x=dat$year[2:N], y=sum.pred[3,], col='red')

# Normal
sum.pred <- apply(pars$pred, 2, quantile, 
                  probs=c(0.025, 0.25, 0.5, 0.75, 0.975))/1e3
plot(est/1e3 ~ year, data=dat, type='b',
     xlab="Return Year", ylab="Chinook Salmon Abundance (thousands)",
     main="Kuskokwim River Chinook Salmon: AR1 Model")
points(est/1e3 ~ year, data=dat, pch=21, bg='gray')
grid(col="gray")
polygon(x=c(dat$year[2:N], rev(dat$year[2:N])),
        y=c(sum.pred[1,], rev(sum.pred[5,])),
        col=rgb(1,0,0, alpha=0.2), border=FALSE)
polygon(x=c(dat$year[2:N], rev(dat$year[2:N])),
        y=c(sum.pred[2,], rev(sum.pred[4,])),
        col=rgb(1,0,0, alpha=0.2), border=FALSE)
lines(x=dat$year[2:N], y=sum.pred[3,], col='red')

dev.off()

# Posterior Predictive Distribution =================================

pdf(file.path(dir.figs, "Posterior Pred.pdf"), height=5, width=7)
par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(4,4,4,1))
# Log Space
sum.pred <- apply(pars$post_ln_pred, 2, quantile, 
                  probs=c(0.025, 0.25, 0.5, 0.75, 0.975))
plot(ln_est ~ year, data=dat, type='b',
     xlab="Return Year", ylab="ln(Return Abundance)",
     main="Kuskokwim River Chinook Salmon: AR1 Model")
points(ln_est ~ year, data=dat, pch=21, bg='gray')
grid(col="gray")
polygon(x=c(dat$year[2:N], rev(dat$year[2:N])),
        y=c(sum.pred[1,], rev(sum.pred[5,])),
        col=rgb(1,0,0, alpha=0.2), border=FALSE)
polygon(x=c(dat$year[2:N], rev(dat$year[2:N])),
        y=c(sum.pred[2,], rev(sum.pred[4,])),
        col=rgb(1,0,0, alpha=0.2), border=FALSE)
lines(x=dat$year[2:N], y=sum.pred[3,], col='red')

# Normal
sum.pred <- apply(pars$post_pred, 2, quantile, 
                  probs=c(0.025, 0.25, 0.5, 0.75, 0.975))/1e3
plot(est/1e3 ~ year, data=dat, type='b',
     xlab="Return Year", ylab="Chinook Salmon Abundance (thousands)",
     main="Kuskokwim River Chinook Salmon: AR1 Model")
points(est/1e3 ~ year, data=dat, pch=21, bg='gray')
grid(col="gray")
polygon(x=c(dat$year[2:N], rev(dat$year[2:N])),
        y=c(sum.pred[1,], rev(sum.pred[5,])),
        col=rgb(1,0,0, alpha=0.2), border=FALSE)
polygon(x=c(dat$year[2:N], rev(dat$year[2:N])),
        y=c(sum.pred[2,], rev(sum.pred[4,])),
        col=rgb(1,0,0, alpha=0.2), border=FALSE)
lines(x=dat$year[2:N], y=sum.pred[3,], col='red')

dev.off()

# Plot: Residuals ==============================================
resid.mean <- apply(pars$residual, 2, mean)
resid.med <- apply(pars$residual, 2, median)

hist(resid.mean) #0.03910859
hist(resid.med) #0.03920533

mean(resid.mean)
mean(resid.med)

sd(resid.mean)

dim(pars$ln_pred)

plot(resid.mean, type='b')
abline(h=0)

# 
resid.df <- data.frame(pars$residual)
names(resid.df) <- dat$year[2:N]
resid.list <- melt(resid.df)
names(resid.list) <- c("year", "resid")

g <- resid.list %>% ggplot(aes(x=year, y=resid)) +
       theme_linedraw() +
       geom_hline(yintercept=0, col="red") +
       geom_eye(.width=c(0.95,0.5)) +
       theme(axis.text.x=element_text(angle=90)) +
       xlab("Return Year") + ylab("Model Residuals (log space)") +
       ggtitle("Kuskokwim River Chinook Salmon: AR1-Empirical Model")
g
ggsave(file.path(dir.figs,"Residuals.pdf"), plot=g, height=6, width=8, units="in")
       


# Plot: Forecast =================================================
par(mfrow=c(3,1))
hist(pars$fcst/1e3, main="AR1 Model: Estimation Uncertainty in Point Estimate",
       xlab="2020 Predicted Run Size (thousands)", col='blue', 
       freq=FALSE, breaks=100)


hist(pars$post_fcst/1e3, main="AR1 Model: Posterior Predictive Distribution",
     xlab="2020 Predicted Run Size (thousands)", col='blue', 
     freq=FALSE, breaks=100)


hist(pars$post_fcst_unc/1e3, main="AR1 Model: Posterior Predictive Distribution (uncorrected)",
     xlab="2020 Predicted Run Size (thousands)", col='blue', 
     freq=FALSE, breaks=100)


# ggplot version
df.fcst <- data.frame("Point Estimate", pars$fcst)
df.post <- data.frame("Posterior Predictive", pars$post_fcst)

names(df.fcst) <- c("Estimate","Forecast")
names(df.post) <- c("Estimate","Forecast")

df.fcst.comb <- rbind(df.fcst, df.post)

g <- df.fcst.comb %>% ggplot(aes(x=Forecast/1e3, color=Estimate, fill=Estimate)) +
       theme_linedraw() +
       scale_fill_colorblind() +
       geom_density(alpha=0.5) 
       
g

plotPost(pars$fcst/1e3, xlim=c(150,300))

plotPost(pars$post_fcst/1e3, xlim=c(150,300))
plotPost(pars$post_fcst_unc/1e3, xlim=c(100,400))

x.lim <- c(100,400)
pdf(file.path(dir.figs, "Best post_fcst.pdf"), height=6, width=6)
plotPost(pars$post_fcst/1e3, xlim=x.lim,
         xlab="2020 Predicted Run Size (thousands)", ylab="Relative Probability",
         main="Kuskokwim River Chinook Salmon: AR1-Empirical Model")
plotPost(pars$post_fcst/1e3, xlim=x.lim,
         xlab="2020 Predicted Run Size (thousands)", ylab="Relative Probability",
         main="Kuskokwim River Chinook Salmon: AR1-Empirical Model", showCurve=TRUE)
dev.off()

# Plot: Cumulative Probability of Forecast ======================
g.cum <- data.frame(pars$post_fcst) %>% ggplot(aes(x=pars.post_fcst/1e3)) +
  theme_linedraw() +
  # scale_color_colorblind() +
  geom_hline(yintercept=0.5, color="blue") +
  geom_hline(yintercept=c(0.25,0.75), lty=2, color="blue") +
  stat_ecdf(lwd=2, alpha=0.7, col="red") +
  # facet_wrap(~Forecast, ncol=1, scales="free_x") +
  # theme(legend.position='none') +
  xlab("2020 Predicted Run Size (thousands)") +
  ylab("Probability of a Run Size < X") +
  coord_cartesian(xlim=c(150,300)) + 
  ggtitle("Kuskokwim River Chinook Salmon: AR1-Empirical Model")
g.cum

ggsave(file.path(dir.figs, "Cum Post_fcst.pdf"), plot=g.cum, width=6, height=4)

# Calculate Performance Metrics =============================
# Extract log likelihood
logLike <- extract_log_lik(fit, parameter_name="logLike")

waic <- loo::waic(logLike)
write.csv(waic['estimates'], file=file.path(dir.figs,"waic.csv"))

# Loo


# RISK ANALYSIS =============================================
trial.esc <- c(65e3, 120e3)
n.trial.esc <- length(trial.esc)

# Calcualte potential harvest
harv.lb <- data.frame("Lower Bound: 65,000", pars$post_fcst-65e3)
harv.ub <- data.frame("Upper Bound: 120,000", pars$post_fcst-120e3)
names(harv.lb) <- c("Escapement", "harvest"); names(harv.ub) <- c("Escapement", "harvest")

harv.all <- rbind(harv.lb, harv.ub)

g.harv.cum <- harv.all %>% ggplot(aes(x=harvest/1e3, color=Escapement, group=Escapement)) +
  theme_linedraw() +
  scale_color_colorblind() +
  geom_hline(yintercept=0.5, color="blue") +
  geom_hline(yintercept=c(0.25,0.75), lty=2, color="blue") +
  stat_ecdf(lwd=2, alpha=0.7) +
  # facet_wrap(~Escapement, ncol=1) +
  # theme(legend.position='none') +
  xlab("2020 Potential Harvest (thousands)") +
  ylab("Probability of a Total Harvest < X") +
  # coord_cartesian(xlim=c(150,300)) + 
  ggtitle("Kuskokwim River Chinook Salmon: AR1-Empirical Model") +
  theme(legend.position = "top")
g.harv.cum

ggsave(file.path(dir.figs, "Potential Harvest.pdf"), plot=g.harv.cum, height=5, width=8, units='in')

# Calculate Forecast Metric in Normal Space ======================================
fcst <- c(NA,apply(pars$post_pred, 2, median))

dat <- data.frame(dat, fcst)

dat <- dat %>% mutate("diff"=est-fcst,
                      "pct"=diff/est,
                      "prop"=fcst/est,
                      "resid"=est-fcst,
                      "ln_resid"=log(est)-log(fcst))
# Subset for last 10, 20 all
dat.10 <- dat %>% filter(year> max(dat$year)-10)

dat.20 <- dat %>% filter(year> max(dat$year)-20)

# summary.all <- dat %>% summarize("mape"=mape(truth=est, estimate=fcst))
s.names <- c("1977+ (all years)", "Last 20 years", "Last 10 years")
mape <- c(as.numeric(mape(dat, est, fcst)[3]), as.numeric(mape(dat.20, est, fcst)[3]), as.numeric(mape(dat.10, est, fcst)[3]))
rmse <- c(as.numeric(rmse(dat, est, fcst)[3]), as.numeric(rmse(dat.20, est, fcst)[3]), as.numeric(rmse(dat.10, est, fcst)[3]))
rsq <- c(as.numeric(rsq_trad(dat, est, fcst)[3]), as.numeric(rsq_trad(dat.20, est, fcst)[3]), as.numeric(rsq_trad(dat.10, est, fcst)[3]))

summary <- data.frame(s.names, mape, rmse, rsq)

write.csv(summary, file=file.path(dir.figs, "Summary.csv"))


# Plot: Posteriors for other model parameters ======================
pdf(file.path(dir.figs, "Model Parameters.pdf"), height=5, width=10)
par(mfrow=c(1,2), mar=c(4,4,1,0), oma=c(0,0,2,0))
plotPost(pars$alpha,
         xlab="Alpha Parameter", #ylab="Relative Probability",
         showCurve=FALSE)
mtext("Relative Probability", side=2, font=1, line=2, outer=FALSE, cex=1.5)
plotPost(pars$phi,
         xlab=bquote("Autocorrelation Coefficient ", phi), #ylab="Relative Probability",
         showCurve=FALSE)
mtext("Kuskokwim River Chinook Salmon: AR1 Models", side=3, font=2, line=0, outer=TRUE, cex=1.5)

dev.off()
