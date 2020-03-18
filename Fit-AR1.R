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
require(dplyr)
require(smooth)
require(Mcomp)

#CONTROL SECTION ==========================================================
do.est <- TRUE

n.chains <- 4
n.iter <- 3e4#2e4
n.thin <- 15
(n.iter/n.thin)*0.5*n.chains
# Model Designations
# model_file <- "Deriso-Schnute3.stan" #Already Run, no AR term
# model_name <- "Deriso-Schnute3"

model_file <- "AR1.stan" #Already Run, no AR term
model_name <- "AR1"

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

dat

# Fit Stan Model ===============================================================
N <- length(dat$year)
ln_est <- dat$year

fit <- NULL

# if(do.est==TRUE) {
  
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
                        "ln_sd"=dat$ln_sd),
              chains=n.chains, iter=n.iter, thin=n.thin,
              cores=n.chains, verbose=TRUE,
              # chains=1, iter=n.iter, thin=n.thin,
              # cores=1, verbose=FALSE,
              seed=101,
              control=list(adapt_delta=0.99))#,
              # init=init_ll)
  
  
  
  #Save Output
  saveRDS(fit, file=file.path(dir.output,"fit.rds"))
  
  
}else {
  fit <- readRDS(file=file.path(dir.output, "fit.rds"))
}

# Save Summary file as a .csv
write.csv(summary(fit)$summary, file=file.path(dir.figs, paste0(temp.species,"_summary.csv")))


# Extract Parameter Samples
pars <- rstan::extract(fit)

# Plot and Write Output ========================================================

