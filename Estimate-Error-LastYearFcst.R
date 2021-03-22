#==================================================================================================
#Project Name: KUSKOKWIM RIVER CHINOOK SALMON FORECAST MODEL - Calculate Error For Last-year's forecast
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
require(tidyverse)
require(dplyr)
require(ggplot2)
require(ggthemes)
require(yardstick)


#CONTROL SECTION ==========================================================
model_name <- "Last-Year-Fcst-Error"

# Define Workflow Paths ============================================
# *Assumes you are working from the Sergent_Streamflow R project
wd <- getwd()
# dir.output <- file.path(wd, "output", model_name)
dir.figs <- file.path(wd, "figs", model_name)
dir.data <- file.path(wd,"data")
dir.R <- file.path(wd,"R")

#Create
dir.create(dir.figs, recursive=TRUE)
# dir.create(dir.output, recursive=TRUE)


# Read in Data =================================================================
dat <- read.csv(file.path(dir.data, "Kusko-RunSize-2020.csv"), header=TRUE)
head(dat)

# Calculate Normal Error =======================================================
dat <- dat %>% select(year, est) %>% mutate("fcst"=lag(est))

# g <- dat %>% ggplot(aes(x=year, y=est/1e3)) +
#       theme_linedraw() +
#       geom_line() +
#       geom_point(pch=21, bg='gray') +
#       geom_line(aes(y=fcst/1e3), col=rgb(1,0,0, alpha=0.5), lwd=2) +
#       xlab("Return Year") + ylab("Chinook Salmon Abundance (thousands)") +
#      
# 
# 
# g

pdf(file.path(dir.figs, "Plot Last Year Forecast.pdf"), height=5, width=6)
plot(x=dat$year, y=dat$est/1e3, type='l',
     xlab="Return Year", ylab="Chinook Salmon Abundance (thousands)",
     main="Kuskokwim Forecast: Last Year")
grid(col='gray')
axis(1, at=dat$year, lwd.ticks=0.5, col.ticks='darkgray', labels=FALSE)
points(x=dat$year, y=dat$est/1e3, pch=21, bg="gray")
# Forecast
lines(x=dat$year, y=dat$fcst/1e3, col=rgb(1,0,0, alpha=0.5), lwd=2)
dev.off()

# Calculate attributes
dat <- dat %>% mutate("diff"=est-fcst,
                      "pct"=diff/est,
                      "prop"=fcst/est,
                      "resid"=est-fcst,
                      "ln_resid"=log(est)-log(fcst))

mean(dat$ln_resid, na.rm=TRUE)
sd(dat$ln_resid, na.rm=TRUE)

# Calculate Current year forecast ===================================
# Extract % error for the last 7 years
max.ref <- length(dat$pct)
pct.err.7 <- dat$pct[(max.ref-6):max.ref]
length(pct.err.7)

# Check for 2020 fcst
# mean(abs(dat$pct[(max.ref-7):(max.ref-1)]))

# Average absolute percent error over last seven years
aape.7 <- mean(abs(pct.err.7))
aape.7

# Calculate forecast
curr.adfg.fcst <- dat$est[length(dat$est)]
curr.adfg.range.low <- curr.adfg.fcst - aape.7*curr.adfg.fcst
curr.adfg.range.up <- curr.adfg.fcst + aape.7*curr.adfg.fcst

# Approximate ADFG forecast
write.csv(data.frame(aape.7,curr.adfg.fcst, curr.adfg.range.low, curr.adfg.range.up), 
          file=file.path(dir.figs, "Approximate ADFG Forecast.csv"))


# Subset for last 10, 20 all =========================================
dat.10 <- dat %>% filter(year> max(dat$year)-10)

dat.20 <- dat %>% filter(year> max(dat$year)-20)

summary.all <- dat %>% summarize("mape"=mape(truth=est, estimate=fcst))


s.names <- c("1977+ (all years)", "Last 20 years", "Last 10 years")
mape <- c(as.numeric(mape(dat, est, fcst)[3]), as.numeric(mape(dat.20, est, fcst)[3]), as.numeric(mape(dat.10, est, fcst)[3]))
rmse <- c(as.numeric(rmse(dat, est, fcst)[3]), as.numeric(rmse(dat.20, est, fcst)[3]), as.numeric(rmse(dat.10, est, fcst)[3]))
rsq <- c(as.numeric(rsq_trad(dat, est, fcst)[3]), as.numeric(rsq_trad(dat.20, est, fcst)[3]), as.numeric(rsq_trad(dat.10, est, fcst)[3]))

summary <- data.frame(s.names, mape, rmse, rsq)

write.csv(summary, file=file.path(dir.figs, "Summary.csv"))

# Gggplot of Error ACross time
par(mfrow=c(1,3), oma=c(2,2,0,0), mar=c(2,2,2,1))
plot(fcst~est, data=dat, main=s.names[1], pch=21, bg='blue')
abline(a=0, b=1, col="red")

g <- dat %>% ggplot(aes(x=est/1e3, y=fcst/1e3, color=year)) +
       theme_dark() +
       geom_abline(aes(intercept=0, slope=1), col=rgb(1,0,0, alpha=0.5), lwd=1) +
       geom_point() +
       scale_color_viridis_c(option="D") +
       # scale_color_viridis_c(option="C") +
       xlab("Observed Run Size (thousands)") + ylab("Predicted Run Size (thousands)") +
       ggtitle("Kuskokwim River Chinook Salmon: Prior Year Forecast")
     
      
g
ggsave(file.path(dir.figs, "Prior Year 1to1.png"), plot=g, height=4, width=6, units='in')

       




