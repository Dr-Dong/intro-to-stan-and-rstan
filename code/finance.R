library(rvest)
library(quantmod)
library(xts)
library(xtsPlots) ## github.com/michaelweylandt/xtsPlots
library(rstan)
options(mc.cores=parallel::detectCores())
rstan_options(auto_write=TRUE)

if(!file.exist("SPY.RData")){
    SPY <- getSymbols("SPY", auto.assign=FALSE)
    saveRDS(SPY, "SPY.RData")
} else {
    SPY <- readRDS("SPY.RData")
}

R <- na.omit(ROC(Ad(SPY)));

png("fin.png", 720, 720)
plot(Ad(SPY), main="")
dev.off()

png("fin2.png", 720, 720)
plot(ROC(Ad(SPY)), main="")
dev.off()

png("fin3.png", 720, 720)
plot(volatility(SPY), main="")
dev.off()

SPY <- getSymbols("SPY", auto.assign=FALSE)
R <- na.omit(ROC(Ad(SPY)));

SM <- stan_model("sv.stan")
SAMPLES <- sampling(SM, data=list(y=as.vector(R), T=length(R)))

PP <- apply(exp(extract(SAMPLES, "h")[[1]]/2), 2, quantile, c(0.05, 0.50, 0.95))
PP <- sqrt(252) * PP ## Convert to more conventional annualized unit

png("fin4.png", 720, 720)
plot(cbind(Ad(SPY)[index(R)], R, t(PP)),
     col=c("black", "black", "green4", "red4", "green4"),
     screens=c(1, 2, 3,3,3),
     lwd=c(1, 1, 1, 2, 1), main="SPY & Inferred Volatility",
     layout.screens=c(1, 2, 3, 3))
dev.off()
