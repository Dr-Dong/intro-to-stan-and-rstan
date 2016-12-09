compiler::enableJIT(0) ## JIT compilation struggles to keep up with shinystan
TUMOR_DATA <- dget("http://www.stat.washington.edu/people/pdhoff/Book/Data/data/XY.tumor")

png("tumor_data.png", 720, 720)

plot(seq(0.05, 1, by=0.05), colMeans(TUMOR_DATA$Y), col="black", lwd=3,
     ylim=range(TUMOR_DATA$Y), main="Tumor Data",
     type="l", xlab="Measurement Site", ylab="Number of Tumors")

for(i in 1:21){
    lines(seq(0.05, 1, by=0.05), TUMOR_DATA$Y[i, ], col="grey60")
}

dev.off()


library(reshape2); library(rstanarm);
options(mc.cores=parallel::detectCores())
DATA <- cbind(expand.grid(mouse_id=1:21, location=seq(0.05, 1, by=0.05)),
              count=melt(TUMOR_DATA$Y)$value)
M <- stan_glmer(count ~ poly(location, 4) + (1|mouse_id),
                family=poisson, data=DATA)

png("tumor_data_fit.png", 720, 720)

X <- seq(0.05, 1, by=0.05)
PP <- posterior_predict(M, re.form=~0, newdata=data.frame(location=X))
PI <- apply(PP, 2, quantile, c(0.05, 0.50, 0.95))

plot(X, colMeans(TUMOR_DATA$Y), col="black", lwd=3,
     ylim=range(TUMOR_DATA$Y), main="Tumor Data",
     type="l", xlab="Measurement Site", ylab="Number of Tumors")

for(i in 1:21){
    lines(X, TUMOR_DATA$Y[i, ], col="grey40")
}

lines(X, PI[1,], lwd=3, col="green4")
lines(X, PI[2,], lwd=3, col="red4")
lines(X, PI[3,], lwd=3, col="green4")

legend("topleft", lwd=3, col=c("black", "red4", "green4"),
       legend=c("Sample Mean", "Posterior Mean", "Posterior 90% Interval"))

dev.off()

launch_shinystan(M)
