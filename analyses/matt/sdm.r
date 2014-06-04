setwd("~/Desktop/markov")
dat <- read.csv("raw_data.csv", row.names=NULL)


## filter data
filtered <- dat[
	is.na(dat$disturbance) & 
	((dat$drainage >= 20 & dat$drainage <= 41) | is.na(dat$drainage)),]

# compute presence/absence
filtered$ers[is.na(filtered$ers)] <- 0
filtered$ers_presence <- (filtered$ers > 0)


##
sdm <- glm(ers_presence ~ annual_pp * annual_mean_temp + I(annual_pp^2) + I(annual_pp^3)
	+ I(annual_mean_temp^2) + I(annual_mean_temp^3), data=filtered, family=binomial)
	
	
x <- seq(min(filtered$annual_mean_temp), max(filtered$annual_mean_temp), 0.01)
plot(filtered$annual_mean_temp, as.numeric(filtered$ers_presence), pch=16)
lines(x, predict(sdm, newdata=data.frame(annual_mean_temp=x, annual_pp=rep(mean(filtered$annual_pp), length(x))), type='response'))

x <- seq(min(filtered$annual_pp), max(filtered$annual_pp), 0.01)
plot(filtered$annual_pp, as.numeric(filtered$ers_presence), pch=16)
lines(x, predict(sdm, newdata=data.frame(annual_pp = x, annual_mean_temp=rep(mean(filtered$annual_mean_temp), length(x))), type='response'))

save(sdm, file="sdm.rdata")