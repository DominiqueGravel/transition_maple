setwd("~/Desktop/markov")
load("sdm.rdata")
dat <- read.csv("transitions.csv", row.names=NULL)

## filter the data; using functions because they are a bit complicated
filtered <- function(X) {
	# returns a vector which is TRUE for each row that is filtered
	a <- drainage_filter(dat$drainage1)
	a <- a & drainage_filter(dat$drainage2)
#	a <- a | disturbance_filter(dat$disturbance1)
#	a <- a | disturbance_filter(dat$disturbance2)
	return(a)
	
}
drainage_filter <- function(x, filter.na = FALSE) {
	# returns true if filtered
	# we are keeping na's, because many plots have missing drainage data
	x <- as.numeric(x)
	a <- x < 20 | x > 41

	a[is.na(a)] <- filter.na

	return(a)
}
disturbance_filter <- function(x) {
	a <- !is.na(x)
	return(a)
}

## apply the filter
filtered <- dat[ !filtered(dat), ]

## calculate instantaneous probability of presence using the glm
filtered$pr_presence <- predict(sdm, newdata=data.frame(annual_mean_temp=filtered$mean_temp, 
	annual_pp=filtered$precip), type='response')


## build the colonization model
coldat <- filtered[filtered$type=="Colonization" | filtered$type=="Absent",]

models <- list(
	glm(type=="Colonization" ~ mean_temp * pr_presence + I(mean_temp^2) + I(mean_temp^3), data=coldat, family=binomial),
	glm(type=="Colonization" ~ mean_temp + I(mean_temp^2) + I(mean_temp^3) + pr_presence, data=coldat, family=binomial),
	glm(type=="Colonization" ~ mean_temp + I(mean_temp^2) + pr_presence, data=coldat, family=binomial),
	glm(type=="Colonization" ~ mean_temp + I(mean_temp^2), data=coldat, family=binomial),
	glm(type=="Colonization" ~ mean_temp + pr_presence, data=coldat, family=binomial),
	glm(type=="Colonization" ~ pr_presence, data=coldat, family=binomial))
	
sapply(models, AIC)
sapply(models, AIC) - min(sapply(models,AIC))
colonization <- models[[2]]


## make some plots
pres_levs <- c(0,0.2,0.4,0.6)
x_temp <- seq(min(coldat$mean_temp), max(coldat$mean_temp), 0.1)
par(mfrow=c(2,2))
for(i in pres_levs) {
	pred <- data.frame(mean_temp = x_temp, pr_presence=rep(i, length(x_temp)))
	y <- predict(colonization, newdata=pred, type='response', se.fit=T)
	plot(pred$mean_temp, y$fit, type='l', xlab="Mean Annual Temp", ylab="Pr(Colonization)", ylim=c(0,0.4), bty='n')
	lines(pred$mean_temp, y$fit + 2*y$se.fit, lty=2)
	lines(pred$mean_temp, y$fit - 2*y$se.fit, lty=2)
	text(-4, 0.38, paste("pr = ", i, sep=''))
}

## all on one
dev.new()
par(mfrow=c(1,1))
pres_levs <- c(pres_levs,1)
plot(0,0,type='n', xlim=range(x_temp), ylim=c(0,0.4), bty='n', xlab="Mean Annual Temp", ylab="Pr(Colonization)")
cols <- c('#000000', '#330000', '#660000', '#990000','#CC0000')
for(j in 1:length(pres_levs)) {
	i<- pres_levs[j]
	y <- predict(colonization, newdata=pred, type='response', se.fit=T)
	lines(pred$mean_temp, y$fit, type='l', xlab="Mean Annual Temp", ylab="Pr(Colonization)", col=cols[j])
}


## build the extinction model
extdat <- filtered[filtered$type=="Extinction" | filtered$type=="Present",]

models <- list(
	glm(type=="Extinction" ~ mean_temp * pr_presence + I(mean_temp^2) + I(mean_temp^3), data=extdat, family=binomial),
	glm(type=="Extinction" ~ mean_temp + I(mean_temp^2) + I(mean_temp^3) + pr_presence, data=extdat, family=binomial),
	glm(type=="Extinction" ~ mean_temp + I(mean_temp^2) + pr_presence, data=extdat, family=binomial),
	glm(type=="Extinction" ~ mean_temp + I(mean_temp^2), data=extdat, family=binomial),
	glm(type=="Extinction" ~ mean_temp + pr_presence, data=extdat, family=binomial),
	glm(type=="Extinction" ~ pr_presence, data=extdat, family=binomial),
	glm(type=="Extinction" ~ 1, data=extdat, family=binomial))
	
sapply(models, AIC) - min(sapply(models, AIC))
extinction <- models[[3]]
## make some plots
pres_levs <- c(0,0.2,0.4,0.6)
x_temp <- seq(min(extdat$mean_temp), max(extdat$mean_temp), 0.1)
par(mfrow=c(2,2))
for(i in pres_levs) {
	pred <- data.frame(mean_temp = x_temp, pr_presence=rep(i, length(x_temp)))
	y <- predict(extinction, newdata=pred, type='response', se.fit=T)
	plot(pred$mean_temp, y$fit, type='l', xlab="Mean Annual Temp", ylab="Pr(Extinction)", ylim=c(0,0.4), bty='n')
	lines(pred$mean_temp, y$fit + 2*y$se.fit, lty=2)
	lines(pred$mean_temp, y$fit - 2*y$se.fit, lty=2)
	text(0, 0.38, paste("pr = ", i, sep=''))
}




## plot C and e Curves
pred <- data.frame(
	mean_temp = seq(min(filtered$mean_temp), max(filtered$mean_temp), 0.1),
	pr_presence = rep(1,length(x_temp)))

dev.new()
y_c <- predict(colonization, newdata = pred, type='response')
y_e <- predict(extinction, newdata = pred, type='response')
y_p <- predict(sdm, newdata=data.frame(annual_mean_temp=pred$mean_temp, annual_pp = rep(mean(filtered$precip), length(pred$mean_temp))), type='response')
plot(pred$mean_temp, y_c, type='l', log='y', ylim=c(exp(-20),0.4), lwd=1.5, col='blue', bty='n', xlab="Mean Annual Temperature", ylab="Pr")
lines(pred$mean_temp, y_e, lwd=1.5, col='red')
lines(pred$mean_temp, y_p, lwd=1, col='black')
legend(-4,0.4, legend=c("Colonization", "Extinction", "Presence"), col=c("blue", "red", "black"), lwd=1)

y_exp <- 1-(y_e/y_c)
y_exp[y_c < y_e] <- exp(-20)
lines(pred$mean_temp, y_exp, lwd=1, lty=2)

plot(pred$mean_temp, y_exp, lwd=1, lty=2, type='l')
lines(pred$mean_temp, y_p, lwd=1, col='black')
lines(pred$mean_temp, y_c, type='l', log='y', ylim=c(exp(-20),0.4), lwd=1.5, col='blue', bty='n', xlab="Mean Annual Temperature", ylab="Pr")

save(sdm, colonization, extinction, file="sdm_col_ext.rdata")