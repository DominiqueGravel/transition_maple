
ENV0 = 3.2
Ecc = ENV0 + 2*c(1:10)/10

# Run climate warming with the SDM model
# Load data
setwd("/Users/DGravel/Desktop/transition_maple/analyses/data")
data = as.data.frame(read.table("data_categorical.txt"))
data$E = data$av_annual_mean_tp
data$E2 = data$E^2

# Climate change simulation with the SDM
library(nnet)
SDM = multinom(t1 ~ E + E2, data)
res_SDM = matrix(nr = 10, nc = 4)
for(i in 1:10) res_SDM[i,] = predict(SDM,new=data.frame(E=Ecc[i],E2=Ecc[i]^2),"probs")
p0 = predict(SDM,new=data.frame(E=ENV0,E2=ENV0^2),"probs")
res_SDM = rbind(p0,res_SDM)

# Run the transition model without climate change
par = read.table("par.txt")

setwd("/Users/DGravel/Desktop/transition_maple/analyses/scripts")
source("get_transitions.R")

res_noCC = matrix(nr= 10, nc = 4)
p = p0
for(i in 1:10) {
	mat = get_matrix(EC = p[1],ED = p[2],EM = p[3],ENV=Ecc[1],par)
	p = p%*%mat
	res_noCC[i,] = p
}
res_noCC = rbind(p0,res_noCC)


# Run climate warming with the transition model
setwd("/Users/DGravel/Desktop/transition_maple/analyses/data")
par = read.table("par.txt")

setwd("/Users/DGravel/Desktop/transition_maple/analyses/scripts")
source("get_transitions.R")

res_CC = matrix(nr= 10, nc = 4)
p = p0
for(i in 1:10) {
	mat = get_matrix(EC = p[1],ED = p[2],EM = p[3],ENV=Ecc[i],par)
	p = p%*%mat
	res_CC[i,] = p
}
res_CC = rbind(p0,res_CC)

# Plot the results
quartz(height = 6, width = 8)
par(mar=c(5,5,2,1))
plot(2000+c(0:10)/0.1,res_CC[,2],type = "l",lwd = 3,xlab = "Année",ylab = "Proportion du paysage",cex.lab = 1.5, cex.axis = 1.25,ylim=c(0.15,0.35),col = "darkred")
lines(2000+c(0:10)/0.1,res_noCC[,2],lwd = 3)
lines(2000+c(0:10)/0.1,res_SDM[,2],col = "darkred",lwd = 3,lt=3)


legend("top",bty = "n", col = c("black","darkred","darkred"),legend = c("Transition - No CC","Transition - CC", "SDM - CC"),lty = c(1,1,3),lwd = 3)

setwd("/Users/DGravel/Desktop/transition_maple/analyses/figures")
dev.copy2pdf(file = "CC_MF.pdf")

