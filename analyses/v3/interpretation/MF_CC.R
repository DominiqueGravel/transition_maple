rm(list = ls())

setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/v3")
TP0 = 3
TPcc = TP0 + 3*c(0:10)/10
TPcc = c(TPcc, numeric(10)+max(TPcc))
PP = 1.2

###########################################
# Run climate warming with the SDM model
# Load data
data = as.data.frame(read.table("data/data_categorical.txt"))
data$TP = data$av_annual_mean_tp
data$TP2 = data$TP^2

# Climate change simulation with the SDM
library(nnet)
SDM = multinom(t1 ~ TP + TP2, data)
res_SDM = matrix(nr = length(TPcc), nc = 4)
for(i in 1:length(TPcc)) res_SDM[i,] = predict(SDM,new=data.frame(TP=TPcc[i],TP2=TPcc[i]^2),"probs")

###########################################
# Run climate warming with the transition model
par = read.table("par_v2.txt")

source("interpretation/get_transitions.R")

# Pre - CC equilibrium:
p = c(0.5,0.5,0,0)
for(i in 1:500) p = p %*% get_matrix(EC = p[1],ED = p[2],EM = p[3],TP=3, PP = PP, par)

res_CC = matrix(nr = length(TPcc), nc = 4)
res_CC[1,] = p

res_CCeq = matrix(nr = length(TPcc), nc = 4)
res_CCeq[1,] = p

for(i in 2:length(TPcc)) {

	res_CCeq[i,] = get_eq(p = c(0.5,0.5,0,0),TP = TPcc[i],PP,par)

	mat = get_matrix(EC = p[1],ED = p[2],EM = p[3],TP=TPcc[i], PP = PP, par)
	p = p%*%mat
	res_CC[i,] = p
}

###########################################
# Plot the results
quartz(height = 5, width = 6)
par(mar=c(5,6,2,1))
plot(2000+c(0:20)/0.1,res_CC[,2]+res_CC[,3],type = "l",lwd = 3,xlab = "Année",ylab = "Proportion du paysage",cex.lab = 1.5, cex.axis = 1.25,ylim=c(0,1),col = "darkred")
lines(2000+c(0:20)/0.1,res_CCeq[,2]+res_CCeq[,3],col = "black",lwd = 3)

legend("topleft",bty = "n", col = c("black","darkred"),legend = c("Instantané", "Dynamique"),lwd = 3)

dev.copy2pdf(file = "figures/MF_CC.pdf")

