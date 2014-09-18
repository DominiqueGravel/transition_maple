library(nnet)
# Load data
setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/v3")
data = read.table("data/data_categorical.txt")
data$E = data$av_annual_mean_tp
data$P = data$av_annual_pp

# Run the model
library(randomForest)
set.seed(23)
SDM = randomForest(t1 ~ . , data = data[, c("t1", "E", "P")],ntree = 1000)

# Get predictions
newdat = expand.grid(E = seq(-1,6,0.01),P = 1100)

#pred_real = predict(SDM,new=data.frame(E=data$E, P=data$P),"probs")
#pred_gradient = predict(SDM,new=data.frame(newdat),"probs")

set.seed(23)
pred_real = predict(SDM,new=data.frame(E=data$E, P=data$P),"prob")
set.seed(23)
pred_gradient = predict(SDM,new=data.frame(newdat),"prob")

write.table(pred_real,"data_pred_states.txt")

# Plot the results
colo = c("darkcyan","orange","palegreen3","darkred")

plotfct <- function(env_var, leg)
{
Cmax = lapply(split(pred_gradient[,"C"], as.factor(env_var)), max)
Dmax = lapply(split(pred_gradient[,"D"], as.factor(env_var)), max)
Mmax= lapply(split(pred_gradient[,"T"], as.factor(env_var)), max)
Tmax = lapply(split(pred_gradient[,"M"], as.factor(env_var)), max)
pred_gradient_max = cbind(Cmax, Dmax, Mmax, Tmax)

quartz(height = 5, width = 6)
par(mar=c(5,6,2,1))
xscale = as.numeric(rownames(pred_gradient_max))
plot(xscale,pred_gradient_max[,1],type = "l",ylim=c(0,1.1),cex.axis = 1.25, cex.lab = 1.5, xlab = leg, ylab = "Proportion",lwd = 2, col=colo[1])
lines(xscale,pred_gradient_max[,2],col = colo[2],lwd = 2)
#lines(xscale,pred_gradient_max[,3],col = colo[3],lwd = 2)
#lines(xscale,pred_gradient_max[,4],col = colo[4],lwd = 2)
legend("topright",bty = "n", col = colo[c(1,2)],legend = c("Boréal","Tempéré"),lty=1, lwd = 2)
}

plotfct (newdat$E ,leg = "Température moyenne annuelle")
dev.copy2pdf(file = "figures/SDM_temp.pdf")



#########

# MULTINOMIAL 

library(nnet)

# Load data
setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/v3")
data = as.data.frame(read.table("data/data_categorical.txt"))
data$E = data$av_annual_mean_tp
data$E2 = data$E^2

# Run the model
SDM = multinom(t1 ~ E + E2, data)

# Get predictions
pred_real = predict(SDM,new=data.frame(E=data$E,E2=data$E2),"probs")
pred_gradient = predict(SDM,new=data.frame(E=seq(-2,6,0.01),E2=seq(-2,6,0.01)^2),"probs")
write.table(pred_real,"data/data_pred_states.txt")
write.table(cbind(seq(-2,6,0.01),pred_gradient),"data/data_pred_gradient.txt")

# Plot the results
quartz(height = 6, width = 8)
par(mar=c(5,5,2,1))
plot(seq(-2,6,0.01),pred_gradient[,1],type = "l",ylim=c(0,1),cex.axis = 1.25, col = "darkcyan", cex.lab = 1.25, xlab = "Température moyenne annuelle", ylab = "Proportion",lwd = 2)
lines(seq(-2,6,0.01),pred_gradient[,2],col = "orange",lwd = 2)
lines(seq(-2,6,0.01),pred_gradient[,3],col = "palegreen3",lwd = 2)
lines(seq(-2,6,0.01),pred_gradient[,4],col = "darkred",lwd = 2)
abline(v = 3.2,lty = 3)
legend("topright",bty = "n", col = c("darkcyan","orange","palegreen3","darkred"),legend = c("Boréal","Tempéré","Mixte","Régénération"),lty=1,lwd = 2)

dev.copy2pdf(file = "figures/SDM_multinom.pdf")






