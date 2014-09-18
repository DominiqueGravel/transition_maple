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



