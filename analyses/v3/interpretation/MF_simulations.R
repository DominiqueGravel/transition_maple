setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/v3")
attach(read.table("par.txt"))
par  = read.table("par.txt")

source("interpretation/get_transitions.R")

# Compute equilibrium for two initial conditions
Tgrad = seq(1,6,0.05)
PP = 1.2
res = matrix(nr = length(Tgrad), nc = 4)

for(j in 1:length(Tgrad)) {
	cat(j,'\n')
	res[j,] = get_eq(p = c(0.5,0.5,0,0),TP = Tgrad[j],PP,par)
}

# Plot the results
quartz(height = 5, width = 6)
par(mar=c(5,6,2,1))
plot(Tgrad,res[,1],type ="l",ylim=c(0,1.1),cex.axis = 1.25, cex.lab = 1.5, xlab = "Température moyenne annuelle", ylab = "Proportion du paysage",lwd = 2,col = "darkcyan" )
lines(Tgrad,res[,2],col="orange",lwd = 2)
lines(Tgrad,res[,3],col ="palegreen3",lwd = 2)
lines(Tgrad,res[,4],col ="darkred",lwd = 2)

legend("topleft",bty = "n", col = c("darkcyan","orange","palegreen3","darkred"),legend = c("Boréal","Tempéré","Mixte","Régénération"),lty = 1,lwd=3)

dev.copy2pdf(file = "figures/SDMeq.pdf")


