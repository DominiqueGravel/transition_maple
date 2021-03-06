setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/data")
attach(read.table("par.txt"))
par  = read.table("par.txt")

setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/scripts")
source("get_transitions.R")

# Compute equilibrium for two initial conditions
Tgrad = seq(-1,5,0.05)
reslow = matrix(nr = length(Tgrad), nc = 4)
reshigh = matrix(nr = length(Tgrad), nc = 4)

res_M = matrix(nr = length(Tgrad), nc = 4)

for(j in 1:length(Tgrad)) {
	p0 = get_eq(p = c(1,0.0,0,0),ENV = Tgrad[j],par)
	p0[2] = 0.001
	p0[1] = p0[1]-0.001
#	reslow[j,] 	= get_eq(p = p0,ENV = Tgrad[j],par)
	
	p0 = get_eq(p = c(0,1,0,0),ENV = Tgrad[j],par)
	p0[1] = 0.001
	p0[2] = p0[1]-0.001
	reshigh[j,] = get_eq(p = p0,ENV = Tgrad[j],par)
	
#	res_M[j,] = get_eq(p = c(0,1,0,0),ENV = Tgrad[j],par)
}

# Plot the results
quartz(height = 6, width = 8)
par(mar=c(5,5,2,1))
plot(Tgrad,reslow[,1],type ="l",ylim=c(0,1),cex.axis = 1.25, cex.lab = 1.25, xlab = "Température moyenne annuelle", ylab = "Proportion du paysage",lwd = 2,)
lines(Tgrad,reslow[,2],col="darkred",lwd = 2)
lines(Tgrad,reslow[,3],col ="darkblue",lwd = 2)
lines(Tgrad,reslow[,4],col = "darkgreen",lwd = 2)

lines(Tgrad,reshigh[,1],col="black",lty = 3,lwd = 2)
lines(Tgrad,reshigh[,2],col="darkred",lty = 3,lwd = 2)
lines(Tgrad,reshigh[,3],col ="darkblue",lty = 3,lwd = 2)
lines(Tgrad,reshigh[,4],col = "darkgreen",lty = 3,lwd = 2)

legend("top",bty = "n", col = c("black","darkred","darkblue","darkgreen"),legend = c("C","D","M","T"),lty = 1,horiz=TRUE,lwd=3)

setwd("/Users/DGravel/Desktop/transition_maple/analyses/figures")
dev.copy2pdf(file = "SDMeq.pdf")

