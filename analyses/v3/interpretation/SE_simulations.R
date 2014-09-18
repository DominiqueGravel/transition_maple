rm(list=ls())
setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/v3")

# Parameters and functions
par = read.table("par_v2.txt")

source("interpretation/get_transitions.R")
source("interpretation/spatial_model.R")
source("interpretation/plot_map.R")

# Run the model over a large landscape
Xlim = 100
Ylim = 50
N = Xlim*Ylim
X = c(1:Xlim)
Y = c(1:Ylim)
XY = expand.grid(X,Y)
TP = rev(6*(XY[,1]-1)/(Xlim-1)) 
PP = 1.2
initial_probs = matrix(0.25,nr = N, nc = 4)
pres = apply(initial_probs,1,draw)
large_run = spatial_model(pres=pres,TP=TP, PP = PP,XY=XY,par=par,nstep = 100)

# Plot the results
X = sort(unique(XY[,1]))
Y = sort(unique(XY[,2]))
pres_int = as.factor(large_run$map)
n = 1
Z = matrix(nr = length(X), nc = length(Y))
for(x in 1:length(X)) 
	for(y in 1:length(Y)) 	
		Z[x,y] = pres_int[XY[,1] == X[x] & XY[,2]==Y[y]]	
quartz(width = 5, height = 9)
layout(matrix(c(1,2),nr=2,nc=1,byrow=TRUE),heights = c(2,7))
par(mar=c(0,0,2,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
#title(2015,cex=2)
legend("center",legend = c("Boréal","Tempéré","Mixte","Régénération"),fill = c("darkcyan","orange","palegreen3","darkred"),bty = "n",cex = 1.5)
par(mar=c(5,5,0,5))
image(Y,X,t(Z),cex.axis = 1.5, cex.lab = 1.25, col = c("darkcyan","orange","palegreen3","darkred"))

dev.copy2pdf(file = "figures/largeplot_2015.pdf")

f = function(X,sp) sum(X%in%sp)/Ylim
rel = matrix(nr = Xlim, nc = 4)
rel[,1] = apply(Z,1,f,sp = 1)
rel[,2] = apply(Z,1,f,sp = 2)
rel[,3] = apply(Z,1,f,sp = 3)
rel[,4] = apply(Z,1,f,sp = 4)

# Compute the relative abundance of each state along the lattice
quartz(height = 5, width = 6)
par(mar = c(5,6,2,1))
Tgrad = rev(6*(c(1:Xlim)-1)/(Xlim-1))
plot(Tgrad,rel[,1]/(rel[,1]+rel[,2]+rel[,3]),type ="l",ylim=c(0,1),cex.axis = 1.25, cex.lab = 1.5, xlab = "Température moyenne annuelle", ylab = "Proportion du paysage",lwd = 2,col = "darkcyan" )
lines(Tgrad,rel[,2]/(rel[,1]+rel[,2]+rel[,3]),col="orange",lwd = 2)
lines(Tgrad,rel[,3]/(rel[,1]+rel[,2]+rel[,3]),col ="palegreen3",lwd = 2)
#lines(Tgrad,rel[,4],col ="darkred",lwd = 2)
legend("right",bty = "n", col = c("darkcyan","orange","palegreen3"),legend = c("Boréal", "tempéré", "Mixte"),lty = 1,lwd=3)
dev.copy2pdf(file = "figures/SEeq.pdf")


