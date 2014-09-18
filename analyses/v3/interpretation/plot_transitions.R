setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/v3")
attach(read.table("par_v2.txt"))
TP = seq(-1,5,0.01)
PP = 1.2

# Compute the logit
logit_alphac 	= ac0 + act1*TP + act2*TP^2 + acp1*PP + acp2*PP^2
logit_alphad 	= ad0 + adt1*TP + adt2*TP^2 + adp1*PP + adp2*PP^2
logit_betac 	= bc0 + bct1*TP + bct2*TP^2 + bcp1*PP + bcp2*PP^2
logit_betad 	= bd0 + bdt1*TP + bdt2*TP^2 + bdp1*PP + bdp2*PP^2
logit_thetac	= tc0 + tct1*TP + tct2*TP^2 + tcp1*PP + tcp2*PP^2
logit_thetad	= td0 + tdt1*TP + tdt2*TP^2 + tdp1*PP + tdp2*PP^2
logit_eps 		= e0 + et1*TP + et2*TP^2 + ep1*PP + ep2*PP^2

# Back transform into probabilities
alphac = exp(logit_alphac)/(1+exp(logit_alphac))
alphad = exp(logit_alphad)/(1+exp(logit_alphad))
betac = exp(logit_betac)/(1+exp(logit_betac))
betad = exp(logit_betad)/(1+exp(logit_betad))
thetac = exp(logit_thetac)/(1+exp(logit_thetac))
thetad = exp(logit_thetad)/(1+exp(logit_thetad))
eps = exp(logit_eps)/(1+exp(logit_eps))

# Plot the results
quartz(height = 4, width = 10)
par(mar=c(5,6,2,1),mfcol = c(1,2))

plot(TP,alphad,type = "l",ylim=c(0,0.6),cex.axis = 1.25, cex.lab = 1.25, xlab = "Température moyenne annuelle", ylab = "Probabilité",lwd = 2,col = "darkgreen")
lines(TP,betad,col = "darkred",lwd = 2)
lines(TP,thetad,col = "darkblue",lwd = 2)
lines(TP,eps,lwd = 2)
legend("top",bty = "n", col = c("darkgreen","darkred","darkblue"),legend = c("T-->D","C-->M","M-->D"),lty=1,horiz=TRUE,lwd = 3)

plot(TP,alphac,type = "l",ylim=c(0,0.6),cex.axis = 1.25, cex.lab = 1.25, xlab = "Température moyenne annuelle", ylab = "Probabilité",lwd = 2,col = "darkgreen")
lines(TP,betac,col = "darkred",lwd = 2)
lines(TP,thetac,col = "darkblue",lwd = 2)
lines(TP,eps,lwd = 2)
legend("top",bty = "n", col = c("darkgreen","darkred","darkblue"),legend = c("T-->C","D-->M","M-->C"),lty=1,horiz=TRUE,lwd = 3)

"/Users/DGravel/Desktop/transition_maple/analyses/figures")
dev.copy2pdf(file = "Transitions.pdf")



