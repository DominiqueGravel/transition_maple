rm(list = ls())
setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/v2/scripts")
attach(read.table("par.txt"))

PP = 1
TP = seq(-1,5,0.01) 

# Compute the logit
logit_alphad 	= ad0 + adt1*TP + adt2*TP^2 + adt3*TP^3 + adp1*PP + adp2*PP^2 + adp3*PP^3
logit_alphac 	= ac0 + act1*TP + act2*TP^2 + act3*TP^3 + acp1*PP + acp2*PP^2 + acp3*PP^3
logit_betad 	= bd0 + bdt1*TP + bdt2*TP^2 + bdt3*TP^3 + bdp1*PP + bdp2*PP^2 + bdp3*PP^3
logit_betac 	= bc0 + bct1*TP + bct2*TP^2 + bct3*TP^3 + bcp1*PP + bcp2*PP^2 + bcp3*PP^3
logit_thetad	= td0 + tdt1*TP + tdt2*TP^2 + tdt3*TP^3 + tdp1*PP + tdp2*PP^2 + tdp3*PP^3
logit_thetac	= tc0 + tct1*TP + tct2*TP^2 + tct3*TP^3 + tcp1*PP + tcp2*PP^2 + tcp3*PP^3
logit_eps 		= e0 + et1*TP + et2*TP^2 + et3*TP^3 + ep1*PP + ep2*PP^2 + ep3*PP^3

# Back transform into probabilities
aC = exp(logit_alphac)/(1+exp(logit_alphac))
aD = exp(logit_alphad)/(1+exp(logit_alphad))
bC = exp(logit_betac)/(1+exp(logit_betac))
bD = exp(logit_betad)/(1+exp(logit_betad))
sC = exp(logit_thetac)/(1+exp(logit_thetac))
sD = exp(logit_thetad)/(1+exp(logit_thetad))
e = exp(logit_eps)/(1+exp(logit_eps))

# Compute the equilibrium abundance in absence 

# Compute the first eigenvalues for C and D as invaders
invC = 1/2*((aD - e)*bC - (aD - e)*bD + aC*e - 2*aD*e - aD*sC - aD*sD + sqrt(aC^2*e^2 + aD^2*sC^2 + aD^2*sD^2 + (aD^2 - 2*aD*e + e^2)*bC^2 + (aD^2 - 2*aD*e + e^2)*bD^2 + 2*(aC*aD*e - aC*e^2)*bC + 2*(aC*aD*e - aC*e^2 + (aD^2 - 2*aD*e + e^2)*bC)*bD + 2*(aC*aD*e + (aD^2 - aD*e)*bC + (aD^2 - aD*e)*bD)*sC - 2*(2*aC*aD^2*e - (2*e^2 + e)*aC*aD - aD^2*sC + (aD^2 - aD*e)*bC + (aD^2 - aD*e)*bD)*sD))/aD

invD = -1/2*((aC - e)*bC - (aC - e)*bD + 2*aC*e - aD*e + aC*sC + aC*sD - sqrt(aD^2*e^2 + aC^2*sC^2 + aC^2*sD^2 + 2*(aC*e - e^2)*aD*bC + (aC^2 - 2*aC*e + e^2)*bC^2 + (aC^2 - 2*aC*e + e^2)*bD^2 + 2*((aC*e - e^2)*aD + (aC^2 - 2*aC*e + e^2)*bC)*bD - 2*((2*aC^2*e - (2*e^2 + e)*aC)*aD + (aC^2 - aC*e)*bC + (aC^2 - aC*e)*bD)*sC + 2*(aC*aD*e + aC^2*sC + (aC^2 - aC*e)*bC + (aC^2 - aC*e)*bD)*sD))/aC

plot(TP, invD, type = "l", col = "darkred", ylim = range(invD,invC), xlab = "Temperature", ylab = "Per capita growth rate as invader")
lines(TP,invC, col = "darkblue")
abline(h = 0,lwd = 3)
legend("topright",lty = 1, col = c("darkred","darkblue"), legend = c("Deciduous", "Coniferous"))
dev.copy2pdf(file = "Invasion_rate")

# Plot the extinction probability
plot(TP,e,type = "l", col = "darkred",ylim = range(e,aD), xlab = "Temperature", ylab = "Probability")
lines(TP,aD,col = "darkblue")
legend("topright",lty = 1, col = c("darkred","darkblue"), legend = c("Extinction", "Colonization"))
#lines(TP, 1-e/aD)
title(main = "Deciduous trees")
#dev.copy2pdf(file ="TransitionsDeciduous.pdf")

plot(TP,e,type = "l", col = "darkred",ylim = range(e,aC), xlab = "Temperature", ylab = "Probability")
lines(TP,aC,col = "darkblue")
legend("topright",lty = 1, col = c("darkred","darkblue"), legend = c("Extinction", "Colonization"))
#lines(TP, 1-e/aC)
title(main = "Coniferous trees")
#dev.copy2pdf(file ="TransitionsConiferous.pdf")

