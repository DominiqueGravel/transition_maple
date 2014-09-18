model = function(t0,t1,ED,EC,EM,TP,PP,
ad0,adt1,adt2,adp1,adp2,
ac0,act1,act2,acp1,acp2,
bd0,bdt1,bdt2,bdp1,bdp2,
bc0,bct1,bct2,bcp1,bcp2,
td0,tdt1,tdt2,tdp1,tdp2,
tc0,tct1,tct2,tcp1,tcp2,
e0,et1,et2,ep1,ep2) 
{
	lik = numeric(length(t0))

	# Compute the logit
	logit_alphad 	= ad0 + adt1*TP + adt2*TP^2 + adp1*PP + adp2*PP^2
	logit_alphac 	= ac0 + act1*TP + act2*TP^2 + acp1*PP + acp2*PP^2
	logit_betad 	= bd0 + bdt1*TP + bdt2*TP^2 + bdp1*PP + bdp2*PP^2
	logit_betac 	= bc0 + bct1*TP + bct2*TP^2 + bcp1*PP + bcp2*PP^2
	logit_thetad	= td0 + tdt1*TP + tdt2*TP^2 + tdp1*PP + tdp2*PP^2
	logit_thetac	= tc0 + tct1*TP + tct2*TP^2 + tcp1*PP + tcp2*PP^2
	logit_eps 		= e0 + et1*TP + et2*TP^2 + ep1*PP + ep2*PP^2

	# Back transform into probabilities
	alphac = exp(logit_alphac)/(1+exp(logit_alphac))
	alphad = exp(logit_alphad)/(1+exp(logit_alphad))
	betac = exp(logit_betac)/(1+exp(logit_betac))*(EC+EM)
	betad = exp(logit_betad)/(1+exp(logit_betad))*(ED+EM)
	thetac = exp(logit_thetac)/(1+exp(logit_thetac))
	thetad = exp(logit_thetad)/(1+exp(logit_thetad))
	eps = exp(logit_eps)/(1+exp(logit_eps))
	phic = alphac*(EM + EC)*(1-alphad*(ED+EM))
	phid = alphad*(EM + ED)*(1-alphad*(EC+EM))
	phim = alphac*(EM + EC)*alphad*(EM + ED)
		
	# Compute the likelihood of observations
	lik[t0 == "C" & t1 == "M"] = betad[t0 == "C" & t1 == "M"] 
	lik[t0 == "C" & t1 == "T"] = eps[t0 == "C" & t1 == "T"] 	
	lik[t0 == "C" & t1 == "C"] = (1 - eps - betad)[t0 == "C" & t1 == "C"]

	lik[t0 == "D" & t1 == "D"] = (1 - eps - betac)[t0 == "D" & t1 == "D"] 	
	lik[t0 == "D" & t1 == "M"] = betac[t0 == "D" & t1 == "M"] 			
	lik[t0 == "D" & t1 == "T"] = eps[t0 == "D" & t1 == "T"] 		
	
	lik[t0 == "M" & t1 == "C"] = thetac[t0 == "M" & t1 == "C"]	
	lik[t0 == "M" & t1 == "D"] = thetad[t0 == "M" & t1 == "D"] 	
	lik[t0 == "M" & t1 == "M"] = (1 - eps - thetac - thetad)[t0 == "M" & t1 == "M"] 			
	lik[t0 == "M" & t1 == "T"] = eps[t0 == "M" & t1 == "T"] 
	
	lik[t0 == "T" & t1 == "C"] = phic[t0 == "T" & t1 == "C"] 	
	lik[t0 == "T" & t1 == "D"] = phid[t0 == "T" & t1 == "D"]	
	lik[t0 == "T" & t1 == "M"] = phim[t0 == "T" & t1 == "M"] 			
	lik[t0 == "T" & t1 == "T"] = (1 - phic - phid - phim)[t0 == "T" & t1 == "T"] 

	lik[lik==0] = NA

	return(lik)
}

PDF = function(t1,lik) log(lik)





