# optimisation
# prerun the model with glm to find some parameter values that are not jointly constrained
# use logits


model = function(t0,t1,ED,EC,EM,ENV,ac0,ac1,ac2,ad0,ad1,ad2,bc0,bc1,bc2,bd0,bd1,bd2,tc0,tc1,tc2,td0,td1,td2,e0c,e1c,e2c,e0d,e1d,e2d,e0m,e1m,e2m) 
{
	lik = numeric(length(t0))

	# Compute the logit
	logit_alphac 	= ac0 + ac1*ENV + ac2*ENV^2
	logit_alphad 	= ad0 + ad1*ENV + ad2*ENV^2
	logit_betac 	= bc0 + bc1*ENV + bc2*ENV^2
	logit_betad 	= bd0 + bd1*ENV + bd2*ENV^2
	logit_thetac	= tc0 + tc1*ENV + tc2*ENV^2
	logit_thetad	= td0 + td1*ENV + td2*ENV^2
	logit_epsd 	= e0d  + e1d*ENV  + e2d*ENV^2
	logit_epsc 	= e0c  + e1c*ENV  + e2c*ENV^2
	logit_epsm 	= e0m  + e1m*ENV  + e2m*ENV^2

	# Back transform into probabilities
	alphac = exp(logit_alphac)/(1+exp(logit_alphac))
	alphad = exp(logit_alphad)/(1+exp(logit_alphad))
	betac = exp(logit_betac)/(1+exp(logit_betac))*(EC+EM)
	betad = exp(logit_betad)/(1+exp(logit_betad))*(ED+EM)
	thetac = exp(logit_thetac)/(1+exp(logit_thetac))
	thetad = exp(logit_thetad)/(1+exp(logit_thetad))
	epsd = exp(logit_epsd)/(1+exp(logit_epsd))
	epsc = exp(logit_epsc)/(1+exp(logit_epsc))
	epsm = exp(logit_epsm)/(1+exp(logit_epsm))		
	
	phic = alphac*(EM + EC)*(1-alphad*(ED+EM))
	phid = alphad*(EM + ED)*(1-alphac*(EC+EM))
	phim = phic*phid

	# Compute the likelihood of observations
	lik[t0 == "C" & t1 == "M"] = betad[t0 == "C" & t1 == "M"] 
	lik[t0 == "C" & t1 == "T"] = epsc[t0 == "C" & t1 == "T"] 	
	lik[t0 == "C" & t1 == "C"] = (1 - epsc - betad)[t0 == "C" & t1 == "C"]

	lik[t0 == "D" & t1 == "D"] = (1 - epsd - betac)[t0 == "D" & t1 == "D"] 	
	lik[t0 == "D" & t1 == "M"] = betac[t0 == "D" & t1 == "M"] 			
	lik[t0 == "D" & t1 == "T"] = epsd[t0 == "D" & t1 == "T"] 		
	
	lik[t0 == "M" & t1 == "C"] = thetac[t0 == "M" & t1 == "C"]	
	lik[t0 == "M" & t1 == "D"] = thetad[t0 == "M" & t1 == "D"] 	
	lik[t0 == "M" & t1 == "M"] = (1 - epsm - thetac - thetad)[t0 == "M" & t1 == "M"] 			
	lik[t0 == "M" & t1 == "T"] = epsm[t0 == "M" & t1 == "T"] 
	
	lik[t0 == "T" & t1 == "C"] = phic[t0 == "T" & t1 == "C"] 	
	lik[t0 == "T" & t1 == "D"] = phid[t0 == "T" & t1 == "D"]	
	lik[t0 == "T" & t1 == "M"] = phim[t0 == "T" & t1 == "M"] 			
	lik[t0 == "T" & t1 == "T"] = (1 - phic - phid - phim)[t0 == "T" & t1 == "T"] 

	lik[lik==0] = NA

	return(lik)
}

PDF = function(t1,lik) log(lik)





