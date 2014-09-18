# optimisation
# prerun the model with glm to find some parameter values that are not jointly constrained
# use logits

######################################################
get_transitions = function(t0,EC,ED,EM,TP,PP,par) 
{
with(par, {

	p = numeric(4)

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
	phid = alphad*(EM + ED)*(1-alphac*(EC+EM))
	phim = alphac*(EM + EC)*alphad*(EM + ED)
		
	if(t0 == "C") { 
		p[1] = (1 - eps - betad)
		p[2] = 0
		p[3] = betad
		p[4] = eps
	}
	
	else if(t0 == "D") { 
		p[1] = 0
		p[2] = (1 - eps - betac)
		p[3] = betac	
		p[4] = eps
	}	

	else if(t0 == "M") { 
		p[1] = thetac
		p[2] = thetad
		p[3] = 1 - thetac - thetad - eps
		p[4] = eps
	}	

	else if(t0 == "T") { 
		p[1] = phic
		p[2] = phid
		p[3] = phim
		p[4] = 1-phic-phid-phim
	}			
	p
	})
}

######################################################
get_transitions_landscape = function(x) get_transitions(t0=x[1],EC=as.numeric(x[2]),ED=as.numeric(x[3]),EM=as.numeric(x[4]),TP=as.numeric(x[5]),PP=as.numeric(x[6]),par)

######################################################
get_matrix = function(EC,ED,EM,TP,PP,par) {
	mat = matrix(nr = 4, nc = 4)
	mat[1,] = get_transitions(t0="C",EC,ED,EM,TP,PP,par)
	mat[2,] = get_transitions(t0="D",EC,ED,EM,TP,PP,par)
	mat[3,] = get_transitions(t0="M",EC,ED,EM,TP,PP,par)
	mat[4,] = get_transitions(t0="T",EC,ED,EM,TP,PP,par)			
	return(mat)
}

######################################################
get_eq = function(p,TP,PP,par) {
	for(i in 1:1000) {
		mat = get_matrix(EC = p[1],ED = p[2],EM = p[3],TP,PP,par)
		p = p%*%mat
		}	
	return(p)
}





