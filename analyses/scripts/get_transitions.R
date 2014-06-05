# optimisation
# prerun the model with glm to find some parameter values that are not jointly constrained
# use logits

######################################################
get_transitions = function(t0,EC,ED,EM,ENV,par) 
{
with(par, {
	p = numeric(4)

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
	
	if(t0 == "C") { 
		p[1] = (1 - epsc - betad)
		p[2] = 0
		p[3] = betad
		p[4] = epsc
	}
	
	else if(t0 == "D") { 
		p[1] = 0
		p[2] = (1 - epsd - betac)
		p[3] = betac	
		p[4] = epsd
	}	

	else if(t0 == "M") { 
		p[1] = thetac
		p[2] = thetad
		p[3] = 1 - thetac - thetad - epsm
		p[4] = epsm
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
get_transitions_landscape = function(x) get_transitions(t0=x[1],EC=as.numeric(x[2]),ED=as.numeric(x[3]),EM=as.numeric(x[4]),ENV=as.numeric(x[5]),par)

######################################################
get_matrix = function(EC,ED,EM,ENV,par) {
	mat = matrix(nr = 4, nc = 4)
	mat[1,] = get_transitions(t0="C",EC,ED,EM,ENV,par)
	mat[2,] = get_transitions(t0="D",EC,ED,EM,ENV,par)
	mat[3,] = get_transitions(t0="M",EC,ED,EM,ENV,par)
	mat[4,] = get_transitions(t0="T",EC,ED,EM,ENV,par)			
	return(mat)
}

######################################################
get_eq = function(p,ENV,par) {
	for(i in 1:1000) {
		mat = get_matrix(EC = p[1],ED = p[2],EM = p[3],ENV,par)
		p = p%*%mat
			}	
	return(p)
}





