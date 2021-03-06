spatial_model = function(pres,ENV,XY,par,nstep) {

	# Prepare the object to store the time series
	series = matrix(nr = nstep, nc = 4)

	# Compute the connectivity matrix
	distMat = as.matrix(dist(XY,method = "euclidean", upper = T, diag = T))
	ConMat = matrix(0, nr=N,nc=N)
	ConMat[distMat<1.5] = 1
	diag(ConMat) = 0
	N = nrow(ConMat)

	# MAIN LOOP OVER TIME
	for(step in 1:nstep) {

		# Transform states into binary presence-absence
		presC = numeric(N)
		presC[pres == "C"] = 1
		presD = numeric(N)
		presD[pres == "D"] = 1
		presM = numeric(N)
		presM[pres == "M"] = 1		
	
		# Compute occupancy in the neighbourhood
		ConC = (ConMat%*%presC)/8
		ConD = (ConMat%*%presD)/8
		ConM = (ConMat%*%presM)/8

		# Compute the probability of being in each state at t1
		x = cbind(pres,ConC,ConD,ConM,ENV)
		transitions = t(apply(x,1,get_transitions_landscape))
		
		# Draw the identify of the state at t1
		pres = apply(transitions,1,draw)
	
		# Save regional abundnace of each state
		series[step,] = c(sum(pres%in%"C"),sum(pres%in%"D"),sum(pres%in%"M"),sum(pres%in%"T"))/N

		cat(step,'\n')
		}
	return(list(map = pres,series=series))
}	

	
################################	
draw = function(p) {
	states = c("C","D","M","T")
	draw = rmultinom(n=1,size=1,prob=p)
	states[which(draw==1)]
}


