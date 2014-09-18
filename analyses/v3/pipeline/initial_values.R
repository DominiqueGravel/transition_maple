# Take previous evaluation of maximum likelihood estimates
par = read.table("../v2/scripts/second_order/par_v2.txt")

# Compute the initial step
par_initStep =lapply(par, function(x){x+0.01})
