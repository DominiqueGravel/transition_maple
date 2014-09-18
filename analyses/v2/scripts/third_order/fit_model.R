# Prepare the data
setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/v2/data")
data = as.data.frame(read.table("data_categorical.txt"))
data$TP = data$av_annual_mean_tp
data$PP = data$av_annual_pp/1000

# Remove direct transitions from C to D and D to C
test = numeric(nrow(data))
test[data$t0 == "D" & data$t1 == "C"] = 1
test[data$t0 == "C" & data$t1 == "D"] = 1
data = subset(data, test!=1)

# Expectations from the SDM
pred = read.table("data_pred_states.txt")
pred = subset(pred, test!=1)
data$EC = pred[,1]
data$ED = pred[,2]
data$EM = pred[,3] 

# Source packages
setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/v2/scripts")
source("anneal.R")
source("likeli.R")
source("analyze_function.R")
source("likdisplay.R")
source("transition_model.R")
source("support_limits.R")

# Prepare variables
var = list()
var$t0 = "t0"
var$t1 = "t1"
var$TP = "TP"
var$PP = "PP"
var$lik = "predicted"
var$EC = "EC"
var$ED = "ED"
var$EM = "EM"

# Set initial values
source("initial_values.R")

# Maximum likelihood estimation
test = anneal(model = model, par = par, var = var, source_data = data, 
par_lo = par_lo, par_hi = par_hi, dep_var = "t1", pdf = PDF, 
max_iter = 25000, hessian = FALSE, initial_temp = 1)

# Record the results
write.table(test$best_pars,"par.txt")




