# Clean the space
rm(list = ls())

# Prepare the data
setwd("/Users/DGravel/Documents/Projects_On_Going/Maple_migration/transition_maple/analyses/v3")
data = as.data.frame(read.table("data/data_categorical.txt"))
data$TP = data$av_annual_mean_tp
data$PP = data$av_annual_pp/1000

# Remove direct transitions from C to D and D to C
test = numeric(nrow(data))
test[data$t0 == "D" & data$t1 == "C"] = 1
test[data$t0 == "C" & data$t1 == "D"] = 1
data = subset(data, test!=1)

# Expectations from the SDM
pred = read.table("data/data_pred_states.txt")
pred = subset(pred, test!=1)
data$EC = pred[,1]
data$ED = pred[,2]
data$EM = pred[,3] 

# Source packages
source("fit_functions/anneal_ibou.R")
source("fit_functions/likeli.R")
source("fit_functions/analyze_function.R")
source("fit_functions/likdisplay.R")
source("fit_functions/support_limits.R")

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
source("pipeline/initial_values.R")
par = read.table("par.txt")

# Load the model
source("pipeline/transition_model.R")

# Maximum likelihood estimation
test = anneal(model = model, par = par, var = var, source_data = data, 
dep_var = "t1", pdf = PDF, par_initStep = par_initStep, par_testFct = testBounds, max_iter = 10000, initial_temp = 0.01,
temp_red = 0.95, ns = 10, nt = 50, min_change = 0.01, min_drops = 20, delta = 100, slimit = 2, note = "", progress = TRUE, display=FALSE, support = TRUE)

# Record the results
write.table(test$best_pars,"par.txt")




