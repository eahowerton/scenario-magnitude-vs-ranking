### GENERATE SIR MODEL OUTCOMES FOR A RANGE OF MODEL PARAMETER SETS
### UNDER VACCINATION, NPIs AND NO INTERVENTION
renv::restore()

library(deSolve)
library(dplyr)
library(data.table)

source("code/SIR-simulations/functions.R")
source("code/SIR-simulations/setup_model.R")

# create data.frame with all combinations of model parameters and interventions
one_panel <- expand.grid(N = 1000,
                         beta = seq(.75,1.25,by=0.01),
                         gamma = seq(.1, 0.5, by=0.01),
                         V = c(0,pick_v), # vaccination level
                         d = c(0,pick_d)) # distancing intensity
one_panel = one_panel[(one_panel$V + one_panel$d) %in% c(0, pick_v, pick_d),]
one_panel$cases=NA
one_panel$peak_cases = NA

for(row in 1:nrow(one_panel)){
  outcomes= ode(y=start, times=times_short, func = sirmod, parms = one_panel[row,])
  outcomes=as.data.frame(outcomes)
  one_panel[row,"cases"]=outcomes[nrow(outcomes),"C"]
  one_panel[row,"peak_cases"]=max(outcomes[,"I"])
  if(one_panel[row,"cases"]>1000){browser()}
  print(row)
}
one_panel$intervention=ifelse(one_panel$V>0,"v", ifelse(one_panel$d>0,"d","n"))

# save output
write.csv(one_panel, "output-data/SIR-simulations/SIR_intervention_outcomes.csv")
