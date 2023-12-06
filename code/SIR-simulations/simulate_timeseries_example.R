### GENERATE SIR MODEL TIMESERIES FOR TWO MODEL PARAMETER SETS
### UNDER VACCINATION, NPIs AND NO INTERVENTION
renv::restore()

library(deSolve)
library(dplyr)
library(data.table)

source("code/SIR-simulations/functions.R")
source("code/SIR-simulations/setup_model.R")

multi_panel = list()
for(i in 1:nrow(pick_range)){
  print(i)
  pick_v = pick_range[i, "v"]
  pick_d = pick_range[i, "d"]
  multi_panel[[i]] = expand.grid(N=1000,
                                 beta =seq(.75,1.25,by=0.05),
                                 gamma = seq(.1, 0.5, by=0.05),
                                 V=c(0,pick_v), # vaccination level
                                 d=c(0,pick_d)) # distancing intensity
  multi_panel[[i]] = multi_panel[[i]][(multi_panel[[i]]$V + multi_panel[[i]]$d) %in% c(0, pick_v, pick_d),]
  multi_panel[[i]]$cases=NA
  multi_panel[[i]]$peak_cases = NA
  for(row in 1:nrow(multi_panel[[i]])){
    outcomes= ode(y=start, times=times_short, func = sirmod, parms = multi_panel[[i]][row,])
    outcomes=as.data.frame(outcomes)
    multi_panel[[i]][row,"cases"]=outcomes[nrow(outcomes),"C"]
    multi_panel[[i]][row,"peak_cases"]=max(outcomes[,"I"])
    if(multi_panel[[i]][row,"cases"]>1000){browser()}
  }
  multi_panel[[i]]$intervention=ifelse(multi_panel[[i]]$V>0,"v", ifelse(multi_panel[[i]]$d>0,"d","n"))
  multi_panel[[i]]$v_rate = pick_v
  multi_panel[[i]]$d_rate = pick_d
}


test_runs = expand.grid(N=1000,
                              beta =seq(.75,1.25,by=0.1),
                              gamma = seq(.1, 0.5, by=0.1),
                              V=c(0,pick_v), # vaccination level
                              d=c(0,pick_d)) # distancing intensity)
test_runs$id = 1:nrow(test_runs)

dynam <- list()
for(row in 1:nrow(test_runs)){
  dynam[[row]] = ode(y=start, times=times, func = sirmod, parms = test_runs[row,])
  dynam[[row]] = as.data.frame(dynam[[row]])
  print(row)
}
dynam <- rbindlist(dynam, idcol = "id") %>%
  .[test_runs, on = .(id)]

#### PLOT VARIOUS TRAJECTORIES -------------------------------------------------
dynam %>% 
  .[, ]
ggplot(aes(x = time, y = I, ))
