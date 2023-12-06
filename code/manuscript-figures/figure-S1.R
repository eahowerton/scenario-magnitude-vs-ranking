### GENERATE SIR MODEL TIMESERIES FOR RANGE OF PARAMETER SETS
### UNDER VACCINATION, NPIs AND NO INTERVENTION
renv::restore()

library(deSolve)
library(dplyr)
library(data.table)

source("code/SIR-simulations/functions.R")
source("code/SIR-simulations/setup_model.R")

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
  .[V*d == 0] %>%
  .[, intervention_id := ifelse(V == 0 & d == 0, "no intervention", 
                                ifelse(V > 0, "vaccination", "NPIs"))] %>%
ggplot(aes(x = time, y = I, linetype = intervention_id)) + 
  geom_line() + 
  facet_grid(cols = vars(beta), rows = vars(gamma), 
             labeller = label_both) + 
  labs(y = "infected individuals") + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid = element_blank(), 
        strip.background = element_blank())
ggsave("figures/figureS1.pdf", width = 8.5, height = 7)

