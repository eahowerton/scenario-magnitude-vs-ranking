### CALCULATE AGREEMENT BETWEEN FOR RANDOM SETS OF MODELS
renv::restore()

library(deSolve)
library(dplyr)
library(data.table)

source("code/SIR-simulations/setup_model.R")
source("code/irr_functions.R")
source("code/SIR-simulations/functions.R")

one_panel <- read.csv("output-data/SIR-simulations/SIR_intervention_outcomes.csv")
setDT(one_panel)
one_panel_alt <- one_panel %>%
  select(beta, gamma, cases, peak_cases, intervention) %>%
  reshape2::melt(c("beta", "gamma", "intervention")) %>%
  reshape2::dcast(beta + gamma + variable ~ intervention, value.var = "value") %>%
  mutate(diff = (v - d),
         rel_diff = (v-d)/n, 
         rec = ifelse(diff < 0, "vaccination", "distancing"))
setDT(one_panel_alt)


#### execute random draws and calculate agreement ------------------------------
n_models = nrow(one_panel[intervention == "n"])
n_sims = 1e4
set.seed(47)

samp = lapply(1:n_sims, approx_agreement, 
              dat = one_panel_alt[variable == "cases"] %>%
                .[, ":=" (d_rate = pick_d, 
                          v_rate = pick_v)], 
              n_mods = n_models,
              max_set_size = 12, 
              est_thresh = c(10,25,50)
)

# repeat for "similar" models only
samp_sim = lapply(1:n_sims, approx_agreement, 
                  dat = one_panel_alt[variable == "cases"] %>%
                    .[, ":=" (d_rate = pick_d, 
                              v_rate = pick_v)],
                  n_mods = n_models,
                  max_set_size = 12, 
                  est_thresh = c(10,25,50),
                  similar_flag = TRUE)

# repeat for "similar" models with smaller tol
samp_sim_lwr = lapply(1:n_sims, approx_agreement, 
                      dat = one_panel_alt[variable == "cases"] %>%
                        .[, ":=" (d_rate = pick_d, 
                                  v_rate = pick_v)],
                      n_mods = n_models,
                      max_set_size = 12, 
                      est_thresh = c(10,25,50),
                      similar_flag = TRUE, 
                      sim_thresh = 0.05)

# repeat for "similar" models with larger tol
samp_sim_upr = lapply(1:n_sims, approx_agreement, 
                      dat = one_panel_alt[variable == "cases"] %>%
                        .[, ":=" (d_rate = pick_d, 
                                  v_rate = pick_v)],
                      n_mods = n_models,
                      max_set_size = 12, 
                      est_thresh = c(10,25,50),
                      similar_flag = TRUE, 
                      sim_thresh = 0.15)

# combine results
samp = rbindlist(samp) 
samp_sim = rbindlist(samp_sim)
samp_sim_lwr = rbindlist(samp_sim_lwr)
samp_sim_upr = rbindlist(samp_sim_upr)
samp <- rbindlist(list(
  samp[, mod_type := "all"], 
  samp_sim[, mod_type := "similar"], 
  samp_sim_lwr[, mod_type := "similar_lwr"],
  samp_sim_upr[, mod_type := "similar_upr"]
)) %>% 
  .[, v := "samp"]

write.csv(samp, "output-data/SIR-simulations/SIR_sample_model_sets.csv")

