# load R environment
renv::init()

library(data.table)
library(dplyr)

source("code/irr_functions.R")
#source("code/SMH-analysis/load-SMH-projections.R")


p_max_horiz <- proj %>%
  .[, horiz := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
  .[, max_horiz := max(horiz), by =.(round)] %>%
  .[quantile ==0.5 &                         # only use medians
      target == "cum hosp" &                 # cumulative hosp only
      substr(model_name,1,3) != "Ens" &      # exclude ensembles
      horiz == max_horiz                     # maximum horizon only
  ] %>%   
  .[, r := rank(value), by = .(round, target, target_end_date, location, model_name, quantile)] %>%
  .[, scenario_letter := substr(scenario_id,1,1)]

### NULL MODEL: SAMPLE MODELS UNIFORMLY WITHIN SCENARIO BOUNDS -----------------
### (DEFINED BY RANGE OF MODEL PROJECTIONS)
n_reps = 1e3

null_mod <- p_max_horiz %>%
  .[, .(min = min(value),
        max = max(value),
        n_mods = .N),
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round)] %>%
  .[, .(sim_id = 1:n_reps), 
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round, min, max, n_mods)] %>%
  .[, .(value = runif(n_mods, min, max),
        model_name = LETTERS[1:n_mods]),
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round, min, max, n_mods, sim_id)]

# calculate agreement for each simulation
null_agreement <- null_mod %>%
  .[, r := rank(value), 
    by = .(round, target, target_end_date, 
           location, model_name, quantile, sim_id)] %>%
  .[, scenario_letter := substr(scenario_id,1,1)] %>%
  .[, as.list(calc_scores_manual(.SD)),
    by = .(round, target, target_end_date, location, quantile, sim_id)] %>%
  .[, icc_a_nonzero := ifelse(icc_a < 0, 0, icc_a)]

write.csv(null_agreement, "output-data/SMH-analysis/null_agreement.csv")


### ALT.NULL 1: SAMPLE MODELS UNIFORMLY WITHIN RANDOM RANGES -------------------
### uniform draws, randomly select center, bounds = +/- max range/4
alt_null1 <- p_max_horiz %>%
  .[, .(min = min(value),
        max = max(value),
        n_mods = .N),
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round)] %>%
  # find min and max across scenarios
  .[, ":=" (min_allscen = min(min),
            max_allscen = max(max)),
    by = .(target, target_end_date, location, quantile, round)] %>%
  .[, .(sim_id = 1:n_reps),
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round, min_allscen, max_allscen, n_mods)] %>%
  # randomly draw four values to "center" the draws for each scenario
  # set range of uniform distribution evenly across all four scenarios
  .[, ":=" (center = runif(1, min_allscen, max_allscen),
            range = (max_allscen - min_allscen)/4),
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round, sim_id)] %>%
  # now set min and max for uniform draw by center +/- range
  .[, ":=" (min = center - range,
            max = center + range)] %>%
  .[, .(value = runif(n_mods, min, max),
        model_name = LETTERS[1:n_mods]),
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round, min, max, n_mods, sim_id)]

### ALT.NULL 2: SAMPLE MODELS UNIFORMLY W/IN RANGES, RANDOM CENTER/FIXED BOUNDS ------------
### RANDOM CENTER AND FIXED BOUNDS
### uniform draws, randomly select center, bounds = scenario bounds
alt_null2 <- p_max_horiz %>%
  .[, .(min_scen = min(value),
        max_scen = max(value),
        n_mods = .N),
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round)] %>%
  # find min and max across scenarios
  .[, ":=" (min_allscen = min(min_scen),
            max_allscen = max(max_scen)),
    by = .(target, target_end_date, location, quantile, round)] %>%
  .[, .(sim_id = 1:n_reps),
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round, min_scen, max_scen, min_allscen, max_allscen, n_mods)] %>%
  # randomly draw four values to "center" the draws for each scenario
  # set range of uniform distribution evenly across all four scenarios
  .[, ":=" (mean = runif(1, min_allscen, max_allscen),
            range = (max_scen - min_scen)/2),
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round, sim_id)] %>%
  # now set min and max for uniform draw by mean +/- range
  .[, ":=" (min = mean - range,
            max = mean + range)] %>%
  .[, .(value = runif(n_mods, min, max),
        model_name = LETTERS[1:n_mods]),
    by = .(scenario_id, scenario_name, target, target_end_date, location, quantile, round, min, max, n_mods, sim_id)]

### ALT.NULL 3: RESAMPLE PROJECTIONS WITH REPLACEMENT --------------------------
### resample within scenarios with replacement
alt_null3 <- p_max_horiz %>% 
  .[, .(sim_id = 1:n_reps), 
    by = .(scenario_id, scenario_name, target, target_end_date, 
           location, quantile, round, model_name, value)] %>%
  .[, .(value = sample(value, .N, replace = TRUE), 
        model_name = model_name), 
    by = .(target, target_end_date, location, quantile, round, sim_id, scenario_id)]

#### ALT.NULL AGREEMENT --------------------------------------------------------
# calculate agreement for each simulation
alt_null_agreement <- rbindlist(list(
  alt_null1[, ":=" (null_mod = 1, scenario_name = NULL,
                    min = NULL, max = NULL, n_mods = NULL)],
  alt_null2[, ":=" (null_mod = 2, scenario_name = NULL,
                    min = NULL, max = NULL, n_mods = NULL)],
  alt_null3[, ":=" (null_mod = "3", scenario_name = NULL,
                    min = NULL, max = NULL, n_mods = NULL)]),
  use.names = TRUE) %>%
  .[, r := rank(value), 
    by = .(round, target, target_end_date, 
           location, model_name, quantile, null_mod, sim_id)] %>%
  .[, scenario_letter := substr(scenario_id,1,1)] %>%
  .[, as.list(calc_scores_manual(.SD)),
    by = .(round, target, target_end_date, location, quantile, null_mod, sim_id)] %>%
  .[, icc_a_nonzero := ifelse(icc_a < 0, 0, icc_a)]

ggsave(alt_null_agreement, "output-data/SMH-analysis/alt_null_agreement.csv")

