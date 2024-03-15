# load R environment
renv::restore()

library(data.table)
library(dplyr)

source("code/irr_functions.R")
source("code/functions.R")
#source("code/SMH-analysis/load-SMH-projections.R")

#### CALCULATE AGREEMENT METRICS FOR SMH PROJECTIONS ---------------------------
# create df to calculate agreement for
p <- proj[quantile %in% c(0.5, 0.25, 0.75) &       # perform for 3 quantiles 
            substr(target, 1, 3) == "cum" &        # cumulative targets only
            substr(model_name,1,3) != "Ens"] %>%   # exclude ensembles
  .[, horiz := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
  .[, include := ifelse(horiz %in% c(4,8,12,16,20,26, max(horiz)), 1, 0), by = .(round)] %>% # only 7 horizons
  .[include == 1] %>%
  .[, include := NULL] %>%
  .[, r := rank(value), by = .(round, target, target_end_date, location, model_name, quantile)] %>%
  .[, scenario_letter := substr(scenario_id,1,1)]
write.csv(p, "output-data/SMH-analysis/SMH_ranks.csv")

# perform agreement calculations
SMH_agreement <- p[, as.list(calc_scores_manual(.SD)),
                   by = .(round, target, target_end_date, horiz, location, quantile)] %>%
  .[, icc_a_nonzero := ifelse(icc_a < 0, 0, icc_a)]

write.csv(SMH_agreement, "output-data/SMH-analysis/SMH_agreement.csv")

# perform tolerance method calculations
start.time <- Sys.time()
SMH_agreement_tolerance_method <- p %>% 
  .[, horiz := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
  .[, max_horiz := max(horiz), by =.(round)] %>%
  .[quantile == 0.5 & horiz == max_horiz & target == "cum hosp"] %>% 
  .[, as.list(calculate_agreement_SMH(.SD, est_thresh_abs = c(0.01,0.05,0.1), # percent of population 
                                       est_thresh_rel = c(0.1,0.25,0.5,0.75))), # percent of projected magnitude
         by = .(round, target, target_end_date, horiz, location, quantile, scenario_id)]
Sys.time() - start.time
write.csv(SMH_agreement_tolerance_method, "output-data/SMH-analysis/SMH_agreement_tolerance_method.csv")
