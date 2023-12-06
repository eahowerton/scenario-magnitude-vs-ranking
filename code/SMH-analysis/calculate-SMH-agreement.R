# load R environment
renv::init()

library(data.table)
library(dplyr)

source("code/irr_functions.R")
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

# perform agreement calculations
SMH_agreement <- p[, as.list(calc_scores_manual(.SD)),
                   by = .(round, target, target_end_date, horiz, location, quantile)]

write.csv(SMH_agreement, "output-data/SMH-analysis/SMH_agreement.csv")

