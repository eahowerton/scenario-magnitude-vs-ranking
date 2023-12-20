# load R environment
renv::restore()

library(data.table)
library(dplyr)
library(arrow)
library(SMHEvaluationUtils)

#### NOTE: TO LOAD THIS DATA,  THE FOLLOWING PUBLIC GITHUB REPOSITORY IS REQUIRED LOCALLY
#### https://github.com/midas-network/covid19-scenario-hub_evaluation
#### ENTER PATH TO THIS REPO HERE:

eval_repo_path <- "~/Documents/GitHub/covid19-scenario-hub_evaluation"

#### LOAD DATA -----------------------------------------------------------------
# input function to read data
source(file.path(paste(eval_repo_path), "code/read_data_files.R"))
# set path of data repo
data_repo_path <- file.path(paste(eval_repo_path), "data-raw/")


# scenario info
scenario_info <- read.csv(file.path(paste0(data_repo_path, 
                                           "data-scenarios/scenario_round_info.csv"))) %>%
  select(-date_round) %>%
  unique()
setDT(scenario_info)[,round:=sub("round","",round)]

# projection period by round 
proj_period <- read.csv(paste0(data_repo_path, 
                               "data-scenarios/all_dates_by_round.csv")) %>%
  setDT() %>%
  .[, ":=" (target_end_date = as.IDate(target_end_date, format = "%m/%d/%y"))] %>%
  .[proj_period_flag == 1] 

# laod projections
proj <- read_data_files(data_repo_path = data_repo_path, 
                        API = FALSE,
                        truth_data = FALSE, 
                        raw_file = TRUE)
list2round <- sort(as.character(1:length(proj)))

# exclusions
proj <- SMHEvaluationUtils::compile_SMH_projections(proj = proj, 
                                                    list2round = list2round, 
                                                    proj_period_key = proj_period, 
                                                    scenario_round_key = scenario_info, 
                                                    inc_only = FALSE,
                                                    rounds_to_include = c(1:7,9,11:16),
                                                    summarize_exclusions = file.path(paste0(data_repo_path,"../data-output/exclusions")))


#### add location information --------------------------------------------------
# location information
locations <- read.csv(file.path(paste0(data_repo_path, "data-locations/locations.csv")))
# remove territories
locations <- setDT(locations)[!(location %in% c("60", "66", "69", "72", "74", "78"))]
proj <- proj[locations, on = .(location)]

