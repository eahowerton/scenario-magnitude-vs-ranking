#### FUNCTIONS FOR IRR CALCULATIONS -----------------------------------------------

# function to compute interclass correlation coefficient
# formulas mirror that of icc() function from irr package
# assuming two way model and single units, calculates both consistency and agreement icc
# notation follows Liljequist et al. (2019)
# ratings is nxk matrix of scores (rows = subjects, or here scenarios, cols = measurements, or here models)
icc_twoway_manual <- function(ratings){
  n = nrow(ratings)                                # number of subjects/scenarios
  k = ncol(ratings)                                # number of models
  SST = var(as.numeric(ratings))*(n*k-1)           # sum of squares total
  MSBS = var(apply(ratings, 1, mean)) * k          # mean squares between subjects/scenarios
  MSBM = var(apply(ratings, 2, mean)) * n          # mean squares between models
  MSE <- (SST - MSBS*(n - 1) - MSBM*(k-1))/        # mean square error 
    ((n-1) * (k-1))
  # equivalent calculations following Liljequist formulas exactly
  # SST = sum((ratings-mean(ratings))^2)   
  # MSBS = k*sum((apply(ratings, 1, mean) - mean(ratings))^2)/(n-1)
  # MSBM = n*sum((apply(ratings, 2, mean)-mean(ratings))^2)/(k-1)
  # MSE = (SStotal -                                              ## SST
  #        k*sum((apply(ratings, 1, mean) - mean(ratings))^2) - ## SSBS
  #        n*sum((apply(ratings, 2, mean)-mean(ratings))^2))/   ## SSBM
  #        ((n-1)*(k-1))
  #
  ## now calculate ICC 
  # agreement
  icc_a = (MSBS - MSE)/(MSBS + (k - 1)*MSE + (k/n)*(MSBM - MSE))
  # consistency
  icc_c = (MSBS - MSE)/(MSBS + (k - 1) * MSE)
  return(c("MSBS" = MSBS, "MSBM" = MSBM, "MSE" = MSE, 
           "icc_a" = icc_a, "icc_c" = icc_c))
}

# function to compute kendall's W
# formulas mirror that of kendall() from the irr package, including correction for ties
# same notation is used as icc function above
# ranks is nxk matrix of ranked values (rows = subjects, or here scenarios, cols = measurements, or here models)
kendall_w_manual <- function(ranks){
  n = nrow(ranks)                                # number of subjects/scenarios
  k = ncol(ranks)                                # number of models
  rank_totals = apply(ranks, 1, sum)             # sum of ranks across models for each scenario
  sse = sum((rank_totals - mean(rank_totals))^2)
  # in case of ties
  Tj <- 0
  for (i in 1:k) {
    rater <- table(ranks[,i])
    ties  <- rater[rater>1]
    l 	  <- as.numeric(ties)
    Tj	  <- Tj + sum(l^3-l)
  }
  kendall_w = (12*sse)/(k^2*(n^3-n)-k*Tj)
  return(c("kendall_w" = kendall_w))
}

#### APPLY CALCULATIONS TO DATA FROM DATA.FRAME --------------------------------
calc_kw_manual <- function(dat){
  mat_rank <- dat %>%
    dcast(scenario_id ~ model_name, value.var = "r") %>%
    .[, scenario_id := NULL] %>%
    as.matrix()
  if(any(is.na(mat_rank))){return(NULL)}
  return(kendall_w_manual(mat_rank))
}

calc_icc_manual <- function(dat){
  mat_mag <- dat %>% 
    dcast(scenario_id ~ model_name, value.var = "value") %>% 
    .[, scenario_id := NULL] %>%
    as.matrix()
  return(icc_twoway_manual(mat_mag)) 
}

calc_scores_manual <- function(dat){
  mat_rank <- dat %>%
    data.table::dcast(scenario_id ~ model_name, value.var = "r") %>%
    .[, scenario_id := NULL] %>%
    as.matrix()
  if(any(is.na(mat_rank))){return(NULL)}
  kw <- kendall_w_manual(mat_rank)
  mat_mag <- dat %>% 
    data.table::dcast(scenario_id ~ model_name, value.var = "value") %>% 
    .[, scenario_id := NULL] %>%
    as.matrix()
  icc_a <- icc_twoway_manual(mat_mag)
  return(c(kw, icc_a))
}

# function to calculate ICC
#   using "twoway" model because XX
#   using "agreement" because we are interested in absolute differences)
# calc_icc <- function(dat){
#   mat_mag <- dat %>% 
#     dcast(scenario_id ~ model_name, value.var = "value") %>% 
#     .[, scenario_id := NULL] %>%
#     as.matrix()
#   return(icc(mat_mag, model = "twoway", type = "agreement", unit = "single")$value)
# }
# 
# 
# calc_icc_c <- function(dat){
#   mat_mag <- dat %>% 
#     dcast(scenario_id ~ model_name, value.var = "value") %>% 
#     .[, scenario_id := NULL] %>%
#     as.matrix()
#   return(icc(mat_mag, model = "twoway", type = "consistency", unit = "single")$value)
# }
# 
# # function to calculate kendall's W
# calc_kw <- function(dat){
#   mat_rank <- dat %>% 
#     dcast(scenario_id ~ model_name, value.var = "value") %>% 
#     .[, scenario_id := NULL] %>%
#     as.matrix()
#   if(any(is.na(mat_rank))){return(NULL)}
#   return(kendall(mat_rank, correct = TRUE)$value)
# }

