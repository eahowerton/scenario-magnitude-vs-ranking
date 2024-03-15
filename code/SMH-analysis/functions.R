calculate_agreement_SMH <- function(k, 
                                    est_thresh_abs, 
                                    est_thresh_rel){
  # compare recommendations
  n_agree_rec <- k %>% 
    .[, .(n = .N), by = .(r)]
  n_agree_rec_max <- max(n_agree_rec$n)
  # compare estimates
  thrsh_abs <- thrsh_rel <- window_rel <- list()
  num_models_in_set = nrow(k)
  for(j in 1:length(est_thresh_abs)){
    t <- est_thresh_abs[j]
    n_agree_mag <- copy(k) 
    n_agree_mag <- n_agree_mag %>% 
      .[, thresh_u := value + t*population/1E5] %>% 
      .[, thresh_l := value - t*population/1E5]
    n_agree_mag <- sapply(1:(num_models_in_set+1), 
                          function(i){return(with(n_agree_mag, 
                                                  length(which(value <= thresh_u[i] & 
                                                                 value >= thresh_l[i]))))})
    thrsh_abs[[j]] <- max(n_agree_mag)
  }
  for(j in 1:length(est_thresh_rel)){
    t <- est_thresh_rel[j]
    n_agree_mag_df_rel <- copy(k)
    n_agree_mag_df_rel <- n_agree_mag_df_rel %>% 
      .[, thresh_u := value + t*value] %>% 
      .[, thresh_l := value - t*value]
    n_agree_mag_rel <- sapply(1:(num_models_in_set+1), 
                              function(i){return(with(n_agree_mag_df_rel, 
                                                      length(which(value <= thresh_u[i] & 
                                                                     value >= thresh_l[i]))))})
    thrsh_rel[[j]] <- max(n_agree_mag_rel)
    window_rel[[j]] <- min((n_agree_mag_df_rel$thresh_u - n_agree_mag_df_rel$thresh_l)[which(n_agree_mag_rel == max(n_agree_mag_rel))])
  }
  ret <- c(num_models_in_set, n_agree_rec_max, thrsh_abs, thrsh_rel, window_rel)
  names(ret) = c("n_models","n_agree_rec", paste0("n_agree_mag_abs_",est_thresh_abs), paste0("n_agree_mag_rel_",est_thresh_rel), paste0("window_rel_",est_thresh_rel))
  return(ret)
}

