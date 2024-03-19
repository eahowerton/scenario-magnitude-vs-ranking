#### simple SIR model in closed population -------------------------------------
sirmod = function(t, y, parms){
  #Pull state variables from y vector
  S = y[1]
  I = y[2]
  R = y[3]
  C = y[4]
  #Pull parameter values from parms vector
  beta = parms["beta"]
  gamma = parms ["gamma"]
  N = parms ["N"]
  V=parms["V"]
  d = parms["d"]
  #Define equations
  dS = -(1-d)*beta*S*I/N - V*S
  dI = (1-d)*beta*S*I/N - gamma*I
  dR = gamma*I + V*S
  dC = (1-d)*beta*S*I/N
  res = c (dS, dI, dR, dC)
  #Return list of gradients
  list (res)
}

#### approximate probabilities of agreement ------------------------------------
approx_agreement <- function(it, 
                       dat,
                       n_mods, 
                       max_set_size = 10, 
                       est_thresh_abs = seq(10,50, by = 5), 
                       est_thresh_rel = c(0.02, 0.05, 0.1),
                       similar_flag = FALSE, 
                       sim_thresh = 0.1){
  d <- list()
  dat <- dat %>% 
    .[, id := paste0("b", beta, "g", gamma, "d", d_rate, "v",v_rate)]
  for(i in 1:(max_set_size-1)){
    # draw set of models
    k <- draw_models_to_compare(dat = dat, 
                                n_mods = n_mods, 
                                similar_flag = similar_flag, 
                                sim_thresh = sim_thresh, 
                                num_models_in_set = i)
    # calculate agreement
    a <- calculate_agreement(k, 
                             est_thresh_abs,
                             est_thresh_rel,
                             num_models_in_set = i)
    n_agree_rec <- a[["n_agree_rec"]]
    n_agree_mag <- a[["n_agree_mag"]]
    # calculate Kendall's W
    k_mat_rank <-  k %>% 
      .[, .(id, n, d, v)] %>% 
      data.table::melt(c("id")) %>%
      .[, r := rank(value), by = .(id)] %>% 
      data.table::dcast(variable ~ id, value.var = "r") %>% 
      .[, variable := NULL] %>%
      as.matrix()
    kw <- kendall_w_manual(k_mat_rank)
    # calculate Kendall's W without no intervention case
    k_mat_rank2 <-  k %>% 
      .[, .(id, d, v)] %>% 
      data.table::melt(c("id")) %>%
      .[, r := rank(value), by = .(id)] %>% 
      data.table::dcast(variable ~ id, value.var = "r") %>% 
      .[, variable := NULL] %>%
      as.matrix()
    kw2 <- kendall_w_manual(k_mat_rank2)
    # calculate ICC
    k_mat_val <-  k %>% 
      .[, .(id, n, d, v)] %>%
      data.table::melt(c("id")) %>%
      .[, r := rank(value), by = .(id)] %>% 
      data.table::dcast(variable ~ id, value.var = "value") %>% 
      .[, variable := NULL] %>%
      as.matrix()
    icc <- icc_twoway_manual(k_mat_val)
    # save results
    d[[i]] <- data.frame(it = it,
                         id = paste(sort(k$samp), collapse = "-"),
                         s = i+1,
                         variable = c("rec", paste0("mag_abs_", est_thresh_abs), paste0("mag_rel_", est_thresh_rel), paste0("window_rel_", est_thresh_rel),"kw", "kw2", "icc"), 
                         value = c(max(n_agree_rec$n), unlist(a[["thrsh_abs"]]), unlist(a[["thrsh_rel"]]), unlist(a[["window_rel"]]), kw, kw2, icc["icc_a"]))
  }
  return(rbindlist(d))
}

draw_models_to_compare <- function(dat, 
                                   n_mods, 
                                   similar_flag, 
                                   sim_thresh, 
                                   num_models_in_set){
  # find set of models to compare
  if(similar_flag){
    # find all models in a "similar" neighborhood
    k <- draw_similar_models(dat, n_mods, sim_thresh, num_models_in_set)
  }
  else{
    k <- draw_any_model(dat, n_mods, num_models_in_set)
  }
  return(k)
}

draw_any_model <- function(dat, n_mods, num_models_in_set){
  # randomly select num_models_in_set+1 models to compare
  samp <- sample(1:n_mods, num_models_in_set+1)
  k <- dat[samp,]
  k$samp = samp
  return(k)
}

draw_similar_models <- function(dat, n_mods, sim_thresh, num_models_in_set){
  # draw one model, use this model to define the "similar" neighborhood
  k_draw <- sample(1:n_mods, 1)
  beta_range <- c(-sim_thresh, sim_thresh) + unlist(dat[k_draw, "beta"])
  gamma_range <- c(-sim_thresh, sim_thresh) + unlist(dat[k_draw, "gamma"])
  d_range <- c(-sim_thresh, sim_thresh) + unlist(dat[k_draw, "d_rate"])
  v_range <- c(-sim_thresh, sim_thresh) + unlist(dat[k_draw, "v_rate"])
  k <- dat[beta >= beta_range[1] & beta <= beta_range[2] &
             gamma >= gamma_range[1] & gamma <= gamma_range[2] & 
             d_rate >= d_range[1] & d_rate <= d_range[2] & 
             v_rate >= v_range[1] & v_rate <= v_range[2] & 
             id != unlist(dat[k_draw, "id"])]
  if(nrow(k) < num_models_in_set){next}
  # choose i other models from neighborhood to complete comparison set
  samp <- c(k_draw, sample(1:nrow(k), num_models_in_set, replace = FALSE))
  k <- rbindlist(list(
    dat[k_draw,],
    k[samp[-1],]
  ))
  k$samp = samp
  return(k)
}

calculate_agreement <- function(k, 
                                est_thresh_abs, 
                                est_thresh_rel,
                                num_models_in_set){
  # compare recommendations
  n_agree_rec <- k %>% 
    .[, .(n = .N), by = .(rec)]
  # compare estimates
  thrsh_abs <- thrsh_rel <- window_rel <- list()
  for(j in 1:length(est_thresh_abs)){
    t <- est_thresh_abs[j]
    n_agree_mag_df <- k %>% 
      .[, thresh_u := n + t] %>% 
      .[, thresh_val := n] %>%
      .[, thresh_l := n - t]  %>%
    n_agree_mag_u <- sapply(1:(num_models_in_set+1), 
                            function(i){return(with(n_agree_mag_df, 
                                                    length(which(n <= thresh_u[i] & 
                                                                   n >= thresh_val[i]))))})
    n_agree_mag_l <- sapply(1:(num_models_in_set+1),
                            function(i){return(with(n_agree_mag_df, 
                                                    length(which(n <= thresh_val[i] & 
                                                                   n >= thresh_l[i]))))})
    thrsh_abs[[j]] <- max(c(n_agree_mag_u,n_agree_mag_l))
  }
  for(j in 1:length(est_thresh_rel)){
    t <- est_thresh_rel[j]
    n_agree_mag_df_rel <- k %>% 
      .[, thresh_u := n + t*n] %>% 
      .[, thresh_val := n] %>% 
      .[, thresh_l := n - t*n]
    n_agree_mag_rel_u <- sapply(1:(num_models_in_set), 
                                function(i){return(with(n_agree_mag_df_rel, 
                                                        length(which(n <= thresh_u[i] & 
                                                                       n >= thresh_val[i]))))})
    n_agree_mag_rel_l <- sapply(1:(num_models_in_set), 
                                function(i){return(with(n_agree_mag_df_rel, 
                                                        length(which(n <= thresh_val[i] & 
                                                                       n >= thresh_l[i]))))})
    thrsh_rel[[j]] <- max(c(n_agree_mag_rel_u, n_agree_mag_rel_l))
    window_rel[[j]] <- min(c(n_agree_mag_df_rel$thresh_u - n_agree_mag_df_rel$thresh_val,
                             n_agree_mag_df_rel$thresh_val - n_agree_mag_df_rel$thresh_l)[which(c(n_agree_mag_rel_u,n_agree_mag_rel_l) == thrsh_rel[[j]])])
  }
  return(list(n_agree_rec = n_agree_rec, n_agree_mag = c(n_agree_mag_u, n_agree_mag_l), 
              n_agree_mag_rel = c(n_agree_mag_rel_u, n_agree_mag_rel_l), thrsh_abs = thrsh_abs, thrsh_rel = thrsh_rel, 
              window_rel = window_rel))
}

