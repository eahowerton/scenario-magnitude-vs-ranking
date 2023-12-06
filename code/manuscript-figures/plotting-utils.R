#### PLOT LABELS ---------------------------------------------------------------
comp_labs <- c("infectious individuals", "cumulative infections")
names(comp_labs) <- c("I", "C")

labs_v <- paste0("vaccination rate: ", c("0.5%","1%", "1.5%", "2%"))
names(labs_v) <- seq(0.005, 0.02, 0.005)

labs_d <- paste0("NPI reduction: ", c("10%", "20%", "30%", "40%"))
names(labs_d) <- seq(0.1,0.4,0.1)

mod_labs <- paste(2:10, "models in set")
names(mod_labs) <- 2:10

mod_type_labs <- c("all models", 
                   "similar models within 0.1", 
                   "similar models within 0.05", 
                   "similar models within 0.15")
names(mod_type_labs) <- c("all", 
                          "similar", 
                          "similar_lwr", 
                          "similar_upr")

est_labs <- c("Estimates agree within\n20 infections",
              "Estimates agree within\n50 infections",
              "Estimates agree within\n100 infections",
              "Intervention\nrecommendations agree")
names(est_labs) <- c("mag_10", "mag_25", "mag_50", "rec")
