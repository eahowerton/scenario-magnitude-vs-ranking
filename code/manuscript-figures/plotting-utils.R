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
              "Intervention scenario\nranking agrees")
names(est_labs) <- c("mag_10", "mag_25", "mag_50", "rec")


round_labs <- c("Round 1 (vaccination, NPIs)\nbegin Jan. 9, 2021, 26 weeks", 
                "Round 2 (vaccination, NPIs)\nbegin Jan. 30, 2021, 26 weeks", 
                "Round 3 (vaccination, NPIs)\nbegin Mar. 13, 2021, 26 weeks", 
                "Round 4 (vaccination, NPIs)\nbegin Apr. 3, 2021, 26 weeks",
                "Round 5 (vaccination, NPIs)\nbegin May 8, 2021, 26 weeks", 
                "Round 6 (vaccination, variant)\nbegin Jun. 5, 2021, 26 weeks", 
                "Round 7 (vaccination, variant)\nbegin Jul. 10, 2021, 26 weeks", 
                "Round 9 (vaccination, variant)\nbegin Sep. 18, 2021, 26 weeks", 
                "Round 11 (variant)\nbegin Dec. 25, 2021, 12 weeks",
                "Round 12 (variant)\nbegin Jan 15, 2022, 12 weeks", 
                "Round 13 (vaccination, variant)\nbegin Mar. 13, 2022, 52 weeks", 
                "Round 14 (vaccination, variant)\nbegin Jun. 5, 2022, 50 weeks", 
                "Round 15 (vaccination, variant)\nbegin Jul. 31, 2022, 40 weeks", 
                "Round 16 (vaccination, variant)\nbegin Oct. 30, 2022, 26 weeks")
names(round_labs) <- c(1:7, 9, 11:16)

round_labs_simp <- paste("Round",c(1:7, 9, 11:16))
names(round_labs_simp) <- c(1:7, 9, 11:16)

horiz_labs <- paste(c(4,8,12,16,20,26,40,50,52), "wks\nahead")
names(horiz_labs) <- c(4,8,12,16,20,26,40,50,52)

#### OTHER PLOT SETTINGS -------------------------------------------------------
bs <- 7

smh_rel_levels <- c(0.25,0.5,0.75)
smh_abs_levels <- c(200,500,1000)
sim_rel_levels <- c(0.02,0.05,0.1,0.15)
sim_abs_levels <- c(20,50,100)
