renv::restore()

library(data.table)
library(dplyr)
library(ggplot2)
library(metR)
library(scales)
library(cowplot)
library(RColorBrewer)
library(reshape2)

source("code/manuscript-figures/plotting-utils.R")

#### FIGURE 5 ------------------------------------------------------------------
SMH_agreement_tolerance_method <- setDT(read.csv("output-data/SMH-analysis/SMH_agreement_tolerance_method.csv"))

SMH_agreement_tolerance_method %>% 
  data.table::melt(c("round", "target", "target_end_date", "horiz", 
                     "location", "quantile", "scenario_id", "n_models")) %>%
  .[, pct_agree := value/n_models] %>%
  .[, pct_agree_bin := case_when(pct_agree < 0.25 ~ 1, 
                                 pct_agree < 0.5 ~ 2, 
                                 pct_agree < 0.75 ~ 3, 
                                 pct_agree <= 1 ~ 4)] %>%
  .[, scenario_letter := substr(scenario_id, 1,1)] %>%
  .[variable %in% c("n_agree_rec", paste0("n_agree_mag_abs_", c(10,50,100)))] %>%
  .[, value := round(value,2)] %>%
  .[, .(n = .N), by = .(round, target, scenario_letter, pct_agree_bin, variable)] %>%
  .[, variable := factor(variable, levels = c(paste0("n_agree_mag_abs_",rev(c(10,50,100))), "n_agree_rec"))] %>%
  .[, y := as.numeric(variable)] %>%
  .[, ybar := as.numeric(y) + (n/sum(n)*0.95), by = .(round, target, variable, scenario_letter)] %>%
  .[scenario_letter == "B"] %>%
  ggplot() + 
  geom_hline(aes(yintercept = 4)) + 
  geom_hline(aes(yintercept = 3), color = 'lightgray') + 
  geom_hline(aes(yintercept = 2), color = 'lightgray') + 
  geom_hline(aes(yintercept = 1), color = 'lightgray') + 
  geom_rect(aes(xmin = pct_agree_bin-0.5, 
                xmax = pct_agree_bin + 0.5, 
                ymin = y, ymax = ybar), color = "black", fill = "gray", linewidth = 0.35) + 
  facet_wrap(vars(round), labeller = labeller(round = round_labs)) +
  labs(x = "percent of models agreeing", 
       y = "number of locations") + 
  scale_x_continuous(breaks = 1:4, 
                     labels = c("0-25%", "25-50%", "50-75%", "75-100%")) + 
  scale_y_continuous(breaks = 1:4+0.5,
                     expand = c(0,0),
                     labels = c(paste0("magnitude within\n\u00B1",rev(c(10,50,100))," hosp/100K"), "ranking"),
                     limits = c(1,5)) +
  theme_bw(base_size = bs) + 
  theme(axis.ticks.y = element_blank(),
        panel.grid = element_blank(), 
        strip.background = element_blank())

ggsave("figures/Figure5.pdf", width = 170, height = 170, units = "mm")



# full version for supp
# tst %>% 
#   data.table::melt(c("round", "target", "target_end_date", "horiz", 
#                      "location", "quantile", "scenario_id", "n_models")) %>%
#   .[, pct_agree := value/n_models] %>%
#   .[, pct_agree_bin := case_when(pct_agree < 0.25 ~ 1, 
#                                  pct_agree < 0.5 ~ 2, 
#                                  pct_agree < 0.75 ~ 3, 
#                                  pct_agree < 1 ~ 4)] %>%
#   .[, scenario_letter := substr(scenario_id, 1,1)] %>%
#   .[variable != "n_agree_mag_NA"] %>%
#   .[, value := round(value,2)] %>%
#   .[, .(n = .N), by = .(round, target, scenario_letter, pct_agree_bin, variable)] %>%
#   .[, variable := factor(variable, levels = c(paste0("n_agree_mag_rel",rev(c(0.1,0.25,0.5,0.75))), "n_agree_rec"))] %>%
#   .[, y := as.numeric(variable)] %>%
#   .[, ybar := as.numeric(y) + (n/sum(n)*0.95), by = .(round, target, variable, scenario_letter)] %>%
#   .[scenario_letter == "B"] %>%
#   ggplot() + 
#   geom_hline(aes(yintercept = 5)) + 
#   geom_hline(aes(yintercept = 4), color = 'lightgray') + 
#   geom_hline(aes(yintercept = 3), color = 'lightgray') + 
#   geom_hline(aes(yintercept = 2), color = 'lightgray') + 
#   geom_hline(aes(yintercept = 1), color = 'lightgray') + 
#   geom_rect(aes(xmin = pct_agree_bin-0.5, 
#                 xmax = pct_agree_bin + 0.5, 
#                 ymin = y, ymax = ybar), color = "black", fill = "gray", linewidth = 0.35) + 
#   facet_wrap(vars(round), labeller = labeller(round = round_labs)) +
#   labs(x = "percent of models agreeing", 
#        y = "number of locations") + 
#   scale_x_continuous(breaks = 1:4, 
#                      labels = c("0-25%", "25-50%", "50-75%", "75-100%")) + 
#   scale_y_continuous(breaks = 1:5+0.5, 
#                      expand = c(0,0),
#                      labels = c(paste0("magnitude\nwithin \u00B1",rev(c(10,25,50,75)),"%"), "ranking"), 
#                      limits = c(1, 6)) +
#   theme_bw(base_size = bs) + 
#   theme(axis.ticks.y = element_blank(),
#         panel.grid = element_blank(), 
#         strip.background = element_blank())
# 
# ggsave("figures/scenario_B_agreement_v2.pdf", width = 170, height = 170, units = "mm")
