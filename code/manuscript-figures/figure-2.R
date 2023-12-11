renv::restore()

library(ggplot2)
library(metR)
library(scales)
library(cowplot)
library(RColorBrewer)
library(reshape2)

source("code/manuscript-figures/plotting-utils.R")

#### FIGURE 2 ------------------------------------------------------------------
samp <- read.csv("output-data/SIR-simulations/SIR_sample_model_sets.csv")
setDT(samp)

# summarize
plt_data <- merge(setDT(expand.grid(s = 2:12, 
                                    variable = unique(samp$variable),
                                    mod_type = unique(samp$mod_type),
                                    value = 2:12)) %>% 
                    .[value <= s], 
                  samp %>% 
                    .[, .(n = .N), by = .(s, variable, v, mod_type, value)] %>%
                    .[, p := n/sum(n), by = .(s, variable, v, mod_type)], 
                  by = c("s", "variable", "mod_type", "value"), all = TRUE) %>% 
  .[!(variable %in% c("kw", "kw2", "icc"))] %>%
  .[, p := ifelse(is.na(p), 0, p)] %>%
  .[order(-value),] %>%
  .[, ncum := cumsum(p), by = .(s, variable, mod_type)] %>%
  .[value != 1]
plt_data <- plt_data[, c("variable_type", "variable_tol") := data.table(stringr::str_split_fixed(plt_data$variable, "_", 2))]

ggplot(data = plt_data %>%
         .[, rel_value := value/s] %>%
         .[rel_value == 2/3 & mod_type %in% c("all", "similar") & 
             variable %in% c("mag_10", "mag_25", "mag_50", "rec")], 
       aes(x = s)) +  #, y = value0
  geom_line(aes(y = ncum, color = as.factor(variable)), 
            size = 0.25, alpha = 0.7) +
  geom_point(aes(y = ncum, color = as.factor(variable)), size = 1.5) +
  facet_grid(cols = vars(paste(mod_type, "models"))) +
  guides(color = guide_legend(nrow = 2)) +
  scale_color_manual(labels = c("Estimates agree within 20 infections",
                                "Estimates agree within 50 infections",
                                "Estimates agree within 100 infections",
                                "Intervention scenario ranking agrees"),
                     values = c(brewer.pal(4, "RdYlBu")[1:3], "black")) +
  scale_x_continuous(breaks = seq(3,12,3), 
                     name = "number of models in set") +
  scale_y_continuous(expand = c(0,0), 
                     labels = percent, 
                     limits = c(0,1),
                     name = "probability at least 66% of models agree") + 
  theme_bw() + 
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_blank(), 
    strip.placement = "outside")

ggsave("figures/figure2.pdf", width = 6, height = 4)
