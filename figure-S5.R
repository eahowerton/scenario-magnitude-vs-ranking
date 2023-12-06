renv::restore()

library(ggplot2)
library(metR)
library(scales)
library(cowplot)
library(RColorBrewer)
library(reshape2)

source("code/manuscript-figures/plotting-utils.R")

#### FIGURE S4 ------------------------------------------------------------------
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


# create data.frames to draw lines on heatmap at 50% 
stps_50pct <- plt_data %>%
  .[mod_type %in% c("all", "similar") & 
      variable %in% c("mag_10", "mag_25", "mag_50", "rec")] %>%
  .[, less_50pct_flag := ifelse(ncum < 0.5, 1, 0)] %>%
  .[less_50pct_flag == 1] %>%
  .[, .(min_less_50 = min(value)), by = .(s, mod_type, variable)] 
# add one extra segment to finish line at s = 12
stps_50pct <- rbindlist(list(
  stps_50pct, 
  stps_50pct[s == 12] %>% 
    .[, s := s+1]))

# repeat for 90%
stps_90pct <- plt_data %>%
  .[mod_type %in% c("all", "similar") & 
      variable %in% c("mag_10", "mag_25", "mag_50", "rec")] %>%
  .[, less_90pct_flag := ifelse(ncum < 0.9, 1, 0)] %>%
  .[less_90pct_flag == 1] %>%
  .[, .(min_less_90 = min(value)), by = .(s, mod_type, variable)]
# add one extra segment to finish line at s = 12
stps_90pct <- rbindlist(list(
  stps_90pct, 
  stps_90pct[s == 12] %>% 
    .[, s := s+1]))


plt_data %>%
  .[mod_type %in% c("all", "similar") & 
      variable %in% c("mag_10", "mag_25", "mag_50", "rec")] %>%
  ggplot(aes(x = s, y = value)) +
  geom_tile(aes(fill = ncum)) + 
  geom_step(data = stps_50pct, 
            aes(x = s-0.5, 
                y = min_less_50-0.5), 
            linetype = "solid", 
            size = 0.7) +
  geom_step(data = stps_90pct, 
            aes(x = s-0.5,
                y = min_less_90-0.5), 
            linetype = "dashed", 
            size = 0.7) +
  facet_grid(cols = vars(variable), 
             rows = vars(paste(mod_type, "models")), 
             labeller = labeller(variable = est_labs)) + 
  scale_fill_viridis_c(name = "probability of agreement") + 
  scale_linetype_manual(values = c("solid","dashed")) +
  scale_x_continuous(breaks = 2:12, 
                     expand = c(0,0), 
                     name = "number of models in set") +
  scale_y_continuous(breaks = 2:12, 
                     expand = c(0,0), 
                     name = "number of models agreeing") + 
  theme_bw() + 
  theme(legend.key.width = unit(1,"cm"),
        legend.position = "bottom", 
        panel.grid = element_blank(), 
        strip.background = element_blank())

ggsave("figures/figureS5.pdf", width = 8, height = 5)

