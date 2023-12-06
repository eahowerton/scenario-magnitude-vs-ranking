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

ggplot(data = plt_data[value %in% seq(2,8,2)], 
       aes(x = s)) +  #, y = value0
  geom_line(aes(y = ncum, color = as.factor(variable)), 
            size = 0.25, alpha = 0.7) +
  geom_point(aes(y = ncum, color = as.factor(variable)), size = 1.5) +
  # geom_tile(aes(fill = ncum)) + 
  # geom_tile(data = plt_data[ncum == 1], aes(fill = ncum), color = "white", size =1) + 
  #geom_contour(aes(z = ncum), breaks = c(0.5, 0.25, 0.75, 0.9)) +
  facet_grid(rows = vars(as.factor(paste0("at least ", value, " models agree"))), 
             cols = vars(mod_type),
             labeller = labeller(mod_type = mod_type_labs),
             switch = "y") +
  guides(color = guide_legend(nrow = 2)) +
  scale_color_manual(# breaks = c(0,10,25,50,100),
    labels = c("Estimates agree within 20 cases",
               "Estimates agree within 50 cases",
               "Estimates agree within 100 cases",
               "Intervention recommendations agree"),
    values = c(brewer.pal(4, "RdYlBu")[1:3], "black")) +
  #name = "number of models making predictions") + 
  scale_x_continuous(name = "number of models in set") +
  scale_y_continuous(labels = percent, 
                     name = "probability") + 
  theme_bw() + 
  theme(#axis.title.y = element_blank(), 
    legend.position = "bottom", 
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(), 
    panel.grid.minor = element_blank(), 
    strip.background = element_blank(), 
    strip.placement = "outside")#, strip.text = element_text(hjust = 0)
ggsave("figures/figureS4.pdf", width = 8, height = 8)