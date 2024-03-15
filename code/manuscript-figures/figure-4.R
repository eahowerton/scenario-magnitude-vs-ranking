renv::restore()

library(data.table)
library(dplyr)
library(ggplot2)
library(metR)
library(scales)
library(cowplot)
library(reshape2)
library(RColorBrewer)

source("code/manuscript-figures/plotting-utils.R")

p <- setDT(read.csv("output-data/SMH-analysis/SMH_ranks.csv"))

#### FIGURE 4A -----------------------------------------------------------------
ex_a <- data.frame(round = 4, 
                   target = "cum hosp", 
                   location = "40") # oklahoma

pA <- p[, max_horiz := max(horiz), by =.(round)] %>%
  .[quantile == 0.5 &
      horiz == max_horiz &
      substr(model_name,1,3) != "Ens"& 
      round == ex_a$round & 
      target == ex_a$target & 
      location == ex_a$location] %>% 
  .[, key := factor(letters[as.numeric(as.factor(model_name))], 
                    levels = letters[9:1])] %>%
  # .[, key := factor(as.character(as.roman(as.numeric(as.factor(model_name)))), 
  #                   levels = rev(as.character(as.roman(1:6))))] %>%
  ggplot(aes(x = value, y = key, color = as.factor(r))) + 
  geom_point() +
  guides(color = guide_legend(nrow=1)) +
  facet_grid(rows = vars(paste0("scenario ", scenario_letter)), switch = "y") +
  labs(title = "Round 4, Oklahoma") + 
  scale_color_manual(breaks = seq(1, 4, 0.5), 
                     values = rev(brewer.pal(11,"RdYlBu")[c(2:4,5,8:10)]), 
                     name = "rank", 
                     labels = c("best", rep("", 5), "worst")) +
  scale_x_continuous(label = unit_format(scale = 1e-3, suffix = ""), 
                     name = "cumulative hospitalizations (K)") +
  scale_y_discrete(position = "right") +
  theme_bw(base_size = bs) +
  theme(axis.title.y.right = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside")


#### FIGURE 4B -----------------------------------------------------------------
ex_b <- data.frame(round = 11, 
                   target = "cum hosp", 
                   location = "42") # Pennsylvania

pB <- p[, max_horiz := max(horiz), by =.(round)] %>%
  .[quantile == 0.5 &
      horiz == max_horiz &
      substr(model_name,1,3) != "Ens"& 
      round == ex_b$round & 
      target == ex_b$target & 
      location == ex_b$location] %>% 
  .[, key := factor(letters[as.numeric(as.factor(model_name))], 
                    levels = letters[9:1])] %>%
  # .[, key := factor(as.character(as.roman(as.numeric(as.factor(model_name)))), 
  #                   levels = rev(as.character(as.roman(1:6))))] %>%
  ggplot(aes(x = value, y = key, color = as.factor(r))) + 
  geom_point() +
  guides(color = guide_legend(nrow=1)) +
  facet_grid(rows = vars(paste0("scenario ", scenario_letter)), switch = "y") +
  labs(title = "Round 11, Pennsylvania") + 
  scale_color_manual(breaks = seq(1, 4, 0.5), 
                     values = rev(brewer.pal(11,"RdYlBu")[c(2:4,5,8:10)]), 
                     name = "rank", 
                     labels = c("best", rep("", 5), "worst")) +
  scale_x_continuous(label = unit_format(scale = 1e-3, suffix = ""), 
                     name = "cumulative hospitalizations (K)") +
  scale_y_discrete(position = "right") +
  theme_bw(base_size = bs) +
  theme(axis.title.y.right = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside")


#### FIGURE 4C -----------------------------------------------------------------
ex_c <- data.frame(round = 16, 
                   target = "cum hosp", 
                   location = "08") # colorado

pC <- p[, max_horiz := max(horiz), by =.(round)] %>%
  .[quantile == 0.5 &
      horiz == max_horiz &
      substr(model_name,1,3) != "Ens"& 
      round == ex_c$round & 
      target == ex_c$target & 
      location == ex_c$location] %>% 
  .[, key := factor(letters[as.numeric(as.factor(model_name))], 
                    levels = letters[9:1])] %>%
  # .[, key := factor(as.character(as.roman(as.numeric(as.factor(model_name)))), 
  #                   levels = rev(as.character(as.roman(1:6))))] %>%
  ggplot(aes(x = value, y = key, color = as.factor(r))) + 
  geom_point() +
  guides(color = guide_legend(nrow=1)) +
  facet_grid(rows = vars(paste0("scenario ", scenario_letter)), switch = "y") +
  labs(title = "Round 16, Colorado") + 
  scale_color_manual(breaks = seq(1, 4, 0.5), 
                     values = rev(brewer.pal(11,"RdYlBu")[c(2:4,5,8:10)]), 
                     name = "rank", 
                     labels = c("best", rep("", 5), "worst")) +
  scale_x_continuous(label = unit_format(scale = 1e-3, suffix = ""), 
                     name = "cumulative hospitalizations (K)") +
  scale_y_discrete(position = "right") +
  theme_bw(base_size = bs) +
  theme(axis.title.y.right = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside")


plot_grid(pA, pB, pC, nrow = 1, labels = LETTERS[1:3])
ggsave("figures/Figure4.pdf", width = 6, height = 3.25)

