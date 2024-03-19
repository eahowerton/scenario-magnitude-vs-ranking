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

lims <- p[target == "cum hosp" & 
      round == 11 &
      quantile == 0.5 &
      horiz == max_horiz] %>%
  .[, plt_val := value/population*1E5] %>% 
  pull(plt_val) %>% range()


#### FIGURE 4A -----------------------------------------------------------------
ex_a <- data.frame(round = 11, 
                   target = "cum hosp", 
                   location = "12") # florida

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
  ggplot(aes(x = value/population*1E5, y = key, color = as.factor(r))) + 
  geom_point() +
  guides(color = guide_legend(nrow=1)) +
  facet_grid(rows = vars(paste0("scenario ", scenario_letter)), switch = "y") +
  labs(title = "Round 11, Florida") + 
  scale_color_manual(breaks = seq(1, 4, 0.5), 
                     values = rev(brewer.pal(11,"RdYlBu")[c(2:4,5,8:10)]), 
                     name = "rank", 
                     labels = c("best", rep("", 5), "worst")) +
  scale_x_continuous(label = comma, 
                     limits = lims, 
                     name = "cumulative hospitalzations\nper 100,000 population") +
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
  ggplot(aes(x = value/population*1E5, y = key, color = as.factor(r))) + 
  geom_point() +
  guides(color = guide_legend(nrow=1)) +
  facet_grid(rows = vars(paste0("scenario ", scenario_letter)), switch = "y") +
  labs(title = "Round 11, Pennsylvania") + 
  scale_color_manual(breaks = seq(1, 4, 0.5), 
                     values = rev(brewer.pal(11,"RdYlBu")[c(2:4,5,8:10)]), 
                     name = "rank", 
                     labels = c("best", rep("", 5), "worst")) +
  scale_x_continuous(label = comma, 
                     limits = lims, 
                     name = "cumulative hospitalzations\nper 100,000 population") +
  scale_y_discrete(position = "right") +
  theme_bw(base_size = bs) +
  theme(axis.title.y.right = element_blank(),
        legend.position = "none",
        legend.title = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside")


#### FIGURE 4C -----------------------------------------------------------------
ex_c <- data.frame(round = 11, 
                   target = "cum hosp", 
                   location = "33") # new hampshire

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
  ggplot(aes(x = value/population*1E5, y = key, color = as.factor(r))) + 
  geom_point() +
  guides(color = guide_legend(nrow=1)) +
  facet_grid(rows = vars(paste0("scenario ", scenario_letter)), switch = "y") +
  labs(title = "Round 11, New Hampshire") + 
  scale_color_manual(breaks = seq(1, 4, 0.5), 
                     values = rev(brewer.pal(11,"RdYlBu")[c(2:4,5,8:10)]), 
                     name = "rank", 
                     labels = c("best", rep("", 5), "worst")) +
  scale_x_continuous(label = comma, 
                     limits = lims, 
                     name = "cumulative hospitalzations\nper 100,000 population") +
  scale_y_discrete(position = "right") +
  theme_bw(base_size = bs) +
  theme(axis.title.y.right = element_blank(),
        legend.position = "none",
        legend.title = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside")

#### FIGURE 4C -----------------------------------------------------------------
ex_d <- data.frame(round = 11, 
                   target = "cum hosp", 
                   location = "24") # maryland

illustrate_lvls <- data.frame(scenario_letter = "A", 
                              xmin = lims[2]-2*smh_abs_levels, 
                              xmax = lims[2], 
                              y = 2:4+0.5, 
                              win = paste0(smh_abs_levels))
illustrate_lvls$xmean = rowMeans(cbind(illustrate_lvls$xmin, illustrate_lvls$xmax)) 



pD <- p[, max_horiz := max(horiz), by =.(round)] %>%
  .[quantile == 0.5 &
      horiz == max_horiz &
      substr(model_name,1,3) != "Ens"& 
      round == ex_d$round &
      target == ex_d$target & 
      location == ex_d$location] %>% 
  .[, key := factor(letters[as.numeric(as.factor(model_name))], 
                    levels = letters[9:1])] %>%
  # .[, key := factor(as.character(as.roman(as.numeric(as.factor(model_name)))), 
  #                   levels = rev(as.character(as.roman(1:6))))] %>%
  ggplot(aes(x = value/population*1E5, y = key, color = as.factor(r))) + 
  geom_point() +
  geom_segment(data = illustrate_lvls, 
               aes(x = xmin, xend = xmax, y = y, yend = y), color = "black") + 
  geom_text(data = data.frame(x = lims[2], y = 5.3, lab = "window size", scenario_letter= "A"),
                              aes(x = x, y = y, label = lab), 
                              hjust = 1, vjust = 0, color = "black", size = 1.25) + 
  geom_text(data = illustrate_lvls, aes(x = xmax, y = y+0.15, label = win), hjust = 1, vjust = 0, color = "black", size = 1.2) + 
  guides(color = guide_legend(nrow=1)) +
  facet_grid(rows = vars(paste0("scenario ", scenario_letter)), switch = "y") +
  labs(title = "Round 11, Maryland") + 
  scale_color_manual(breaks = seq(1, 4, 0.5), 
                     values = rev(brewer.pal(11,"RdYlBu")[c(2:4,5,8:10)]), 
                     name = "rank", 
                     labels = c("best", rep("", 5), "worst")) +
  scale_x_continuous(label = comma, 
                     limits = lims, 
                     name = "cumulative hospitalzations\nper 100,000 population") +
  scale_y_discrete(position = "right") +
  theme_bw(base_size = bs) +
  theme(axis.title.y.right = element_blank(),
        legend.position = "none",
        legend.title = element_blank(), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside")

l <- get_legend(pA)

plot_grid(plot_grid(pA + theme(legend.position = "none"), pB, pC, pD, nrow = 1, labels = LETTERS[1:4], label_size = bs+2), 
          l, ncol = 1, rel_heights = c(0.9, 0.1))

ggsave("figures/Figure4.pdf", width = 8, height = 3.5)

