renv::restore()

library(ggplot2)
library(metR)
library(scales)
library(cowplot)
library(reshape2)
library(RColorBrewer)

#source("code/SMH-analysis/load-SMH-projections.R")
source("code/manuscript-figures/plotting-utils.R")

SMH_agreement <- setDT(read.csv("output-data/SMH-analysis/SMH_agreement.csv")) %>%
  .[, ":=" (target_end_date = as.IDate(target_end_date), 
            X = NULL)]


#### FIGURE 6A -----------------------------------------------------------------
ex_a <- data.frame(round = 11, 
                   target = "cum hosp", 
                   location = "42")

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


#### FIGURE 6B -----------------------------------------------------------------
pnt <- SMH_agreement  %>%
  .[, horiz := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
  .[, max_horiz := max(horiz), by =.(round)] %>%
  .[horiz == max_horiz & 
      quantile == 0.5 &
      round == ex_a$round & 
      target == ex_a$target & 
      location == ex_a$location]
dns <- null_agreement %>%
  .[round == ex_a$round & 
      target == ex_a$target & 
      location == ex_a$location] %>% 
  .[, .(round, target, location, sim_id, kendall_w, icc_a_nonzero)] %>%
  data.table::melt(c("round", "target", "location", "sim_id")) %>%
  .[, .(x = density(value)$x, 
        y = density(value)$y), 
    by = .(round, target, location, variable)]

p1 = null_agreement %>%
  .[round == ex_a$round & 
      target == ex_a$target & 
      location == ex_a$location] %>%
  ggplot(aes(x = icc_a_nonzero, y = kendall_w)) + 
  geom_vline(data = pnt, aes(xintercept = icc_a_nonzero), 
             color = "red", linetype = "dotted") +
  geom_hline(data = pnt, aes(yintercept = kendall_w), 
             color = "red", linetype = "dotted") +
  geom_point(data = pnt, color = "red") +
  geom_point(alpha = 0.3) +
  scale_x_continuous(breaks = c(0,0.5,1),
                     limits = c(0,1),
                     expand = c(0,0), 
                     name = "agreement between values\n(intraclass correlation coefficient)") +
  scale_y_continuous(breaks = c(0,0.5,1),
                     limits = c(0,1), 
                     expand = c(0,0), 
                     name = "agreement between ranks\n(Kendall's W)") +
  theme_bw(base_size = bs) +
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(),
        plot.margin = margin(c(0,0,0,0), unit = "cm"),
        strip.background = element_blank())
p2 = ggplot(data = dns[variable == "icc_a_nonzero"],
            aes(x = x, y = y)) + 
  geom_line(size = 0.3) +
  geom_segment(data = pnt, 
               aes(x = icc_a_nonzero, xend = icc_a_nonzero, 
                   y = 0, yend = approx(unlist(dns[variable == "icc_a_nonzero", "x"]), 
                                        unlist(dns[variable == "icc_a_nonzero", "y"]), 
                                        pnt$icc_a_nonzero)$y), color = "red", linetype = "dotted") +
  geom_density(aes(x = x),
               . %>% filter(between(x, pnt$icc_a_nonzero, Inf)),
               stat = 'identity', alpha = 0.25, fill = "red", size = 0.3)+
  scale_x_continuous(breaks = seq(0,1,0.25), 
                     expand = c(0,0), 
                     limits = c(0,1)) + 
  theme_minimal(base_size = bs) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        legend.position = "bottom", 
        panel.grid = element_blank(), 
        plot.margin = margin(b = -0.8, unit = "cm"),
        strip.background = element_blank())
p3 = ggplot(data = dns[variable == "kendall_w"],
            aes(x = x, y = y)) + 
  geom_line(size = 0.3) +
  geom_segment(data = pnt, 
               aes(x = kendall_w, xend = kendall_w, 
                   y = 0, yend = approx(unlist(dns[variable == "kendall_w", "x"]), 
                                        unlist(dns[variable == "kendall_w", "y"]), 
                                        pnt$kendall_w)$y), color = "red", linetype = "dotted") +
  geom_density(aes(x = x),
               . %>% filter(between(x, pnt$kendall_w, Inf)),
               stat = 'identity', alpha = 0.25, fill = "red", size = 0.3)+
  coord_flip() +
  scale_x_continuous(breaks = seq(0,1,0.25), 
                     expand = c(0,0), 
                     limits = c(0,1)) + 
  theme_minimal(base_size = bs) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        legend.position = "bottom", 
        panel.grid = element_blank(), 
        plot.margin = margin(c(0,0,0,0), unit = "cm"),
        strip.background = element_blank())


pB = plot_grid(p2, NULL ,p1, p3,
               align = "vh", 
               axis = "btlr",
               nrow = 2, 
               ncol = 2, 
               rel_widths = c(0.8,0.2), 
               rel_heights = c(0.2,0.8))
ggsave("figures/Figure6B.pdf", width = 190*0.35, height = 190*0.35, units = "mm")


#### FIGURE 6C -----------------------------------------------------------------
pC <- SMH_agreement  %>%
  .[locations, on = .(location)] %>%
  .[, horiz := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
  .[, max_horiz := max(horiz), by =.(round)] %>%
  .[horiz == max_horiz & 
      target == "cum hosp" &
      quantile == 0.5] %>%
  .[, .(round, target, horiz, location, population, quantile, MSBS, MSBM, MSE)] %>% 
  data.table::melt(c("round", "target", "horiz", "location","population", "quantile")) %>%
  .[, pct := value/sum(value), by = .(round, target, horiz, location, population, quantile)] %>%
  ggplot(aes(x = reorder(location, population), y = pct, fill = variable)) + 
  geom_bar(position = "stack", stat = "identity", width = 1) + 
  facet_wrap(vars(round), nrow = 4, labeller = labeller(round = round_labs)) +  
  scale_fill_brewer(palette = "Greys", 
                    labels = c("mean square between scenarios", 
                               "mean square between models", 
                               "mean square error")) + 
  scale_x_discrete(name = "locations projected", 
                   expand = c(0,0))+ 
  scale_y_continuous(name = "% of sum of squares", 
                     expand = c(0,0), 
                     labels = percent) + 
  theme_bw(base_size = bs) + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        legend.position = "bottom", 
        legend.title = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank())


#### COMBINE INTO FIGURE 6 -----------------------------------------------------
plot_grid(
  plot_grid(pA, pB, 
            ncol = 1, 
            labels = LETTERS[1:2], 
            label_size = bs,
            rel_heights = c(0.55, 0.45)), 
  pC, 
  labels = c(NA, LETTERS[3]), 
  label_size = bs,
  rel_widths = c(0.25, 0.75),
  nrow = 1)
ggsave("figures/Figure6.pdf", width = 190, height = 150, units = "mm")
