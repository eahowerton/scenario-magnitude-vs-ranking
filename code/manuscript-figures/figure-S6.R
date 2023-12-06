renv::restore()

library(ggplot2)
library(metR)
library(scales)
library(cowplot)
library(RColorBrewer)
library(reshape2)

source("code/manuscript-figures/plotting-utils.R")

#### FIGURE S6 ------------------------------------------------------------------
samp <- read.csv("output-data/SIR-simulations/SIR_sample_model_sets.csv")
setDT(samp)

n = 2
kw_sig = data.frame(s = 2:10)
kw_sig$p_0.05 = qchisq(0.05, df = n-1, lower.tail = FALSE)/(kw_sig$s*(n-1))
setDT(kw_sig)

samp %>% 
  .[variable %in% c("kw2", "icc") & 
      mod_type %in% c("all", "similar")] %>%
  data.table::dcast(it + id + mod_type + s ~ variable, value.var = "value") %>%
  .[s %in% c(4,6,8,10)] %>%
  ggplot(aes(x = icc, y = kw2, color = mod_type)) +
  geom_point(alpha = 0.05) + 
  geom_hline(data = kw_sig %>%
               .[s %in% c(4,6,8,10)], 
             aes(yintercept = p_0.05), linetype = "dotted") +
  stat_ellipse() +
  facet_grid(cols = vars(s), 
             labeller = labeller(s = mod_labs))+#, rows = vars(mod_type)) + 
  scale_color_brewer(labels = c("all models", "similar models"), 
                     palette = "Set1") +
  scale_x_continuous(breaks = c(0,0.5,1),
                     limits = c(0,1), 
                     name = "agreement between values\n(intraclass correlation coefficient)")+
  scale_y_continuous(breaks = c(0,0.5,1),
                     limits = c(0,1), 
                     name = "agreement between ranks\n(Kendall's W)") + 
  theme_bw() + 
  theme(legend.margin = margin(rep(0,4)),
        legend.position = c(0.92, 0.14),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(), 
        strip.placement = "outside")

ggsave("figures/figureS6.pdf", width = 8, height = 3)