renv::restore()

library(ggplot2)
library(metR)
library(scales)
library(cowplot)
library(reshape2)

source("code/manuscript-figures/plotting-utils.R")

multi_panel <- read.csv("output-data/SIR-simulations/SIR_varying_intervention_params.csv")

multi_panel_alt <- multi_panel %>%
  select(beta, gamma, cases, v_rate, d_rate, peak_cases, intervention) %>%
  reshape2::melt(c("beta", "gamma", "v_rate", "d_rate", "intervention")) %>%
  reshape2::dcast(beta + gamma + v_rate + d_rate + variable ~ intervention, 
                  value.var = "value") %>%
  mutate(diff = (v - d),
         rel_diff = (v-d)/n,
         rec = ifelse(diff < 0, "vaccination", "distancing"))

m_case <- multi_panel_alt %>%
  filter(variable == "cases") %>%
  mutate(diff = abs(diff)) %>%
  pull(diff) %>% max()

multi_panel_alt  %>%
  filter(variable == "cases") %>%
  ggplot(aes(x = beta, y = gamma)) +
  geom_tile(aes(fill = diff)) +
  geom_contour(aes(z = diff), breaks = 0, color = "grey") +
  geom_contour(aes(z = beta/gamma),
               color = "black",
               breaks = c(2,3.5,6,10)) +
  geom_text_contour(aes(z = beta/gamma),
                    color = "black",
                    breaks = c(2,2.5,3.5,5,6,7,10),
                    stroke  = 0.2) +
  facet_grid(cols = vars(v_rate), rows = vars(d_rate), 
             labeller = labeller(v_rate = labs_v, 
                                 d_rate = labs_d)) +
  scale_x_continuous(expand = c(0,0), name = "transmission rate") +
  scale_y_continuous(expand = c(0,0), name = "recovery rate") +
  scale_fill_distiller(palette = "PuOr",
                       limits= c(-1,1)*m_case,
                       name = "difference in\ncumulative\ninfections",
                       breaks = seq(-500,500, 250),
                       labels = c("-500\nvaccination", "-250", "0", "250", "500\nNPIs")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(0.6, "in"), 
        strip.background = element_blank())
ggsave("figures/figureS2.pdf", width = 8, height = 7.5)