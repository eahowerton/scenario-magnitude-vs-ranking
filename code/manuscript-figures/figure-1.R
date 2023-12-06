renv::restore()

library(ggplot2)
library(metR)
library(scales)
library(cowplot)
library(reshape2)

source("code/SIR-simulations/setup_model.R")
source("code/manuscript-figures/plotting-utils.R")

#### FIGURE 1A -----------------------------------------------------------------
timeseries <- read.csv("output-data/SIR-simulations/SIR_timeseries_example.csv")

# plot timeseries
pA <- timeseries %>%
  select(time, C, model, intervention) %>%
  filter(time < 60) %>%
  reshape2::melt(c("time", "model", "intervention")) %>%
  mutate(intervention = factor(intervention, levels = c("n", "d", "v"))) %>%
  ggplot() +
  geom_line(aes(x = time, y = value, linetype = intervention)) +
  guides(color = "none") +
  facet_wrap(vars(substr(model, 1,7)), nrow = 2) +
  scale_linetype_manual(labels = c("no intervention", "NPIs", "vaccination"),
                        values = c("solid", "dashed", "dotted")) +
  #scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(name = "cumulative infections") +
  theme_bw() +
  theme(#axis.title.y = element_blank(),
    legend.position = c(0.83,0.12),
    legend.title = element_blank(),
    legend.margin = margin(t = 0, unit = "cm"),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside")

#### FIGURE 1B -----------------------------------------------------------------
one_panel <- read.csv("output-data/SIR-simulations/SIR_intervention_outcomes.csv")
setDT(one_panel)

one_panel_alt <- one_panel %>%
  select(beta, gamma, cases, peak_cases, intervention) %>%
  reshape2::melt(c("beta", "gamma", "intervention")) %>%
  reshape2::dcast(beta + gamma + variable ~ intervention, value.var = "value") %>%
  mutate(diff = (v - d),
         rel_diff = (v-d)/n, 
         rec = ifelse(diff < 0, "vaccination", "distancing"))
setDT(one_panel_alt)

m_case <- one_panel_alt %>%
  filter(variable == "cases") %>%
  mutate(diff = abs(diff)) %>%
  pull(diff) %>% max()

pB <- one_panel_alt  %>%
  filter(variable == "cases") %>%
  ggplot(aes(x = beta, y = gamma)) +
  geom_tile(aes(fill = diff)) +
  geom_contour(aes(z = diff), breaks = 0, color = "grey65") +
  geom_contour(aes(z = beta/gamma),
               color = "black",
               breaks = c(2,3.5,6,10)) +
  geom_text_contour(aes(z = beta/gamma),
                    color = "black",
                    breaks = c(2,2.5,3.5,5,6,7,10),
                    stroke  = 0.2) +
  geom_point(data = data.frame(beta = c(m1_beta, m2_beta),
                               gamma = c(m1_gamma, m2_gamma)),
             size = 2) +
  geom_text(data = data.frame(beta = c(m1_beta, m2_beta),
                              gamma = c(m1_gamma+0.01, m2_gamma+0.01),
                              name = c("model 1", "model 2")),
            aes(label = name),
            vjust = 0) +
  scale_x_continuous(expand = c(0,0), name = "transmission rate") +
  scale_y_continuous(expand = c(0,0), name = "recovery rate") +
  scale_fill_distiller(palette = "PuOr",
                       limits= c(-1,1)*max(abs(one_panel_alt$diff)),
                       name = "difference in\ncumulative infections",
                       breaks = seq(-200,200, 100),
                       labels = c("-200\nvaccination", "-100", "0", "100", "200\nNPIs")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.width = unit(0.5, "in"))

#### FIGURE 1C -----------------------------------------------------------------
pC = one_panel_alt  %>%
  filter(variable == "cases") %>%
  mutate(id = paste(beta, gamma, "-"),
         rec = ifelse(diff < 0, "vaccination", "distancing")) %>%
  ggplot(aes(x = beta/gamma, y = n, color = rec)) +
  geom_rect(xmin = 3, xmax = 4, ymin = 925,  ymax = 975,
            color = "black", fill = "white", size = 0.25) +
  geom_point(alpha = 0.1, size = 2, shape = 19) +
  scale_color_manual(values = c("#f1a340", "#998ec3"), 
                     labels = c("NPIs\nrecommended", "vaccination\nrecommended")) +
  scale_x_continuous(name = "individual model R0", 
                     breaks = c(2,3.5, 6, 10)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(500,1000),
                     name = "estimated cumulative infections without intervention") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid = element_blank())

#### FIGURE 1D -----------------------------------------------------------------
pD = one_panel_alt  %>%
  filter(variable == "cases") %>%
  mutate(id = paste(beta, gamma, "-"),
         rec = ifelse(diff < 0, "vaccination", "distancing")) %>%
  ggplot(aes(x = beta/gamma, y = n, color = rec)) +
  geom_point(alpha = 0.1, size = 2, shape = 19) +
  scale_color_manual(values = c("#f1a340", "#998ec3"),
                     labels = c("NPI\nrecommended", "vaccination\nrecommended")) +
  scale_x_continuous(expand = c(0,0),
                     name = "individual model R0",
                     limits = c(3,4)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(925,975),
                     name = "estimated cumulative infections") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid = element_blank())

#### FIGURE 1E -----------------------------------------------------------------
pE <- one_panel_alt  %>%
  filter(variable == "cases") %>%
  mutate(id = paste(beta, gamma, "-"),
         rec = ifelse(diff < 0, "vaccination", "distancing"),
         group = round(beta/gamma, 1)) %>%
  group_by(group) %>%
  summarise(n_vacc = sum(group[rec == "vaccination"])/group,
            n_dist = sum(group[rec == "distancing"])/group) %>%
  ggplot(aes(x = group, y = n_vacc/(n_dist + n_vacc))) +
  geom_line(aes(color = "vaccination intervention\nrecommended"), size = 2) +
  geom_line(aes(x = group,
                y = n_dist/(n_dist + n_vacc),
                color = "NPI\nrecommended"),
            size = 1.5)+
  scale_color_manual(values = c("#f1a340", "#998ec3"),
                     labels = c("NPIs recommended", "vaccination recommended")) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(3,4),
                     name = "individual model R0") +
  scale_y_continuous(name = "% of models",
                     labels = percent) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid = element_blank())

#### FIGURE 1C,D,E -------------------------------------------------------------
pDE <- plot_grid(pD + theme(legend.position = "none"),
                pE + theme(legend.position = "none"),
                ncol = 1,
                labels = c("D", "E"),
                align = "v",
                axis = "l")

pCDE <- ggdraw(pC +
               draw_plot(pDE, x = 5, y = 500, width = 7.5, height = 475)
)

#### FIGURE 1 COMBINED ---------------------------------------------------------
l <- get_legend(pE)
l1 <- get_legend(pB)

p <- plot_grid(
  plot_grid(
    plot_grid(pA, 
              pB + theme(legend.position = "none"), 
              labels = c("A", "B"), 
              rel_widths = c(0.33, 0.66)), 
    plot_grid(NULL, l1), ncol = 1, rel_heights = c(0.9, 0.1)),
  plot_grid(
    pCDE,
    l,
    labels = c("C", NA),
    ncol = 1,
    rel_heights = c(0.9, 0.1)), nrow = 1, align = "h", axis = "b", rel_widths = c(1, 0.66))

ggsave("figures/figure1.pdf", p, width = 14, height = 6)
