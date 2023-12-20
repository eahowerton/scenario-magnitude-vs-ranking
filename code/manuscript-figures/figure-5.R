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
null_agreement <- setDT(read.csv("output-data/SMH-analysis/null_agreement.csv")) %>%
  .[, ":=" (target_end_date = as.IDate(target_end_date), 
            X = NULL)]


#### FIGURE 5A -----------------------------------------------------------------
comp_null_smh <- null_agreement %>%
  rename(kendall_w_null = kendall_w) %>%
  rename(icc_a_nonzero_null = icc_a_nonzero) %>%
  .[, c("round", "target", "target_end_date", "location", "quantile", 
        "sim_id", "icc_a_nonzero_null", "kendall_w_null")] %>%
  .[, .(mean_icc_a_nonzero_null = mean(icc_a_nonzero_null), 
        mean_kw_null = mean(kendall_w_null)), 
    by = .(round, target, target_end_date, location, quantile)] %>%
  .[SMH_agreement %>%
      .[, horiz := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
      .[, max_horiz := max(horiz), by =.(round)] %>%
      .[quantile ==0.5 &                         # only use medians
          target == "cum hosp" &                 # cumulative hosp only
          horiz == max_horiz                     # maximum horizon only
      ]%>%
      .[, c("round", "target", "target_end_date", "location", "quantile", 
            "icc_a_nonzero", "kendall_w")], 
    on = .(round, target, target_end_date, location, quantile)] %>%
  .[, ":=" (rel_change_icc = log((icc_a_nonzero)/(mean_icc_a_nonzero_null)), 
            rel_change_kw = log(kendall_w/mean_kw_null))]

pA <- comp_null_smh %>% 
  ggplot(aes(x = rel_change_icc, 
             y = rel_change_kw)) + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_point(alpha = 0.7, color = "gray40") +
  facet_wrap(vars(round), nrow = 4, labeller = labeller(round = round_labs)) +  
  scale_x_continuous(limits = c(-1,1)*max(abs(comp_null_smh$rel_change_icc[!is.infinite(comp_null_smh$rel_change_icc)])), 
                     name = "relative agreement between projection magnitude\nlog(ICC(SMH)/ICC(null))") +
  scale_y_continuous(limits = c(-1,1)*max(abs(comp_null_smh$rel_change_kw[!is.infinite(comp_null_smh$rel_change_kw)])),
                     name = "relative agreement between scenario ranks\nlog(KW(SMH)/KW(null))") +
  theme_bw(base_size = bs) + 
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(), 
        strip.background = element_blank())


#### FIGURE 5B -----------------------------------------------------------------
# summarize SMH agreement results relative to null results
null_agreement_sig <- null_agreement %>%
  rename(kendall_w_null = kendall_w) %>%
  rename(icc_a_nonzero_null = icc_a_nonzero) %>%
  .[, c("round", "target", "target_end_date", "location", "quantile", 
        "sim_id", "icc_a_nonzero_null", "kendall_w_null")] %>%
  .[SMH_agreement %>%
      .[, horiz := (target_end_date - min(target_end_date))/7 + 1, by = .(round)] %>%
      .[, max_horiz := max(horiz), by =.(round)] %>%
      .[quantile ==0.5 &                         # only use medians
          target == "cum hosp" &                 # cumulative hosp only
          horiz == max_horiz                     # maximum horizon only
      ]%>%
      .[, c("round", "target", "target_end_date", "location", "quantile", 
            "icc_a_nonzero", "kendall_w")], 
    on = .(round, target, target_end_date, location, quantile)] %>%
  # summarize each probability
  .[, ":=" (kendall_w_flag = ifelse(kendall_w_null >= kendall_w, 1, 0), 
            icc_flag = ifelse(icc_a_nonzero_null >= icc_a_nonzero, 1, 0))] %>%
  .[, .(marginal_p_icc = sum(icc_flag)/.N, 
        marginal_p_kw = sum(kendall_w_flag)/.N, 
        joint_p = sum(icc_flag * kendall_w_flag)/.N), 
    by = .(round, target, target_end_date, location, quantile)]

stat_name_labs <- c("% of null samples with higher ICC", 
                    "% of null samples with higher Kendall's W")
names(stat_name_labs) <- c("sig_pct_icc", 
                           "sig_pct_kw")

pB <- null_agreement_sig %>% 
  .[, ":=" (sig_pct_icc = ifelse(marginal_p_icc < 0.05, "<5%", 
                                 ifelse(marginal_p_icc < 0.1, "5%-10%", 
                                        ifelse(marginal_p_icc < 0.25, "10%-25%",
                                               ifelse(marginal_p_icc < 0.75, "25%-75%",
                                                      ifelse(marginal_p_icc < 0.9, "75%-90%", 
                                                             ifelse(marginal_p_icc < 0.95, "90%-95%", ">95%")))))),
            sig_pct_kw = ifelse(marginal_p_kw < 0.05, "<5%", 
                                ifelse(marginal_p_kw < 0.1, "5%-10%", 
                                       ifelse(marginal_p_kw < 0.25, "10%-25%",
                                              ifelse(marginal_p_kw < 0.75, "25%-75%",
                                                     ifelse(marginal_p_kw < 0.9, "75%-90%", 
                                                            ifelse(marginal_p_kw < 0.95, "90%-95%", ">95%")))))))] %>%
  .[, c("round", "target", "target_end_date", "location", "quantile", "sig_pct_icc", "sig_pct_kw")] %>%
  data.table::melt(c("round", "target", "target_end_date", "location", "quantile")) %>%
  .[, value := factor(value, levels = c("<5%","5%-10%","10%-25%","25%-75%", "75%-90%", "90%-95%", ">95%"))] %>%
  ggplot(aes(x = as.factor(round))) + 
  geom_bar(aes(fill = as.factor(value)), position = "stack") + 
  guides(fill = guide_legend(nrow = 2)) + 
  facet_wrap(vars(variable), 
             labeller = labeller(variable = stat_name_labs),
             ncol = 1) + 
  scale_fill_brewer(palette = "YlGn") +
  scale_x_discrete(expand = c(0,0), 
                   name = "SMH round") + 
  scale_y_continuous(expand = c(0,0), 
                     name = "number of locations") +
  theme_bw(base_size = bs) + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        panel.grid = element_blank(),
        strip.background = element_blank())

#### COMBINE -------------------------------------------------------------------
plot_grid(pA, pB,  
          labels = LETTERS[1:2], 
          label_size = bs, 
          rel_widths = c(0.695, 0.305)
          ) 
ggsave("figures/Figure5.pdf", width = 190, height = 155, units = "mm")


#### VALUES FOR TEXT -----------------------------------------------------------
# null correlations
null_agreement %>% 
  .[, .(cr = cor(kendall_w, icc_a_nonzero)),  
    by = .(round, target, target_end_date, location, quantile)] %>%
  .[cr > 0.8] %>% 
  nrow()

null_agreement %>% 
  .[, .(cr = cor(kendall_w, icc_a_nonzero)),  
    by = .(round, target, target_end_date, location, quantile)] %>%
  .[cr > 0.8] %>% 
  nrow()/728

# mean % change in ICC
comp_null_smh %>% 
  .[!is.infinite(rel_change_icc)] %>% 
  .[ , .(mean_change = 1-exp(mean(rel_change_icc)))]

# % change in ICC by round
comp_null_smh %>% 
  .[!is.infinite(rel_change_icc)] %>% 
  .[ , .(mean_change = 1-exp(mean(rel_change_icc))), 
     by = .(round)] %>% 
  .[order(mean_change)]

# mean % change in KW
comp_null_smh %>% 
  .[!is.infinite(rel_change_kw)] %>% 
  .[ , .(mean_change = exp(mean(rel_change_kw)))]

# % change in KW by round
comp_null_smh %>% 
  .[!is.infinite(rel_change_kw)] %>% 
  .[ , .(mean_change = exp(mean(rel_change_kw))), 
     by = .(round)] %>% 
  .[order(mean_change)]

# % of locations with SMH ICC lower than 90% of null sims
nrow(null_agreement_sig %>% 
  .[marginal_p_icc>0.90])

nrow(null_agreement_sig %>% 
       .[marginal_p_icc>0.90])/nrow(null_agreement_sig)

# by round
null_agreement_sig %>% 
       .[, flg := ifelse(marginal_p_icc>0.90, 1, 0)] %>% 
  .[, .(f = sum(flg), 
        n = .N, 
        p = sum(flg)/.N), 
    by = .(round)] %>% 
  .[order(round)] %>%
  #.[round %in% 1:6] %>% 
  .[round %in% 13:16] %>%
  .[, .(f = sum(f), 
        n = sum(n), 
        p = sum(f)/sum(n))]

# % of locations with SMH KW higher than 90% of null sims
nrow(null_agreement_sig %>% 
       .[marginal_p_kw<0.10])

nrow(null_agreement_sig %>% 
       .[marginal_p_kw<0.10])/nrow(null_agreement_sig)

# by round
null_agreement_sig %>% 
  .[, flg := ifelse(marginal_p_kw<0.10, 1, 0)] %>% 
  .[, .(f = sum(flg), 
        n = .N, 
        p = sum(flg)/.N), 
    by = .(round)] %>% 
  .[order(round)] %>%
  #.[round %in% 1:6] %>% 
  .[round %in% 13:16] %>%
  .[, .(f = sum(f), 
        n = sum(n), 
        p = sum(f)/sum(n))]

