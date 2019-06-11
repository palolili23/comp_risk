# Functions ---------------------------------------------------------------
source("02_R/00_call_libraries.R")
source("01_functions/direct_ipw_prost.R")
source("01_functions/direct_gf_prost.R")
source("01_functions/bootsamples.R")
source("01_functions/risk_diff_ratio.R")
source("01_functions/survival_graph.R")

# Data setup --------------------------------------------------------------

data <- import("00_rawdata/prostate_data.RData")


data <- data %>% 
  filter(rx == 1 | rx == 4) %>% 
  mutate(rx = ifelse(rx == 4, 1, 0),
         death = case_when(
           status == 1 ~ 0,
           status == 2 ~ 1,
           TRUE ~ 2),
         death_pr = ifelse(death == 1, 1, 0),
         death_other = ifelse(death == 2, 1, 0),
         cens = ifelse(status == 1, 1, 0),
         age_f = case_when(
           age <= 59 ~ 1,
           age >59 & age < 75 ~ 2,
           age >= 75 ~ 3),
         age_f = as.factor(age_f),
         hg_f = ifelse(hg < 12, 1, 0),
         hg_f = as.factor(hg_f),
         pf_f = ifelse(pf == 1, 0, 1),
         pf_f = as.factor(pf_f)) %>% 
  rename(id = patno,
         max = dtime,
         exposure = rx,
         outcome = death_pr,
         competing = death_other) 

factors <- c("pf_f", "age_f", "hg_f", "hx")
factors_cr <- c("pf_f", "age_f", "hg_f", "hx", "competing_plr")
time_fx <-  "time^3"

data %>% filter(competing == 1) %>% 
  ggplot(aes(max)) + geom_histogram()

# Person time -------------------------------------------------------------

n_expanding_rows <- max(data$max) + 1
data_long <- data[rep(seq(nrow(data)), n_expanding_rows),]

data_long %<>% 
  group_by(id) %>% 
  mutate(
    time = row_number() - 1,
    outcome_plr = ifelse(time == max, outcome, 0),
    competing_plr = ifelse(time == max, competing, 0),
    no_cr = 1 - competing_plr,
    cens_plr = ifelse(time == max, cens, 0),
    no_cens = 1 - cens) %>% 
  filter(time <= max) %>%
  filter(time <60) %>% 
  ungroup() %>% 
  arrange(id, time)

# Direct effects with g-formula Pr(Ya,c=0,d=0) -------------------------------------------------------------------------
results <- direct_gf(data, factors, time_fx)

bootresults <- bootsamples(data, 100, factors,
                           time_fx, surv_model = direct_gf)

effect_measures <- risk_diff_ratio(results, bootresults)

# surv_curves(results, bootresults, control = "a",
#        intervention = "b",
#        title = "test gform",
#        xaxis = "months",
#        limit_end = 60,
#        breaks = 10)

cif_curves(results, bootresults, control = "placebo",
           intervention = "High-dose DES",
           title = "Direct effect with G-Formula",
           xaxis = "months",
           limit_end = 60,
           breaks = 10)

# Direct effect IPW (Only WD) ---------------------------------------------

res_ipw_cr <- direct_ipw(data, factors, time_fx)
boots_ipw_cr <- bootsamples(data, 100, factors, time_fx, direct_ipw)


effect_measures_ipw_cr <- risk_diff_ratio(res_ipw_cr, boots_ipw_cr)

# surv_curves(res_ipw_cr, boots_ipw_cr, control = "placebo",
#        intervention = "control",
#        title = "Direct effect with IPW",
#        xaxis = "months",
#        limit_end = 60,
#        breaks = 10)

cif_curves(res_ipw_cr, boots_ipw_cr, control = "placebo",
            intervention = "High-dose DES",
            title = "Direct effect with IPW",
            xaxis = "months",
            limit_end = 60,
            breaks = 10)


effect_measures %>% filter(time == 60)
effect_measures_ipw_cr %>% filter(time == 60)

# Total effects with g-formula Pr(Ya,c=0) ------------------------------------------------------------------

res_cr <- survival_cr(data, factors, time_fx)
boots_cr <- bootsamples(data, 10, factors, time_fx, survival_cr)


effect_measures_cr <- risk_diff_ratio(res_cr, boots_cr)

surv_curves(res_cr, boots_cr, control = "a",
            intervention = "b",
            title = "test gform with cr",
            xaxis = "months",
            limit_end = 70,
            breaks = 10)


cif_curves(res_cr, boots_cr, control = "a",
           intervention = "b",
           title = "test gform with cr",
           xaxis = "months",
           limit_end = 70,
           breaks = 10)