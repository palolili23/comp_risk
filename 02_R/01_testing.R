# Functions ---------------------------------------------------------------
source("02_R/00_call_libraries.R")
source("01_functions/risk_diff_ratio.R")
source("01_functions/survival_graph.R")


# Data setup --------------------------------------------------------------

data <- import("00_rawdata/prostate.csv")

data <- data %>%
  filter(rx == "placebo" | rx == "5.0 mg estrogen") %>%
  mutate(
    rx = ifelse(rx == "5.0 mg estrogen", 1, 0),
    death = case_when(status == "alive" ~ 0,
                      status == "dead - prostatic ca" ~ 1,
                      TRUE ~ 2),
    death_pr = ifelse(death == 1, 1, 0),
    death_other = ifelse(death == 2, 1, 0),
    cens = ifelse(status == "alive", 1, 0),
    age_f = case_when(age < 60 ~ 1,
                      age >= 60 & age < 75 ~ 2,
                      age >= 75 ~ 3),
    age_f = as.factor(age_f),
    hg_f = ifelse(hg < 12, 1, 0),
    hg_f = as.factor(hg_f),
    pf_f = ifelse(pf == "normal activity", 1, 0),
    pf_f = as.factor(pf_f)
  ) %>%
  rename(
    id = patno,
    max = dtime,
    exposure = rx,
    outcome = death_pr,
    competing = death_other
  ) 
# Define arguments

y_model <- c("exposure*(time + I(time^2) + I(time^3))", "pf_f", "age_f", "hg_f", "hx")
d_model <- c("exposure", "time", "I(time^2)", "pf_f", "age_f", "hg_f", "hx")
c_model <- c("exposure", "pf_f", "age_f", "hg_f")


# data %>% filter(cens == 1) %>% 
#   ggplot(aes(max)) + geom_histogram()
# data %>% filter(competing == 1) %>% 
#   ggplot(aes(max)) + geom_histogram()

# Person time -------------------------------------------------------------

# n_expanding_rows <- max(data$max) + 1
# data_long <- data[rep(seq(nrow(data)), n_expanding_rows),]
# 
# data_long %<>% 
#   group_by(id) %>% 
#   mutate(
#     time = row_number() - 1,
#     outcome_plr = ifelse(time == max, outcome, 0),
#     competing_plr = ifelse(time == max, competing, 0),
#     no_cr = 1 - competing_plr,
#     cens_plr = ifelse(time == max, cens, 0),
#     no_cens = 1 - cens) %>% 
#   filter(time <= max) %>%
#   filter(time <60) %>% 
#   ungroup() %>% 
#   arrange(id, time)
# 

# Direct effect IPW (Only WD) ---------------------------------------------

source("01_functions/36_direct_ipw_prost.R")
source("01_functions/36_bootsamples.R")
source("01_functions/36_wrapper.R")

##Eventually needs a a_model for the weights on the exposure

y_model <- c("exposure*(time + I(time^2) + I(time^3))") 
d_model <- c("exposure", "time", "I(time^2)", "pf_f", "age_f", "hg_f", "hx")
c_model <- c("exposure", "pf_f", "age_f", "hx")

results_ipw <- direct_ipw_pr_helper(
  data,
  factors_outcome = y_model,
  factors_cens = c_model,
  factors_cr = d_model,
  rows = 60,
  seed = 123,
  n = 50)

effect_measures_ipw_cr <- risk_diff_ratio(results_ipw)

cif_curves(
  results_ipw,
  control = "placebo",
  intervention = "High-dose DES",
  title = "Direct effect of DES in Prostate cancer death with IPW",
  xaxis = "months",
  limit_end = 60,
  breaks = 10,
  max_cif = 0.6
)

# surv_curves(results_ipw, breaks = 10, limit_end = 60)

effect_measures_ipw_cr %>% filter(Time == 59)

# Direct effects with g-formula Pr(Ya,c=0,d=0) -------------------------------------------------------------------------
source("01_functions/gformula.R")
source("01_functions/gformula_boot.R")
source("01_functions/gformula_wrapper.R")

output_dir_gf <- gformula_wrapper(data,
                                    factors_outcome = y_model,
                                    rows = 60, 
                                    n = 100, 
                                    seed = 123)

y_model <- c("exposure*(time + I(time^2) + I(time^3))", "pf_f", "age_f", "hg_f", "hx")

effect_measures_dir_gf <- risk_diff_ratio(output_dir_gf)

cif_curves(
  output_dir_gf,
  control = "placebo",
  intervention = "High-dose DES",
  title = "Direct effect of DES in Prostate cancer death with G-formula",
  xaxis = "months",
  limit_end = 60,
  breaks = 10,
  max_cif = 0.6
)

effect_measures_dir_gf %>% filter(Time ==59)

# Total effects with IPW: Cause-specific hazard approach -----------------
source("01_functions/39_total_ipwcs_prost.R")
source("01_functions/39_bootsamples.R")
source("01_functions/39_wrapper.R")

y_model <- c("exposure*(time + I(time^2) + I(time^3))") 
c_model <- c("exposure", "pf_f", "age_f", "hx")
d_model <- c("exposure", "time", "I(time^2)")

results_ipwcs <- total_ipwcs_helper(data, factors_outcome = y_model,
                               factors_cens = c_model,
                               factors_cr = d_model,
                               rows = 60,
                               n = 100, seed = 123)


effect_measures_ipwcs <- risk_diff_ratio(results_ipwcs)


cif_curves(results_ipwcs, control = "placebo",
           intervention = "High-dose DES",
           title = "Total effect of DES in Prostate cancer death with IPWcs",
           xaxis = "months",
           limit_end = 60,
           breaks = 10,
           max_cif = 0.5)

# surv_curves(results_ipwcs, breaks = 10)
effect_measures_ipwcs %>% filter (Time ==59)


# Total effects with IPW sub-hazard approach ------------------------------------------------
source("01_functions/38_total_ipwsh_prost.R")
source("01_functions/38_bootsamples.R")
source("01_functions/38_wrapper.R")

y_model <- c("exposure*(time + I(time^2) +  I(time^3))")

c_model <- c("exposure", "pf_f", "age_f", "hx")
number_rows <- 60


results_ipwsh <- total_ipwsh_helper(data, factors_outcome = y_model,
                                   factors_cens = c_model,
                                   rows = number_rows,
                                   n = 50, seed = 123)


effect_measures_ipwsh <- risk_diff_ratio(results_ipwsh)

cif_curves(results_ipwsh, control = "placebo",
           intervention = "High-dose DES",
           title = "Total effect of DES in Prostate cancer death with IPWsh",
           xaxis = "months",
           limit_end = 60,
           breaks = 10,
           max_cif = 0.6)

# surv_curves(output_total, breaks = 10)
effect_measures_ipwsh %>% filter(Time ==59)


# Total effects with G-formula ------------------------------------------------

factors_outcome <- c("exposure*(time + I(time^2) + I(time^3))", "pf_f", "age_f", "hg_f", "hx")
factors_cr <- c("exposure", "time", "I(time^2)", "pf_f", "age_f", "hg_f", "hx")

results_totalgf <- gformula_wrapper(data,
                             factors_outcome = factors_outcome,
                             factors_cr = factors_cr,
                             rows = 60,
                             n = 100, 
                             seed = 123)

effect_measures_gf <- risk_diff_ratio(results_totalgf) 

effect_measures_gf %>% filter(Time ==59)

cif_curves(results_totalgf, control = "placebo",
           intervention = "High-dose DES",
           title = "Total effect of DES in Prostate cancer death with G-formula",
           xaxis = "months",
           limit_end = 60,
           breaks = 10)

# surv_curves(results_totalgf, breaks = 10)

effect_measures_ipw_cr %>% filter(Time == 59) %>% 
  mutate(estimate = "direct ipw") %>% 
  bind_rows((effect_measures_dir_gf %>% filter(Time ==59) %>%  mutate(estimate = "direct G-f")),
            (effect_measures_ipwsh %>% filter(Time ==59) %>%  mutate(estimate = "Total IPWsh")),
            (effect_measures_ipwcs %>% filter(Time ==59) %>%  mutate(estimate = "Total IPWcs")), 
            (effect_measures_gf %>% filter(Time ==59)) %>%  mutate(estimate = "Total G-f"))
 
