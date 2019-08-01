source("01_functions/39_ipwcs_v02.R")
source("01_functions/39_ipwcs_v02_bootsamples.R")
source("01_functions/39_ipwcs_v02_wrapper.R")
source("01_functions/risk_diff_ratio.R")
source("01_functions/survival_graph.R")

# Import data -------------------------------------------------------------

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

# Test --------------------------------------------------------------------

y_model <- c("exposure*(time + I(time^2) + I(time^3))") 
c_model <- c("exposure", "pf_f", "age_f", "hx")
d_model <- c("exposure", "time", "I(time^2)")

test_unstabilized_weights_notrunc <-
  total_ipwcs_helper(
    data,
    factors_outcome = y_model,
    factors_cr = d_model,
    factors_cens = c_model,
    rows = 60, n = 10)

results_A <- test_unstabilized_weights %>% pluck(1)
effect_measurements_A <- risk_diff_ratio(results_A)

test_unstabilized_weights_notrunc %>% pluck(2)
test_unstabilized_weights_notrunc %>% pluck(3)


cens_numerator <- "exposure"

test_stabilized_weights_notrunc <-
  total_ipwcs_helper(
    data,
    factors_outcome = y_model,
    factors_cr = d_model,
    factors_cens = c_model,
    factors_num_cens = cens_numerator,
    rows = 60, n = 10)

test_stabilized_weights_notrunc %>% pluck(1)
test_stabilized_weights_notrunc %>% pluck(2)
test_stabilized_weights_notrunc %>% pluck(3)

# With truncation ---------------------------------------------------------
test_unstabilized_weights_trunc <-
  total_ipwcs_helper(
    data,
    factors_outcome = y_model,
    factors_cr = d_model,
    factors_cens = c_model,
    truncation_percentile = 0.99,
    rows = 60, n = 10)

test_unstabilized_weights_trunc %>% pluck(1)
test_unstabilized_weights_trunc %>% pluck(2)
test_unstabilized_weights_trunc %>% pluck(3)


test_stabilized_weights_trunc <-
  total_ipwcs_helper(
    data,
    factors_outcome = y_model,
    factors_cr = d_model,
    factors_cens = c_model,
    factors_num_cens = cens_numerator,
    truncation_percentile = 0.99,
    rows = 60, n = 10)

test_stabilized_weights_trunc %>% pluck(1)
test_stabilized_weights_trunc %>% pluck(2)
test_stabilized_weights_trunc %>% pluck(3)
