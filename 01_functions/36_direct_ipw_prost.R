direct_ipw_pr <- function(data,
                          factors_outcome,
                          factors_cens,
                          factors_cr,
                          rows = max(data$max)) {
  #transform from wide to long and create necessary variables
  
  # To go from wide to long when only outcome changes over time, we will create as many rows as timepoints.
  
  n_expanding_rows <- rows
  
  data_long <- data[rep(seq(nrow(data)), n_expanding_rows), ]
  
  # Groups by ID, creates a "time" variable.
  # Outcome_plr, competing_plr and cens_plr are 0 until the last row, when they developed one of the events.
  # No_cr and no_cens are indicators for not having the competing event and not censored at each timepoint.
  # filter time<= max eliminates the extra rows in case their last timepoint is smaller than the time for complete follow up.
  
  data_long %<>%
    group_by(id) %>%
    mutate(
      time = row_number() - 1,
      outcome_plr = ifelse(time == max, outcome, 0),
      competing_plr = ifelse(time == max, competing, 0),
      no_cr = 1 - competing_plr,
      cens_plr = ifelse(time == max, cens, 0),
      no_cens = 1 - cens_plr,
    ) %>%
    filter(time <= max) %>%
    ungroup() %>%
    arrange(id, time)
  
  data_long %<>% 
    mutate(
      outcome_plr = case_when(
        cens_plr == 1 ~ NA_real_,
        competing_plr == 1 ~ NA_real_,
        TRUE ~ outcome_plr),
      competing_plr = ifelse(cens_plr == 1, NA, competing_plr))
  
  # Create weights ----------------------------------------------------------
  
  # fits a model for not having the competing event using the vector of parameters defined in the argument "factors_cr".
  # num_cr <- glm(no_cr ~ 1, data = data_long, family = binomial()) for stabilized weights
  
  model_denom_cr <-
    reformulate(termlabels = factors_cr, response = "no_cr")
  
  denom_cr <-
    glm(model_denom_cr, data = data_long, family = quasibinomial())
  
  # fits a model for not being censored using the vector of parameters defined in the argument "factors_cens".
  # num_cens <- glm(no_cens ~ 1, data = data_long, family = binomial()) for stabilized weights
  
  model_denom_cens <-
    reformulate(termlabels = factors_cens, response = "no_cens")
  
  denom_cens <-
    glm(model_denom_cens,
        data = subset(data_long, time >= 50 & no_cr == 1),
        family = quasibinomial())
  
  # We use the previous models to calculate the weights for C and D.
  
  data_long %<>%
    mutate(
      cr_num = 1,
      cr_denom = predict(denom_cr, data_long, type = "response"),
      cens_num = 1,
      cens_denom = predict(denom_cens, data_long, type = "response"),
      cens_denom = ifelse(time < 50, 1, cens_denom) #This is specific of this study because no one was censored before time 50) #because if you had the competing event you are not censored
    ) %>%
    group_by(id) %>%
    mutate(
      cr_num_cum = cumprod(cr_num),
      cr_denom_cum = cumprod(cr_denom),
      cens_num_cum = cumprod(cens_num),
      cens_denom_cum = cumprod(cens_denom)
    ) %>%
    ungroup() %>%
    mutate(
      cr_sw = cr_num_cum / cr_denom_cum,
      cens_sw = cens_num_cum / cens_denom_cum,
      cens_sw = ifelse(competing_plr == 1, 1, cens_sw),
      sw = cr_sw * cens_sw
    )
  
  # For weight truncation:
  # data_long <- data_long %>%
  #   mutate(sw = ifelse((sw > quantile(sw, 0.99)), quantile(sw, 0.99), sw))
  
  # fits a model for the outcome using the vector of parameters defined in the argument "factors_outcome"
  # This model should only include a paramater for the exposure, a flexible function of time and interaction between exposure and time
  # Uses the calculated weights.
  
  model <-
    reformulate(termlabels = factors_outcome, response = "outcome_plr")
  
  adj_plr <- glm(model,
                 data = data_long,
                 family = quasibinomial(),
                 weights = sw)
  
  # creates two cloned datasets with all timepoints for all individuals
  
  data0 <- data1 <- data[rep(seq(nrow(data)), n_expanding_rows),]
  
  # In one dataset we fix the exposure to 0 for all individuals.
  
  data0 %<>%
    group_by(id) %>%
    mutate(exposure = 0,
           time = (row_number() - 1)) %>%
    ungroup()
  
  # predict the survival probability of the outcome at each time when exposure = 0 for all individuals.
  # uses the model calculated on the observed data
  # Calculates the cumulative product of the probability of surviving at each time point, for each individual.
  # cif is the cumulative incidence of the outcome
  
  data0 <- data0 %>%
    mutate(p = 1 - predict(adj_plr, newdata = data0, type = "response")) %>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(s = cumprod(p),
           cif = 1 - s) %>%
    ungroup()
  
  # In this dataset we fix the exposure to 1 for all individuals
  
  data1 %<>%
    group_by(id) %>%
    mutate(exposure = 1,
           time = (row_number() - 1)) %>%
    ungroup()
  
  data1 %<>%
    mutate(p = 1 - predict(adj_plr, newdata = data1, type = "response")) %>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(s = cumprod(p),
           cif = 1 - s) %>%
    ungroup()
  
  #combine sets and estimate the average survival probability and average cumulative incidence at each time
  
  results <- data0 %>%
    bind_rows(data1) %>%
    select(time, exposure, s, cif) %>%
    group_by(time, exposure) %>%
    summarize(mean_survival = mean(s),
              mean_cif = mean(cif)) %>%
    ungroup()
  
  return(results)
}
