total_ipwcs_pr <- function(data,
                          factors_outcome,
                          factors_cr,
                          factors_cens,
                          rows = max(data$max)) {
  
  #transform from wide to long and create necessary variables
  n_expanding_rows <- rows
  
  data_long <- data[rep(seq(nrow(data)), rows),]
  
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
  model_denom_cens <- 
    reformulate(termlabels = factors_cens, response = "no_cens")
  denom_cens <- 
    glm(
    model_denom_cens,
    data = subset(data_long, time > 50),
    family = quasibinomial()
  )
  
  data_long %<>%
    mutate(
      cens_num = 1,
      cens_denom = predict(denom_cens, data_long, type = "response"),
      cens_denom = ifelse(time <= 50, 1, cens_denom)
    ) %>%
    group_by(id) %>%
    mutate(cens_num_cum = 1,
           cens_denom_cum = cumprod(cens_denom)) %>%
    ungroup() %>%
    mutate(sw = cens_num_cum / cens_denom_cum)
  
  # data_long <- data_long %>%
  #   mutate(sw = ifelse((sw > quantile(sw, 0.95)), quantile(sw, 0.95), sw))
  
  # fit of weighted hazards model
  model_y <- reformulate(termlabels = factors_outcome, response = "outcome_plr")
  adj_plr_y <- glm(model_y, data = data_long, family = quasibinomial(), weights = sw)
  
  model_cr <- reformulate(termlabels = factors_cr, response = "competing_plr")
  adj_plr_cr <- glm(model_cr, data = data_long, family = quasibinomial(), weights = sw)

  #create clones
  data0 <- data1 <- data[rep(seq(nrow(data)), rows),]
  
  #predict probabilities when exposure = 0
  data0 %<>%
    group_by(id) %>%
    mutate(exposure = 0,
           time = (row_number() - 1)) %>%
    ungroup()
  
  data0 <- data0 %>%
    mutate(py = predict(adj_plr_y, newdata = data0, type = "response"),
           pd = predict(adj_plr_cr, newdata = data0, type = "response")) %>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(
      s = (1 - py)*(1 - pd),
      cum_s = cumprod(s),
      s_lag = lag(cum_s),
      cif = py * (1 - pd) * s_lag, 
      cif = ifelse(is.na(cif), (py * (1 - pd)), cif), #This is for time 0
      cif_cum = cumsum(cif)) %>%
    ungroup()
  
  #predict probabilities when exposure = 1
  data1 %<>%
    group_by(id) %>%
    mutate(exposure = 1,
           time = (row_number() - 1)) %>%
    ungroup()
  
  data1 %<>%
    mutate(py = predict(adj_plr_y, newdata = data1, type = "response"),
           pd = predict(adj_plr_cr, newdata = data1, type = "response")) %>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(
      s = (1 - py)*(1 - pd),
      cum_s = cumprod(s),
      s_lag = lag(cum_s),
      cif = py * (1 - pd) * s_lag, 
      cif = ifelse(is.na(cif), (py * (1 - pd)), cif), #This is for time 0
      cif_cum = cumsum(cif)) %>%
    ungroup()
  
  #combine sets and estimate the mean_survival
  results <- data0 %>%
    bind_rows(data1) %>%
    select(time, exposure, cum_s, cif_cum) %>%
    group_by(time, exposure) %>%
    summarize(mean_survival = mean(cum_s),
              mean_cif = mean(cif_cum)) %>%
    ungroup()

  return(results)
}
