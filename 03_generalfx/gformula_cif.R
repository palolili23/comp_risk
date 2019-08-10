gformula_cif <- function(data,
                         factors_outcome,
                         factors_cr = NULL,
                         rows = max(data$max)) {
  #transform from wide to long and create necessary variables
  
  data_long <- data[rep(seq(nrow(data)), rows),]
  
  data_long %<>%
    group_by(id) %>%
    mutate(
      time = row_number() - 1,
      outcome_plr = ifelse(time == max, outcome, 0),
      cens_plr = ifelse(time == max, cens, 0)
    ) %>%
    filter(time <= max) %>%
    ungroup() %>%
    arrange(id, time)
  
  if (!is.null(factors_cr)) {
    data_long %<>%
      mutate(competing_plr = ifelse(time == max, competing, 0))
  }
  
  if (!is.null(factors_cr)) {
    data_long %<>%
      mutate(
        outcome_plr = case_when(
          cens_plr == 1 ~ NA_real_,
          competing_plr == 1 ~ NA_real_,
          TRUE ~ outcome_plr
        ),
        competing_plr = ifelse(cens_plr == 1, NA, competing_plr)
      )
  } else{
    data_long %<>%
      mutate(outcome_plr = case_when(cens_plr == 1 ~ NA_real_,
                                     TRUE ~ outcome_plr))
  }
  
  #fit the model
  model_y <-
    reformulate(termlabels = factors_outcome, response = "outcome_plr")
  adj_plr_y <- glm(model_y, data = data_long, family = binomial)
  
  if (!is.null(factors_cr)) {
    model_cr <-
      reformulate(termlabels = factors_cr, response = "competing_plr")
    adj_plr_cr <- glm(model_cr, data = data_long, family = binomial)
  }
  
  #create clones
  data0 <- data1 <- data[rep(seq(nrow(data)), rows),]
  
  #predict probabilities when exposure = 0
  data0 %<>%
    group_by(id) %>%
    mutate(exposure = 0,
           time = (row_number() - 1)) %>%
    ungroup()
  
  data0 %<>%
    mutate(py = predict(adj_plr_y, newdata = data0, type = "response"))
  
  if (!is.null(factors_cr)) {
    data0 %<>%
      mutate(pd = predict(adj_plr_cr, newdata = data0, type = "response"))
  } else{
    data0 %<>%
      mutate(pd = 0)
  }
  
  data0 %<>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(
      s = (1 - py) * (1 - pd),
      cum_s = cumprod(s),
      s_lag = lag(cum_s),
      cif = py * (1 - pd) * s_lag,
      cif = ifelse(is.na(cif), (py * (1 - pd)), cif),
      #This is for time 0
      cif_cum = cumsum(cif)
    ) %>%
    ungroup()
  
  #predict probabilities when exposure = 1
  data1 %<>%
    group_by(id) %>%
    mutate(exposure = 1,
           time = (row_number() - 1)) %>%
    ungroup()
  
  data1 %<>%
    mutate(py = predict(adj_plr_y, newdata = data1, type = "response"))
  
  if (!is.null(factors_cr)) {
    data1 %<>%
      mutate(pd = predict(adj_plr_cr, newdata = data1, type = "response"))
  } else{
    data1 %<>%
      mutate(pd = 0)
  }
  
  data1 %<>%
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(
      s = (1 - py) * (1 - pd),
      cum_s = cumprod(s),
      s_lag = lag(cum_s),
      cif = py * (1 - pd) * s_lag,
      cif = ifelse(is.na(cif), (py * (1 - pd)), cif),
      cif_cum = cumsum(cif)
    ) %>%
    ungroup()
  
  #combine sets and estimate the mean_survival
  results <- data0 %>%
    bind_rows(data1) %>%
    select(time, exposure, cum_s, cif_cum) %>%
    group_by(exposure, time) %>%
    summarize(mean_survival = mean(cum_s),
              mean_cif = mean(cif_cum)) %>%
    ungroup()
  
  return(results)
  
}
