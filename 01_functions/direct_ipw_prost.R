direct_ipw <- function(data, factors, time_fx) {
  
  #transform from wide to long and create necessary variables
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
  
  # Create weights ----------------------------------------------------------
  num_cr <- glm(no_cr ~ 1, data=data_long, family=binomial())
  model_denom_cr <- reformulate(termlabels = factors, response = "no_cr")
  denom_cr <- glm(model_denom_cr, data=data_long, family=binomial())
  
  num_cens <- glm(no_cens ~ 1, data = subset(data_long, time>50), family=binomial())
  model_denom_cens <- reformulate(termlabels = factors, response = "no_cens")
  denom_cens <- glm(model_denom_cens, data=subset(data_long, time>50), family=binomial())
  
  data_long %<>%
    mutate(cr_num = predict(num_cr, data_long, type="response"),
           cr_denom = predict(denom_cr, data_long, type="response"),
           cens_num = predict(num_cens, data_long, type="response"),
           cens_num = ifelse(time <= 50, 1, cens_num),
           cens_denom = predict(denom_cens, data_long, type="response"),
           cens_denom = ifelse(time <= 50, 1, cens_denom)) %>% 
    group_by(id) %>% 
    mutate(cr_num_cum = cumprod(cr_num),
           cr_denom_cum = cumprod(cr_denom),
           cens_num_cum = cumprod(cens_num),
           cens_denom_cum = cumprod(cens_denom)) %>% 
    ungroup() %>% 
    mutate(cr_sw = cr_num_cum/cr_denom_cum, 
           cens_sw = cens_num_cum/cens_denom_cum,
           sw = cr_sw*cens_sw)
  
    data_long <- data_long %>% 
    mutate(sw = ifelse(sw >10, 10, sw))
  
  # fit of weighted hazards model
  model <- as.formula(paste(glue("outcome_plr ~ (exposure) * (time + I({time_fx})) +"), paste(factors, collapse="+")))
  adj_plr <- glm(model, data = data_long, family = quasibinomial(), 
                 weights = sw)
  #create clones
  data0 <- data1 <- data[rep(seq(nrow(data)), n_expanding_rows),]
  
  #predict probabilities when exposure = 0
  data0 %<>% 
    group_by(id) %>% 
    mutate(exposure = 0,
           time = (row_number()-1)) %>% 
    ungroup()
  
  data0 <- data0 %>%
    mutate(p = 1 - predict(adj_plr, newdata = data0, type = "response")) %>%
    arrange(id, time) %>% 
    group_by(id) %>% 
    mutate(s = cumprod(p),
           cif = 1 - s) %>% 
    ungroup()
  
  #predict probabilities when exposure = 1
  data1 %<>% 
    group_by(id) %>% 
    mutate(exposure = 1,
           time = (row_number()-1)) %>% 
    ungroup() 
  
  data1 %<>% 
    mutate(p = 1 - predict(adj_plr, newdata = data1, type = "response")) %>%
    arrange(id, time) %>% 
    group_by(id) %>% 
    mutate(s = cumprod(p),
           cif = 1 - s) %>% 
    ungroup()
  
  #combine sets and estimate the mean_survival
  results <- data0 %>% 
    bind_rows(data1) %>% 
    select(time, exposure, s, cif) %>% 
    group_by(time, exposure)%>%
    summarize(mean_survival = mean(s),
              mean_cif = mean(cif)) %>% 
    ungroup()
  
  return(results)
  
}
