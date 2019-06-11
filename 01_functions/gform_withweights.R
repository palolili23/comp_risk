survival_gf_wt <- function(data, adj_model) {
  
  #transform from wide to long and create necessary variables
  n_expanding_rows <- max(data$max) + 1
 
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
           cif = 1-s) %>% 
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
           cif = 1-s) %>% 
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
