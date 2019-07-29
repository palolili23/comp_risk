treated <- treated %>% group_by(patno) %>% mutate(s_2 = cumprod(s)) %>% ungroup() 

surv_prob2 <- treated %>% pull(s_2) 

surv_prob2 <- matrix(surv_prob2, nrow = 60, ncol = 252)

max(cumsum(survivalProb == surv_prob2))



treated <- treated %>% group_by(patno) %>% mutate(s_lag = lag(s_2)) %>% ungroup()
treated <- treated %>% mutate(cuminc = hazardO * s_lag)
treated <- treated %>% mutate(cuminc = ifelse(is.na(cuminc), hazardO, cuminc))

cuminc2 <- treated %>% pull(cuminc)
cuminc2 <- matrix(cuminc2, nrow= 60, ncol = 252)

max(cumsum(cumulativeIncidence == cuminc2))
