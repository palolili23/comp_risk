# Risk difference and risk ratio ------------------------------------------

risk_diff_ratio <- function(data_results){
  results_wide <- data_results %>%
      filter(is.na(sample)) %>%
      select(-c(mean_survival, sample)) %>% 
    mutate(exposure = ifelse(exposure == 1, "exposure", "control")) %>% 
    spread(exposure, mean_cif) %>% 
    mutate(
      rd = round((exposure - control),4),
      rr = round((exposure)/(control),4))
  
  boots_wide <- data_results %>% 
    filter(!is.na(sample)) %>% 
    select(-mean_survival) %>% 
    mutate(exposure = ifelse(exposure == 1, "exposure", "control")) %>% 
    spread(exposure, mean_cif) %>% 
    mutate(
      rd = round((exposure - control),4),
      rr = round((exposure)/(control),4)) %>% 
    group_by(time) %>% 
    summarise(rd_min = quantile(rd, 0.025),
              rd_max = quantile(rd, 0.975),
              rr_min = quantile(rr, 0.025),
              rr_max = quantile(rr, 0.975))
  
  risk_difference <- results_wide %>% 
    left_join(boots_wide) %>% 
    select(time, exposure, control, starts_with("rd"), starts_with("rr")) %>% 
    mutate_if(is.numeric, round, 3) %>% 
    unite(`RD CI95%`, c("rd_min", "rd_max"), sep =", ", remove = TRUE) %>%
    unite(`RR CI95%`, c("rr_min", "rr_max"), sep =", ", remove = TRUE) %>%
    rename(
      `Risk Ya=1` = exposure,
      `Risk Ya=0` = control,
      `Risk Rifference` = rd,
      `Risk Ratio` = rr
    )
  
  return(risk_difference)
}


surv_diff_ratio <- function(data_results){
  
  results_wide <- data_results %>%
    filter(is.na(sample)) %>%
    select(-c(mean_cif, sample)) %>% 
    mutate(exposure = ifelse(exposure == 1, "exposure", "control")) %>% 
    spread(exposure, mean_survival) %>% 
    mutate(
      rd = round(100*((1-exposure) - (1-control)),2),
      rr = round((1-exposure)/(1-control),2))
  
  boots_wide <- data_results %>% 
    filter(!is.na(sample)) %>% 
    select(-mean_cif) %>% 
    mutate(exposure = ifelse(exposure == 1, "exposure", "control")) %>% 
    spread(exposure, mean_survival) %>% 
    mutate(rd = round(100*((1-exposure) - (1-control)),2),
           rr = round((1-exposure)/(1-control),2)) %>% 
    group_by(time) %>% 
    summarise(rd_min = quantile(rd, 0.025),
              rd_max = quantile(rd, 0.975),
              rr_min = quantile(rr, 0.025),
              rr_max = quantile(rr, 0.975))
  
  surv_difference <- results_wide %>% 
    left_join(boots_wide) %>% 
    select(time, starts_with("rd"), starts_with("rr"))
  
  return(surv_difference)
  
}
