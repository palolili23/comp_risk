results_wide <- results_ipwcs %>%
    filter(is.na(sample)) %>%
    select(time, exposure, mean_survival) %>% 
    mutate(exposure = ifelse(exposure == 1, "exposure", "control")) %>% 
    spread(exposure, mean_survival) %>% 
    mutate(
      rd = round((exposure - control),4),
      rr = round((exposure)/(control),4))
  
boots_wide <- results_ipwcs %>% 
    filter(!is.na(sample)) %>% 
  select(time, sample, exposure, mean_survival) %>% 
  mutate(exposure = ifelse(exposure == 1, "exposure", "control")) %>% 
    spread(exposure, mean_survival) %>% 
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
    `Risk Ratio` = rr)