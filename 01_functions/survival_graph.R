# Survival graph ----------------------------------------------------------

cif_curves <- function(results, bootsamples, control, intervention, title, xaxis, limit_end, breaks){
  test <- results %>%  
    mutate(risk = round((mean_cif), 4),
           exposure = ifelse(exposure == 0, control, intervention))
  
  test_boot <- bootsamples %>% 
    mutate(risk = round((mean_cif), 4),
           exposure = ifelse(exposure == 0, control, intervention)) %>% 
    group_by(exposure, time) %>% 
    summarise(risk_min = quantile(risk, 0.025),
              risk_max = quantile(risk, 0.975))
  
  test0 <- test %>% 
    filter(time == 0) %>% 
    mutate(risk = 0,
           risk_min = 0,
           risk_max = 0)
  
  test %>% 
    left_join(test_boot) %>% 
    mutate(time = time + 1) %>%
    bind_rows(test0) %>% 
    ggplot() +
    aes(x = time, y = risk, group = exposure) +
    geom_line(aes(color = exposure), size = 0.8) + 
    scale_color_manual(values=c("#235AA7", "#FFDB6D")) +
    geom_ribbon(aes(ymin = risk_min, ymax = risk_max ), alpha = 0.10) +
    labs(title = title,
         x = xaxis,
         y = "Cumulative incidence",
         color = "Intervention") +
    scale_x_continuous(limits = c(0, limit_end), breaks=seq(0,limit_end, breaks)) +
    theme_minimal() +
    theme(legend.position = 'bottom')
}

surv_curves <- function(results, bootsamples, control, intervention, title, xaxis, limit_end, breaks){
  test <- results %>%  
    mutate(risk = round((mean_survival), 4),
           exposure = ifelse(exposure == 0, control, intervention))
  
  test_boot <- bootsamples %>% 
    mutate(risk = round((mean_survival), 4),
           exposure = ifelse(exposure == 0, control, intervention)) %>% 
    group_by(exposure, time) %>% 
    summarise(risk_min = quantile(risk, 0.025),
              risk_max = quantile(risk, 0.975))
  
  test0 <- test %>% 
    filter(time == 0) %>% 
    mutate(risk = 1,
           risk_min = 1,
           risk_max = 1)
  
  test %>% 
    left_join(test_boot) %>% 
    mutate(time = time + 1) %>%
    bind_rows(test0) %>% 
    ggplot() +
    aes(x = time, y = risk, group = exposure) +
    geom_line(aes(color = exposure), size = 0.8) + 
    scale_color_manual(values=c("#235AA7", "#FFDB6D")) +
    geom_ribbon(aes(ymin = risk_min, ymax = risk_max ), alpha = 0.10) +
    labs(title = title,
         x = xaxis,
         y = "Survival probability",
         color = "Intervention") +
    scale_x_continuous(limits = c(0, limit_end), breaks=seq(0,limit_end, breaks)) +
    theme_minimal() +
    theme(legend.position = 'bottom')
}
