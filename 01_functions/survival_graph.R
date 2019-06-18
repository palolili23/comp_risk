# CIF graph ---------------------------------------------------------------
cif_curves <-
  function(data_results,
           control,
           intervention,
           title,
           xaxis,
           limit_end,
           breaks){
    results <- data_results %>%
      filter(is.na(sample)) %>%
      select(-sample) %>%
      mutate(risk = round((mean_cif), 4),
             exposure = ifelse(exposure == 0, control, intervention))
    
    boot_samples <- data_results %>%
      filter(!is.na(sample)) %>%
      mutate(risk = round((mean_cif), 4),
             exposure = ifelse(exposure == 0, control, intervention)) %>%
      group_by(exposure, time) %>%
      summarise(risk_min = quantile(risk, 0.025),
                risk_max = quantile(risk, 0.975))
    
    results0 <- results %>%
      filter(time == 0) %>%
      mutate(risk = 0,
             risk_min = 0,
             risk_max = 0)
    
    results %<>%
      left_join(boot_samples) %>%
      mutate(time = time + 1) %>%
      bind_rows(results0) 
    
    results %>%
            ggplot() +
      aes(x = time, y = risk, group = exposure) +
      geom_line(aes(color = exposure), size = 0.8) +
      scale_color_manual(values = c("#235AA7", "#FFDB6D")) +
      geom_ribbon(aes(ymin = risk_min, ymax = risk_max), alpha = 0.10) +
      labs(
        title = title,
        x = xaxis,
        y = "Cumulative incidence",
        color = "Intervention"
      ) +
      scale_x_continuous(limits = c(0, limit_end),
                         breaks = seq(0, limit_end, breaks)) +
      theme_minimal() +
      theme(legend.position = 'bottom')
  }

# Survival graph ----------------------------------------------------------

surv_curves <- function(data_results,
           control = "control",
           intervention = "intervention",
           title = "Direct effect using IPW",
           xaxis = "Time",
           limit_end = max(results$time),
           breaks = 1){
  
  results <- data_results %>%
      filter(is.na(sample)) %>%
      select(-sample) %>%
      mutate(risk = round((mean_survival), 4),
             exposure = ifelse(exposure == 0, control, intervention))
    
    boot_samples <- data_results %>%
      filter(!is.na(sample)) %>%
      mutate(risk = round((mean_survival), 4),
             exposure = ifelse(exposure == 0, control, intervention)) %>%
      group_by(exposure, time) %>%
      summarise(risk_min = quantile(risk, 0.025),
                risk_max = quantile(risk, 0.975))
    
    results0 <- results %>%
      filter(time == 0) %>%
      mutate(risk = 1,
             risk_min = 1,
             risk_max = 1)
    
    results %<>%
      left_join(boot_samples) %>%
      mutate(time = time + 1) %>%
      bind_rows(results0)
    
    results %>%
      ggplot() +
      aes(x = time, y = risk, group = exposure) +
      geom_line(aes(color = exposure), size = 0.8) +
      scale_color_manual(values = c("#235AA7", "#FFDB6D")) +
      geom_ribbon(aes(ymin = risk_min, ymax = risk_max), alpha = 0.10) +
      labs(
        title = title,
        x = xaxis,
        y = "Survival probability",
        color = "Intervention"
      ) +
      scale_x_continuous(limits = c(0, limit_end),
                         breaks = seq(0, limit_end, breaks)) +
      theme_minimal() +
      theme(legend.position = 'bottom')
  }
