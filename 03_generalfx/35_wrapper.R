direct_gf_pr_helper <- function(data, surv_model,
                                 factors_outcome,
                                 rows = max(data$max),
                                 n, 
                                 seed = 123){
  
  results <- surv_model(data, factors_outcome, rows)
  
  bootresults <- bootsamples35(data, n, seed,
                             factors_outcome,
                             rows)
  
  binded <- results %>% bind_rows(bootresults)
}
