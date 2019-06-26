total_ipwsh_helper <- function(data, factors_outcome,
                               factors_cens,
                               rows = max(data$max),
                               n, 
                               seed = 123){
  
  results <- total_ipwsh_pr(data, factors_outcome, factors_cens, rows)
  
  bootresults <- bootsamples38(data, n, seed,
                               factors_outcome,
                               factors_cens,
                               rows)
  
  binded <- results %>% bind_rows(bootresults)
}
