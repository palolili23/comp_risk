## The idea of this wrapper is to put together the results and boostraps
## That way I don´t have to run all the things by separate
## However the idea of doing them step by step is great to understand what is
## going on in the 

total_gf_helper <- function(data, factors_outcome,
                               factors_cr,
                                rows = max(data$max),
                                n, 
                                seed = 123){
  
  results <- total_gf_pr(data, factors_outcome, factors_cr, rows)
  
  bootresults <- bootsamples37(data, n, seed,
                               factors_outcome,
                               factors_cr,
                               rows)
  
  binded <- results %>% bind_rows(bootresults)
}
