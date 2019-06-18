## The idea of this wrapper is to put together the results and boostraps
## That way I don´t have to run all the things by separate
## However the idea of doing them step by step is great to understand what is
## going on in the 

wrapper <- function(data, surv_model,
                    factors_outcome,
                    factors_cens,
                    factors_cr,
                    rows = max(data$max),
                    n, 
                    seed = 123){
  
  results <- surv_model(data, factors_outcome, 
                        factors_cens, factors_cr, rows)
  
  bootresults <- bootsamples(data, n, seed,
                             factors_outcome,
                             factors_cens,
                             factors_cr,
                             rows,
                             surv_model)
  
  binded <- results %>% bind_rows(bootresults)
}
