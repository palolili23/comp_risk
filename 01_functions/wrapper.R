## The idea of this wrapper is to put together the results and boostraps
## That way I don´t have to run all the things by separate
## However the idea of doing them step by step is great to understand what is
## going on in the 

wrapper <- function(data, factors, time_fx, n, surv_model){
  results <- surv_model(data, factors, time_fx)
  bootresults <- bootsamples(data, n, factors, time_fx, surv_model)
  binded <- results %>% bind_rows(bootresults)
}

