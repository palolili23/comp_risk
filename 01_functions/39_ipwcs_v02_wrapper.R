## The idea of this wrapper is to put together the results and boostraps
## That way I don´t have to run all the things by separate
## However the idea of doing them step by step is great to understand what is
## going on in the 

total_ipwcs_helper <- function(data,
                               factors_outcome,
                               factors_cens,
                               factors_cr,
                               rows = max(data$max),
                               factors_num_cens = NULL,
                               truncation_percentile = NULL,
                               n,
                               seed = 123) {
  results <-
    total_ipwcs_pr(
      data,
      factors_outcome,
      factors_cr,
      factors_cens,
      rows,
      factors_num_cens,
      truncation_percentile
    )
  
  weights <- results %>% pluck(2)
  
  bootresults <- bootsamples39(
    data,
    factors_outcome,
    factors_cr,
    factors_cens,
    rows,
    factors_num_cens,
    truncation_percentile,
    n,
    seed
  )
  
  binded <- results %>% pluck(1) %>% bind_rows(bootresults)
  
  return(list(binded, weights))
}
