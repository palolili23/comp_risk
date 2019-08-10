bootsamples39 <- function(data,
                          factors_outcome,
                          factors_cr,
                          factors_cens,
                          rows = max(data$max),
                          factors_num_cens = NULL,
                          truncation_percentile = NULL,
                          n,
                          seed = 123){
  # Set seed
  set.seed(seed)
  
  # Creates bootsamples and runs the model to each sample
  bootsamps <- replicate(n = n, expr = {
    d <- sample(1:nrow(data),size = nrow(data), replace = T)
    ds_b <- data[d,] %>%
      mutate(id = row_number()) 
    return(total_ipwcs_pr(ds_b, 
                          factors_outcome,
                          factors_cr,
                          factors_cens,
                          rows,
                          factors_num_cens,
                          truncation_percentile))
  }, simplify = F)
  
  samples <- as.list(c(1:n))
  bootsamps_results <- map(bootsamps, ~.x[[1]])
  boot_bind <- Map(cbind, bootsamps_results, sample = samples)
  totalboot <- reduce(boot_bind, bind_rows)
  
  return(totalboot)
}

