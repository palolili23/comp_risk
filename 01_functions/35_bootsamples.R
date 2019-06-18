bootsamples35 <- function(data, n, 
                        seed = 123,
                        factors_outcome,
                        rows = max(data$max)){
  # Set seed
  set.seed(seed)
  
  # Creates bootsamples and runs the model to each sample
  bootsamps <- replicate(n = n, expr = {
    d <- sample(1:nrow(data),size = nrow(data), replace = T)
    ds_b <- data[d,] %>%
      mutate(id = row_number()) 
    return(direct_gf_pr(ds_b, 
                      factors_outcome,
                      rows))
  }, simplify = F)
  
  samples <- as.list(c(1:n))
  
  boot_bind <- Map(cbind, bootsamps, sample = samples)
  totalboot <- reduce(boot_bind, bind_rows)
  
  return(totalboot)
}
