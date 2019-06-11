bootsamples <- function(data, n, factors, time_fx, surv_model) {
  set.seed(123)
  bootsamps <- replicate(n = n, expr = {
    d <- sample(1:nrow(data),size = nrow(data), replace = T)
    ds_b <- data[d,] %>%
      mutate(id = row_number())
    return(surv_model(ds_b, factors, time_fx))
  }, simplify = F)
  
  samples <- as.list(c(1:n))
  
  boot_bind <- Map(cbind, bootsamps, sample = samples)
  totalboot <- reduce(boot_bind, bind_rows)
  
  return(totalboot)
}
