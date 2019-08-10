# needs in data:
# - exposure
# - max (time)
# - competing (competing event)
# - outcome(outcome event)
# - cens(censored)
# - id


y_model <- c("exposure*(time + I(time^2) + I(time^3))", "pf_f", "age_f", "hg_f", "hx")
d_model <- c("exposure", "time", "I(time^2)", "pf_f", "age_f", "hg_f", "hx")
rows<-60

direct_results <- gformula_wrapper(data,
                 n = 100,
                 factors_outcome = y_model,
                 rows = 60)

cif_curves(direct_results,
            breaks = 10,
           control = "placebo",
           intervention = "High-dose DES")

risk_diff_ratio(direct_results) %>% filter(Time == 59)

total_effect <- gformula_wrapper(data,
                                 n = 100,
                                 factors_outcome = y_model,
                                 factors_cr = d_model,
                                 rows = 60)

cif_curves(total_effect,
           breaks = 10,
           control = "placebo",
           intervention = "High-dose DES")

risk_diff_ratio(total_effect) %>% filter(Time == 59)
