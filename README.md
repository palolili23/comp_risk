
# Project on Competing Risks

This repository contains code to estimate the different estimands
proposed in the following paper [by Young et
al](https://arxiv.org/abs/1806.06136).

The example dataset is **Byar & Greene prostate cancer data**,
downloaded from the following
[website](http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets).

The estimands are presented as follows:

**Direct Effects:**

  - Via G-formula (35)

  - Via IPW (36)

**Total Effects:**

  - Via G-formula (37) (*In process*)

  - Via IPW Sub-distribution hazard (38)

  - Via IPW Cause-specific hazard (39) (*In process*)

Currently, the folder `01_functions` contains the code for the
following:

  - `n_estimate_function`: Is the function for the estimate (indicated
    by the prefix number)

  - `n_bootsamples`: The boostrap function for the estimate

  - `n_wrapper`: A function that wraps the estimate and bootsamples
    results, creates a tibble with all the results for all timepoints.

  - `risk_diff_ratio`: Uses the output from `n_wrapper` to calculate:
    
      - Risk at *k + 1* under each intervention
    
      - Risk difference at *k + 1* with 95%CI
    
      - Risk ratio at *k + 1* with 95%CI

  - `survival_graph`: Contains two different functions:
    
      - `cif_curves`: creates curves for the cumulative incidence
    
      - `surv_curves`: creates the survival curves.

An example of how to use the code is available on the following link:
<https://palolili23.github.io/comp_risk/02_R/Step_by_step.html>
