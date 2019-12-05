# Code

To begin an R session, click and run `analysis_Rproject.Rproj` to start a new R project/session that sets up your working directory correctly. The `brms_models` directory is where `brms` models will be saved when you run the code.

All `.R` files that begin with `m_` are `R` scripts that fit Bayesian models using the `brms` package.

`helpfuncs.R`: helper functions called by the R scripts below

`m_a_condition_congruency.R`: boundary ~ condition + congruency

`m_a_condition_congruency_interact.R`: boundary ~ condition * congruency

`m_a_phenom.R`: boundary ~ phenomenology (5 different self-reported ratings)

`m_acc_condition_congruency_interact.R`: stroop accuracy ~ condition * congruency

`m_phenom_condition.R`: phenomenology ~ condition

`m_rtCorrect_condition_congruency_interact.R`:  reaction time ~ condition * congruency

`m_v_condition_congruency.R`: drift rate ~ condition + congruency

`m_v_condition_congruency_interact.R`: drift rate ~ condition * congruency

`m_v_phenom.R`: drirt rate ~ phenomenology (5 different self-reported ratings)

`run_scripts.sh`: bash script to run all the R scripts above on SLURM clusters



All R code has been tested using R version 3.6.1 (2019-07-05). R package versions: bayesplot_1.7.0, broom_0.5.2, brms_2.10.0, Rcpp_1.0.3, glue_1.3.1, data.table_1.12.6, forcats_0.4.0, stringr_1.4.0, dplyr_0.8.3, tidyr_1.0.0, tibble_2.1.3, ggplot2_3.2.1     tidyverse_1.2.1