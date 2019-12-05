library(tidyverse); library(data.table); library(glue); library(brms); library(broom); library(bayesplot)
source("helpfuncs.R")

prior_informed_cohensd <- 0.28 # cohen's d
nchains <- 20
samples <- 2000

ratings <- fread("../Data/ratings.csv")
ratings$conditionEC <- ifelse(ratings$condition == "control", -0.5, 0.5)
ratings[, bored := bored / 10]
ratings[, effort := effort / 10]
ratings[, fatigue := fatigue / 10]
ratings[, frustrate := frustrate / 10]
ratings[, mentaldemand := mentaldemand / 10]







#### mentaldemand
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(mentaldemand, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(mentaldemand, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_mentaldemand_condition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_mentaldemand_condition[[s]] <- brm(mentaldemand ~ conditionEC + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_mentaldemand_condition_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_mentaldemand_condition[[5]] <- brm(mentaldemand ~ conditionEC + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_mentaldemand_condition_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
print("conditionEC effect")
mbayes_mentaldemand_condition_results <- lapply(1:5, function(x) summarizebrms(mbayes_mentaldemand_condition[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_mentaldemand_condition_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayes_mentaldemand_condition_results[[x]]))
tableformat

# NULL model
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(mentaldemand, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(mentaldemand, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            # set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_mentaldemand_condition_nocondition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_mentaldemand_condition_nocondition[[s]] <- brm(mentaldemand ~ 1 + (1 | pNo), data = ratings[study == s],
                                              # family = lognormal(),
                                              cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                              file = glue("brms_models/mbayes_mentaldemand_condition_study{s}_nocondition"),
                                              control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_mentaldemand_condition_nocondition[[5]] <- brm(mentaldemand ~ 1 + (1 | study/pNo), data = ratings,
                                          # family = lognormal(),
                                          cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                          file = "brms_models/mbayes_mentaldemand_condition_study_all_nocondition",
                                          control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_mentaldemand_condition, mbayes_mentaldemand_condition_nocondition)





























#### effort
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(effort, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(effort, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_effort_condition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_effort_condition[[s]] <- brm(effort ~ conditionEC + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_effort_condition_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_effort_condition[[5]] <- brm(effort ~ conditionEC + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_effort_condition_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
print("conditionEC effect")
mbayes_effort_condition_results <- lapply(1:5, function(x) summarizebrms(mbayes_effort_condition[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_effort_condition_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayes_effort_condition_results[[x]]))
tableformat

# NULL model
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(effort, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(effort, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            # set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_effort_condition_nocondition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_effort_condition_nocondition[[s]] <- brm(effort ~ 1 + (1 | pNo), data = ratings[study == s],
                                        # family = lognormal(),
                                        cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                        file = glue("brms_models/mbayes_effort_condition_study{s}_nocondition"),
                                        control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_effort_condition_nocondition[[5]] <- brm(effort ~ 1 + (1 | study/pNo), data = ratings,
                                    # family = lognormal(),
                                    cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                    file = "brms_models/mbayes_effort_condition_study_all_nocondition",
                                    control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_effort_condition, mbayes_effort_condition_nocondition)






























#### frustrate
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(frustrate, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(frustrate, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_frustrate_condition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_frustrate_condition[[s]] <- brm(frustrate ~ conditionEC + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_frustrate_condition_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_frustrate_condition[[5]] <- brm(frustrate ~ conditionEC + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_frustrate_condition_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
print("conditionEC effect")
mbayes_frustrate_condition_results <- lapply(1:5, function(x) summarizebrms(mbayes_frustrate_condition[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_frustrate_condition_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayes_frustrate_condition_results[[x]]))
tableformat

# NULL model
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(frustrate, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(frustrate, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            # set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_frustrate_condition_nocondition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_frustrate_condition_nocondition[[s]] <- brm(frustrate ~ 1 + (1 | pNo), data = ratings[study == s],
                                           # family = lognormal(),
                                           cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                           file = glue("brms_models/mbayes_frustrate_condition_study{s}_nocondition"),
                                           control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_frustrate_condition_nocondition[[5]] <- brm(frustrate ~ 1 + (1 | study/pNo), data = ratings,
                                       # family = lognormal(),
                                       cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                       file = "brms_models/mbayes_frustrate_condition_study_all_nocondition",
                                       control = list(adapt_delta = 0.99))


# bridge sampling bayes factors
compute_bfs(mbayes_frustrate_condition, mbayes_frustrate_condition_nocondition)

























#### bored
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(bored, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(bored, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_bored_condition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_bored_condition[[s]] <- brm(bored ~ conditionEC + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_bored_condition_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_bored_condition[[5]] <- brm(bored ~ conditionEC + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_bored_condition_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
print("conditionEC effect")
mbayes_bored_condition_results <- lapply(1:5, function(x) summarizebrms(mbayes_bored_condition[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_bored_condition_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayes_bored_condition_results[[x]]))
tableformat

# NULL model
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(bored, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(bored, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            # set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_bored_condition_nocondition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_bored_condition_nocondition[[s]] <- brm(bored ~ 1 + (1 | pNo), data = ratings[study == s],
                                       # family = lognormal(),
                                       cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                       file = glue("brms_models/mbayes_bored_condition_study{s}_nocondition"),
                                       control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_bored_condition_nocondition[[5]] <- brm(bored ~ 1 + (1 | study/pNo), data = ratings,
                                   # family = lognormal(),
                                   cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples  * 3,
                                   file = "brms_models/mbayes_bored_condition_study_all_nocondition",
                                   control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_bored_condition, mbayes_bored_condition_nocondition)




























#### fatigue
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(fatigue, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(fatigue, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_fatigue_condition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_fatigue_condition[[s]] <- brm(fatigue ~ conditionEC + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_fatigue_condition_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_fatigue_condition[[5]] <- brm(fatigue ~ conditionEC + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_fatigue_condition_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
print("conditionEC effect")
mbayes_fatigue_condition_results <- lapply(1:5, function(x) summarizebrms(mbayes_fatigue_condition[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_fatigue_condition_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayes_fatigue_condition_results[[x]]))
tableformat

# NULL model
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(fatigue, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(fatigue, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            # set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_fatigue_condition_nocondition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_fatigue_condition_nocondition[[s]] <- brm(fatigue ~ 1 + (1 | pNo), data = ratings[study == s],
                                         # family = lognormal(),
                                         cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                         file = glue("brms_models/mbayes_fatigue_condition_study{s}_nocondition"),
                                         control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_fatigue_condition_nocondition[[5]] <- brm(fatigue ~ 1 + (1 | study/pNo), data = ratings,
                                     # family = lognormal(),
                                     cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                     file = "brms_models/mbayes_fatigue_condition_study_all_nocondition",
                                     control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_fatigue_condition, mbayes_fatigue_condition_nocondition)














