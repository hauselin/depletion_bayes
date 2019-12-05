library(tidyverse); library(data.table); library(dtplyr); library(glue); library(brms); library(broom); library(sjstats); library(broom.mixed); library(bayesplot)

prior_informed_cohensd <- 0.28 # cohen's d
nchains <- 5
samples <- 6000

source("helpfuncs.R")

ddm <- fread("./Gather data/Data/ddm.csv")
stroop <- fread("./Gather data/Data/stroop.csv")
# code
ddm[condition == "control", conditionEC := -0.5]
ddm[condition == "deplete", conditionEC := 0.5]
stroop[condition == "control", conditionEC := -0.5]
stroop[condition == "deplete", conditionEC := 0.5]

ddm[congruency == "congruent", congruentEC := -0.5]
ddm[congruency == "incongruent", congruentEC := 0.5]
stroop[congruency == "congruent", congruentEC := -0.5]
stroop[congruency == "incongruent", congruentEC := 0.5]

interfere <- fread("./Gather data/Data/interference.csv")
interfere$conditionEC <- ifelse(interfere$condition == "control", -0.5, 0.5)

ratings <- fread("./Gather data/Data/ratings.csv")
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
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_mentaldemand_condition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_mentaldemand_condition[[s]] <- brm(mentaldemand ~ conditionEC + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_mentaldemand_condition_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_mentaldemand_condition[[5]] <- brm(mentaldemand ~ conditionEC + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_mentaldemand_condition_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
print("conditionEC effect")
mbayesNullN01Prior_mentaldemand_condition_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_mentaldemand_condition[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_mentaldemand_condition_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_mentaldemand_condition_results[[x]]))
tableformat





#### effort
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(effort, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(effort, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_effort_condition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_effort_condition[[s]] <- brm(effort ~ conditionEC + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_effort_condition_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_effort_condition[[5]] <- brm(effort ~ conditionEC + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_effort_condition_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
print("conditionEC effect")
mbayesNullN01Prior_effort_condition_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_effort_condition[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_effort_condition_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayesNullN01Prior_effort_condition_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_effort_condition_results[[x]]))
tableformat







#### frustrate
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(frustrate, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(frustrate, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_frustrate_condition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_frustrate_condition[[s]] <- brm(frustrate ~ conditionEC + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_frustrate_condition_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_frustrate_condition[[5]] <- brm(frustrate ~ conditionEC + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_frustrate_condition_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
print("conditionEC effect")
mbayesNullN01Prior_frustrate_condition_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_frustrate_condition[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_frustrate_condition_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayesNullN01Prior_frustrate_condition_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_frustrate_condition_results[[x]]))
tableformat






#### bored
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(bored, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(bored, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_bored_condition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_bored_condition[[s]] <- brm(bored ~ conditionEC + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_bored_condition_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_bored_condition[[5]] <- brm(bored ~ conditionEC + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_bored_condition_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
print("conditionEC effect")
mbayesNullN01Prior_bored_condition_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_bored_condition[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_bored_condition_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayesNullN01Prior_bored_condition_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_bored_condition_results[[x]]))
tableformat







#### fatigue
prior_coef <- expectedBeta(expected_d = prior_informed_cohensd,
                           sd1 = ratings[condition == "control", sd(fatigue, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[condition == "deplete", sd(fatigue, na.rm = T)])
prior_coef
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_fatigue_condition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_fatigue_condition[[s]] <- brm(fatigue ~ conditionEC + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_fatigue_condition_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_fatigue_condition[[5]] <- brm(fatigue ~ conditionEC + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_fatigue_condition_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
print("conditionEC effect")
mbayesNullN01Prior_fatigue_condition_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_fatigue_condition[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_fatigue_condition_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayesNullN01Prior_fatigue_condition_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_fatigue_condition_results[[x]]))
tableformat
























# a ~ conditionEC + congruentEC
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ddm[condition == "control", sd(a, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ddm[condition == "deplete", sd(a, na.rm = T)])
prior_coef
get_prior(a ~ conditionEC + (1 | study/pNo), ddm[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_bound_condition_congruency <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_bound_condition_congruency[[s]] <- brm(a ~ conditionEC + congruentEC + (1 | pNo), data = ddm[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_bound_condition_congruency_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_bound_condition_congruency[[5]] <- brm(a ~ conditionEC + congruentEC + (1 | study/pNo), data = ddm,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_bound_condition_congruency_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayesNullN01Prior_bound_condition_congruency_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_bound_condition_congruency[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_bound_condition_congruency_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayesNullN01Prior_bound_condition_congruency_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_bound_condition_congruency_results[[x]]))
tableformat

# summarize model results
mbayesNullN01Prior_bound_condition_congruency_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_bound_condition_congruency[[x]], conf.method = "HPDinterval", effect = "congruentEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_bound_condition_congruency_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayesNullN01Prior_bound_condition_congruency_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_bound_condition_congruency_results[[x]]))
tableformat












# v ~ conditionEC + congruentEC
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ddm[condition == "control", sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ddm[condition == "deplete", sd(v, na.rm = T)])
prior_coef
get_prior(v ~ conditionEC + (1 | study/pNo), ddm[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_drift_condition_congruency <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_drift_condition_congruency[[s]] <- brm(v ~ conditionEC + congruentEC + (1 | pNo), data = ddm[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_drift_condition_congruency_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_drift_condition_congruency[[5]] <- brm(v ~ conditionEC + congruentEC + (1 | study/pNo), data = ddm,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_drift_condition_congruency_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayesNullN01Prior_drift_condition_congruency_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_drift_condition_congruency[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_drift_condition_congruency_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayesNullN01Prior_drift_condition_congruency_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_drift_condition_congruency_results[[x]]))
tableformat

mbayesNullN01Prior_drift_condition_congruency_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_drift_condition_congruency[[x]], conf.method = "HPDinterval", effect = "congruentEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_drift_condition_congruency_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayesNullN01Prior_drift_condition_congruency_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_drift_condition_congruency_results[[x]]))
tableformat











#### Model: a ~ frustrate
# meancenter
ratings[, bored := bored - mean(bored, na.rm = T), by = .(study, pNo)]
ratings[, effort := effort - mean(effort, na.rm = T), by = .(study, pNo)]
ratings[, fatigue := fatigue - mean(fatigue, na.rm = T), by = .(study, pNo)]
ratings[, frustrate := frustrate - mean(frustrate, na.rm = T), by = .(study, pNo)]
ratings[, mentaldemand := mentaldemand - mean(mentaldemand, na.rm = T), by = .(study, pNo)]

prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(a, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(a, na.rm = T)])
prior_coef
get_prior(a ~ frustrate + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "frustrate"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_bound_frustrate <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_bound_frustrate[[s]] <- brm(a ~ frustrate + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_bound_frustrate_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_bound_frustrate[[5]] <- brm(a ~ frustrate + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_bound_frustrate_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayesNullN01Prior_bound_frustrate_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_bound_frustrate[[x]], effect = "frustrate"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_bound_frustrate_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_bound_frustrate_results[[x]]))
tableformat













#### Model: a ~ bored
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(a, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(a, na.rm = T)])
prior_coef
get_prior(a ~ bored + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "bored"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_bound_bored <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_bound_bored[[s]] <- brm(a ~ bored + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_bound_bored_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_bound_bored[[5]] <- brm(a ~ bored + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_bound_bored_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayesNullN01Prior_bound_bored_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_bound_bored[[x]], effect = "bored"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_bound_bored_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_bound_bored_results[[x]]))
tableformat










#### Model: a ~ fatigue
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(a, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(a, na.rm = T)])
prior_coef
get_prior(a ~ fatigue + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "fatigue"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_bound_fatigue <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_bound_fatigue[[s]] <- brm(a ~ fatigue + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_bound_fatigue_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_bound_fatigue[[5]] <- brm(a ~ fatigue + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_bound_fatigue_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayesNullN01Prior_bound_fatigue_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_bound_fatigue[[x]], effect = "fatigue"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_bound_fatigue_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_bound_fatigue_results[[x]]))
tableformat


















#### Model: v ~ frustrate
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ frustrate + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "frustrate"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_drift_frustrate <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_drift_frustrate[[s]] <- brm(v ~ frustrate + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_drift_frustrate_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_drift_frustrate[[5]] <- brm(v ~ frustrate + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_drift_frustrate_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayesNullN01Prior_drift_frustrate_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_drift_frustrate[[x]], effect = "frustrate"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_drift_frustrate_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_drift_frustrate_results[[x]]))
tableformat













#### Model: v ~ bored
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ bored + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "bored"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_drift_bored <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_drift_bored[[s]] <- brm(v ~ bored + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_drift_bored_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_drift_bored[[5]] <- brm(v ~ bored + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_drift_bored_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayesNullN01Prior_drift_bored_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_drift_bored[[x]], effect = "bored"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_drift_bored_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_drift_bored_results[[x]]))
tableformat










#### Model: v ~ fatigue
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ fatigue + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "fatigue"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayesNullN01Prior_drift_fatigue <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayesNullN01Prior_drift_fatigue[[s]] <- brm(v ~ fatigue + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = glue("brms_models/mbayesNullN01Prior_drift_fatigue_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayesNullN01Prior_drift_fatigue[[5]] <- brm(v ~ fatigue + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = FALSE, prior = priors, iter = 2000,
                             file = "brms_models/mbayesNullN01Prior_drift_fatigue_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayesNullN01Prior_drift_fatigue_results <- lapply(1:5, function(x) summarizebrms(mbayesNullN01Prior_drift_fatigue[[x]], effect = "fatigue"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayesNullN01Prior_drift_fatigue_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayesNullN01Prior_drift_fatigue_results[[x]]))
tableformat