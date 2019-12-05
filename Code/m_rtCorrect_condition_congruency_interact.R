# done, Last modified by Hause Lin 19-11-22 23:02 hauselin@gmail.com

library(tidyverse); library(data.table); library(dtplyr); library(glue); library(brms); library(broom); library(sjstats); library(broom.mixed)

prior_informed_cohensd <- 0.28 # cohen's d
nchains <- 5
samples <- 6000


source("helpfuncs.R")

ddm <- fread("./Gather data/Data/ddm.csv")
stroop <- fread("../Data/stroop.csv")
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







# fit models
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = stroop[condition == "control", sd(rtCorrect, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = stroop[condition == "deplete", sd(rtCorrect, na.rm = T)])
prior_coef
get_prior(rtCorrect ~ conditionEC + (1 | study/pNo), stroop[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC:congruentEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_rtCorrect_condition_congruency_interact <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_rtCorrect_condition_congruency_interact[[s]] <- brm(rtCorrect ~ conditionEC * congruentEC + (1 | pNo), data = stroop[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_rtCorrect_condition_congruency_interact_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_rtCorrect_condition_congruency_interact[[5]] <- brm(rtCorrect ~ conditionEC * congruentEC + (1 | study/pNo), data = stroop,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_rtCorrect_condition_congruency_interact_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayes_rtCorrect_condition_congruency_interact_results <- lapply(1:5, function(x) summarizebrms(mbayes_rtCorrect_condition_congruency_interact[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_rtCorrect_condition_congruency_interact_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayes_rtCorrect_condition_congruency_interact_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayes_rtCorrect_condition_congruency_interact_results[[x]]))
tableformat

mbayes_rtCorrect_condition_congruency_interact_results <- lapply(1:5, function(x) summarizebrms(mbayes_rtCorrect_condition_congruency_interact[[x]], conf.method = "HPDinterval", effect = "congruentEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_rtCorrect_condition_congruency_interact_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayes_rtCorrect_condition_congruency_interact_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayes_rtCorrect_condition_congruency_interact_results[[x]]))
tableformat

mbayes_rtCorrect_condition_congruency_interact_results <- lapply(1:5, function(x) summarizebrms(mbayes_rtCorrect_condition_congruency_interact[[x]], conf.method = "HPDinterval", effect = "conditionEC:congruentEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_rtCorrect_condition_congruency_interact_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayes_rtCorrect_condition_congruency_interact_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayes_rtCorrect_condition_congruency_interact_results[[x]]))
tableformat

# R2 for model
mbayes_rtCorrect_condition_congruency_interact[[5]]
brms::bayes_R2(mbayes_rtCorrect_condition_congruency_interact[[5]])





















# NULL (no condition)
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = stroop[condition == "control", sd(rtCorrect, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = stroop[condition == "deplete", sd(rtCorrect, na.rm = T)])
prior_coef
get_prior(rtCorrect ~ conditionEC + (1 | study/pNo), stroop[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC:congruentEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_rtCorrect_condition_congruency_interact_nocondition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_rtCorrect_condition_congruency_interact_nocondition[[s]] <- brm(rtCorrect ~ congruentEC + conditionEC:congruentEC + (1 | pNo), data = stroop[study == s],
                                                               # family = lognormal(),
                                                               cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                                               file = glue("brms_models/mbayes_rtCorrect_condition_congruency_interact_study{s}_nocondition"),
                                                               control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_rtCorrect_condition_congruency_interact_nocondition[[5]] <- brm(rtCorrect ~ congruentEC + conditionEC:congruentEC + (1 | study/pNo), data = stroop,
                                                           # family = lognormal(),
                                                           cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                                           file = "brms_models/mbayes_rtCorrect_condition_congruency_interact_study_all_nocondition",
                                                           control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_rtCorrect_condition_congruency_interact, mbayes_rtCorrect_condition_congruency_interact_nocondition)























# NULL (no interaction)
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = stroop[condition == "control", sd(rtCorrect, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = stroop[condition == "deplete", sd(rtCorrect, na.rm = T)])
prior_coef
get_prior(rtCorrect ~ conditionEC + (1 | study/pNo), stroop[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            # set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC:congruentEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_rtCorrect_condition_congruency_interact_nointeraction <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_rtCorrect_condition_congruency_interact_nointeraction[[s]] <- brm(rtCorrect ~ congruentEC + conditionEC + (1 | pNo), data = stroop[study == s],
                                                                           # family = lognormal(),
                                                                           cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                                                           file = glue("brms_models/mbayes_rtCorrect_condition_congruency_interact_study{s}_nointeraction"),
                                                                           control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_rtCorrect_condition_congruency_interact_nointeraction[[5]] <- brm(rtCorrect ~ congruentEC + conditionEC + (1 | study/pNo), data = stroop,
                                                                       # family = lognormal(),
                                                                       cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                                                       file = "brms_models/mbayes_rtCorrect_condition_congruency_interact_study_all_nointeraction",
                                                                       control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_rtCorrect_condition_congruency_interact, mbayes_rtCorrect_condition_congruency_interact_nointeraction)
