# done, Last modified by Hause Lin 19-11-22 16:30 hauselin@gmail.com

library(tidyverse); library(data.table); library(dtplyr); library(lme4); library(lmerTest); library(ggbeeswarm); library(cowplot); library(glue); library(brms); library(broom); library(sjstats); library(bayesplot)

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

prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ddm[condition == "control", sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ddm[condition == "deplete", sd(v, na.rm = T)])
prior_coef
get_prior(v ~ conditionEC + (1 | study/pNo), ddm[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_condition_congruency <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_condition_congruency[[s]] <- brm(v ~ conditionEC + congruentEC + (1 | pNo), data = ddm[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_drift_condition_congruency_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_condition_congruency[[5]] <- brm(v ~ conditionEC + congruentEC + (1 | study/pNo), data = ddm,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_drift_condition_congruency_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayes_drift_condition_congruency_results <- lapply(1:5, function(x) summarizebrms(mbayes_drift_condition_congruency[[x]], conf.method = "HPDinterval", effect = "conditionEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_drift_condition_congruency_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayes_drift_condition_congruency_results[[x]]))
tableformat


mbayes_drift_condition_congruency_results <- lapply(1:5, function(x) summarizebrms(mbayes_drift_condition_congruency[[x]], conf.method = "HPDinterval", effect = "congruentEC"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_drift_condition_congruency_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat

tableformat <- lapply(1:5, function(x) formattable(mbayes_drift_condition_congruency_results[[x]]))
tableformat



# NULL (no condition)
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ddm[condition == "control", sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ddm[condition == "deplete", sd(v, na.rm = T)])
prior_coef
get_prior(v ~ conditionEC + (1 | study/pNo), ddm[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_condition_congruency_nocondition <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_condition_congruency_nocondition[[s]] <- brm(v ~ congruentEC + (1 | pNo), data = ddm[study == s],
                                                  # family = lognormal(),
                                                  cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                                  file = glue("brms_models/mbayes_drift_condition_congruency_study{s}_nocondition"),
                                                  control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_condition_congruency_nocondition[[5]] <- brm(v ~ congruentEC + (1 | study/pNo), data = ddm,
                                              # family = lognormal(),
                                              cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                              file = "brms_models/mbayes_drift_condition_congruency_study_all_nocondition",
                                              control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
# compute_bfs(mbayes_drift_condition_congruency, mbayes_drift_condition_congruency_nocondition)


# NULL (no congruency)
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ddm[condition == "control", sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ddm[condition == "deplete", sd(v, na.rm = T)])
prior_coef
get_prior(v ~ conditionEC + (1 | study/pNo), ddm[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "conditionEC"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_condition_congruency_nocongruency <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_condition_congruency_nocongruency[[s]] <- brm(v ~ conditionEC + (1 | pNo), data = ddm[study == s],
                                                               # family = lognormal(),
                                                               cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                                               file = glue("brms_models/mbayes_drift_condition_congruency_study{s}_nocongruency"),
                                                               control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_condition_congruency_nocongruency[[5]] <- brm(v ~ conditionEC + (1 | study/pNo), data = ddm,
                                                           # family = lognormal(),
                                                           cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                                           file = "brms_models/mbayes_drift_condition_congruency_study_all_nocongruency",
                                                           control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_drift_condition_congruency, mbayes_drift_condition_congruency_nocongruency)