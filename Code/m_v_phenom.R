# done, Last modified by Hause Lin 19-11-22 22:16 hauselin@gmail.comm

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

# meancenter
ratings[, bored := bored - mean(bored, na.rm = T), by = .(study, pNo)]
ratings[, effort := effort - mean(effort, na.rm = T), by = .(study, pNo)]
ratings[, fatigue := fatigue - mean(fatigue, na.rm = T), by = .(study, pNo)]
ratings[, frustrate := frustrate - mean(frustrate, na.rm = T), by = .(study, pNo)]
ratings[, mentaldemand := mentaldemand - mean(mentaldemand, na.rm = T), by = .(study, pNo)]


















#### Model: v ~ frustrate
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ frustrate + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "frustrate"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_frustrate <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_frustrate[[s]] <- brm(v ~ frustrate + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_drift_frustrate_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_frustrate[[5]] <- brm(v ~ frustrate + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_drift_frustrate_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayes_drift_frustrate_results <- lapply(1:5, function(x) summarizebrms(mbayes_drift_frustrate[[x]], effect = "frustrate"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_drift_frustrate_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayes_drift_frustrate_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayes_drift_frustrate_results[[x]]))
tableformat

# plot prior and posterior
hypo <- brms::hypothesis(mbayes_drift_frustrate[[5]], "frustrate = 0")
p <- plot(hypo)[[1]]
p = p + geom_vline(xintercept = 0, linetype = 'dashed') + labs(x = 'Estimate', y = "Posterior density") +
    labs(title = glue('Posterior: {mbayes_drift_frustrate_results[[5]][effect == "coefficient", result]}'))
p
write_rds(p, "Figures/drift_frustrate.rds")

# NULL
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ frustrate + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            # set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "frustrate"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_frustrate_nofrustrate <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_frustrate_nofrustrate[[s]] <- brm(v ~ 1 + (1 | pNo), data = ratings[study == s],
                                       # family = lognormal(),
                                       cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                       file = glue("brms_models/mbayes_drift_frustrate_study{s}_nofrustrate"),
                                       control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_frustrate_nofrustrate[[5]] <- brm(v ~ 1 + (1 | study/pNo), data = ratings,
                                   # family = lognormal(),
                                   cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                   file = "brms_models/mbayes_drift_frustrate_study_all_nofrustrate",
                                   control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_drift_frustrate, mbayes_drift_frustrate_nofrustrate)




































#### Model: v ~ bored
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ bored + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "bored"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_bored <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_bored[[s]] <- brm(v ~ bored + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_drift_bored_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_bored[[5]] <- brm(v ~ bored + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_drift_bored_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayes_drift_bored_results <- lapply(1:5, function(x) summarizebrms(mbayes_drift_bored[[x]], effect = "bored"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_drift_bored_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayes_drift_bored_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayes_drift_bored_results[[x]]))
tableformat

# plot prior and posterior
hypo <- brms::hypothesis(mbayes_drift_bored[[5]], "bored = 0")
p <- plot(hypo)[[1]]
p = p + geom_vline(xintercept = 0, linetype = 'dashed') + labs(x = 'Estimate', y = "Posterior density") +
    labs(title = glue('Posterior: {mbayes_drift_bored_results[[5]][effect == "coefficient", result]}'))
p
write_rds(p, "Figures/drift_bored.rds")

# NULL
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ bored + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            # set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "bored"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_bored_nobored <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_bored_nobored[[s]] <- brm(v ~ 1 + (1 | pNo), data = ratings[study == s],
                                   # family = lognormal(),
                                   cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                   file = glue("brms_models/mbayes_drift_bored_study{s}_nobored"),
                                   control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_bored_nobored[[5]] <- brm(v ~ 1 + (1 | study/pNo), data = ratings,
                               # family = lognormal(),
                               cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                               file = "brms_models/mbayes_drift_bored_study_all_nobored",
                               control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_drift_bored, mbayes_drift_bored_nobored)
























#### Model: v ~ fatigue
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ fatigue + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "fatigue"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_fatigue <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_fatigue[[s]] <- brm(v ~ fatigue + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_drift_fatigue_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_fatigue[[5]] <- brm(v ~ fatigue + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_drift_fatigue_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayes_drift_fatigue_results <- lapply(1:5, function(x) summarizebrms(mbayes_drift_fatigue[[x]], effect = "fatigue"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_drift_fatigue_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayes_drift_fatigue_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayes_drift_fatigue_results[[x]]))
tableformat

# plot prior and posterior
hypo <- brms::hypothesis(mbayes_drift_fatigue[[5]], "fatigue = 0")
p <- plot(hypo)[[1]]
p = p + geom_vline(xintercept = 0, linetype = 'dashed') + labs(x = 'Estimate', y = "Posterior density") +
    labs(title = glue('Posterior: {mbayes_drift_fatigue_results[[5]][effect == "coefficient", result]}'))
p
write_rds(p, "Figures/drift_fatigue.rds")


# prior predictive checks (revision1)
# mbayes_drift_fatigue_priorpredictive <- brm(v ~ fatigue + (1 | study/pNo), data = ratings, cores = 5, chains = 5, sample_prior = "only", save_all_pars = TRUE, prior = priors, iter = samples)
# mbayes_drift_fatigue_priorpredictive
# tempplot <- plot(mbayes_drift_fatigue_priorpredictive)
# ggsave(filename = "./Figures/mbayes_drift_fatigue_priorpredictive_plot.pdf", plot = tempplot[[1]], width = 8, height = 8)
# mbayes_drift_fatigue_priorpredictive_draws <- posterior_predict(mbayes_drift_fatigue_priorpredictive, draws = 25)
#
# # ppc_dens_overlay(ratings[, v], mbayes_drift_fatigue_priorpredictive_draws[1:100,])
# # ppc_freqpoly_grouped(ratings[, a], mbayes_drift_fatigue_priorpredictive_draws[101:120,], group = ratings$conditionEC) + yaxis_text()
# ppc_violin_grouped(ratings[, v], mbayes_drift_fatigue_priorpredictive_draws[101:120,], group = ratings$conditionEC, y_draw = "both") + labs(x = "Condition (low vs high demand)")
# ggsave(filename = "./Figures/mbayes_drift_fatigue_priorpredictive_overlay.pdf", width = 5, height = 5)
#
# me_loss_prior1 <- marginal_effects(mbayes_drift_fatigue_priorpredictive, re_formula = NULL, method = "predict")
# plot(me_loss_prior1, points = TRUE, plot = TRUE, ncol = 5, theme = theme_bw())
# ggsave(filename = "./Figures/mbayes_drift_fatigue_priorpredictive_marginaleffects.pdf", width = 7, height = 7)

# NULL
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ fatigue + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            # set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal({prior_coef}, {abs(prior_coef/2)})"), class = "b", coef = "fatigue"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_fatigue_nofatigue <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_fatigue_nofatigue[[s]] <- brm(v ~ 1 + (1 | pNo), data = ratings[study == s],
                                     # family = lognormal(),
                                     cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                     file = glue("brms_models/mbayes_drift_fatigue_study{s}_nofatigue"),
                                     control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_fatigue_nofatigue[[5]] <- brm(v ~ 1 + (1 | study/pNo), data = ratings,
                                 # family = lognormal(),
                                 cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                 file = "brms_models/mbayes_drift_fatigue_study_all_nofatigue",
                                 control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_drift_fatigue, mbayes_drift_fatigue_nofatigue)




























#### Model: v ~ mentaldemand (not preregistered)
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ mentaldemand + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "mentaldemand"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_mentaldemand <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_mentaldemand[[s]] <- brm(v ~ mentaldemand + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_drift_mentaldemand_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_mentaldemand[[5]] <- brm(v ~ mentaldemand + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_drift_mentaldemand_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayes_drift_mentaldemand_results <- lapply(1:5, function(x) summarizebrms(mbayes_drift_mentaldemand[[x]], effect = "mentaldemand"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_drift_mentaldemand_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayes_drift_mentaldemand[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayes_drift_mentaldemand_results[[x]]))
tableformat

# NULL
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ mentaldemand + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            # set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "mentaldemand"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_mentaldemand_nomentaldemand <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_mentaldemand_nomentaldemand[[s]] <- brm(v ~ 1 + (1 | pNo), data = ratings[study == s],
                                          # family = lognormal(),
                                          cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                          file = glue("brms_models/mbayes_drift_mentaldemand_study{s}_nomentaldemand"),
                                          control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_mentaldemand_nomentaldemand[[5]] <- brm(v ~ 1 + (1 | study/pNo), data = ratings,
                                      # family = lognormal(),
                                      cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                      file = "brms_models/mbayes_drift_mentaldemand_study_all_nomentaldemand",
                                      control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_drift_mentaldemand, mbayes_drift_mentaldemand_nomentaldemand)
































#### Model: v ~ effort (not preregistered)
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ effort + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            set_prior("normal(0, 1)", class = "b"),
            set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "effort"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_effort <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_effort[[s]] <- brm(v ~ effort + (1 | pNo), data = ratings[study == s],
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                             file = glue("brms_models/mbayes_drift_effort_study{s}"),
                             control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_effort[[5]] <- brm(v ~ effort + (1 | study/pNo), data = ratings,
                             # family = lognormal(),
                             cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                             file = "brms_models/mbayes_drift_effort_study_all",
                             control = list(adapt_delta = 0.99))

# summarize model results
mbayes_drift_effort_results <- lapply(1:5, function(x) summarizebrms(mbayes_drift_effort[[x]], effect = "effort"))

manuscriptformat <- data.table(results = sapply(1:5, function(x) mbayes_drift_effort_results[[x]][effect == "manuscriptformat", result]))
manuscriptformat
mbayes_drift_effort_results[[5]]

tableformat <- lapply(1:5, function(x) formattable(mbayes_drift_effort_results[[x]]))
tableformat

# NULL
prior_coef <- expectedBeta(expected_d = -prior_informed_cohensd,
                           sd1 = ratings[, sd(v, na.rm = T)], # REMEMBER TO CHANGE VARIALBE!
                           sd2 = ratings[, sd(v, na.rm = T)])
prior_coef
get_prior(v ~ effort + (1 | study/pNo), ratings[study == 1])
priors <- c(set_prior("normal(0, 1)", class = "Intercept"),
            # set_prior("normal(0, 1)", class = "b"),
            # set_prior(glue("normal(0, {abs(prior_coef/2)})"), class = "b", coef = "effort"),
            set_prior("normal(0, 1)", class = "sd"),
            set_prior("normal(0, 1)", class = "sigma")) %>% print()

# fit model for each study (2-level model)
mbayes_drift_effort_noeffort <- vector(mode = "list", length = 5)
for (s in 1:4) {
    mbayes_drift_effort_noeffort[[s]] <- brm(v ~ 1 + (1 | pNo), data = ratings[study == s],
                                    # family = lognormal(),
                                    cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples,
                                    file = glue("brms_models/mbayes_drift_effort_study{s}_noeffort"),
                                    control = list(adapt_delta = 0.99))
}

# fit 3-level model
mbayes_drift_effort_noeffort[[5]] <- brm(v ~ 1 + (1 | study/pNo), data = ratings,
                                # family = lognormal(),
                                cores = nchains, chains = nchains, sample_prior = TRUE, save_all_pars = TRUE, prior = priors, iter = samples * 3,
                                file = "brms_models/mbayes_drift_effort_study_all_noeffort",
                                control = list(adapt_delta = 0.99))

# bridge sampling bayes factors
compute_bfs(mbayes_drift_effort, mbayes_drift_effort_noeffort)



