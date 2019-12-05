expectedBeta <- function(expected_d, sd1, sd2) {
    pooledSD <- sqrt(sum(sd1^2 + sd2^2) / 2)
    print(glue("Pooled SD: {pooledSD}"))
    return(round(expected_d * pooledSD, 5))
}

bese <- function(summaryhresults2, dp = 2) { # b and se
    temp <- summaryhresults2[, c(1:3, 6)]
    temp[, results := paste0(round(estimate,dp), ' (', round(std.error,dp), ")")]
    temp[p.value < 0.05, results := paste0(results, "*")]
}

summarizebrms <- function(brmsmodel, conf.method = "HPDinterval", effect = "conditionEC", dp = 2) {

    digits <- paste0("%.", dp, "f") # e.g, 0.10 not 0.1, 0.009, not 0.01

    # population effects and 95% HPD
    popeffects <- broom.mixed::tidy(brmsmodel, parameters = glue("b_{effect}"), conf.method = conf.method)[1, ]
    popeffectscopy <- as.data.frame(popeffects[, c("estimate", "conf.low", "conf.high")])
    popeffectsround <- popeffectscopy
    popeffectsround[abs(popeffectscopy) >= 0.01] <- round(popeffectsround[abs(popeffectscopy) >= 0.01], dp)
    popeffectsround[abs(popeffectscopy) >= 0.01] <- sprintf(digits, popeffectscopy[abs(popeffectscopy) >= 0.01])
    popeffectsround[abs(popeffectscopy) < 0.01] <- signif(popeffectscopy[abs(popeffectscopy) < 0.01], digits = 1)
    popeffectsround[abs(popeffectscopy) < 0.0005] <- '0.00'
    popeffectsstr <- glue("b = {popeffectsround$estimate}, 95% HPD [{popeffectsround$conf.low}, {popeffectsround$conf.high}]")
    popeffectsstr <- gsub("-", replacement = "\u2212", popeffectsstr)

    # cohen's D and 95% HPD
    mcmc <- as.matrix(brmsmodel)
    cohenD <- mcmc[, glue("b_{effect}")] / mcmc[, "sigma"]
    cohensd <- broom.mixed::tidy(as.mcmc(cohenD), conf.int = TRUE, conf.method = conf.method)
    cohensdcopy <- as.data.frame(cohensd[, c("estimate", "conf.low", "conf.high")])
    cohensdround <- cohensdcopy
    cohensdround[abs(cohensdcopy) >= 0.01] <- round(cohensdcopy[abs(cohensdcopy) >= 0.01], dp)
    cohensdround[abs(cohensdcopy) >= 0.01] <- sprintf(digits, cohensdcopy[abs(cohensdcopy) >= 0.01])
    cohensdround[abs(cohensdcopy) < 0.01] <- signif(cohensdcopy[abs(cohensdcopy) < 0.01], digits = 1)
    cohensdround[abs(cohensdcopy) < 0.0005] <- "0.00"
    cohensdcopystr <- glue("d = {cohensdround$estimate}, 95% HPD [{cohensdround$conf.low}, {cohensdround$conf.high}]")
    cohensdcopystr <- gsub("-", replacement = "\u2212", cohensdcopystr)

    # BFs
    hypo <- brms::hypothesis(brmsmodel, glue("{effect} = 0"))
    tempbf <- round(1/hypo$hypothesis$Evid.Ratio, 2)
    if (tempbf > 500) {
        tempbfstr <- "BF > 500" #BF
    } else {
        tempbfstr <- glue("BF = {tempbf}") #BF
    }

    # manuscript result
    cohensdround$estimate <- gsub("-", replacement = "\u2212", cohensdround$estimate)
    manuscriptstr <- glue("{popeffectsstr}, d = {cohensdround$estimate}")

    # print results
    message(glue("{popeffectsstr}, {tempbfstr}, {cohensdcopystr}"))
    return(data.table::data.table(effect = c(effect, "coefficient", "bayesfactor", "cohensd", "manuscriptformat"),
                                  result = c(glue("{popeffectsstr}, {tempbfstr}, {cohensdcopystr}"), popeffectsstr, tempbfstr, cohensdcopystr, manuscriptstr)))

}

formattable <- function(summarizedmodel) {
  coefficient <- summarizedmodel[effect == 'coefficient', result]
  bayesfactor <- summarizedmodel[effect == 'bayesfactor', result]
  cohensd <- summarizedmodel[effect == 'cohensd', result]

  coefficient <- gsub(pattern = "b = ", replacement = "", x = coefficient)
  coefficient <- gsub(pattern = ", 95% HPD", replacement = "", x = coefficient)

  bayesfactor <- glue("({bayesfactor})")

  cohensd <- gsub(pattern = ", 95% HPD", replacement = "", x = cohensd)
  cohensd

  coefficient <- gsub("-", replacement = "\u2212", coefficient)
  bayesfactor <- gsub("-", replacement = "\u2212", bayesfactor)
  cohensd <- gsub("-", replacement = "\u2212", cohensd)

  return(glue("{coefficient} {bayesfactor}
       {cohensd}") )
}



compute_bfs <- function(modelsList_full, modelsList_null, iters = 5) {
  results <- vector(mode = "list", length = iters)
  bfs <- vector(mode = "list", length = iters)
  for (m in 1:length(modelsList_full)) { # for each model

    # output file names
    outputtxtfile <- modelsList_full[[m]]$file
    outputtxtfile <- gsub(".rds", "", outputtxtfile)

    outputtxtfile2 <- modelsList_null[[m]]$file
    outputtxtfile2 <- gsub(".rds", "", outputtxtfile2)
    outputtxtfile2 <- gsub("brms_models/", "", outputtxtfile2)

    outputtxtfile3 <- paste(outputtxtfile, outputtxtfile2, sep = "__") # final output name
    outputtxtfile3 <- paste0(outputtxtfile3, ".txt")

    if (file.exists(outputtxtfile3)) {
      return("Already computed Bayes factors!")
    }

    for (i in 1:iters) { # compute bfs multiple times for stability
      results[[i]] <- brms::bayes_factor(modelsList_full[[m]], modelsList_null[[m]])
      bfs[[i]] <- results[[i]]$bf
    }
    bf_mean <- mean(unlist(bfs))  # mean BF

    cat("BFs (full against null model)", file = outputtxtfile3, sep = "\n")
    cat(glue(outputtxtfile, "  vs  ", outputtxtfile2), file = outputtxtfile3, sep = "\n", append = TRUE)
    cat(unlist(bfs), file = outputtxtfile3 , sep = ", ", append = TRUE)
    cat(" ", file = outputtxtfile3 , sep = "\n", append = TRUE)
    cat("Mean BF", file = outputtxtfile3, sep = "\n", append = TRUE)
    cat(bf_mean, file = outputtxtfile3, sep = " ", append = TRUE)
  }
}