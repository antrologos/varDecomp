weighted.var <- function(x, w) {
    sumw <- sum(w)
    sumw2 <- sum(w^2)
    meanw <- sum(x * w) / sum(w)
    (sumw / (sumw^2 - sumw2)) * sum(w * (x - meanw)^2)
}

# Estimates beta and lambda coefficients
#' @import glm2
get_parameters <- function(data_i, formula) {
        dep_var <- all.vars(formula[[2]])
        indep_vars <- all.vars(formula[[3]])

        X <- stats::model.matrix(formula[c(1, 3)], data = data_i)
        w <- data_i[["weight"]]
        y <- data_i[[dep_var]]

        loglik_hist <- NULL
        diff_step <- 1
        iteration_count <- 0

        # Step 1 - OLS for the mean
        reg_mean <- stats::lm.wfit(x = X, y = y, w = w)
        e2 <- stats::residuals(reg_mean)^2

        # Getting stating values for the Gamma regression
        reg_var <- glm2::glm.fit2(x = X, y = e2, weights = w,
                              family = stats::Gamma(link = "log"),
                              control = list(maxit = 1000))
        coef_start <- reg_var$coefficients

        # Iterarative MLE  (from Western & Bloome, 2009)
        while (diff_step > 1e-8){
                # Step 2 - Gamma regression for the variance
                reg_var <- glm2::glm.fit2(x = X, y = e2, weights = w,
                                      family = stats::Gamma(link = "log"),
                                      start  = coef_start,
                                      control = list(maxit = 1000))
                coef_start <- reg_var$coefficients
                sigma_hat <- reg_var$fitted.values

                # Step 3 - WLS for the mean
                reg_mean <- stats::lm.wfit(x = X, y = y, w = w * (1 / sigma_hat))
                e2 <- stats::residuals(reg_mean)^2

                # Evatuating the log-likelihood
                Xl <- reg_var$linear.predictors
                loglik <- sum(-.5 * (Xl + e2 * exp(-Xl)))

                loglik_hist <- c(loglik_hist, loglik)

                iteration_count <- iteration_count + 1
                #print(iteration_count)
                if (iteration_count == 1){
                        next
                }

                diff_step <- last(diff(loglik_hist))
        }

        parameters <- data.table(coef = names(stats::coef(reg_mean)),
                beta = stats::coef(reg_mean),
                lambda = stats::coef(reg_var))

        freq <- data_i[, list(n = sum(weight)), by = indep_vars]
        freq[, p := n / sum(n)]

        list(parameters = parameters, freq = freq)
}


# Function for producing counterfactual GROUP PROPORTIONS
counterfactual_p <- function(freqs,
                             adjust_vars,
                             indep_vars,
                             association_effect,
                             max_iterations = 500,
                             zeros_replacement = 1e-10,
                             precision = 1e-8) {

        if (all(freqs[, p1 == p2])) {
            return(freqs[["p1"]])
        } else if (length(adjust_vars) == 0 & association_effect == FALSE) {
            return(freqs[["p1"]])
        } else if (length(adjust_vars) == length(indep_vars) & association_effect == TRUE) {
            return(freqs[["p2"]])
        }

        # subset
        vars <- c(indep_vars, "p1", "p2")
        ipf <- freqs[, ..vars]
        ipf[p1 == 0, p1 := zeros_replacement]
        ipf[p2 == 0, p2 := zeros_replacement]
        ipf[, p1 := p1 / sum(p1)]
        ipf[, p2 := p2 / sum(p2)]

        if (association_effect == TRUE) {
            ipf[, p := p2]
        } else {
            ipf[, p := p1]
        }

        # find s (source) and t (target) margins
        for (var in indep_vars) {
            ipf[, paste0("s_margin_", var) := sum(p), by = var]

            if (var %in% adjust_vars) {
                ipf[, paste0("t_margin_", var) := sum(p2), by = var]
            } else {
                ipf[, paste0("t_margin_", var) := sum(p1), by = var]
            }
        }

        converged <- FALSE

        for (i in 1:max_iterations) {
            for (var in sample(indep_vars)) {
                # adjust
                t <- paste0("t_margin_", var)
                s <- paste0("s_margin_", var)
                ipf[, p := p * get(t) / get(s)]
                ipf[, p := p / sum(p)]
            }

            # update all margins
            for (var in indep_vars) {
                ipf[, paste0("s_margin_", var) := sum(p), by = var]
            }

            # collect ratios
            ratios <- sapply(indep_vars, function(var) {
                t <- paste0("t_margin_", var)
                s <- paste0("s_margin_", var)
                ratio <- ipf[, list(first(get(s)), first(get(t))), by = var][, abs(V1 - V2)]
                all(ratio <= precision)
            })

            if (all(ratios)) {
                converged <- TRUE
                break
            }
        }

        if (!converged) {
            warning("IPF did not converge, increase max_iterations or lower precision")
        }

        ipf$p
}

counterfactuals_simple <- function(factors, freqs) {
        if ("mean" %in% factors)
                mu_group <- freqs[, "mu_group2"]
        else
                mu_group <- freqs[, "mu_group1"]

        if ("var" %in% factors)
                var <- freqs[, "var2"]
        else
                var <- freqs[, "var1"]

        if ("comp" %in% factors)
                p <- freqs[, "p2"]
        else
                p <- freqs[, "p1"]

        mu <- sum(p * mu_group)
        sum((mu_group - mu)^2 * p) + sum(var * p)
}

# Counterfactual function for the Shapley decomposition
#' @import stringr
counterfactuals <- function(factors, indep_vars, parameters, freqs, modelmatrix,
                            mf_counterfactual_p, ...) {

        if (any(str_detect(factors, "^mean_"))) {
                factors_mean <- factors[str_detect(factors, "^mean_")]
                factors_mean <- str_remove_all(factors_mean, "^mean_")
                factors_mean <- paste(factors_mean, collapse = "|")

                cntf <- parameters[, ifelse(str_detect(coef, factors_mean), beta2, beta1)]
                mu_group <- (modelmatrix %*% cntf)[, 1]
        } else {
                mu_group <- freqs[["mu_group1"]]
        }

        if (any(str_detect(factors, "^var_"))) {
                factors_var <- factors[str_detect(factors, "^var_")]
                factors_var <- str_remove_all(factors_var, "^var_")
                factors_var <- paste(factors_var, collapse = "|")

                cntf <- parameters[, ifelse(str_detect(coef, factors_var), lambda2, lambda1)]
                var <- (exp(modelmatrix %*% cntf))[, 1]
        } else {
                var <- freqs[["var1"]]
        }

        if (any(str_detect(factors, "^comp_"))) {
                factors_comp <- factors[str_detect(factors, "^comp_")]
                factors_comp <- str_remove_all(factors_comp, "^comp_")

                p <- mf_counterfactual_p(freqs = freqs,
                                         adjust_vars = factors_comp[factors_comp != "association"],
                                         indep_vars = indep_vars,
                                         association_effect = "association" %in% factors_comp, ...)
        } else {
                p <- freqs[["p1"]]
        }

        mu <- sum(p * mu_group)
        sum((mu_group - mu)^2 * p) + sum(var * p)
}
