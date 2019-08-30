#' Variance decomposition
#'
#' @param data1 First dataset
#' @param data2 Second dataset
#' @param formula Model formula
#' @param weight Weight variable
#' @param ... Additional arguments for IPF.
#' @return A list of decompositions
#' @import data.table
#' @import shapley
#' @import memoise
#' @export
varDecomp <- function(data1, data2, formula, weight = NULL, iterative.mle = F, ...) {

    data1 <- as.data.table(data1)
    data2 <- as.data.table(data2)

    # make sure that weights are always in "weight"
    if (is.null(weight)) {
        data1[, weight := 1]
        data2[, weight := 1]
    } else if (weight != "weight") {
        data1[, weight := get(weight)]
        data2[, weight := get(weight)]
    }

    # Getting var names
    dep_var    <- all.vars(formula[[2]])
    indep_vars <- all.vars(formula[[3]])

    # Selecting only the necessary variables
    data1 <- data1[, c(all.vars(formula), "weight"), with = FALSE]
    data2 <- data2[, c(all.vars(formula), "weight"), with = FALSE]


    # Removing incomplete cases
    cases_before <- nrow(data1)
    data1 <- data1[stats::complete.cases(data1)]
    data1 <- data1[is.finite(data1[[dep_var]])]
    cases_after <- nrow(data1)
    if (cases_after < cases_before)
        warning(paste0("Dropped ", cases_before - cases_after, " cases from data1"))
    if (nrow(data1) == 0)
        stop("data1 does not contain any rows")

    cases_before <- nrow(data2)
    data2 <- data2[stats::complete.cases(data2)]
    data2 <- data2[is.finite(data2[[dep_var]])]
    cases_after <- nrow(data2)
    if (cases_after < cases_before)
        warning(paste0("Dropped ", cases_before - cases_after, " cases from data2"))
    if (nrow(data2) == 0)
        stop("data2 does not contain any rows")

    # # Removing empty categories
    for (var in indep_vars) {
        data1[[var]] <- droplevels(data1[[var]])
        data2[[var]] <- droplevels(data2[[var]])

        levels1 <- paste0(levels(data1[[var]]), collapse = "|")
        levels2 <- paste0(levels(data2[[var]]), collapse = "|")

        if (levels1 != levels2)
            stop(paste0("factor levels are not identical in variable ", var))

        # apply contrast coding
        n_levels <- length(levels(data1[[var]]))
        contrasts(data1[[var]]) <- contr.sum(n_levels)
        contrasts(data2[[var]]) <- contr.sum(n_levels)
    }

    # Estimating the beta and lambda coefficients
    model1 <- get_parameters(data1, formula, iterative.mle)
    model2 <- get_parameters(data2, formula, iterative.mle)

    # parameters
    setnames(model1$parameter, c("beta", "lambda"), c("beta1", "lambda1"))
    setnames(model2$parameter, c("beta", "lambda"), c("beta2", "lambda2"))
    # sort = FALSE is important here -- otherwise the coefficients no longer line up
    parameters <- merge(model1$parameter, model2$parameter, by = "coef", all = TRUE,
        sort = FALSE)
    parameters[is.na(parameters)] <- 0

    # cell frequencies
    setnames(model1$freq, c("n", "p"), c("n1", "p1"))
    setnames(model2$freq, c("n", "p"), c("n2", "p2"))
    freqs <- merge(model1$freq, model2$freq, by = indep_vars, all = TRUE)
    freqs[is.na(freqs)] <- 0

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Variance Components

    # Basic design matrix
    modelmatrix <- stats::model.matrix(formula[c(1, 3)], data = freqs)

    # group means
    freqs[, `:=`(
        mu_group1 = (modelmatrix %*% parameters$beta1)[, 1],
        mu_group2 = (modelmatrix %*% parameters$beta2)[, 1])
    ]

    # grand mean
    freqs[, `:=`(
        mu1 = sum(p1 * mu_group1),
        mu2 = sum(p2 * mu_group2))
    ]

    # group variances
    freqs[, `:=`(
        var1 = exp(modelmatrix %*% parameters$lambda1)[, 1],
        var2 = exp(modelmatrix %*% parameters$lambda2)[, 1])
    ]

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Estimating the actual/observed variance from the components

    between1 <- freqs[, sum((mu_group1 - mu1)^2 * p1)]
    within1 <- freqs[, sum(var1 * p1)]
    est_variance1 <- between1 + within1

    between2 <- freqs[, sum((mu_group2 - mu2)^2 * p2)]
    within2 <- freqs[, sum(var2 * p2)]
    est_variance2 <- between2 + within2

    obs_variance1 <- data1[ , weighted.var(eval(formula[[2]]), weight)]
    obs_variance2 <- data2[ , weighted.var(eval(formula[[2]]), weight)]

    #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
    # Counterfactual decomposition

    factors_simple <- c("mean", "var", "comp")
    decomposition_simple <- shapley(counterfactuals_simple,
                             factors_simple,
                             freqs = freqs, silent = TRUE)
    stopifnot(all.equal(sum(decomposition_simple$value),
        est_variance2 - est_variance1))

    factors <- list(
        mean = paste0("mean_", indep_vars),
        var  = c("var_Intercept", paste0("var_", indep_vars)),
        comp = c("comp_association", paste0("comp_", indep_vars)))

    # cache IPF results
    mf_counterfactual_p <- memoise::memoise(counterfactual_p)
    decomposition <- shapley(counterfactuals,
                             factors,
                             indep_vars = indep_vars,
                             parameters = parameters,
                             freqs = freqs,
                             modelmatrix = modelmatrix,
                             mf_counterfactual_p = mf_counterfactual_p, ...)
    memoise::forget(mf_counterfactual_p)

    setDT(decomposition)
    factors <- str_split_fixed(decomposition$factor, "_", n = 2)
    decomposition[, group := factors[, 1]]
    decomposition[, factor := factors[, 2]]
    decomposition[, group_value := sum(value), by = "group"]

    stopifnot(all.equal(decomposition[, sum(value), by = "group"][, V1],
        decomposition_simple$value))

    list(parameters = parameters,
        static = data.table(dataset = c("1", "2", "diff"),
            est_variance = c(est_variance1, est_variance2,
                est_variance2 - est_variance1),
            est_between = c(between1, between2, between2 - between1),
            est_within = c(within1, within2, within2 - within1),
            obs_variance = c(obs_variance1, obs_variance2,
                obs_variance2 - obs_variance1)),
         dynamic = decomposition)
}
