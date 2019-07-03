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
#' @export
varDecomp <- function(data1, data2, formula, weight, ...) {
        setDT(data1)
        setDT(data2)

        # Getting var names
        dep_var <- all.vars(formula[[2]])
        indep_vars <- all.vars(formula[[3]])

        # Selecting only the necessary variables
        data1 <- data1[, c(all.vars(formula), weight), with = FALSE]
        data2 <- data2[, c(all.vars(formula), weight), with = FALSE]

        # Applying transformations to y, if needed
        data1[[dep_var]] <- data1[, eval(formula[[2]])]
        data2[[dep_var]] <- data2[, eval(formula[[2]])]

        # Removing incomplete cases
        data1 <- data1[stats::complete.cases(data1)]
        data1 <- data1[is.finite(data1[[dep_var]])]

        data2 <- data2[stats::complete.cases(data2)]
        data2 <- data2[is.finite(data2[[dep_var]])]

        # TODO: have to make sure that both datasets have the same categories,
        #       and that categories that are non-present in both datasets are dropped

        # # Removing empty categories
        # for (indep_vars_i in indep_vars){
        #         print(table(data1[[indep_vars_i]]))
        #         print(table(data2[[indep_vars_i]]))

        #         categories <- c(levels(data1[[indep_vars_i]]), levels(data2[[indep_vars_i]]))
        #         categories <- unique(categories)

        #         data1[[indep_vars_i]] <- factor(data1[[indep_vars_i]],
        #                                        levels  = categories,
        #                                        labels  = categories,
        #                                        ordered = FALSE)

        #         data[[indep_vars_i]] <- fct_relevel(data[[indep_vars_i]], categories)

        #         data[[indep_vars_i]] <- fct_anon(data[[indep_vars_i]]) %>%
        #                 as.numeric()
        # }

        # Estimating the beta and lambda coefficients
        model1 <- get_parameters(data1, formula, weight)
        model2 <- get_parameters(data2, formula, weight)

        # parameters
        setnames(model1$parameter, c("beta", "lambda"), c("beta1", "lambda1"))
        setnames(model2$parameter, c("beta", "lambda"), c("beta2", "lambda2"))
        # sort = FALSE is important here -- otherwise the coefficients no longer line up
        parameters <- merge(model1$parameter, model2$parameter, by = "coef", all = TRUE,
            sort = FALSE)

        # cell frequencies
        setnames(model1$freq, c("n", "p"), c("n1", "p1"))
        setnames(model2$freq, c("n", "p"), c("n2", "p2"))
        freqs <- merge(model1$freq, model2$freq, by = indep_vars, all = TRUE)
        freqs[is.na(freqs)] <- 0

        #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
        # Variance Components

        # Basic design matrix
        modelmatrix <- stats::model.matrix(formula[c(1,3)], data = freqs)

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

        obs_variance1 <- var(data1[[dep_var]])
        obs_variance2 <- var(data2[[dep_var]])

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

        decomposition <- shapley(counterfactuals,
                                 factors,
                                 indep_vars = indep_vars,
                                 parameters = parameters,
                                 freqs = freqs,
                                 modelmatrix = modelmatrix, ...)
        setDT(decomposition)
        factors <- str_split_fixed(decomposition$factor, "_", n = 2)
        decomposition[, group := factors[, 1]]
        decomposition[, factor := factors[, 2]]
        decomposition[, group_value := sum(value), by = "group"]

        stopifnot(all.equal(decomposition[, sum(value), by = "group"][, V1],
            decomposition_simple$value))

        list(static = data.table(dataset = c("1", "2", "diff"),
                est_variance = c(est_variance1, est_variance2,
                    est_variance2 - est_variance1),
                est_between = c(between1, between2, between2 - between1),
                est_within = c(within1, within2, within2 - within1),
                obs_variance = c(obs_variance1, obs_variance2,
                    obs_variance2 - obs_variance1)),
             dynamic = decomposition)
}
