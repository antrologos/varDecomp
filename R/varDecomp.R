#' @export
varDecomp <- function(formula, weight, periods, data){

        data = setDT(data)

        # Getting var names
        dep_var    = all.vars(formula[[2]])
        indep_vars = all.vars(formula[[3]])

        # Selecting only the necessary variables
        data <- data[ , c(all.vars(formula), weight, periods), with = F]

        # Applying transformations to y, if needed
        data[[dep_var]] <- data[ , eval(formula[[2]])]

        # Removing incomplete cases
        data   <- data[complete.cases(data)]
        finite <- is.finite(data[[dep_var]])
        nan    <- is.nan(data[[dep_var]])
        data   <- data[finite & !(nan)]
        rm(finite, nan); gc()

        data <- data %>%
                arrange_at(c(periods, indep_vars)) %>%
                setDT()

        # Removing empty categories
        for(indep_vars_i in indep_vars){
                #print(indep_vars_i)

                categories <- data[[indep_vars_i]] %>% unique() %>% sort() %>% as.character()

                data[[indep_vars_i]] <- factor(data[[indep_vars_i]],
                                               levels  = categories,
                                               labels  = categories,
                                               ordered = F)

                data[[indep_vars_i]] <- fct_relevel(data[[indep_vars_i]], categories)

                #data[[indep_vars_i]] <- fct_anon(data[[indep_vars_i]]) #%>%
                #z       as.numeric()
        }

        data <- data %>%
                arrange_at(c(periods, indep_vars)) %>%
                setDT()

        data_split <- split(data, data[[periods]])

        # Estimating the beta and lambda coefficients
        parameters_periods <- map(.x = data_split,
                                  .f = varDecomp:::get_parameters,
                                  formula = formula,
                                  weight  = weight,
                                  periods = periods)

        betas_tmp   = map(parameters_periods, function(x) x$beta)
        lambdas_tmp = map(parameters_periods, function(x) x$lambda)

        names_coef = names(betas_tmp[[1]])

        betas   <- do.call(rbind, lapply(1:length(betas_tmp), function(x) betas_tmp[[x]][names_coef])) %>% as.matrix() %>% t()
        lambdas <- do.call(rbind, lapply(1:length(lambdas_tmp), function(x) lambdas_tmp[[x]][names_coef])) %>% as.matrix() %>% t()

        row.names(betas) <- row.names(lambdas) <- names_coef
        colnames(betas)  <- colnames(lambdas)  <- names(betas_tmp)

        betas[is.na(betas)]     <- 0
        lambdas[is.na(lambdas)] <- 0

        #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

        # Estimating the cell frequencies
        freqs <- map_dfr(parameters_periods, function(x) x$freq)

        freqs <- freqs %>%
                arrange_at(indep_vars)

        p_data <- freqs %>%
                tidyr::unite(col = id, sep = "_", indep_vars) %>%
                dplyr::select(-n) %>%
                tidyr::spread(key = period, value = p) %>%
                tidyr::separate(col = id, into = indep_vars, sep = "_") %>%
                data.table::setDT()

        #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
        # Variance Components

        # Basic design matrix
        X = model.matrix(formula[c(1,3)], data = p_data)
        X <- X[, names_coef]

        # Cell's proportion
        p_jt = p_data %>% dplyr::select(-indep_vars) %>% as.matrix()
        p_jt[is.na(p_jt)] <- 0

        # Group means
        mu_jt <- X %*% betas

        # Grand mean
        mu_t   <- colSums( (X %*% betas) * p_jt )

        # Group variances
        s2_jt <- exp( X %*% lambdas)

        #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
        # Estimating the actual/observed variance from the components

        # Between groups component
        between = colSums( ( (t(t(mu_jt)-mu_t))^2) * p_jt )

        # Within groups component
        within  = colSums( s2_jt * p_jt )

        # Variance
        estimated_Var <- between + within

        #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
        #-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-
        # BEGINNIG OF THE COUNTERFACTUAL DECOMPOSITION

        p_data <- p_data %>%
                mutate_all(function(x) {
                        x[is.na(x)] <- 0
                        x}
                )

        factors <- list(meanEffect = paste0("meanEffect_", indep_vars),
                        varEffect  = c("varEffect_Intercept", paste0("varEffect_", indep_vars)),
                        compEffect = c("compEffect_association", paste0("compEffect_", indep_vars)))


        period_names <- names(p_data)[!names(p_data) %in% indep_vars]

        if(!any(c("multiprocess", "multicore", "multisession", "cluster") %in% class(plan()))){
                plan(multiprocess)
        }
        decomposition <- future_map(.x = 1:ncol(betas),
                                    .f = function(t){
                                            decomp_t <- shapley(vfun = varDecomp:::counterfactuals,
                                                                factors,
                                                                t       = t,
                                                                betas   = betas,
                                                                lambdas = lambdas,
                                                                p_data  = p_data,
                                                                indep_vars = indep_vars,
                                                                X       = X)

                                            decomp_t$group <- period_names[t]
                                            decomp_t
                                    },
                                    .progress = T,
                                    .options  = future_options(packages = c("stringr",
                                                                            "data.table",
                                                                            "dplyr")))

        decomposition <- do.call(bind_rows, decomposition)

        list(staticDecomposition  = tibble(periods = period_names, estimated_Var, between, within),
             dynamicDecomposition = decomposition)
}
