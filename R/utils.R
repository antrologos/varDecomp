# Estimates beta and lambda coefficients
get_parameters <- function(data_i, formula, weight, periods){

        setDT(data_i)

        period_i <- unique(data_i[[periods]])

        dep_var    <- all.vars(formula[[2]])
        indep_vars <- all.vars(formula[[3]])

        X <- model.matrix(formula[c(1,3)], data = data_i)
        w <- data_i[[weight]]
        y       <- data_i[[dep_var]]

        loglik_hist <- NULL
        diff_step   <- 1
        iteration_count <- 0

        # Step 1 - OLS for the mean
        reg_mean <- lm.wfit(x = X, y = y, w = w)
        e2       <- residuals(reg_mean)^2

        # Getting stating values for the Gamma regression
        reg_var   <- glm.fit2(x = X, y = e2, weights = w,
                              family = Gamma(link = "log"),
                              control = list(maxit = 1000))
        coef_start = reg_var$coefficients

        # Iterarative MLE  (from Western & Bloome, 2009)
        while(diff_step > 1e-8){
                # Step 2 - Gamma regression for the variance
                reg_var   <- glm.fit2(x = X, y = e2, weights = w,
                                      family = Gamma(link = "log"),
                                      start  = coef_start,
                                      control = list(maxit = 1000))
                coef_start = reg_var$coefficients
                sigma_hat <- reg_var$fitted.values

                # Step 3 - WLS for the mean
                reg_mean <- lm.wfit(x = X, y = y, w = w*(1/sigma_hat))
                e2       <- residuals(reg_mean)^2

                # Evatuating the log-likelihood
                Xl     <- reg_var$linear.predictors
                loglik <- sum(-.5*(Xl + e2*exp(-Xl)))

                loglik_hist <- c(loglik_hist, loglik)

                iteration_count = iteration_count + 1
                #print(iteration_count)
                if(iteration_count == 1){
                        next
                }

                diff_step <- last(diff(loglik_hist))
        }

        beta   = coef(reg_mean)
        lambda = coef(reg_var)

        data_i$w <- data_i[[weight]]

        freq <- data_i[ , list( n = sum(w) ), by = c(indep_vars)]
        freq$period = period_i
        freq[ , p := n/sum(n)]

        list(beta = beta, lambda = lambda, freq = freq)
}


# Function for producing counterfactual coefficients (either beta or lambda)
counterfactual_coef <- function(coefMatrix, var_names, t0_col = 1, t1_col = 2){
        coef0 <- coefMatrix[, t0_col]
        coef1 <- coefMatrix[, t1_col]

        if(is.null(var_names)){
                return(coef0)
        }

        if(length(var_names) == 1){
                if(var_names == ""){
                        return(coef0)
                }
        }

        expression <- paste(var_names, collapse = "|")
        coef0[str_detect(names(coef0), expression)] <- coef1[str_detect(names(coef0), expression)]
        coef0
}

# Function for producing counterfactual GROUP MEANS
counterfactual_mu <- function(betas, var_names, X, t0_col = 1, t1_col = 2){
        betaC <- counterfactual_coef(betas, var_names, t0_col, t1_col)
        as.numeric(X %*% betaC)
}

# Function for producing counterfactual GROUP VARIANCES
counterfactual_s2 <- function(lambdas, var_names, X, t0_col = 1, t1_col = 2){
        lambdasC <- counterfactual_coef(lambdas, var_names, t0_col, t1_col)
        as.numeric(exp(X %*% lambdasC))
}


# Get marginal distribution of the independent variables
get_marginals <- function(data, indep_var){
        data = as.data.frame(data)
        categories <- sort(unique(data[[indep_var]]))

        result <- tibble(categories, p = NA)
        names(result)[1] <- indep_var

        for(i in 1:length(categories)){
                expr = paste0("data$", indep_var, " == '", categories[i],"'")
                result$p[i] <- sum(data[eval(parse(text = expr)), ncol(data)])
        }
        result
}

# Function for producing counterfactual GROUP PROPORTIONS
counterfactual_p <- function(p_data,
                             var_names = NULL,
                             indep_vars,
                             t0_order = 1,
                             t1_order = t,
                             association_effect = T,
                             zeros_replacement = 1e-6, tol = 1e-20){

        period_names <- names(p_data)[!names(p_data) %in% indep_vars]

        t0_name <- period_names[t0_order]
        t1_name <- period_names[t1_order]

        p0_data <- p_data %>%
                dplyr::select(c(indep_vars, t0_name))

        pt_data <- p_data %>%
                dplyr::select(c(indep_vars, t1_name))

        names(pt_data)[ncol(pt_data)] <- names(p0_data)[ncol(p0_data)] <- "Freq"


        if(is.null(var_names) & association_effect == F){
                return(p0_data)
        }

        if(length(var_names) == 1 & association_effect == F){
                if(var_names == ""){
                        return(p0_data)
                }
        }

        m0 <- lapply(indep_vars, function(x) get_marginals(p0_data, x))
        mt <- lapply(indep_vars, function(x) get_marginals(pt_data, x))
        names(m0) = names(mt) = indep_vars

        marginals <- m0
        if(length(var_names) > 0){
                for(var_i in var_names){
                        marginals[[var_i]] <- mt[[var_i]]
                }
        }

        if(association_effect == T){
                dataStructure = pt_data
        }else{
                dataStructure = p0_data
        }

        # ITERATIVE PROPORTIONAL FITTING
        euclidian_distance = as.double(1)
        while(euclidian_distance > tol){

                dataStructure_original = dataStructure

                #var_i = vars[1]
                for(var_i in var_names){
                        #print(var_i)

                        f_s = get_marginals(dataStructure, indep_var = var_i)
                        f_m = marginals[[var_i]]

                        if(!is.null(zeros_replacement)){
                                f_s <- f_s %>%
                                        mutate(p = ifelse(p == 0, zeros_replacement, p))
                        }

                        data_ajust = left_join(f_s %>% rename(f_s = p),
                                               f_m %>% rename(f_m = p),
                                               by = var_i) %>%
                                mutate(adjust = as.numeric(f_m/f_s)) %>%
                                dplyr::select(-f_s, -f_m)

                        data_ajust$adjust[is.nan(data_ajust$adjust)] <- 0
                        dataStructure = left_join(dataStructure, data_ajust, by = var_i)

                        #setDT(dataStructure)
                        #dataStructure[ , Freq := Freq * adjust]
                        dataStructure$Freq = dataStructure$Freq * dataStructure$adjust
                        dataStructure$adjust = NULL
                }
                euclidian_distance <- sum((dataStructure_original$Freq - dataStructure$Freq)^2)

                euclidian_distance
        }

        dataStructure
}


# Counterfactual function for the Shapley decomposition
counterfactuals <- function(factors, t, betas, lambdas, p_data, indep_vars, X){

        factors <- unlist(factors)

        beta0   <- betas[, 1]
        lambda0 <- lambdas[, 1]

        mu_j <- as.numeric(X%*%beta0)
        s2_j <- as.numeric(exp(X%*%lambda0))
        p_j  <- p_data[ , !str_detect(names(p_data), paste(indep_vars, collapse="|"))][,1]

        if(any(str_detect(factors, "meanEffect_"))){
                factors_meanEffect <- factors[str_detect(factors, "meanEffect_")]
                factors_meanEffect <- str_remove_all(factors_meanEffect, "meanEffect_")
                mu_j               <-  counterfactual_mu(betas, factors_meanEffect, X = X, t0_col = 1, t1_col = t)
        }

        if(any(str_detect(factors, "varEffect_"))){
                factors_varEffect <- factors[str_detect(factors, "varEffect_")]
                factors_varEffect <- str_remove_all(factors_varEffect, "varEffect_")
                s2_j              <- counterfactual_s2(lambdas, factors_varEffect, X = X, t0_col = 1, t1_col = t)
        }

        if(any(str_detect(factors, "compEffect_"))){
                factors_compEffect <- factors[str_detect(factors, "compEffect_")]
                factors_compEffect <- str_remove_all(factors_compEffect, "compEffect_")

                association_effect <- any(str_detect(factors_compEffect, "association"))
                var_names          <- factors_compEffect[!str_detect(factors_compEffect, "association")]

                p_j <- counterfactual_p(p_data = p_data,
                                        var_names = var_names,
                                        indep_vars = indep_vars,
                                        t0_order = 1,
                                        t1_order = t,
                                        association_effect = association_effect,
                                        zeros_replacement = 1e-6, tol = 1e-20)

                p_j <- p_j$Freq

        }

        # Recalculating the grand mean
        mu = sum(mu_j*p_j)

        # Variance
        sum(p_j*((mu_j - mu)^2) ) + sum(s2_j*p_j)
}

