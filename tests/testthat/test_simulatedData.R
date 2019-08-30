#rm(list=ls())
#options(scipen = 999)
library("testthat")
library("varDecomp")
library("data.table")

context("test_simulations")

#==========================================================
# Useful functions

# Creates a simulated dataset, using pre-defined parameters
simData <- function(pars, f){

        setDT(pars$freq)
        pars$freq <- pars$freq[order(educr, racer)]

        indep_vars = names(pars$freq)
        indep_vars = indep_vars[!indep_vars %in% c("n", "p")]

        for(var in indep_vars){
                # apply contrast coding
                n_levels <- length(levels(pars$freq[[var]]))
                contrasts(pars$freq[[var]]) <- contr.sum(n_levels)
        }

        f[[2]] <- NULL

        X <- model.matrix(f, data = pars$freq[,-c("n","p")])


        X <- X[ , pars$parameters$coef]
        beta   <- pars$parameters$beta
        lambda <- pars$parameters$lambda

        groupMeans <- X%*%beta
        groupVars  <- exp(X%*%lambda)
        groupSize  <- pars$freq$p

        #sigma2 <- sum(groupVars*groupSize)

        data_sim = data.table()
        for(i in 1:length(groupSize)){

                p = seq(.001,.999,.001)

                wage_group <- qnorm(p = p,
                                    mean = groupMeans[i],
                                    sd   = sqrt(groupVars[i]))

                density <- dnorm(x = wage_group,
                                 mean = groupMeans[i],
                                 sd   = sqrt(groupVars[i]))

                weight <- (density/sum(density))*groupSize[i]

                data_group <- data.table(wage  = exp(wage_group),# + sigma2/2),
                                         racer = pars$freq$racer[i],
                                         educr = pars$freq$educr[i],
                                         weight = weight)

                data_sim = rbind(data_sim, data_group)
        }

        data_sim <- data_sim[order(educr, racer)]
        for(var in indep_vars){
                # apply contrast coding
                n_levels <- length(levels(data_sim[[var]]))
                contrasts(data_sim[[var]]) <- contr.sum(n_levels)
        }

        data_sim
}



p_descr <- function(pars){
        p <- pars$freq %>%
                dplyr::select(-n) %>%
                dplyr::arrange(educr, racer) %>%
                tidyr::spread(key = racer, value = p) %>%
                .[, c(2,3)] %>%
                as.matrix() %>%
                as.table()

        list(col = colSums(p),
             row = rowSums(p),
             odds = vcd::loddsratio(p))
}


compare_p <- function(pars1, pars2, digits = 6){

        t1 <- p_descr(pars1)
        t2 <- p_descr(pars2)


        c(col = all(round(t1$col, digits) == round(t2$col, digits)),

          row = all(round(t1$row, digits) == round(t2$row, digits)),

          odds = all(round(t1$odds$coefficients, digits) == round(t2$odds$coefficients, digits)))

}


#==========================================================
# Setting/Getting initial variables and parameters

data(wage)
setDT(wage)
wage <- wage[wage > 5000]

wage <- wage[order(educr, racer)]
indep_vars = c("educr", "racer")
for(var in indep_vars){
        # apply contrast coding
        n_levels <- length(levels(wage[[var]]))
        contrasts(wage[[var]]) <- contr.sum(n_levels)
}

f <- log(wage) ~ racer * educr
wage$weight = 1

# Getting the original parameter (that will be used and modified for generating simulated data)
pars <- varDecomp:::get_parameters(data_i = wage, formula = f, iterative.mle = F)

#==========================================================

# Simulating the initial data (this will be used, instead of the original dataset)
wage           <- simData(pars, f)


#==========================================================
# TESTS


test_that("mean: racer", {
        pars2 = pars
        pars2$parameters$beta[2] <- 1
        wage2 <- simData(pars2, f)
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)

        # Test
        values_for_testing <- v$dynamic[group == "mean" & factor == "racer"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)
})


test_that("mean: educr", {


        pars2 = pars
        pars2$parameters$beta[3] <- 1
        wage2 <- simData(pars2, f)
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)
        values_for_testing <- v$dynamic[group == "mean" & factor == "educr"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)


        pars2 = pars
        pars2$parameters$beta[4] <- 1
        wage2 <- simData(pars2, f)
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)
        values_for_testing <- v$dynamic[group == "mean" & factor == "educr"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)

        pars2 = pars
        pars2$parameters$beta[5] <- 1
        wage2 <- simData(pars2, f)
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)
        values_for_testing <- v$dynamic[group == "mean" & factor == "educr"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)

        pars2 = pars
        pars2$parameters$beta[6] <- 1
        wage2 <- simData(pars2, f)
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)
        values_for_testing <- v$dynamic[group == "mean" & factor == "educr"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)

})

test_that("var: Intercept", {

        # Making a counterfactual version in which just "var: intercept" changes
        pars2 = pars
        pars2$parameters$lambda[1] <- 1
        wage2 <- simData(pars2, f)

        # just checking the parameters:
        test <- varDecomp:::get_parameters(data_i = wage2, formula = f, iterative.mle = F)

        # No changes in the group sizes
        expect_equal(pars$freq[, -"n"], test$freq[, -"n"])

        # Variance decomposition
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)

        # Test
        values_for_testing <- v$dynamic[factor == "Intercept"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)

})


test_that("var: racer", {
        pars2 = pars
        pars2$parameters$lambda[2] <- 1
        wage2 <- simData(pars2, f)
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)

        # Test
        values_for_testing <- v$dynamic[group == "var" & factor == "racer"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)
})



test_that("var: educr", {

        pars2 = pars
        pars2$parameters$lambda[3] <- 1
        wage2 <- simData(pars2, f)
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)
        values_for_testing <- v$dynamic[group == "var" & factor == "educr"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)

        pars2 = pars
        pars2$parameters$lambda[4] <- 1
        wage2 <- simData(pars2, f)
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)
        values_for_testing <- v$dynamic[group == "var" & factor == "educr"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)

        pars2 = pars
        pars2$parameters$lambda[5] <- 1
        wage2 <- simData(pars2, f)
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)
        values_for_testing <- v$dynamic[group == "var" & factor == "educr"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)

        pars2 = pars
        pars2$parameters$lambda[6] <- 1
        wage2 <- simData(pars2, f)
        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative = F)
        values_for_testing <- v$dynamic[group == "var" & factor == "educr"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)

})

#============================================================
#============================================================

# COMPOSITION EFFECTS

# Preliminaries
pars_tmp <- pars

set.seed(123)
pars_tmp$freq$n <- pars_tmp$freq$n*(rpois(nrow(pars_tmp$freq), 3) + 1)
pars_tmp$freq$p <- pars_tmp$freq$n/sum(pars_tmp$freq$n)

freqs    <- pars$freq
freqs$p1 <- pars$freq$p
freqs$p2 <- pars_tmp$freq$p
freqs$p  <- freqs$n  <- NULL


#============================================================

# Composition: Education

test_that("comp: educr", {

        pars2 <- pars

        pars2$freq$p <- varDecomp:::counterfactual_p(freqs = freqs,
                                                     adjust_vars = "educr",
                                                     indep_vars = c("educr", "racer"),
                                                     association_effect = F)

        wage2 <- simData(pars2, f)

        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative.mle = F)

        values_for_testing <- v$dynamic[group == "comp" & factor == "educr"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)

})

test_that("comp: racer", {

        pars2 <- pars

        pars2$freq$p <- varDecomp:::counterfactual_p(freqs = freqs,
                                                     adjust_vars = "racer",
                                                     indep_vars = c("educr", "racer"),
                                                     association_effect = F)

        wage2 <- simData(pars2, f)

        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative.mle = F)

        values_for_testing <- v$dynamic[group == "comp" & factor == "racer"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)
})


test_that("comp: association", {

        pars2 <- pars
        pars2$freq$p <- varDecomp:::counterfactual_p(freqs = freqs,
                                                     adjust_vars = NULL,
                                                     indep_vars = c("educr", "racer"),
                                                     association_effect = T)

        wage2 <- simData(pars2, f)

        v <- varDecomp(wage, wage2, f, weight = "weight", silent = TRUE, iterative.mle = F)

        values_for_testing <- v$dynamic[factor == "association"]
        expect_equal(values_for_testing$value, values_for_testing$group_value)
})


