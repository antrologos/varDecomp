#rm(list=ls())
#options(scipen = 999)
library("testthat")
library("varDecomp")
library("data.table")

context("test_changingExistingData")

data(wage)
setDT(wage)
wage <- wage[wage > 5000]
f <- log(wage) ~ -1 + racer * educr  ######### THE INTERCEPT WAS OMITTED

# this file tests whether its possible to manipulate each component in isolation,
# using existing data

test_that("no change", {
    wage2 <- copy(wage)
    v <- varDecomp(wage, wage2, f, silent = TRUE, precision = 1e-11)

    expect_equal(v$static$est_variance[[3]], 0)
    expect_equal(sum(v$dynamic$value), 0)
})


test_that("mean effect: educr", {
    # increase college premium
    wage2 <- copy(wage)
    wage2[educr == "4-year college+", wage := 10 * wage]
    v <- varDecomp(wage, wage2, f, silent = TRUE, precision = 1e-11)

    # static and dynamic match
    expect_equal(v$static$est_variance[[3]], sum(v$dynamic$value))

    # change is only explained by mean educr effect, i.e. all others are 0
    expect_equal(v$static$est_variance[[3]], v$dynamic[group == "mean" & factor == "educr", value])
})


test_that("mean effect: racer", {
    # wage penalty for blacks
    wage2 <- copy(wage)
    wage2[racer == "Black", wage := .8 * wage]
    v <- varDecomp(wage, wage2, f, silent = TRUE, precision = 1e-11)

    # static and dynamic match
    expect_equal(v$static$est_variance[[3]], sum(v$dynamic$value))
    # change is only explained by mean racer effect, i.e. all others are 0
    expect_equal(v$static$est_variance[[3]], v$dynamic[group == "mean" & factor == "racer", value])
})


test_that("var: old Intercept Effect (Intercept was ommited)", {

    # increase variance in earnings without affecting the mean

    wage2 <- copy(wage)
    setDT(wage2, key = c("educr", "racer"))

    groups <- expand.grid(racer = unique(wage$racer),
                          educr = unique(wage$educr))
    setDT(groups, key = c("educr", "racer"))

    data_tmp = data.table()
    for(i in 1:nrow(groups)){
        group_i = groups[i, ]

        dataGroup <- wage2[group_i]
        n = nrow(dataGroup)

        geoMean <- dataGroup[, exp(mean(log(wage)))]

        above <- dataGroup[, which(wage > geoMean)]
        below <- dataGroup[, which(wage < geoMean)]

        cat(" length(above) =", length(above), "\n",
            "length(below) =", length(below), "\n",
            "round(.1*n))  =", round(.1*n), "\n\n\n\n")

        size <- min(length(above), length(below), round(.2*n))

        above = above[1:size]
        below = below[1:size]

        dataGroup[above, wage := wage * 5]
        dataGroup[below, wage := wage / 5]

        data_tmp = rbind(data_tmp, dataGroup)
    }
    wage2 = data_tmp

    # (geometric) means are unchanged
    mean1 <- wage[, exp(mean(log(wage)))]
    mean2 <- wage2[, exp(mean(log(wage)))]
    expect_equal(mean1, mean2)

    # group (geometric) means are unchanged
    group_means1 <-  wage[order(racer, educr), exp(mean(log(wage))), by = c("racer", "educr")]
    group_means2 <- wage2[order(racer, educr), exp(mean(log(wage))), by = c("racer", "educr")]
    expect_equivalent(group_means1, group_means2)

    # all group log variances changed
    group_vars1 <-  wage[order(racer, educr), var(log(wage)), by = c("racer", "educr")]
    group_vars2 <- wage2[order(racer, educr), var(log(wage)), by = c("racer", "educr")]
    expect_true(all(!group_vars1$V1 == group_vars2$V1))

    v <- varDecomp(wage, wage2, f, silent = TRUE, precision = 1e-15)

    # All changes/effects should be due only to the variance components of race and education
    values_for_testing <- v$dynamic[group == "var"]

    expect_equal(sum(values_for_testing$value[-1]), v$static$est_variance[3])
})



test_that("var: educr", {
    # increase variance in earnings without affecting the mean

    wage2 <- copy(wage)
    setDT(wage2, key = c("educr", "racer"))

    groups <- expand.grid(racer = unique(wage$racer),
                          educr = unique(wage$educr))
    setDT(groups, key = c("educr", "racer"))

    other_groups <- groups[educr != "4-year college+"]
    groups       <- groups[educr == "4-year college+"]

    data_tmp = data.table()
    for(i in 1:nrow(groups)){
        group_i = groups[i, ]

        dataGroup <- wage2[group_i]
        geoMean <- dataGroup[, exp(mean(log(wage)))]

        above <- dataGroup[, which(wage > geoMean)]
        below <- dataGroup[, which(wage < geoMean)]

        size <- min(length(above), length(below))

        above = above[1:size]
        below = below[1:size]

        dataGroup[above, wage := wage * 2]
        dataGroup[below, wage := wage / 2]

        data_tmp = rbind(data_tmp, dataGroup)
    }
    wage2 = rbind(wage2[other_groups], data_tmp)

    # (geometric) means are unchanged
    mean1 <- wage[, exp(mean(log(wage)))]
    mean2 <- wage2[, exp(mean(log(wage)))]
    expect_equal(mean1, mean2)

    # grop (geometric) means are unchanged
    group_means1 <-  wage[order(racer, educr), exp(mean(log(wage))), by = c("racer", "educr")]
    group_means2 <- wage2[order(racer, educr), exp(mean(log(wage))), by = c("racer", "educr")]
    expect_equivalent(group_means1, group_means2)

    # all group log variances are unchanged, but "4-year college+"
    group_vars1 <-  wage[educr == "4-year college+", var(log(wage)), by = c("racer", "educr")]
    group_vars2 <- wage2[educr == "4-year college+", var(log(wage)), by = c("racer", "educr")]
    expect_true(all(!group_vars1$V1 == group_vars2$V1))

    group_vars1 <-  wage[educr != "4-year college+", var(log(wage)), by = c("racer", "educr")]
    group_vars2 <- wage2[educr != "4-year college+", var(log(wage)), by = c("racer", "educr")]
    expect_true(all(group_vars1[order(racer, educr)]$V1 == group_vars2[order(racer, educr)]$V1))

    v <- varDecomp(wage, wage2, f, silent = TRUE, precision = 1e-11)
    values_for_testing <- v$dynamic[factor == "educr" & group == "var"]

    # TODO: this produces relatively large effects for var_racer
    # Is this some how related to group composition?
    # THE SAME THING DOES NOT OCCUR IN THE EXAMPLE I SEND TO BEN IN SLACK
    # (see 'Coding Schemes and Intercept Effects.R')
    expect_equal(values_for_testing$value, values_for_testing$group_value)
})


test_that("var: racer", {
    # increase variance in earnings without affecting the mean

    wage2 <- copy(wage)
    setDT(wage2, key = c("educr", "racer"))

    groups <- expand.grid(racer = unique(wage$racer),
                          educr = unique(wage$educr))
    setDT(groups, key = c("educr", "racer"))

    other_groups <- groups[racer != "Black"]
    groups       <- groups[racer == "Black"]

    data_tmp = data.table()
    for(i in 1:nrow(groups)){
        group_i = groups[i, ]

        dataGroup <- wage2[group_i]

        geoMean <- dataGroup[, exp(mean(log(wage)))]

        above <- dataGroup[, which(wage > geoMean)]
        below <- dataGroup[, which(wage < geoMean)]

        n <- min(c(length(above), length(below)))

        above = above[1:n]
        below = below[1:n]

        dataGroup[above, wage := wage * 2]
        dataGroup[below, wage := wage / 2]

        data_tmp = rbind(data_tmp, dataGroup)
    }
    wage2 = rbind(wage2[other_groups], data_tmp)

    # (geometric) means are unchanged
    mean1 <- wage[, exp(mean(log(wage)))]
    mean2 <- wage2[, exp(mean(log(wage)))]
    expect_equal(mean1, mean2)

    # grop (geometric) means are unchanged
    group_means1 <-  wage[order(racer, educr), exp(mean(log(wage))), by = c("racer", "educr")]
    group_means2 <- wage2[order(racer, educr), exp(mean(log(wage))), by = c("racer", "educr")]
    expect_equivalent(group_means1, group_means2)

    # all group log variances are unchanged, but "Black"
    group_vars1 <-  wage[racer == "Black", var(log(wage)), by = c("racer", "educr")]
    group_vars2 <- wage2[racer == "Black", var(log(wage)), by = c("racer", "educr")]
    expect_true(all(!group_vars1$V1 == group_vars2$V1))

    group_vars1 <-  wage[racer != "Black", var(log(wage)), by = c("racer", "educr")]
    group_vars2 <- wage2[racer != "Black", var(log(wage)), by = c("racer", "educr")]
    expect_true(all(group_vars1[order(racer, educr)]$V1 == group_vars2[order(racer, educr)]$V1))

    v <- varDecomp(wage, wage2, f, silent = TRUE, precision = 1e-11)
    values_for_testing <- v$dynamic[factor == "racer" & group == "var"]

    # TODO: this produces relatively large effects for var_educr
    # Is this some how related to group composition?
    expect_equal(values_for_testing$value, values_for_testing$group_value)
})


test_that("comp: educr", {

    # Simulates new sample weights (changing the marginals of racer,
    # but keeping  the statistical association and the marginals for education identical)
    ipf <- wage[, .N, by = c("educr", "racer")]

    ipf[, p0 := N/sum(N)] # Original joint distribution

    # The source margins are obtained from the simulated distribution
    ipf[, s_margin_racer := sum(p0), by = racer]
    ipf[, s_margin_educr := sum(p0), by = educr]

    # The target margins for educt are the same
    ipf[, t_margin_racer := sum(p0), by = racer]

    # ... but the target margins for racer are not
    ipf[, t_margin_educr := sum(p0), by = educr]
    ipf[ educr == "4-year college+", t_margin_educr := 3*t_margin_educr]
    ipf[, t_margin_educr := t_margin_educr/sum(t_margin_educr), by=racer]

    # Marginal distributions (for further comparison)
    m_educ0 = ipf[, .(p = sum(p0)), by = educr]
    m_race0 = ipf[, .(p = sum(p0)), by = racer]

    # IPF for getting a distribution with same margins as the original, but
    # with same the statistical association as the simulated one
    indep_vars = c("educr", "racer")
    ipf[, p1 := p0]
    for(i in 1:40) {

        for (var in indep_vars) {
            # adjust
            t <- paste0("t_margin_", var)
            s <- paste0("s_margin_", var)
            ipf[, p1 := p1*(get(t) / get(s))]
            ipf[, p1 := p1 / sum(p1)]
        }

        # update all margins
        for (var in indep_vars) {
            ipf[, paste0("s_margin_", var) := sum(p1), by = var]
        }
    }

    # Odds ratio (for comparison)
    t0 <- tidyr::spread(ipf[, .(educr, racer, p0)], key = racer, value=p0)
    t1 <- tidyr::spread(ipf[, .(educr, racer, p1)], key = racer, value=p1)
    p0_odds <- vcd::loddsratio(as.matrix(t0[, -1]))$coefficients
    p1_odds <- vcd::loddsratio(as.matrix(t1[, -1]))$coefficients

    # the odds ratios are the same
    expect_equal(p0_odds, p1_odds)

    # the joint distribution changed
    expect_false(all(ipf$p0 == ipf$p1))

    # the marginal distribution for race are the same
    m_race1 = ipf[, .(p = sum(p1)), by = racer]
    expect_equal(m_race0, m_race1)

    # the marginal distribution for education changed
    m_educ1 = ipf[, .(p = sum(p1)), by = educr]
    expect_false(all(round(m_educ0[,2], 10) == round(m_educ1[,2], 10)))

    # NEW SAMPLE WEIGHTS!
    ipf[ , wt := (p1*sum(N))/N]
    setDT(ipf, key = c("racer", "educr"))

    # Merging with the original microdata
    wage$wt = NULL
    wage2 = copy(wage)
    setDT(wage2, key = c("racer", "educr"))
    wage2[ipf, wt := wt]

    # Frequency table for the new dataset, with simulated weights (for comparison)
    t2 <- tidyr::spread(wage2[, .(n = sum(wt)), by = c("educr", "racer")], key = racer, value=n)

    # Sorting values (using DT keys)
    setDT(t0, c("educr", "racer"))
    setDT(t1, c("educr", "racer"))
    setDT(t2, c("educr", "racer"))

    t0 <- prop.table(t0[, -1])
    t1 <- prop.table(t1[, -1])
    t2 <- prop.table(t2[, -1])

    # the dataset wage2 must have the same marginal distribution for education as the simulated dataset
    expect_false(all(rowSums(t2) ==rowSums(t0)))

    # the dataset wage2 must have the same marginal distribution for race as the original data
    expect_equal(colSums(t2), colSums(t0))

    # the dataset wage2 must have the same odds ratio as the original distribution
    expect_equal(vcd::loddsratio(as.matrix(t0))$coefficients, vcd::loddsratio(as.matrix(t2))$coefficients)

    # the dataset wage2 must have the same odds ratio as the simulated distribution
    expect_equal(vcd::loddsratio(as.matrix(t1))$coefficients, vcd::loddsratio(as.matrix(t2))$coefficients)

    wage$wt = 1
    v <- varDecomp(wage, wage2, f, weight = "wt", precision = 1e-14)

    values_for_testing <- v$dynamic[group == "comp" & factor == "educr"]

    expect_equal(values_for_testing$value, values_for_testing$group_value)

})


test_that("comp: racer", {

    # Simulates new sample weights (changing the marginals of racer,
    # but keeping  the statistical association and the marginals for education identical)
    ipf <- wage[, .N, by = c("educr", "racer")]

    ipf[, p0 := N/sum(N)] # Original joint distribution

    # The source margins are obtained from the simulated distribution
    ipf[, s_margin_racer := sum(p0), by = racer]
    ipf[, s_margin_educr := sum(p0), by = educr]

    # The target margins for educt are the same
    ipf[, t_margin_educr := sum(p0), by = educr]

    # ... but the target margins for racer are not
    ipf[, t_margin_racer := sum(p0), by = racer]
    ipf[ racer == "Black", t_margin_racer := 3*t_margin_racer]
    ipf[, t_margin_racer := t_margin_racer/sum(t_margin_racer), by=educr]

    # Marginal distributions (for further comparison)
    m_educ0 = ipf[, .(p = sum(p0)), by = educr]
    m_race0 = ipf[, .(p = sum(p0)), by = racer]

    # IPF for getting a distribution with same margins as the original, but
    # with same the statistical association as the simulated one
    indep_vars = c("educr", "racer")
    ipf[, p1 := p0]
    for(i in 1:40) {

        for (var in indep_vars) {
            # adjust
            t <- paste0("t_margin_", var)
            s <- paste0("s_margin_", var)
            ipf[, p1 := p1*(get(t) / get(s))]
            ipf[, p1 := p1 / sum(p1)]
        }

        # update all margins
        for (var in indep_vars) {
            ipf[, paste0("s_margin_", var) := sum(p1), by = var]
        }
    }

    # Odds ratio (for comparison)
    t0 <- tidyr::spread(ipf[, .(educr, racer, p0)], key = racer, value=p0)
    t1 <- tidyr::spread(ipf[, .(educr, racer, p1)], key = racer, value=p1)
    p0_odds <- vcd::loddsratio(as.matrix(t0[, -1]))$coefficients
    p1_odds <- vcd::loddsratio(as.matrix(t1[, -1]))$coefficients

    # the odds ratios are the same
    expect_equal(p0_odds, p1_odds)

    # the joint distribution changed
    expect_false(all(ipf$p0 == ipf$p1))

    # the marginal distribution for race changed
    m_race1 = ipf[, .(p = sum(p1)), by = racer]
    expect_false(all(round(m_race0[,2], 10) == round(m_race1[,2], 10)))

    # the marginal distribution for education is the same
    m_educ1 = ipf[, .(p = sum(p1)), by = educr]
    expect_equal(m_educ0, m_educ1)

    # NEW SAMPLE WEIGHTS!
    ipf[ , wt := (p1*sum(N))/N]
    setDT(ipf, key = c("racer", "educr"))

    # Merging with the original microdata
    wage$wt = NULL
    wage2 = copy(wage)
    setDT(wage2, key = c("racer", "educr"))
    wage2[ipf, wt := wt]

    # Frequency table for the new dataset, with simulated weights (for comparison)
    t2 <- tidyr::spread(wage2[, .(n = sum(wt)), by = c("educr", "racer")], key = racer, value=n)

    # Sorting values (using DT keys)
    setDT(t0, c("educr", "racer"))
    setDT(t1, c("educr", "racer"))
    setDT(t2, c("educr", "racer"))

    t0 <- prop.table(t0[, -1])
    t1 <- prop.table(t1[, -1])
    t2 <- prop.table(t2[, -1])

    # the dataset wage2 must have the same marginal distribution for race as the simulated data
    expect_false(all(colSums(t2) == colSums(t0)))

    # the dataset wage2 must have the same marginal distribution for education as the original data
    expect_equal(rowSums(t2), rowSums(t0))

    # the dataset wage2 must have the same odds ratio as the original distribution
    expect_equal(vcd::loddsratio(as.matrix(t0))$coefficients, vcd::loddsratio(as.matrix(t2))$coefficients)

    # the dataset wage2 must have the same odds ratio as the simulated distribution
    expect_equal(vcd::loddsratio(as.matrix(t1))$coefficients, vcd::loddsratio(as.matrix(t2))$coefficients)

    wage$wt = 1
    v <- varDecomp(wage, wage2, f, weight = "wt", precision = 1e-14)

    values_for_testing <- v$dynamic[group == "comp" & factor == "racer"]

    expect_equal(values_for_testing$value, values_for_testing$group_value)

})


test_that("comp: racer - OLD STRATEGY", {

    wage[, wt := 1]
    wage2 <- copy(wage)
    setDT(wage2)

    wage2[racer == "Black", wt := 50]

    # different margins for race
    margin_race1 <-  wage[ , .(N = sum(wt)), by = racer]
    margin_race2 <- wage2[ , .(N = sum(wt)), by = racer]
    margin_race1[, p := N/sum(N)]
    margin_race2[, p := N/sum(N)]

    expect_false(all(round(margin_race1$p, 8) == round(margin_race2$p, 8)))

    # same margins for education
    margin_educ1 <-  wage[ , .(N = sum(wt)), by = educr]
    margin_educ2 <- wage2[ , .(N = sum(wt)), by = educr]
    margin_educ1[, p := N/sum(N)]
    margin_educ2[, p := N/sum(N)]

    expect_equal(round(margin_educ1$p, 3), round(margin_educ2$p, 3)) # ERROR

    # Odds ratio (for comparison)
    t1 =  wage[ , prop.table(questionr::wtd.table(racer, educr, weights = wt))]
    t2 = wage2[ , prop.table(questionr::wtd.table(racer, educr, weights = wt))]
    p1_odds <- vcd::loddsratio(t1)$coefficients
    p2_odds <- vcd::loddsratio(t2)$coefficients

    # the odds ratios are the same
    expect_equal(p1_odds, p2_odds) # OK!!

    v <- varDecomp(wage, wage2, f, weight = "wt", precision = 1e-14)

    values_for_testing <- v$dynamic[group == "comp" & factor == "racer"]

    expect_equal(values_for_testing$value, values_for_testing$group_value) # ERROR
})


test_that("comp: association", {

    # Simulates new sample weights (keeping the original marginals, but changing the statistical association)

    ipf <- wage[, .N, by = c("educr", "racer")]

    ipf[, p0 := N/sum(N)] # Original joint distribution

    set.seed(12345)
    ipf[, p1 := rpois(10, 10)^3] # New joint distribution
    ipf[, p1 := p1/sum(p1)]

    # The source margins are obtained from the simulated distribution
    ipf[, s_margin_racer := sum(p1), by = racer]
    ipf[, s_margin_educr := sum(p1), by = educr]

    # The target margins are obtained from the orginal distribution
    ipf[, t_margin_racer := sum(p0), by = racer]
    ipf[, t_margin_educr := sum(p0), by = educr]


    # Joint distributions (for further comparison)
    p0_before = ipf$p0
    p1_before = ipf$p1

    # Odds ratio (for further comparison)
    t0 <- tidyr::spread(ipf[, .(educr, racer, p0)], key = racer, value=p0)
    t1 <- tidyr::spread(ipf[, .(educr, racer, p1)], key = racer, value=p1)
    p0_odds_before <- vcd::loddsratio(as.matrix(t0[, -1]))$coefficients
    p1_odds_before <- vcd::loddsratio(as.matrix(t1[, -1]))$coefficients
    rm(t0, t1)


    # IPF for getting a distribution with same margins as the original, but
    # with same the statistical association as the simulated one
    distance = 1
    indep_vars = c("educr", "racer")
    ipf[, p := p1]
    while(distance > 1e-15) {

        ipf[, p1 := p]

        for (var in indep_vars) {
            # adjust
            t <- paste0("t_margin_", var)
            s <- paste0("s_margin_", var)
            ipf[, p1 := p1*(get(t) / get(s))]
            ipf[, p1 := p1 / sum(p1)]
        }

        # update all margins
        for (var in indep_vars) {
            ipf[, paste0("s_margin_", var) := sum(p1), by = var]
        }

        distance_educ <- sqrt(sum((ipf$s_margin_educr - ipf$t_margin_educr)^2))
        distance_race <- sqrt(sum((ipf$s_margin_racer - ipf$t_margin_racer)^2))
        distance <- (distance_educ + distance_educ)
        ipf[, p := p1]
    }


    # Joint distributions (for comparison)
    p0_after = ipf$p0
    p1_after = ipf$p1


    # Odds ratio (for comparison)
    t0 <- tidyr::spread(ipf[, .(educr, racer, p0)], key = racer, value=p0)
    t1 <- tidyr::spread(ipf[, .(educr, racer, p1)], key = racer, value=p1)
    p0_odds_after <- vcd::loddsratio(as.matrix(t0[, -1]))$coefficients
    p1_odds_after <- vcd::loddsratio(as.matrix(t1[, -1]))$coefficients

    # the original joint distribution is the same
    expect_equal(p0_before, p0_after)

    # the simulated joint distribution changed
    expect_false(all(round(p1_before, 4) == round(p1_after, 4)))

    # the original odds ratios is the same
    expect_equal(p0_odds_after, p0_odds_before)

    # the simulated odds ratios is the same
    expect_equal(p1_odds_after, p1_odds_before)


    # NEW SAMPLE WEIGHTS!
    ipf[ , wt := (p1*sum(N))/N]
    setDT(ipf, key = c("racer", "educr"))

    # Merging with the original microdata
    wage$wt = NULL
    wage2 = copy(wage)
    setDT(wage2, key = c("racer", "educr"))
    wage2[ipf, wt := wt]


    # Frequency table for the new dataset, with simulated weights (for comparison)
    t2 <- tidyr::spread(wage2[, .(n = sum(wt)), by = c("educr", "racer")], key = racer, value=n)

    # Sorting values (using DT keys)
    setDT(t0, c("educr", "racer"))
    setDT(t1, c("educr", "racer"))
    setDT(t2, c("educr", "racer"))

    t0 <- prop.table(t0[, -1])
    t1 <- prop.table(t1[, -1])
    t2 <- prop.table(t2[, -1])

    # the dataset wage2 must have the same margins as the original distribution
    expect_equal(colSums(t2), colSums(t0))
    expect_equal(rowSums(t2), rowSums(t0))

    # the dataset wage2 must have the same odds ratio as the transformed distribution
    expect_equal(vcd::loddsratio(as.matrix(t1)), vcd::loddsratio(as.matrix(t2)))


    wage$wt = 1
    v <- varDecomp(wage, wage2, f, weight = "wt", precision = 1e-14)

    values_for_testing <- v$dynamic[factor == "association"]

    expect_equal(values_for_testing$value, values_for_testing$value)

})
