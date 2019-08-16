library("testthat")
library("varDecomp")
library("data.table")

context("test_simulations")

data(wage)
setDT(wage)
wage <- wage[wage > 5000]
f <- log(wage) ~ racer + educr

test_that("symmetric", {
    wage1 <- wage[sample(.N, 2000), ]
    wage2 <- wage[sample(.N, 2000), ]
    f <- log(wage) ~ racer + educr

    v1 <- varDecomp(wage1, wage2, f, silent = TRUE, precision = 1e-11)
    v2 <- varDecomp(wage2, wage1, f, silent = TRUE, precision = 1e-11)

    expect_equal(
        v1$static[1, est_variance:obs_variance],
        v2$static[2, est_variance:obs_variance])
    expect_equal(
        v1$static[2, est_variance:obs_variance],
        v2$static[1, est_variance:obs_variance])
    expect_equal(
        v1$static[3, est_variance:obs_variance],
        -v2$static[3, est_variance:obs_variance])
    expect_equal(
        v1$static[3, est_variance:obs_variance],
        -v2$static[3, est_variance:obs_variance])
    expect_equal(
        v1$dynamic$value,
        -v2$dynamic$value)
})

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


test_that("var: Intercept", {
    # increase variance in earnings without affecting the mean
    wage2 <- copy(wage)

    for (i in 1:100) {
        mean_wage <- wage2[, mean(wage)]

        above <- wage2[, which(wage > mean_wage)]
        below <- wage2[, which(wage < mean_wage)]

        ix <- sample(above, size = 500)
        wage2[ix, wage := wage + 100]
        ix <- sample(below, size = 500)
        wage2[ix, wage := wage - 100]
    }
    # means are unchanged
    expect_equal(mean_wage, wage2[, mean(wage)])

    v <- varDecomp(wage, wage2, f, silent = TRUE, precision = 1e-11)
    # TODO: this produces relatively large effects for mean_educr and var_educr ?
    expect_gt(0, 1) # fail test
})


test_that("var: educr", {
    # increase variance in college earnings for whites without affecting the mean
    wage2 <- copy(wage)
    mean_wage <- wage2[educr == "4-year college+" & racer == "White", mean(wage)]

    college_above <- wage2[, which(educr == "4-year college+" & racer == "White" &
        wage > mean_wage)]
    college_below <- wage2[, which(educr == "4-year college+" & racer == "White" &
        wage < mean_wage)]

    ix <- sample(college_above, size = 2000)
    wage2[ix, wage := wage + 4000]
    ix <- sample(college_below, size = 2000)
    wage2[ix, wage := wage - 4000]

    # means are unchanged
    expect_equal(wage[, mean(wage), by = educr][, V1],
        wage2[, mean(wage), by = educr][, V1])
    expect_equal(wage[, mean(wage), by = racer][, V1],
        wage2[, mean(wage), by = racer][, V1])
    expect_equal(wage[, mean(wage), by = .(educr, racer)][, V1],
        wage2[, mean(wage), by = .(educr, racer)][, V1])

    v <- varDecomp(wage, wage2, f, silent = TRUE, precision = 1e-11)
    # TODO: although the means stay identical here, we get mean effects -- but they are very small
    expect_gt(0, 1) # fail test
})


test_that("comp: educr", {
    # increase in college graduation (those with college have higher variance)
    wage[, wt := 1]
    wage2 <- copy(wage)
    wage2[educr == "4-year college+", wt := 2]

    v <- varDecomp(wage, wage2, f, weight = "wt", silent = TRUE, precision = 1e-11)

    # static and dynamic match
    expect_equal(v$static$est_variance[[3]], sum(v$dynamic$value))
    # change is mostly explained by comp educr effect, i.e. all others are close to 0
    expect_gt(v$dynamic[group == "comp" & factor == "educr", value] / v$static$est_variance[[3]],
        .95)
})


test_that("comp: racer", {
    # increase in Black population (have less variance)
    wage[, wt := 1]
    wage2 <- copy(wage)
    wage2[racer == "Black", wt := 5]

    v <- varDecomp(wage, wage2, f, weight = "wt", silent = TRUE, precision = 1e-11)

    # static and dynamic match
    expect_equal(v$static$est_variance[[3]], sum(v$dynamic$value))
    # change is only explained by comp racer effect, i.e. all others are 0
    # TODO: --> this doesn't work as nicely as does comp: educr
    expect_gt(v$dynamic[group == "comp" & factor == "racer", value] / v$static$est_variance[[3]],
        .99)
})


test_that("comp: association", {
    # TODO
})
