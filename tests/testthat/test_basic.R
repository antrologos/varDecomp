library("testthat")
library("varDecomp")
library("data.table")

context("test_basic")

data(wage)
setDT(wage)
f <- log(wage) ~ racer + educr
wage1 <- wage[sample(.N, 2000), ]
wage2 <- wage[sample(.N, 2000), ]

test_that("symmetric", {
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

test_that("reference category does not matter", {
    wage1$racer <- relevel(wage1$racer, ref = "Black")
    wage2$racer <- relevel(wage2$racer, ref = "Black")
    v1 <- varDecomp(wage1, wage2, wage ~ racer, silent = TRUE, precision = 1e-11)
    wage1$racer <- relevel(wage1$racer, ref = "White")
    wage2$racer <- relevel(wage2$racer, ref = "White")
    v2 <- varDecomp(wage1, wage2, wage ~ racer, silent = TRUE, precision = 1e-11)

    expect_equal(v1$dynamic, v2$dynamic)
})
