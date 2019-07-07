library("testthat")
library("varDecomp")

context("test_weighted")

data(wage)
wage$weight <- 1
wage2 <- copy(wage)
wage2[wage2$educr == "4-year college+", "weight"] <- 2

test_that("no change when no weights supplied", {
    v <- varDecomp(wage, wage2, log(wage) ~ educr, silent = TRUE)
    expect_equal(v$static$est_variance[[3]], 0)
    expect_equal(sum(v$dynamic$value), 0)
})

test_that("change when weights supplied", {
    v <- varDecomp(wage, wage2, log(wage) ~ educr, weight = "weight", silent = TRUE)

    expect_equal(v$static$est_variance[[3]], sum(v$dynamic$value))
    expect_gt(v$static$est_variance[[3]], 0)
    expect_gt(v$static$obs_variance[[3]], 0)
    expect_gt(sum(v$dynamic$value), 0)
})
