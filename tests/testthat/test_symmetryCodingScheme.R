#rm(list=ls())
#options(scipen = 999)
library("testthat")
library("varDecomp")
library("data.table")

context("test_symmetryCodingScheme")

data(wage)
setDT(wage)

f1 <- log(wage) ~ racer + educr
f2 <- log(wage) ~ racer * educr
f3 <- log(wage) ~ -1 + racer * educr

wage1 <- wage[sample(.N, 2000), ]
wage2 <- wage[sample(.N, 2000), ]

test_that("symmetric - f1, contrast coding", {
    v1 <- varDecomp(wage1, wage2, f1, silent = TRUE, precision = 1e-14, contrast.coding = T, iterative.mle = F)
    v2 <- varDecomp(wage2, wage1, f1, silent = TRUE, precision = 1e-14, contrast.coding = T, iterative.mle = F)

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

test_that("symmetric - f1, DUMMY coding", {
    v1 <- varDecomp(wage1, wage2, f1, silent = TRUE, precision = 1e-14, contrast.coding = F, iterative.mle = F)
    v2 <- varDecomp(wage2, wage1, f1, silent = TRUE, precision = 1e-14, contrast.coding = F, iterative.mle = F)

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


test_that("symmetric - f2, contrast coding", {

    v1 <- varDecomp(wage1, wage2, f2, silent = TRUE, precision = 1e-14, contrast.coding = T, iterative.mle = F)
    v2 <- varDecomp(wage2, wage1, f2, silent = TRUE, precision = 1e-14, contrast.coding = T, iterative.mle = F)

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


test_that("symmetric - f2, DUMMY coding", {
    v1 <- varDecomp(wage1, wage2, f2, silent = TRUE, precision = 1e-14, contrast.coding = F, iterative.mle = F)
    v2 <- varDecomp(wage2, wage1, f2, silent = TRUE, precision = 1e-14, contrast.coding = F, iterative.mle = F)

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



test_that("symmetric - f3, contrast coding", {
    v1 <- varDecomp(wage1, wage2, f3, silent = TRUE, precision = 1e-14, contrast.coding = T, iterative.mle = F)
    v2 <- varDecomp(wage2, wage1, f3, silent = TRUE, precision = 1e-14, contrast.coding = T, iterative.mle = F)

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


test_that("symmetric - f3, DUMMY coding", {
    v1 <- varDecomp(wage1, wage2, f3, silent = TRUE, precision = 1e-14, contrast.coding = F, iterative.mle = F)
    v2 <- varDecomp(wage2, wage1, f3, silent = TRUE, precision = 1e-14, contrast.coding = F, iterative.mle = F)

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



# ================================================================================


test_that("reference category does not matter - f1, contrast coding", {
    wage1$racer <- relevel(wage1$racer, ref = "Black")
    wage2$racer <- relevel(wage2$racer, ref = "Black")
    v1 <- varDecomp(wage1, wage2, f1, silent = TRUE, precision = 1e-14, contrast.coding = T)

    wage1$racer <- relevel(wage1$racer, ref = "White")
    wage2$racer <- relevel(wage2$racer, ref = "White")
    v2 <- varDecomp(wage1, wage2, f1, silent = TRUE, precision = 1e-14, contrast.coding = T)

    expect_equal(v1$dynamic, v2$dynamic)
})


test_that("reference category does not matter - f1, dummy coding", {
    wage1$racer <- relevel(wage1$racer, ref = "Black")
    wage2$racer <- relevel(wage2$racer, ref = "Black")
    v1 <- varDecomp(wage1, wage2, f1, silent = TRUE, precision = 1e-14, contrast.coding = F)

    wage1$racer <- relevel(wage1$racer, ref = "White")
    wage2$racer <- relevel(wage2$racer, ref = "White")
    v2 <- varDecomp(wage1, wage2, f1, silent = TRUE, precision = 1e-14, contrast.coding = F)

    expect_equal(v1$dynamic, v2$dynamic)
})



test_that("reference category does not matter - f2, contrast coding", {
    wage1$racer <- relevel(wage1$racer, ref = "Black")
    wage2$racer <- relevel(wage2$racer, ref = "Black")
    v1 <- varDecomp(wage1, wage2, f2, silent = TRUE, precision = 1e-14, contrast.coding = T)

    wage1$racer <- relevel(wage1$racer, ref = "White")
    wage2$racer <- relevel(wage2$racer, ref = "White")
    v2 <- varDecomp(wage1, wage2, f2, silent = TRUE, precision = 1e-14, contrast.coding = T)

    expect_equal(v1$dynamic, v2$dynamic)
})


test_that("reference category does not matter - f2, dummy coding", {
    wage1$racer <- relevel(wage1$racer, ref = "Black")
    wage2$racer <- relevel(wage2$racer, ref = "Black")
    v1 <- varDecomp(wage1, wage2, f2, silent = TRUE, precision = 1e-14, contrast.coding = F)

    wage1$racer <- relevel(wage1$racer, ref = "White")
    wage2$racer <- relevel(wage2$racer, ref = "White")
    v2 <- varDecomp(wage1, wage2, f2, silent = TRUE, precision = 1e-14, contrast.coding = F)

    expect_equal(v1$dynamic, v2$dynamic)
})



test_that("reference category does not matter - f3, contrast coding", {
    wage1$racer <- relevel(wage1$racer, ref = "Black")
    wage2$racer <- relevel(wage2$racer, ref = "Black")
    v1 <- varDecomp(wage1, wage2, f3, silent = TRUE, precision = 1e-14, contrast.coding = T)

    wage1$racer <- relevel(wage1$racer, ref = "White")
    wage2$racer <- relevel(wage2$racer, ref = "White")
    v2 <- varDecomp(wage1, wage2, f3, silent = TRUE, precision = 1e-14, contrast.coding = T)

    expect_equal(v1$dynamic, v2$dynamic)
})


test_that("reference category does not matter - f3, dummy coding", {
    wage1$racer <- relevel(wage1$racer, ref = "Black")
    wage2$racer <- relevel(wage2$racer, ref = "Black")
    v1 <- varDecomp(wage1, wage2, f3, silent = TRUE, precision = 1e-14, contrast.coding = F)

    wage1$racer <- relevel(wage1$racer, ref = "White")
    wage2$racer <- relevel(wage2$racer, ref = "White")
    v2 <- varDecomp(wage1, wage2, f3, silent = TRUE, precision = 1e-14, contrast.coding = F)

    expect_equal(v1$dynamic, v2$dynamic)
})


