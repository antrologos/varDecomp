library("testthat")
library("varDecomp")
library("data.table")

context("test_ipf")

# create test dataset
var_a <- c("Lo", "Med", "Hi")
var_b <- c("A", "B", "C")
freqs <- as.data.table(expand.grid(a = var_a, b = var_b))
freqs[, `:=`(
    p1 = round(rnorm(9, 100, 50)),
    p2 = round(rnorm(9, 100, 10)))]
freqs[p1 < 0, p1 := 1]
freqs[p2 < 0, p2 := 1]
freqs[, `:=`(p1 = p1 / sum(p1), p2 = p2 / sum(p2))]
freqs[, `:=`(p1_margins_a = sum(p1), p2_margins_a = sum(p2)), by = "a"]
freqs[, `:=`(p1_margins_b = sum(p1), p2_margins_b = sum(p2)), by = "b"]

or <- function(v) {
    (v[[1]] / v[[2]]) / (v[[4]] / v[[5]])
}

test_that("IPF: adjust nothing, i.e. should be the same as p1", {
    d <- copy(freqs)
    expect_equal(d[, p1],
        varDecomp:::counterfactual_p(d, adjust_vars = c(), c("a", "b"),
            association_effect = FALSE))
})

test_that("IPF: adjust both margins plus odds ratios, i.e. should be the same as p2", {
    d <- copy(freqs)
    expect_equal(d[, p2],
        varDecomp:::counterfactual_p(d, adjust_vars = c("a", "b"), c("a", "b"),
            association_effect = TRUE))
})

test_that("IPF: adjust both margins, but not odds ratios", {
    d <- copy(freqs)
    d[, adjust_both_margins := varDecomp:::counterfactual_p(d,
        adjust_vars = c("a", "b"), c("a", "b"), association_effect = FALSE)]
    d[, `:=`(adjust_both_margins_a = sum(adjust_both_margins)), by = "a"]
    d[, `:=`(adjust_both_margins_b = sum(adjust_both_margins)), by = "b"]
    expect_equal(d[, p2_margins_a], d[, adjust_both_margins_a], tolerance = .001)
    expect_equal(d[, p2_margins_b], d[, adjust_both_margins_b], tolerance = .001)
})

test_that("IPF: adjust only odds ratios", {
    d <- copy(freqs)
    d[, adjust_only_odds := varDecomp:::counterfactual_p(d,
        adjust_vars = c(), c("a", "b"), association_effect = TRUE)]
    d[, `:=`(adjust_only_odds_a = sum(adjust_only_odds)), by = "a"]
    d[, `:=`(adjust_only_odds_b = sum(adjust_only_odds)), by = "b"]
    expect_equal(d[, p1_margins_a], d[, adjust_only_odds_a], tolerance = .001)
    expect_equal(d[, p1_margins_b], d[, adjust_only_odds_b], tolerance = .001)
    expect_equal(or(d[["adjust_only_odds"]]), or(d[["p2"]]), tolerance = .001)
})

test_that("IPF: adjust only a, but not odds ratios", {
    d <- copy(freqs)
    d[, adjust_a_only := varDecomp:::counterfactual_p(d,
        adjust_vars = "a", c("a", "b"), association_effect = FALSE)]
    d[, `:=`(adjust_a_only_a = sum(adjust_a_only)), by = "a"]
    d[, `:=`(adjust_a_only_b = sum(adjust_a_only)), by = "b"]
    expect_equal(d[, p2_margins_a], d[, adjust_a_only_a], tolerance = .001)
    expect_equal(d[, p1_margins_b], d[, adjust_a_only_b], tolerance = .001)
    expect_equal(or(d[["adjust_a_only"]]), or(d[["p1"]]), tolerance = .001)
})

test_that("IPF: adjust only b, but not odds ratios", {
    d <- copy(freqs)
    d[, adjust_b_only := varDecomp:::counterfactual_p(d,
        adjust_vars = "b", c("a", "b"), association_effect = FALSE)]
    d[, `:=`(adjust_b_only_a = sum(adjust_b_only)), by = "a"]
    d[, `:=`(adjust_b_only_b = sum(adjust_b_only)), by = "b"]
    expect_equal(d[, p1_margins_a], d[, adjust_b_only_a], tolerance = .001)
    expect_equal(d[, p2_margins_b], d[, adjust_b_only_b], tolerance = .001)
    expect_equal(or(d[["adjust_b_only"]]), or(d[["p1"]]), tolerance = .001)
})

test_that("IPF: adjust only a and odds ratios", {
    d <- copy(freqs)
    d[, adjust_a_or_only := varDecomp:::counterfactual_p(d,
        adjust_vars = "a", c("a", "b"), association_effect = TRUE)]
    d[, `:=`(adjust_a_or_only_a = sum(adjust_a_or_only)), by = "a"]
    d[, `:=`(adjust_a_or_only_b = sum(adjust_a_or_only)), by = "b"]
    expect_equal(d[, p2_margins_a], d[, adjust_a_or_only_a], tolerance = .001)
    expect_equal(d[, p1_margins_b], d[, adjust_a_or_only_b], tolerance = .001)
    expect_equal(or(d[["adjust_a_or_only"]]), or(d[["p2"]]), tolerance = .001)
})
