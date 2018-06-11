library(mixage)
context("main functions: best age mixing")

source("common_test_defs.R")

test_that("best_age_mixing runs without error", {
    out <- best_age_mixing(mixing_data, start_ages)
    # was sampled from normal, so it's good that we get normal
    expect_true(out$all_AIC[, 1][which.min(out$all_AIC[, 3])] == 'normal')
})
