library(mixage)
context("main functions: best age mixing")

source("common_test_defs.R")

test_that("best_age_mixing runs without error", {
    out <- best_age_mixing(mixing_data, start_ages)
})
