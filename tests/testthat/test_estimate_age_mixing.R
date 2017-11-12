library(mixage)
context("main functions: estimate_age_mixing and likelihood")

source("common_test_defs.R")

test_that("MOME and FOME have rowsums equal to 1", {
    dists <- c("normal", "laplace", "gamma")
    vars <- c("const", "log", "linear", "sqrt")
    dv <- as.matrix(expand.grid(dists, vars))
    for (i in 1:nrow(dv)) {
        mixing_mats <- estimate_age_mixing(choice_data = mixing_data,
                                           start_ages = start_ages,
                                           distribution = dv[i, 1],
                                           variance_model = dv[i, 2],
                                           max_age = max_age,
                                           age_distribution = age_dist)
        expect_equal(rowSums(mixing_mats$MOME), rep(1, n_age))
        expect_equal(rowSums(mixing_mats$FOME), rep(1, n_age))
    }
})
