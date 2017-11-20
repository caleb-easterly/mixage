library(mixage)
context("main functions: estimate_age_mixing and likelihood")

source("common_test_defs.R")

test_that("MOME and FOME have rowsums equal to 1", {
    dv <- matrix(c("normal", rep("gamma", 2),
                   "identity", "log", "identity"), nrow = 3)
    for (i in 1:nrow(dv)) {
        mixing_mats <- estimate_age_mixing(choice_data = mixing_data,
                                           start_ages = start_ages,
                                           distribution = dv[i, 1],
                                           link = dv[i, 2],
                                           max_age = max_age,
                                           age_distribution = age_dist)
        expect_equal(rowSums(mixing_mats$MOME), rep(1, n_age))
        expect_equal(rowSums(mixing_mats$FOME), rep(1, n_age))
    }
})
