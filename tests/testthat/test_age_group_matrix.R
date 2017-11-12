library(mixage)
context("main functions: grouping of natsal best estimates")

start_ages <- seq(12, 73)
max_age <- 74

data("natsal_estimates")

test_that("age_group_matrix() produces something reasonably close to original natsal estimates", {
    grouped <- define_age_group_matrix(start_ages, max_age)
    expect_equal(grouped$MOME, natsal_estimates$MOME)
    expect_equal(grouped$FOME, natsal_estimates$FOME)
    }
)
