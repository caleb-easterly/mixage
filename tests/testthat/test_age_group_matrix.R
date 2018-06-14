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


test_that("combine 2 groups adds up", {
    start_ages <- c(12, seq(15, 73))
    grouped <- define_age_group_matrix(start_ages, max_age)
    # the probability of other ages mixing with 12-14 (in model) should be
    # the sum of prob. of mix with 12 + prob. mix with 13 + prob. mix with 14 (in natsal)
    expect_equal(grouped$MOME[2:length(start_ages), 1], rowSums(natsal_estimates$MOME[4:nrow(natsal_estimates$MOME), 1:3]))
})