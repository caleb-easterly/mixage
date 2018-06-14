library(mixage)
context("utility functions")

start_ages <- c(12, 18, 24)
max_age <- 30
n_age <- length(start_ages)

test_that("def_age_group_list returns the correct list", {
    correct_list <- list(seq(12, 17), seq(18, 23), seq(24, 29))
    calc_list <- def_age_group_list(start_ages, max_age)
    expect_true(all.equal(correct_list, calc_list))
})

test_that("age distribution sums to one", {
    all_ind <- 12:24
    calc_age_dist <- calculate_single_year_age_distribution(min(all_ind), max(all_ind))
    expect_equal(sum(calc_age_dist), 1)
})
