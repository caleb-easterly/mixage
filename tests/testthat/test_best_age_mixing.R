library(mixage)
context("main functions: best age mixing")

start_ages <- seq(12, 73, by=1)
max_age <- 74
n_age <- length(start_ages)

nsamps <- 100
mixing_data <- data.frame(
    "chsage" = sample(12:max_age, nsamps, replace = TRUE),
    "sex" = sample(c("Male", "Female"), nsamps, replace = TRUE)
)

mixing_data$ptage <- floor(rnorm(nsamps, sd = 2) + mixing_data$chsage)

test_that("best_age_mixing runs without error", {
    out <- best_age_mixing(mixing_data, start_ages)
})
