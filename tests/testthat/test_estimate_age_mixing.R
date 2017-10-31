library(mixage)
context("main functions: estimate_age_mixing and likelihood")

start_ages <- c(12, 18, 24)
max_age <- 30
n_age <- length(start_ages)

nsamps <- 100
mixing_data <- data.frame(
    "chsage" = sample(12:30, nsamps, replace = TRUE),
    "sex" = sample(c("Male", "Female"), nsamps, replace = TRUE)
)

mixing_data$ptage <- round(rnorm(nsamps, sd = 1) + mixing_data$chsage)

test_that("MOME and FOME have rowsums equal to 1", {
    dists <- c("normal", "laplace", "gamma")
    vars <- c("const", "log", "linear", "sqrt")
    dv <- as.matrix(expand.grid(dists, vars))
    for (i in 1:nrow(dv)) {
        mixing_mats <- estimate_age_mixing(choice_data = mixing_data,
                                           max_age = max_age,
                                           start_ages = start_ages,
                                           distribution = dv[i, 1],
                                           variance_model = dv[i, 2])
        expect_equal(rowSums(mixing_mats$MOME), rep(1, n_age))
        expect_equal(rowSums(mixing_mats$FOME), rep(1, n_age))
    }
})
