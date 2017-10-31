library(mixage)
context("data matrix creation")

start_ages <- c(12, 18, 24)
max_age <- 30
n_age <- length(start_ages)


# expected data matrix is (for both male and female)
data_matrix <- 
    matrix(c(2, 2, 0,
            0, 2, 2,
            0, 1, 3),
           byrow = TRUE,
           nrow = 3
    )
mixing_data_F <- data.frame(
    "chsage" = c(rep(12, 4), rep(18, 4), rep(24, 4)),
    "sex" = rep("Female", 12),
    "ptage" = c(12, 12, 18, 18, 
                18, 18, 24, 24, 
                18, 24, 24, 24)
)

mixing_data_M <- data.frame(
    "chsage" = c(rep(12, 4), rep(18, 4), rep(24, 4)),
    "sex" = rep("Male", 12),
    "ptage" = c(12, 12, 18, 18, 
                18, 18, 24, 24, 
                18, 24, 24, 24)
)

mixing_data <- rbind(mixing_data_F, mixing_data_M)
indices <- list(12:17, 18:23, 24:29)

calc_data_matrix <- data_matrix_longform(mixing_data, indices)
test_that("create_data_matrix produces correct output", {
          expect_equal(calc_data_matrix$Mcounts, data_matrix)
          expect_equal(calc_data_matrix$Fcounts, data_matrix)
    }
)
