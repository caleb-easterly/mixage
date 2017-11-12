start_ages <- c(12, 18, 24)
max_age <- 30
n_age <- length(start_ages)

nsamps <- 100
mixing_data <- data.frame(
    "chsage" = sample(12:30, nsamps, replace = TRUE),
    "sex" = sample(c("Male", "Female"), nsamps, replace = TRUE)
)

mixing_data$ptage <- round(rnorm(nsamps, sd = 1) + mixing_data$chsage)

age_dist <- runif(length(seq(min(start_ages), max_age)))
age_dist <- age_dist/sum(age_dist)
