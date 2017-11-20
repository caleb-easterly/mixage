create_sample_data <- function(){
    nreps <- 1000
    chsage <- sample(12:73, size = nreps, replace= TRUE)
    sex <- sample(c(0, 1), nreps, replace = TRUE)
    ptage <- chsage + rnorm(nreps, mean = 0, sd = 3) + 2*sex
    sex <- c("Female", "Male")[sex + 1]
    df <- data.frame("chsage" = chsage, "ptage" = ptage, "sex" = sex)
    return(df)
}

set.seed(1010)
mixage_sample_data <- create_sample_data()
save(mixage_sample_data, file = "data/mixage_sample_data.rda")