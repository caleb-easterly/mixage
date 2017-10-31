# Calculate parameters of Gamma from mean and variance 
# 
# use Method of Moments to calculate the 
#     shape and rate parameters of an age-specific
#     Gamma distribution
gam_param <- function(mean, var) {
    shape <- mean^2 / var
    rate <- mean / var
    return(c(shape, rate))
}