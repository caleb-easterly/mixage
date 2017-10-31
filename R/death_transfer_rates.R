#Age data: uses life table data, etc., to calculate
#information for the age groups used in the model

#-1 * log(1-prob) / n_year
#INPUT: death_prob, from life tables (a vector). indices, a list of vectors that contain the
### years in each age bin
#OUTPUT: vector of death rates
death_rate <- function(death_prob, indices) {
    
    death_rates <- (-1 * log(1 - death_prob))
    death_rates_bin <- our_bins(data = death_rates, indices = indices, fn = mean)
    return(death_rates_bin)
}

#INPUT: death_rate (average death rate over band); band, scalar w/ number of years in band
#OUTPUT: aging rate from one age group to the following age group
#this is a formula from Elbasha 2007 appendix
transfer_rate <- function(death_rate, band, pop_growth = 0) {
    (death_rate + pop_growth)/(exp(band * (death_rate + pop_growth)) - 1)
}

#INPUT: list of indices, named vector of data (from 1 to 70 years)
#OUTPUT: vector of bin-specific rates
our_bins <- function(data, indices, fn = c(sum, mean), weights = NA) {
    fn <- match.fun(fn)
    len <- length(indices)
    bin_spec <- vector(length = len)
    
    if (all(is.na(weights))) weights <- rep(1, length = len)
    
    for (i in 1:len) {
        bin_spec[i] <- fn(data[indices[[i]]] * weights[i]) / weights[i]
    }
    bin_spec
}
