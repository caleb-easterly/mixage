#Age data: uses life table data, etc., to calculate
#information for the age groups used in the model

# @param death_prob, from life tables (a vector). indices, a list of vectors that contain the
### years in each age bin
# @return vector of death rates
death_rate <- function(death_prob, indices) {
    all_death_rates <- (-1 * log(1 - death_prob))
    # death rates for those ages in the population
    model_death_rates <- all_death_rates[indices]
    return(model_death_rates)
}

# calculate the aging rate from one group to the next
# this is a formula from Elbasha 2007 appendix
# 
# @param death_rate (average death rate over band)
# @param band, scalar w/ number of years in band
# @param pop_growth, rate of population growth (always 0 in this model)
# @value: aging rate from one age group to the following age group
transfer_rate <- function(death_rate, band, pop_growth = 0) {
    (death_rate + pop_growth)/(exp(band * (death_rate + pop_growth)) - 1)
}
