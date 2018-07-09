#' Calculate age distribution.
#' @description
#' Calculate an age distribution from annual probabilities of death (from life tables).
#'
#' @param min_age 
#' minimum model age
#'
#' @param max_model_age
#' maximum model age
#' 
#' @return \code{age_prop}: the proportion of the model population with each age
calculate_single_year_age_distribution <- function(min_age, max_model_age){
    single_year_ind <- seq(min_age, max_model_age)

    # calculate start ages from list
    n_age <- length(single_year_ind)

    # import life-table data
    #probabilities of death for age 1 to 100
    #created so that 12th entry is age 12 to 13
    mprob <- as.numeric(uk_life_table$mxM[-1])
    fprob <- as.numeric(uk_life_table$mxF[-1])

    ### convert probabilities of death to rates
    #create vector of death rates, mu
    muM <- death_rate(death_prob = mprob, indices = single_year_ind)
    muF <- death_rate(death_prob = fprob, indices = single_year_ind)

    # get the average of male and female rates
    mu <- rowMeans(cbind(muM, muF))

    ## aging rate calculations ####

    # bandwidths (number of years in each age group) is 1
    # because these are single years
    age_bandwidths <- rep(1, n_age)

    # d is aging rate 
    d <- aging_rate(mu, age_bandwidths)

    ### calculate proportion of population in each year of age ####

    #initialize age_proportions vector
    age_prop <- rep(0, n_age)

    #using Elbasha, et al. 2007, Appendix, pg. 10
    # explicit translation of first formula on pg. 10
    # calculating youngest age group

    # accumulators
    acc <- 0
    inner_acc <- 1
    # inner product/sum
    for (rr in 2:n_age) {
        for (jj in 2:rr) {
            inner_acc <- ( d[jj - 1] / (d[jj] + mu[jj]) ) * inner_acc
        }
        acc <- acc + inner_acc
        inner_acc <- 1
    }

    #calculate proportion in first age group
    age_prop[1] <- 1 / (1 + acc)

    # calculate proportions in other age groups
    for (ag in 2:n_age) {
        age_prop[ag] <- d[ag - 1] * age_prop[ag - 1]  / (d[ag] + mu[ag])
    }
    return(age_prop)
}