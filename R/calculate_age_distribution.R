# calculate age distribution
# @param all_ind
# List of vectors of all ages in each age group
# 
# @param death_prob
# Length 100 vector. First entry is probability of dying in the next year for 1-year-old,
# 12th entry is for 12-year-old, and so on. 
# 
calculate_age_distribution <- function(all_ind, death_prob = NULL){
    # calculate start ages from list
    start_ages <- sapply(all_ind, min)
    
    # if user does not provide death rates, use built in (us life tables)
    if (is.null(death_prob)){
        # import life-table data
        data("life_table_data")
        
        #probabilities of death for age 1 to 100
        #created so that 12th entry is age 12 to 13, for example
        #2nd column is probability of dying, 4th row is 1-2 year-old age cohort
        death_prob <- as.numeric(matrix(life_table_data[4:103, 2]))
    } else {
        print("Using user-supplied death probabilities. Must be annual death probabilities for age 1, 2, ..., 100")
        # check length of death prob
        if (length(death_prob) != 100){
            stop("vector of death probabilities does not have length 100")
        }
    }
    
    names(death_prob) <- as.character(1:100)
    
    # number of age groups
    n_age <- length(all_ind)
    
    ### convert probabilities of death to rates
    #create vector of death rates, mu
    mu <- death_rate(death_prob = death_prob, indices = all_ind)
    
    ## aging rate calculations ####
    
    #calculate bands for aging calculation
    
    age_bandwidths <- sapply(all_ind, length)
    
    # d is aging rate 
    d <- transfer_rate(mu, age_bandwidths)
    
    ### calculate proportion of population in each age class ####
    
    #age are the all-purpose names, which contain the beginning year of each age group
    age <- as.character(start_ages)
    
    #initialize age_proportions vector
    age_prop <- rep(0, n_age)
    names(d) <- names(mu) <- names(age_prop) <- age
    
    #using Elbasha, et al. 2007, Appendix, pg. 10
    # explicit translation of first formula on pg. 10
    # calculating youngest age group
    
    # accumulators
    acc <- 0
    inner_acc <- 1
    # inner product/sum
    for (rr in 2:n_age) {
        for (jj in 2:rr) {
            inner_acc <- ( d[age[jj - 1]] / (d[age[jj]] + mu[age[jj]]) ) * inner_acc
        }
        acc <- acc + inner_acc
        inner_acc <- 1
    }
    
    #calculate proportion in first age group
    age_prop[1] <- 1 / (1 + acc)
    
    # calculate proportions in other age groups
    for (AG in age[-1]) {
        prev_age <- age[which(age == AG) - 1]
        age_prop[AG] <- d[prev_age] * age_prop[prev_age]  / (d[AG] + mu[AG])
    }
    return(list("age_prop" = age_prop, "d" = d, "mu" = mu))
}