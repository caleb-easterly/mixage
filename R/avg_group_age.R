# estimate mean age in each group
# @param age_group_list_indices 
# a list of vectors, with the ith vector containing the ages in the ith age group
# @param full_age_distribution
# proportion of the population with each (single-year) age
# if not provided, is calculated from U.K. life tables
avg_group_age <- function(age_group_list_indices, full_age_distribution = NULL) {
  n_age <- length(age_group_list_indices)
  mean_ages <- vector(length = n_age)
  
  # minimum age that is modeled
  min_age <- min(unlist(age_group_list_indices))
  
  # maximum age that is modeled
  max_model_age <- max(unlist(age_group_list_indices))
  
  # calculate full age distribution for all ages in population
  
  if (is.null(full_age_distribution)){
      full_age_distribution <- calculate_single_year_age_distribution(min_age, max_model_age)
  }

  # for proper indexing, subtract (min_age - 1) from
  # age group indices to make min_age have an index of  1

  index_shift <- min_age - 1
  
  for (i in 1:n_age){
    mean_ages[i] <- age_group_list_indices[[i]] %*%
                      full_age_distribution[c(age_group_list_indices[[i]] - index_shift)]/
                      sum(full_age_distribution[c(age_group_list_indices[[i]] - index_shift)])
  }
  return(mean_ages)
}
