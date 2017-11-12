# estimate mean age in each group
avg_group_age <- function(age_group_list_indices, full_age_distribution = NULL) {
  n_age <- length(age_group_list_indices)
  mean_ages <- vector(length = n_age)
  
  # minimum age that is modeled
  min_age <- min(unlist(age_group_list_indices))
  
  # maximum age that is modeled
  max_age <- max(unlist(age_group_list_indices))
  
  # calculate full age distribution for all ages in population
  all_ages <- as.list(seq(min_age, max_age))
  
  if (is.null(full_age_distribution)){
      full_age_distribution <- calculate_age_distribution(all_ages)$age_prop
  }

  # for proper indexing, subtract (min_age - 1) from age group indices to make min_age into 1

  index_shift <- min_age - 1
  
  for (i in 1:n_age){
    mean_ages[i] <- age_group_list_indices[[i]] %*%
                      full_age_distribution[c(age_group_list_indices[[i]] - index_shift)]/
                      sum(full_age_distribution[c(age_group_list_indices[[i]] - index_shift)])
  }
  return(mean_ages)
}
