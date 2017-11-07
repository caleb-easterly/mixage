#' Create age mixing matrices for the desired age groups. 
#' 
#' @description Uses the best estimates from A Journal Article, 2018, to define male and female age mixing matrices for the supplied age groups. 
#' 
#' @param start_ages A numeric vector, where the ith entry is the youngest age in the ith age group
#' @param max_age The oldest age in the model population. The oldest age group will include the ages from \code{max(start_age)} to \code{max_age - 1}
#' 
#' @details 
#' TODO: allow user to provide age distribution, calculate probabilities using LOTP
#' 
#' @export
define_age_group_matrix <- function(start_ages, max_age = 74){
    # start age must be 12
    if (min(start_ages) != 12) stop('youngest age must be 12')
    
    age_group_list <- def_age_group_list(start_ages, max_age)
    data("natsal_estimates")
    n_age <- length(start_ages)
    MOME <- FOME <- matrix(0, n_age, n_age)
    for (i in 1:n_age){
        # collapse probabilities within same age group
        specific_age_list <- age_group_list[[i]]
        if (length(specific_age_list) > 1) {
            summed_across_rows_M <- rowSums(natsal_estimates$MOME[, specific_age_list - 11])
            summed_across_rows_F <- rowSums(natsal_estimates$FOME[, specific_age_list - 11])
        } else {
            summed_across_rows_M <- natsal_estimates$MOME[, specific_age_list - 11]
            summed_across_rows_F <- natsal_estimates$FOME[, specific_age_list - 11]
        }
        for (j in 1:n_age){
            # choose median age within chooser age group, select that as representative
            MOME[j, i] <- summed_across_rows_M[median(age_group_list[[j]]) - 11]
            FOME[j, i] <- summed_across_rows_F[median(age_group_list[[j]]) - 11]
        }
    }
    return(list(MOME = MOME, FOME = FOME))
}
