#' Create age mixing matrices for the desired age groups. 
#' 
#' @description Uses the best estimates from "Revisiting Assumptions about Age Preferences in Mathematical 
#' Models of Sexually Transmitted Infection" (Easterly, et al., 2018) to define male and female age mixing matrices for the supplied age groups. 
#' 
#' @param start_ages A numeric vector, where the ith entry is the youngest age in the ith age group
#' @param max_age The oldest age in the model population. The oldest age group will include the ages from \code{max(start_age)} to \code{max_age - 1}
#' @param age_distribution A length 99 vector with the proportion of in the population who is ages 1,2,3,..., 99.
#' 
#' @details 
#' 
#' The function takes the best estimates from "Revisiting Assumptions about Age Preferences in Mathematical Models of STIs" and adapts them to user-supplied age groups. 
#' If a population other than the U.S. population is used, an age distribution for that population is needed.
#' This is needed to 'average' probabilities within an age group, by using the law of total probability. 
#' The probability of someone in age group \eqn{i} choosing someone in age group \eqn{j} is 
#' \deqn{Pr(P = j | C = i) = \sum Pr(P = j | C = k) Pr(C = k) }
#' where the sum is taken over \eqn{k} in the set of ages in age group \eqn{i}.
#' The full population age distribution is used to calculate the probability that
#' someone within age group \eqn{i} has age \eqn{k}.
#' 
#' @return A list with the elements \code{MOME} and \code{FOME} (male and female 'omega'), both matrices with a row and column for each age group. The entry MOME[i, j] is the probability that a male in age group i chooses a female of age group j as a partner.
#' 
#' @export
define_age_group_matrix <- function(start_ages,
                                    max_age = 74,
                                    age_distribution = NULL){
    # start age must be 12
    if (min(start_ages) != 12) stop('youngest age must be 12')
    
    # define the list of age group vectors
    age_group_list <- def_age_group_list(start_ages, max_age)
    
    # define minimum and max model ages (start age of oldest model group)
    min_age <- min(start_ages)
    max_model_age <- max(start_ages)
    # if age distribution is null, bring in uk life table data
    if (is.null(age_distribution)){
        age_distribution <- calculate_single_year_age_distribution(min_age, max_model_age)
    } 
    
    n_age <- length(start_ages)
    MOME <- FOME <- matrix(0, n_age, n_age)
    for (i in 1:n_age){
        # collapse probabilities within same age group
        chooser_age_list <- age_group_list[[i]]
        chooser_indices <- chooser_age_list - 11
        
        # get distribution within chooser age group
        dist_chsage_group <- age_distribution[chooser_indices]/sum(age_distribution[chooser_indices])
        for (j in 1:n_age) {
            partner_age_list <- age_group_list[[j]]
            partner_indices <- partner_age_list - 11
            if (length(partner_age_list) > 1) {
                # get probability of each age in group i choosing group j
                # the drop = FALSE prevents coercion to vector if chooser indices is length 1
                summed_across_rows_M <- rowSums(natsal_estimates$MOME[chooser_indices, partner_indices, drop = FALSE])
                summed_across_rows_F <- rowSums(natsal_estimates$FOME[chooser_indices, partner_indices, drop = FALSE])
            } else {
                # no need to do rowSums, because only one column
                summed_across_rows_M <- natsal_estimates$MOME[chooser_indices, partner_indices]
                summed_across_rows_F <- natsal_estimates$FOME[chooser_indices, partner_indices]
            }
            # use law of total probability to calculate the probability that anybody in group i chooses group j
            MOME[i, j] <- summed_across_rows_M %*% dist_chsage_group
            FOME[i, j] <- summed_across_rows_F %*% dist_chsage_group
        }
        # if age group list didn't include 1 to 99 (which it probably didn't)
        # we need to normalize the rows to 1
        MOME[i, ] <- MOME[i, ] / sum(MOME[i, ])
        FOME[i, ] <- FOME[i, ] / sum(FOME[i, ])
    }
    return(list(MOME = MOME, FOME = FOME))
}
