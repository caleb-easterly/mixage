#' Return best-fitting mixing structure
#' 
#' @description A convenience function - calculates the likelihood of observing the provided data under each mixing structure provided in \link[mixage]{estimate_age_mixing}. See \link{mixage}{estimate_age_mixing} for more details. 
#' 
#' @param max_age
#' The non-inclusive right-hand endpoint of the oldest age group within the model population.
#' Default is 74, so if the oldest age group begins at 60, the age interval is 60-73. Must be less than or equal to 100. 
#'
#' @param choice_data
#' a dataframe with chooser age, partner age, sex, and optional survey weights. 
#' These columns should be named \code{chsage}, \code{ptage}, \code{sex}, and \code{weights}, respectively.
#' Sex must be coded as "Male" and "Female"
#' 
#' @param start_ages
#' vector of the youngest ages included in each age group. 
#' If \code{start_ages} is \code{c(12, 20, 30)}, the age groups are 12-19, 20-29, and 30 to \code{max_age - 1}. 
#' 
#'
#' @param age_distribution
#' Optional: a vector of length \code{length(seq(min(start_ages), max_age))},
#' where the \code{i}th entry is the
#' proportion of the model population with age \code{i}. This vector defines the proportion of the model population with every \emph{age}, not \emph{age group}. If not provided, the 2011 U.S.
#' life tables are used to estimate the population age distribution. 
#' 
#' 
#' @export
best_age_mixing <- function(choice_data,
                            start_ages,
                            max_age = 74,
                            age_distribution = NULL) {
    dv <- matrix(c("normal", rep("gamma", 2),
                 "identity", "log", "identity"), nrow = 3)

    mixing_mats <- vector(length = nrow(dv), mode = "list")
    AIC <- rep(0, nrow(dv))
    for (i in 1:nrow(dv)) {
        mixing_mats[[i]] <- estimate_age_mixing(choice_data = choice_data,
                                           start_ages = start_ages,
                                           distribution = dv[i, 1],
                                           link = dv[i, 2],
                                           max_age = max_age,
                                           age_distribution = age_distribution)
        AIC[i] <- mixing_mats[[i]]$AIC
    }
    best_structure = mixing_mats[[which.min(AIC)]]
    return(list("all_AIC" = data.frame(dv, AIC),
           "best_structure" = best_structure))
}
    