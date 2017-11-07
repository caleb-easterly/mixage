#' Return best-fitting mixing structure
#' 
#' @description A convenience function - calculates the likelihood of observing the provided data under each mixing structure provided in \link[mixage]{estimate_age_mixing}
#' @export
best_age_mixing <- function(mixing_data, start_ages, max_age = 74) {
    dists <- c("normal", "laplace", "gamma")
    vars <- c("const", "log", "linear", "sqrt")
    dv <- as.matrix(expand.grid(dists, vars))
    colnames(dv) <- c("distribution", "variance")
    mixing_mats <- vector(length = nrow(dv), mode = "list")
    nll <- rep(0, nrow(dv))
    for (i in 1:nrow(dv)) {
        mixing_mats[[i]] <- estimate_age_mixing(choice_data = mixing_data,
                                           max_age = max_age,
                                           start_ages = start_ages,
                                           distribution = dv[i, 1],
                                           variance_model = dv[i, 2])
        nll[i] <- mixing_mats[[i]]$neg_log_likelihood
    }
    best_structure = mixing_mats[[which.min(nll)]]
    return(list("all_NLL" = data.frame(dv, nll),
           "best_structure" = best_structure))
}
    