#' Calculate age mixing structures 
#' 
#' @description 
#' A function to estimate age mixing matrices for user-supplied data. Choose between 
#' different distributions around mean partner age (Normal or Gamma),
#' and either a identity or log link for the Gamma distribution. See
#' "Revisiting Assumptions about Age Preferences in Mathematical 
#' Models of Sexually Transmitted Infection" (Easterly, et al., 2018) for details
#' about the estimation procedures. For a function 
#' to estimate the best mixing structure for your data, see \link[mixage]{best_age_mixing}
#' 
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
#' @param distribution
#' Provide the distribution of the errors around the mean partner age.
#' Choose from "gamma" or "normal" distributions.
#'   
#' @param link
#' When \code{"gamma"} is chosen as the distribution, the link can be either \code{"identity"} or \code{"log"}.
#' If the distribution is \code{"normal"}, this has no effect.
#'
#' @param max_age
#' The non-inclusive right-hand endpoint of the oldest age group within the model population.
#' Default is 74, so if the oldest age group begins at 60, the age interval is 60-73. Must be less than or equal to 100. 
#' 
#' @param age_distribution
#' Optional: a vector of length \code{length(seq(min(start_ages), max_age))},
#' where the \code{i}th entry is the
#' proportion of the model population with age \code{i}. This vector defines the proportion of the model population with every \emph{age}, not \emph{age group}. If not provided, the 2011 U.S.
#' life tables are used to estimate the population age distribution. 
#' 
#' @return A list, where MOME is the male age mixing matrix, FOME is the female age mixing matrix,
#' and AIC is the AIC of the estimated statistical model. 
#' 
#' @examples
#' load("mixage_sample_data")
#' agemix <- estimate_age_mixing(mixage_sample_data, start_ages = seq(12, 72, by = 2),
#'     max_age = 74)
#'  
#' 
#' @export
estimate_age_mixing <- function(choice_data,
                                start_ages, 
                                distribution = c("gamma", "normal"),
                                link = c("identity", "log"),
                                max_age = 74,
                                age_distribution = NULL){
    # define useful variables
    # a list where the ith entry is a vector with all ages contained in the ith age group
    indices <- def_age_group_list(start_ages, max_age)
    
    # number of age groups
    n_age <- length(start_ages)
    
    # define data
    pt <- choice_data

    # run checks on data - correct column names, etc. 
    # data column names
    expected_colnames <- c("chsage", "ptage", "sex")
    if (! (setequal(colnames(choice_data), expected_colnames) || 
           setequal(colnames(choice_data), c(expected_colnames, "weights"))) ){
        stop("Wrong columns provided in choice_data. See ?estimate_age_mixing for help")
    }
    
    # max age less than 100
    if (max_age > 100) {
        stop("max_age must be less than or equal to 100")
    }
    
    # duplicates in start_ages
    if (any(diff(start_ages) <= 0)){
        stop("start_ages must be unique and increasing. Ex: seq(12, 60, by = 2)")
    }
    
    # wrong coding for sex
    if (!setequal(names(table(choice_data$sex)), c("Male", "Female"))){
        stop("choice_data$sex must be coded as 'Male' and 'Female'")
    }
    
    # if weights not provided, set to null
    if (is.null(choice_data$weights)){
        weights <- NULL
    } 
    
    # check that age_distribution is right length
    if (length(age_distribution) != length(seq(min(start_ages), max_age)) &
        !is.null(age_distribution)){
        stop("age_distribution and min(start_ages):max_age must be the same length")
    }
    
    # calculate the mean age in each age group, using age distribution
    mean_ages <- avg_group_age(age_group_list_indices = indices, age_distribution)
    
    # mean chooser age in each group 
    mean_age_M_df <- data.frame("chsage" = mean_ages, "sex" = "Male")
    mean_age_F_df <- data.frame("chsage" = mean_ages, "sex" = "Female")
    
    # choose distribution and link
    distribution <- match.arg(distribution)
    link <- match.arg(link)
    
    if (distribution == "normal"){
        # standard linear model
        fit_lm <- lm(ptage ~ chsage + sex, data = pt, weights = weights) 
        AIC <- AIC(fit_lm)
        
        #predict m and f
        Mpred <- predict(fit_lm,
                         newdata = mean_age_M_df,
                         interval = "prediction")
        Fpred <- predict(fit_lm,
                         newdata = mean_age_F_df,
                         interval = "prediction")
        
        MpredSD <- (Mpred[, "upr"] - Mpred[, "lwr"])/(1.96*2)
        FpredSD <- (Fpred[, "upr"] - Fpred[, "lwr"])/(1.96*2)
        
    } else {
        ### Generalized Linear model (gamma regression with identity link)
        if (link == "identity") {
            fit_glm <- glm(ptage ~ chsage + sex,
                           family = Gamma(link = "identity"),
                           weights = weights,
                           data = pt)
            AIC <- AIC(fit_glm)
        } else {
            fit_glm <- glm(ptage ~ chsage + sex,
                           family = Gamma(link = "log"),
                           weights = weights,
                           data = pt)
            AIC <- AIC(fit_glm)
        }
        mle_shape <- MASS::gamma.shape(fit_glm)
        Mgampred <- predict(fit_glm,
                           type = "response",
                           dispersion = 1/mle_shape$alpha, 
                           newdata = mean_age_M_df)
        Fgampred <- predict(fit_glm,
                            type = "response",
                            dispersion = 1/mle_shape$alpha, 
                            newdata = mean_age_F_df)
    }
    
    
    #set up and fill MOME and FOME
    #initialize MOME&FOME
    MOME <- FOME <- matrix(0, n_age, n_age)
    # right ends of the age intervals
    high_age <- start_ages + c(diff(start_ages), max_age - max(start_ages))
    temp_start_ages <- start_ages
    temp_start_ages[1] <- 0 #theoretical lowest bound
    
    ## Gamma
    if (distribution == "gamma"){
        #evaluate gamma distribution at 12 to 74 years, using
        #differences in cumulative distribution function
        for (i in 1:length(start_ages)){
            Mreg_raw_probs <- pgamma(high_age, shape = mle_shape$alpha, rate = mle_shape$alpha/Mgampred[i]) -
                pgamma(temp_start_ages, shape = mle_shape$alpha, rate = mle_shape$alpha/Mgampred[i])
            Freg_raw_probs <- pgamma(high_age, shape = mle_shape$alpha, rate = mle_shape$alpha/Fgampred[i]) -
                pgamma(temp_start_ages, shape = mle_shape$alpha, rate = mle_shape$alpha/Fgampred[i])
            MOME[i, ] <- Mreg_raw_probs / sum(Mreg_raw_probs)
            FOME[i, ] <- Freg_raw_probs / sum(Freg_raw_probs)
        }
    }
    
    # Normal distribution
    if (distribution == "normal") {
        for (i in 1:length(start_ages)){
            Mreg_raw_probs_norm <- pnorm(high_age, mean = Mpred[i], sd = MpredSD[i]) -
                pnorm(temp_start_ages,  mean = Mpred[i], sd = MpredSD[i])
            Freg_raw_probs_norm <-  pnorm(high_age, mean = Fpred[i], sd = FpredSD[i]) -
                pnorm(temp_start_ages,  mean = Fpred[i], sd = FpredSD[i])
            
            MOME[i, ] <- Mreg_raw_probs_norm / sum(Mreg_raw_probs_norm)
            FOME[i, ] <- Freg_raw_probs_norm / sum(Freg_raw_probs_norm)
        }
        
    }
    return(list("MOME" = MOME, "FOME" = FOME, "AIC" = AIC))
}


subset_dat_age <- function(i, data, minAges, maxAges){
    subset(data, chsage >= minAges[i] & chsage <= maxAges[i])
}

# define a list of age group indices, where each list entry is a vector of ages contained within that age group
# @param start_ages
# A numeric vector of the first age year contained within each age group. Ex: c(12, 16, 20)
# 
# @param max_age
# The maximum age within the model population. Default is 74, so the last age group is (maximum start age) to (max_age - 1). 
def_age_group_list <- function(start_ages, max_age = 74){
    #number of age groups
    n_age <- length(start_ages)
    
    # define a list of indices containing ages in age groups
    all_ind <- lapply(1:n_age,
                      function(i) {
                          start_ages[i] +
                              0:(c(diff(start_ages) - 1, (max_age - 1) - start_ages[n_age])[i])
                      }
    ) 
    return(all_ind)
}



# # try to transform response instead
# pt_choice_all_response_transform <- function(indices,
#                                                 mean_ages,
#                                                 agevec, 
#                                                 response_transform = c("sqrt", "log", "none")){
#     data("longform_choice_data")
#     pt <- longform_choice_data
#     n_age <- length(indices)
#     
#     # mean chooser age in each group 
#     mean_age_M_df <- data.frame("chsage" = mean_ages, "sex" = "Male")
#     mean_age_F_df <- data.frame("chsage" = mean_ages, "sex" = "Female")
#     
#     ## Model the variance
#     response_transform = match.arg(response_transform)
#     if (response_transform == "sqrt"){
#         ptlm <- lm(sqrt(ptage) ~ chsage + sex, data = pt, weights = weights)
#         # Recover desired variance for each level of x. This is what we need for MoM!
#         Mpred <- (predict(ptlm, newdata = mean_age_M_df, interval = "prediction"))^2
#         Fpred <- (predict(ptlm, newdata = mean_age_F_df, interval = "prediction"))^2
#         Mvar <- ((Mpred[, "upr"] - Mpred[, "lwr"])/4)^2
#         Fvar <- ((Fpred[, "upr"] - Fpred[, "lwr"])/4)^2
#     }
#     if (response_transform == "log"){
#         ptlm <- lm(log(ptage) ~ chsage + sex, data = pt, weights = weights)
#         # Recover desired variance for each level of x. This is what we need for MoM!
#         Mpred <- exp(predict(ptlm, newdata = mean_age_M_df, interval = "prediction"))
#         Fpred <- exp(predict(ptlm, newdata = mean_age_F_df, interval = "prediction"))
#         Mvar <- ((Mpred[, "upr"] - Mpred[, "lwr"])/4)^2
#         Fvar <- ((Fpred[, "upr"] - Fpred[, "lwr"])/4)^2
#     }
#     if (response_transform == "none"){
#         ptlm <- lm(ptage ~ chsage + sex, data = pt, weights = weights)
#         # Recover desired variance for each level of x. This is what we need for MoM!
#         Mpred <- predict(ptlm, newdata = mean_age_M_df, interval = "prediction")
#         Fpred <- predict(ptlm, newdata = mean_age_F_df, interval = "prediction")
#         Mvar <- ((Mpred[, "upr"] - Mpred[, "lwr"])/4)^2
#         Fvar <- ((Fpred[, "upr"] - Fpred[, "lwr"])/4)^2
#     }
#     
#     ## Gamma
#     grp_gam <- shared_gam <- matrix(0, nrow = n_age, ncol = 4)
#     colnames(grp_gam) <- c("Mshape", "Mrate", "Fshape", "Frate")
#     for (i in 1:n_age){
#         grp_gam[i, ] <- c(gam_param(Mpred[i], Mvar[i]),
#                           gam_param(Fpred[i], Fvar[i]))
#     }
#     
#     #set up and fill MOME and FOME
#     #initialize MOME&FOME
#     MOMEreg_gam <- FOMEreg_gam <- matrix(0, n_age, n_age)
#     # right ends of the age intervals
#     high_age <- agevec + c(diff(agevec), 74 - max(agevec))
#     temp_agevec <- agevec
#     temp_agevec[1] <- 0 #theoretical lowest bound
#     
#     #evaluate gamma distribution at 12 to 74 years, using
#     #differences in cumulative distribution function
#     for (i in 1:length(agevec)){
#         ##using pgamma and actual age groups)
#         Mreg_raw_probs <- pgamma(high_age, shape = grp_gam[i, 1], rate = grp_gam[i, 2]) -
#             pgamma(temp_agevec, shape = grp_gam[i, 1], rate = grp_gam[i, 2])
#         Freg_raw_probs <- pgamma(high_age, shape = grp_gam[i, 3], rate = grp_gam[i, 4]) -
#             pgamma(temp_agevec, shape = grp_gam[i, 3], rate = grp_gam[i, 4])
#         MOMEreg_gam[i, ] <- Mreg_raw_probs / sum(Mreg_raw_probs)
#         FOMEreg_gam[i, ] <- Freg_raw_probs / sum(Freg_raw_probs)
#     }
#     
#     ### Laplace
#     #set up and fill MOME and FOME
#     MOMEreg_lap <- FOMEreg_lap <- matrix(0, n_age, n_age)
#     
#     # use regression predictions to get laplace estimates
#     # transpose makes same dimension/format as gamma parameters
#     lap_reg <- t(sapply(1:n_age, 
#                         function(x) {
#                             c(
#                                 laplace_MOM(Mpred[x], Mvar[x]),
#                                 laplace_MOM(Fpred[x], Fvar[x])
#                             )
#                         }
#         )
#     )
#     
#     for (i in 1:length(agevec)){
#         Mreg_raw_probs_lap <- plaplace(high_age, mu = lap_reg[i, 1],
#                                        sigma = lap_reg[i, 2]) -
#             plaplace(temp_agevec, mu = lap_reg[i, 1],
#                      sigma = lap_reg[i, 2])
#         Freg_raw_probs_lap <- plaplace(high_age, mu = lap_reg[i, 3],
#                                        sigma = lap_reg[i, 4]) -
#             plaplace(temp_agevec, mu = lap_reg[i, 3],
#                      sigma = lap_reg[i, 4])
#         MOMEreg_lap[i, ] <- Mreg_raw_probs_lap / sum(Mreg_raw_probs_lap)
#         FOMEreg_lap[i, ] <- Freg_raw_probs_lap / sum(Freg_raw_probs_lap)
#     }
#     
#     # Normal distribution
#     # define normal MOME and FOME
#     MOMEreg_norm <- FOMEreg_norm <- matrix(0, n_age, n_age)
#     
#     for (i in 1:length(agevec)){
#         ##using pgamma and actual age groups)
#         Mreg_raw_probs_norm <- pnorm(high_age, mean = Mpred[i], sd = sqrt(Mvar[i])) -
#             pnorm(temp_agevec,  mean = Mpred[i], sd = sqrt(Mvar[i]))
#         Freg_raw_probs_norm <-  pnorm(high_age, mean = Fpred[i], sd = sqrt(Fvar[i])) -
#             pnorm(temp_agevec,  mean = Fpred[i], sd = sqrt(Fvar[i]))
#         
#         MOMEreg_norm[i, ] <- Mreg_raw_probs_norm / sum(Mreg_raw_probs_norm)
#         FOMEreg_norm[i, ] <- Freg_raw_probs_norm / sum(Freg_raw_probs_norm)
#     }
#     
#     return(list(gamReg = list("MOMEreg_gam" = MOMEreg_gam, "FOMEreg_gam" = FOMEreg_gam),
#                 lapReg = list("MOMEreg_lap" = MOMEreg_lap, "FOMEreg_lap" = FOMEreg_lap),
#                 normReg = list("MOMEreg_norm" = MOMEreg_norm, "FOMEreg_norm" = FOMEreg_norm))
#     )
# }
