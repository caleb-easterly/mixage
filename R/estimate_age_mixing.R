# define a list of age group indices, where each list entry is a vector of ages contained within that age group
# @param start_ages
# A numeric vector of the first age year contained within each age group. Ex: c(12, 16, 20)
# 
# @param max_age
# The maximum age within the model population. Default is 74, so the last age group is (maximum start age) to (max_age - 1). 
# @export
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


#' calculate age mixing structures 
#' 
#' @description 
#' DESCRIBE TRANSFORMATIONS HERE
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
#' @param distribution
#' Provide the distribution of the errors around the mean partner age. Choose from Gamma, Laplace, and Normal distributions. 
#' 
#' @param variance_model
#' describes the transformation of the residuals for estimating the heteroscedastic variance. 
#' Choose \code{linear}, \code{sqrt}, \code{log}, or \code{const}. 
#'
#' @param death_prob
#' Length 100 vector. \code{death_prob[1]} is probability of dying in the next year for 1-year-old,
#' \code{death_prob[12]} is for 12-year-old, and so on. If not supplied, default is taken from  2011 US life tables
#' 
#' @export
estimate_age_mixing <- function(choice_data,
                                max_age,
                                start_ages, 
                                distribution = c("gamma", "laplace", "normal"),
                                variance_model = c("sqrt", "log", "linear", "const"),
                                death_prob = NULL){
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
    
    # calculate the mean age in each age group, using age distribution
    mean_ages <- avg_group_age(age_group_list_indices = indices)
    
    # mean chooser age in each group 
    mean_age_M_df <- data.frame("chsage" = mean_ages, "sex" = "Male")
    mean_age_F_df <- data.frame("chsage" = mean_ages, "sex" = "Female")
    
    # changed from male and female separate
    ptlm <- lm(ptage ~ chsage + sex, data = pt, weights = weights) 

    #predict m and f
    Mpred <- predict(ptlm, newdata = mean_age_M_df)
    Fpred <- predict(ptlm, newdata = mean_age_F_df)
    
    # predict constant variance
    const_var <- 1/(nrow(pt) - 2) * sum(resid(ptlm)^2)
    
    # Greene's method (without log)
    # variance calculation for modeled heteroscedasticity by age
    
    # calculate residuals
    mod_resid <- resid(ptlm)
    
    #add residuals to data frame
    pt$resid <- mod_resid
    
    ## Model the variance
    variance_model = match.arg(variance_model)
    if (variance_model == "sqrt"){
        mod.var <- lm(sqrt(resid^2) ~ chsage + sex, data = pt, weights = weights)
        # Recover desired variance for each level of x. This is what we need for MoM!
        Mvar_reg <- (predict(mod.var, newdata = mean_age_M_df))^2
        Fvar_reg <- (predict(mod.var, newdata = mean_age_F_df))^2
    } 
    if (variance_model == "log"){
        mod.var <- lm(log(resid^2) ~ chsage + sex, data = pt, weights = weights)
        # Recover desired variance for each level of x. This is what we need for MoM!
        Mvar_reg <- exp(predict(mod.var, newdata = mean_age_M_df))
        Fvar_reg <- exp(predict(mod.var, newdata = mean_age_F_df))
    }
    if (variance_model == "linear"){
        mod.var <- lm(resid^2 ~ chsage + sex, data = pt, weights = weights)
        # Recover desired variance for each level of x. This is what we need for MoM!
        Mvar_reg <- predict(mod.var, newdata = mean_age_M_df)
        Mvar_reg[which(Mvar_reg < 0)] <- min(Mvar_reg[which(Mvar_reg > 0)])
        Fvar_reg <- predict(mod.var, newdata = mean_age_F_df)
        Fvar_reg[which(Fvar_reg < 0)] <- min(Fvar_reg[which(Fvar_reg > 0)])
    }
    if (variance_model == "const") {
        Mvar_reg <- rep(const_var, n_age)
        Fvar_reg <- rep(const_var, n_age)
    }
    
    # choose distribution
    distribution <- match.arg(distribution)
    
    #set up and fill MOME and FOME
    #initialize MOME&FOME
    MOME <- FOME <- matrix(0, n_age, n_age)
    # right ends of the age intervals
    high_age <- start_ages + c(diff(start_ages), 74 - max(start_ages))
    temp_start_ages <- start_ages
    temp_start_ages[1] <- 0 #theoretical lowest bound
    
    ## Gamma
    if (distribution == "gamma"){
        grp_gam <- matrix(0, nrow = n_age, ncol = 4)
        colnames(grp_gam) <- c("Mshape", "Mrate", "Fshape", "Frate")
        for (i in 1:n_age){
            grp_gam[i, ] <- c(gam_param(Mpred[i], Mvar_reg[i]),
                              gam_param(Fpred[i], Fvar_reg[i]))
        }
    
        #evaluate gamma distribution at 12 to 74 years, using
        #differences in cumulative distribution function
        for (i in 1:length(start_ages)){
            Mreg_raw_probs <- pgamma(high_age, shape = grp_gam[i, 1], rate = grp_gam[i, 2]) -
                pgamma(temp_start_ages, shape = grp_gam[i, 1], rate = grp_gam[i, 2])
            Freg_raw_probs <- pgamma(high_age, shape = grp_gam[i, 3], rate = grp_gam[i, 4]) -
                pgamma(temp_start_ages, shape = grp_gam[i, 3], rate = grp_gam[i, 4])
            MOME[i, ] <- Mreg_raw_probs / sum(Mreg_raw_probs)
            FOME[i, ] <- Freg_raw_probs / sum(Freg_raw_probs)
        }
    }
    
    
    ### Laplace
    if (distribution == "laplace") {
        # use regression predictions to get laplace estimates
        # transpose makes same dimension/format as gamma parameters
        lap_reg <- t(
            sapply(1:n_age, 
                   function(x) {
                        c(
                            laplace_MOM(Mpred[x], Mvar_reg[x]),
                            laplace_MOM(Fpred[x], Fvar_reg[x])
                        )
                    }
                  )
        )
        
        for (i in 1:length(start_ages)){
            Mreg_raw_probs_lap <- plaplace(high_age, mu = lap_reg[i, 1],
                                           sigma = lap_reg[i, 2]) -
                plaplace(temp_start_ages, mu = lap_reg[i, 1],
                         sigma = lap_reg[i, 2])
            Freg_raw_probs_lap <- plaplace(high_age, mu = lap_reg[i, 3],
                                           sigma = lap_reg[i, 4]) -
                plaplace(temp_start_ages, mu = lap_reg[i, 3],
                         sigma = lap_reg[i, 4])
            MOME[i, ] <- Mreg_raw_probs_lap / sum(Mreg_raw_probs_lap)
            FOME[i, ] <- Freg_raw_probs_lap / sum(Freg_raw_probs_lap)
        }
    }
    
    
    # Normal distribution
    if (distribution == "normal") {
        for (i in 1:length(start_ages)){
            Mreg_raw_probs_norm <- pnorm(high_age, mean = Mpred[i], sd = sqrt(Mvar_reg[i])) -
                pnorm(temp_start_ages,  mean = Mpred[i], sd = sqrt(Mvar_reg[i]))
            Freg_raw_probs_norm <-  pnorm(high_age, mean = Fpred[i], sd = sqrt(Fvar_reg[i])) -
                pnorm(temp_start_ages,  mean = Fpred[i], sd = sqrt(Fvar_reg[i]))
            
            MOME[i, ] <- Mreg_raw_probs_norm / sum(Mreg_raw_probs_norm)
            FOME[i, ] <- Freg_raw_probs_norm / sum(Freg_raw_probs_norm)
        }
        
    }
    
    # calculate log likelihood of data given assumption
    data_matrix <- data_matrix_longform(choice_data, indices)
    loglike <- mixing_matrix_negloglikelihood(list(MOME, FOME),
                                              data_matrix$Mcounts,
                                              data_matrix$Fcounts)
    return(list("MOME" = MOME, "FOME" = FOME, "neg_log_likelihood" = loglike))
}


subset_dat_age <- function(i, data, minAges, maxAges){
    subset(data, chsage >= minAges[i] & chsage <= maxAges[i])
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
