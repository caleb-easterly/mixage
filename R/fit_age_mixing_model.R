# does the work for the package.
# called by estimate_age_mixing(); not meant to be accessed by end-users
# returns a list with fitted parameters
# the list is used by fill_omegas() to fill the mixing matrices
fit_age_mixing_model <- function(choice_data,
                                 distribution,
                                 link,
                                 start_ages,
                                 max_age,
                                 age_distribution){
    
    # define useful variables
    # a list where the ith entry is a vector with all ages contained in the ith age group
    indices <- def_age_group_list(start_ages, max_age)
    
    # number of age groups
    n_age <- length(start_ages)
    
    # define data
    pt <- choice_data
    
    # if weights not provided, set to null
    if (is.null(pt$weights)){
        weights <- NULL
    } 
    
    # calculate the mean age in each age group, using age distribution
    mean_ages <- avg_group_age(age_group_list_indices = indices, age_distribution)
    
    # mean chooser age in each group 
    mean_age_M_df <- data.frame("chsage" = mean_ages, "sex" = "Male")
    mean_age_F_df <- data.frame("chsage" = mean_ages, "sex" = "Female")
    
    if (distribution == "normal"){
        # standard linear model
        fit_lm <- lm(ptage ~ chsage + sex, 
                     weights = weights,
                     data = pt) 
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
        fits <- list("Mpred" = Mpred, "Fpred" = Fpred,
                     "MpredSD" = MpredSD, "FpredSD" = FpredSD,
                     "AIC" = AIC)
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
                            newdata = mean_age_M_df,
                            se = TRUE)
        Fgampred <- predict(fit_glm,
                            type = "response",
                            dispersion = 1/mle_shape$alpha, 
                            newdata = mean_age_F_df,
                            se = TRUE)
        fits <- list("mle_shape" = mle_shape,
                     "Mpred" = Mgampred$fit,
                     "MpredSD" = Mgampred$se.fit,
                     "Fpred" = Fgampred$fit,
                     "FpredSD" = Fgampred$se.fit,
                     "AIC" = AIC)
    }
    return(fits)
}