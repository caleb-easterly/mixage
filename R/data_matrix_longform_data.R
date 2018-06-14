# calculates a mixing matrix from the data, along with counts 
# 
# @param choice_data
# a dataframe with chooser age, partner age, sex, and optional survey weights. 
# These columns should be named \code{chsage}, \code{ptage}, \code{sex}, and \code{weights} (optional), respectively.
# Sex must be coded as "Male" and "Female"
# 
# 
# @param indices a list of vectors, where the ith list entry 
# is a vector of ages in that age group
# 
# @return A list with the following elements:
# - Mcounts,Fcounts: matrices of dim n_age by n_age, with the number of reported partnerships for each
#       age pairing
# - MOMEdat,FOMEdat: same as the counts, but the rows are normalized to sum to 1
# 
# @export
data_matrix_longform <- function(choice_data, indices){
  n_age <- length(indices)
  MOMEdat <- FOMEdat <- 
    Mcounts <- Fcounts <- matrix(0, n_age, n_age)
  
  # wrong coding for sex
  if (!setequal(names(table(choice_data$sex)), c("Male", "Female"))){
      stop("choice_data$sex must be coded as 'Male' and 'Female'")
  }
  
  # if weights not provided, set to null
  if (is.null(choice_data$weights)){
      total_wt <- NULL
  }
  for (ch in 1:n_age){
    for (pt in 1:n_age){
      chsage_min <- min(indices[[ch]])
      chsage_max <- max(indices[[ch]])
      ptage_min <- min(indices[[pt]])
      ptage_max <- max(indices[[pt]])
      Mcounts[ch, pt] <- nrow(subset(choice_data, sex == "Male" &
                                       chsage >= chsage_min & chsage <= chsage_max &
                                       ptage >= ptage_min & ptage <= ptage_max))
      Fcounts[ch, pt] <- nrow(subset(choice_data, sex == "Female" &
                                       chsage >= chsage_min & chsage <= chsage_max &
                                       ptage >= ptage_min & ptage <= ptage_max))
    }
    MOMEdat[ch, ] <- Mcounts[ch, ] / sum(Mcounts[ch, ])
    FOMEdat[ch, ] <- Fcounts[ch, ] / sum(Fcounts[ch, ])
  }
  list(Mcounts = Mcounts, Fcounts = Fcounts, MOMEdat = MOMEdat, FOMEdat = FOMEdat)
}
