#' Best estimated age mixing matrices. 
#'
#' A list of two matrices, "MOME" and "FOME", short for "Male Omega" and "Female Omega", so named because
#' mixing matrices are often denote with the Greek letter omega. 
#' 
#' The mean partner age preference was modeled as a linear function of chooser age, as was the variance.
#' The estimated mean and variance were used to define Laplace distributions. For details, see " "Revisiting Assumptions about Age Preferences in Mathematical Models of Sexually Transmitted Infection", 2018.
#' The data used was from the  2010 - 2012 wave of the National Survey of Sexual Attitudes and Lifestyles,
#' a national British survey. 
#' 
#'
#' @format Each matrix contains 62 rows and 62 columns.
#' The ith row is the age distribution of the partners of people with age i.
#' Thus, MOME[i, j] is the probability that a male i years old chooses a female partner that is j years old. 
#' To convert this matrix to age groups you supply, use \link{mixage}{define_age_group_matrix}
#' 
#' @source \url{https://www.cdc.gov/nchs/products/life_tables.htm}
"natsal_estimates"