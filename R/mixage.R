#' mixage: Estimate preferred age of sexual partners
#' 
#' This package provides functions to estimate age mixing preferences for mathematical models of STIs.
#' The analysis methods were published in PAPER. 
#' 
#'There is one function provided by \code{mixage}, \code{estimate_age_mixing()}, which takes partnership data in the following form:
#'
#'\tabular{cccc}{
#'chsage \tab ptage \tab sex \tab [weights] \cr
#'19 \tab 25 \tab F \tab 0.1 \cr
#'25 \tab 23 \tab F \tab 0.9 \cr
#'45 \tab 48 \tab M \tab 0.3 \cr
#'...
#'}
#'
#'where the weights are optional. If provided, they are passed directly to \code{lm()} like \code{lm(..., weights = weights)}. 
#'
#' The function estimates the age mixing matrix for the user-provided data, analysis parameters, and age groups. See the function documentation (\code{?estimate_age_mixing}) for additional details. 
#' 
#'@import extraDistr dplyr
"_PACKAGE"