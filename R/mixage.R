#' mixage: Estimate preferred age of sexual partners
#' 
#' This package provides functions to estimate age mixing preferences for mathematical models of STIs.
#' The analysis methods were published in "Revisiting Assumptions about Age Preferences in
#' Mathematical Models of Sexually Transmitted Infection" (Easterly, et al., 2018). 
#' 
#'There are three functions provided by \code{mixage}, 
#'
#'\enumerate{
#'\item \link[mixage]{estimate_age_mixing} to estimate age mixing from your data.
#'\item \link[mixage]{best_age_mixing}, to run \link[mixage]{estimate_age_mixing} 
#'with different assumptions and find the best mixing structure. 
#'\item \link[mixage]{define_age_group_matrix}, to take the best age mixing estimates from "Revisiting Assumptions about Age Preferences in Mathematical 
#' Models of Sexually Transmitted Infection" (Easterly, et al., 2018), and adapt them to your age groups and population age distribution. 
#'}
#' 
#'@import extraDistr dplyr
"_PACKAGE"