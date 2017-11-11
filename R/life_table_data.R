#' The U.S. 2011 life tables. 
#'
#' A dataset containing the U.S. 2011 life tables, from the National Vital Statistics System. 
#' The variables are as follows:
#' \itemize{
#'   \item age. In years, given as a range. 
#'   \item qx. Probability of dying between ages x and x + 1,
#'   \item lx. Number surviving to age x,
#'   \item dx. Number dying between ages x and x + 1
#'   \item L. Person-years lived between ages x and x + 1
#'   \item Tx. Total number of person-years lived above age x
#'   \item ex. Expectation of life at age x
#' }
#'
#' @format A data frame with 101 rows and 7 variables
#' @source \url{https://www.cdc.gov/nchs/products/life_tables.htm}
"life_table_data"