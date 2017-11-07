# Calculate Log Likelihood of data given on mixing matrix
# 
# @param listMF
# list of male (1) and female(2) age mixing matrices
# 
# @param Mcounts_data, Fcounts_data matrices where entry in ith row and jth column indicates number of age group i's reported partnerships with age group j
# 
mixing_matrix_negloglikelihood <- function(listMF, Mcounts_data, Fcounts_data){
    Mmat <- listMF[[1]]
    Fmat <- listMF[[2]]
    offset <- 1e-9
    loglike <- sum(Mcounts_data * log(Mmat + offset) + Fcounts_data * log(Fmat + offset))
    return(-loglike)
}