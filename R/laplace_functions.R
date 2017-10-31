# calculate laplace parameters using MOM
laplace_MOM <- function(mean, var){
    location <- mean
    scale <- sqrt(var/2)
    c("loc" = location, "scale" = scale)
}

# return location/scale parameters for each age group
laplace_mle <- function(indices, mean_ages, data, agevec, const_var){
    #from wikipedia, the location estimator is sample median
    pt <- data
    ptM <- subset(pt, sex == "Male")
    ptF <- subset(pt, sex == "Female")
    
    # 2 parameters for each age group (heterosce)
    #define the matrices to hold the parameters
    mLocScale <- fLocScale <- 
    matrix(0, nrow = length(indices), ncol = 2)
    
    # groups *include* min and max ages
    minAges <- sapply(indices, min)
    maxAges <- sapply(indices, max)
    
    for (i in 1:length(indices)){
        mSubset <- subset_dat_age(i, data = ptM, minAges = minAges, maxAges = maxAges)$ptage
        mMedian <- median(mSubset)
        fSubset <- subset_dat_age(i, data = ptF, minAges = minAges, maxAges = maxAges)$ptage
        fMedian <- median(fSubset)
        mLocScale[i, ] <- c(mMedian, 1/length(mSubset) * sum(abs(mMedian - mSubset)))
        fLocScale[i, ] <- c(fMedian, 1/length(fSubset) * sum(abs(fMedian - fSubset)))
    }
    dimnames(mLocScale) <- dimnames(fLocScale) <- list(agevec, c("loc", "scale"))
    
    # 2 parameters for *all* age groups (homosce)
    mAllMean <- mean_ages + mean(ptM$ptage - ptM$chsage)
    mAllVar <- (sd(ptM$ptage - ptM$chsage))^2
    fAllMean <- mean_ages + mean(ptF$ptage - ptF$chsage)
    fAllVar <- (sd(ptF$ptage - ptF$chsage))^2
    alls <- list(mAllMean = mAllMean, mAllVar = mAllVar, fAllMean = fAllMean, fAllVar = fAllVar)
    list("mLocScale" = mLocScale, "fLocScale" = fLocScale, "alls" = alls)
}
