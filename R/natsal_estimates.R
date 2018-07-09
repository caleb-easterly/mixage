#' Best estimated age mixing matrices. 
#'
#' A list of two matrices, "MOME" and "FOME", short for "Male Omega" and "Female Omega", so named because
#' mixing matrices are often denoted with the Greek letter omega.
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
#' @source \url{http://natsal.ac.uk/natsal-3.aspx}
"natsal_estimates"

# # calculation:
# # library(dplyr)
# # first, create pointwise_choice dataset
# natsal <- read.delim("../eul_natsal_2010_for_archive.tab")
#
# ## Relevant Variables
# ### Characteristics of Respondent
# # * Sex:
# #   rsex: respondent's sex
# # 1 is male
# # 2 is female
# #
# # * Heterosexual:
# # everhet: ever had a heterosexual sexual partnership
# # 1 is yes
# # 0 is no
# # 9 is "unclassifiable"
# #
# # * Homosexual:
# # eversam: Ever had a same-sex partnership
# # Responses:
# # 1 is yes
# # 2 is no
# # 9: NA
# # $-1$: not answered
# #
# # ### Most Recent Partner
# # * r1ptage: partner's age at first sex with most recent partner
# # Responses:
# # 996: NA – partner's age at 1 sex < 13 years
# # 997: don’t know
# # 97: don’t know
# # 999: not answered
# # 99: not answered
# # $-1$: not applicable
# #
# # * rafsmr respondent’s age in completed years at 1st sex with most recent partner
# # Responses:
# # 999: not answered
# # 99: not answered
# # $-1$: not applicable
# #
# # ### 2nd and 3rd most recent partners
# # Same as above, but r1ptage2, rafs2mr; r1ptage3, rafs3mr for 2nd and third most recent partners, respectively.
#
# #Variables beginning with "r1" and ending "4" are only
# # for those who reported hetero and homosexual contacts.
#
# #select only the currently relevant variables
# natsal_sub <- subset(natsal, select = c("rsex", "everhet", "sexid",
#                                         "eversam", "r1ptage", "rafsmr",
#                                         "r1ptage2", "rafs2mr", "r1ptage3",
#                                         "rafs3mr", "hetnonew", "total_wt", "dage"))
#
# # choose only those who report heterosexual activity and identity
# natsal_het <- subset(natsal_sub, (everhet == 1 & eversam == 2 & sexid == 1))
#
# first_most_recent <- subset(natsal_het,
#                             r1ptage > 0 & r1ptage < 97 & rafsmr > 0 & rafsmr < 99,
#                             select = c("rsex", "r1ptage", "rafsmr", "total_wt"))
# second_most_recent <- subset(natsal_het, r1ptage2 > 0 & r1ptage2 < 97 & rafs2mr > 0 & rafs2mr < 99,
#                              select = c("rsex", "r1ptage2", "rafs2mr", "total_wt"))
# third_most_recent <- subset(natsal_het, r1ptage3 > 0 & r1ptage3 < 97 & rafs3mr > 0 & rafs3mr < 99,
#                             select = c("rsex", "r1ptage3", "rafs3mr", "total_wt"))
#
# colnames(first_most_recent) <- colnames(second_most_recent) <- colnames(third_most_recent) <-
#     c("sex", "ptage", "chsage", "weights")
#
# pt_choice <- rbind(first_most_recent, second_most_recent, third_most_recent)
# pt_choice$sex[which(pt_choice$sex == 1)] <- "Male"
# pt_choice$sex[which(pt_choice$sex == 2)] <- "Female"
#
# # second, use estimate_age_mixing
# natsal_estimates <- estimate_age_mixing(pt_choice,
#                                             start_ages = seq(12, 73),
#                                             max_age = 74,
#                                             distribution='gamma',
#                                             link = 'identity')
# natsal_estimates <- list("MOME" = natsal_estimates$MOME, "FOME" = natsal_estimates$FOME)
# save(natsal_estimates, file="data/natsal_estimates.rda")
