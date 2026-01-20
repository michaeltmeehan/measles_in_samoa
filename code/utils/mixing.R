## ----------------------------------------------------------------------------
# Mixing / contact matrix utilities
#
# Provides age-stratified contact matrix used by transmission and mixing
# calculations. The matrix is 5x5 with age groups: "0-4", "5-14", "15-34",
# "35-54", and "55+". Values are returned as a numeric matrix with row and
# column names set to the age-group labels.
#
# Reference:
# Watson, C.H., Coriakula, J., Ngoc, D.T.T., Flasche, S., Kucharski, A.J., Lau,
# C.L., Thieu, N.T.V., Waroux, O., Rawalai, K., Van, T.T., Taufa, M., Baker, S.,
# Nilles, E.J., Kama, M., Edmunds, W.J.: Social mixing in Fiji: Who-eats-with-
# whom contact patterns and the implications of age and ethnic heterogeneity for
# disease dynamics in the Pacific Islands. PLOS ONE 12(12), 1–16 (2017).
# https://doi.org/10.1371/journal.pone.0186911
#
# File: code/utils/mixing.R
# Purpose: supply contact matrix for model components
#
## ----------------------------------------------------------------------------


#' Import contact matrix
#'
#' Returns a 5x5 contact matrix with predefined age groups. The rows and
#' columns correspond to the following age groups: "0-4", "5-14", "15-34",
#' "35-54", and "55+". Use this matrix for computing age-structured mixing
#' in transmission models.
#'
#' @return A numeric matrix (5 x 5) with row and column names for age groups.
#' @examples
#' cm <- import_contact_matrix()
#' cm["0-4", "5-14"]
#' @export
import_contact_matrix = function(){
  contact_matrix = matrix(data=c(1.15,1.05,1.66,0.85,0.51,
                                 0.63,3.20,1.25,1.28,0.33,
                                 0.75,0.99,2.31,1.34,0.67,
                                 0.47,1.19,1.42,1.54,0.59,
                                 0.48,0.84,1.08,0.92,0.91),
                          nrow=5,
                          byrow=TRUE)
  rownames(contact_matrix) = c("0-4", "5-14", "15-34", "35-54", "55+")
  colnames(contact_matrix) = rownames(contact_matrix)
  return(contact_matrix)
}