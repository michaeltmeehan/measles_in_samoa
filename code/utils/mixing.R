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