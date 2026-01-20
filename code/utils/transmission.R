
## transmission.R
## Utilities for transmission modelling: next-generation matrices (NGM), effective reproduction number,
## final-size calculation and simple bootstrap routines.

#' Validate inputs for transmission calculations
#'
#' Ensure r0, contact matrix, population and immunity vectors are of correct
#' type and length; normalize population if provided.
#'
#' @param r0 numeric, basic reproduction number (positive)
#' @param contact_matrix square numeric matrix or NULL
#' @param population numeric vector of group populations or NULL
#' @param immunity numeric vector of immunity proportions (0-1) or NULL
#' @param symmetrize logical (currently unused)
#' @return A list with validated contact_matrix, population (normalized) and immunity
validate_inputs_ = function(r0, contact_matrix, population, immunity, symmetrize){
  # Check r0 is numeric and positive
  if(!is.numeric(r0) || r0 <= 0){
    stop("r0 must be a positive numeric value.")
  }
  # Check contact_matrix is a square matrix
  if(!is.null(contact_matrix)){
    if(nrow(contact_matrix) != ncol(contact_matrix)){
      stop("contact_matrix must be a square matrix.")
    }
    }else{
      contact_matrix = 1
    }
  n = nrow(contact_matrix)
  
  # Check population is numeric vector of correct length
  if(!is.null(population)){
    if(!is.numeric(population) || length(population) != n || any(population < 0)){
      stop("population must be a numeric vector of length equal to the dimensions of contact_matrix.")
    }else{
      population = population / sum(population)  # Normalize population
    }
  }else{
    population = rep(1 / n, n)  # Default to vector of ones
  }
  
  # Check immunity is numeric vector of correct length
  if(!is.null(immunity)){
    if(!is.numeric(immunity) || length(immunity) != n || any(immunity < 0) || any(immunity > 1)){
      stop("immunity must be a numeric vector of length equal to the dimensions of contact_matrix.")
    }
  }else{
    immunity = rep(0, n)  # Default to vector of zeroes
  }
  
  return(list(contact_matrix = contact_matrix,
              population = population,
              immunity = immunity))
}



#' Calculate scaling factor to match a target R0
#'
#' Compute q such that the dominant eigenvalue of the unscaled next-generation
#' matrix equals r0.
#'
#' @param r0 numeric, target basic reproduction number
#' @param contact_matrix square contact matrix
#' @param population numeric vector of group population shares (sum to 1)
#' @return numeric scalar q
calc_scale_factor_ = function(r0, contact_matrix, population){
  NGM_unscaled = diag(population) %*% contact_matrix %*% diag(1 / population)
  q = r0 / max(Re(eigen(NGM_unscaled, only.values=TRUE)$values))
  return(q)
}



#' Construct the effective next-generation matrix (NGM)
#'
#' Build the NGM scaled to a target r0 and accounting for group-specific
#' immunity and population structure.
#'
#' @param r0 numeric, basic reproduction number
#' @param contact_matrix square contact matrix or NULL
#' @param population numeric vector of group population shares or NULL
#' @param immunity numeric vector of immunity proportions or NULL
#' @param symmetrize logical (currently unused)
#' @return matrix, effective next-generation matrix
make_NGM = function(r0, 
                    contact_matrix = NULL, 
                    population = NULL, 
                    immunity = NULL,
                    symmetrize = FALSE){
  
  inputs = validate_inputs_(r0, contact_matrix, population, immunity, symmetrize)
  contact_matrix = inputs$contact_matrix
  population = inputs$population
  immunity = inputs$immunity
  
  q = calc_scale_factor_(r0, contact_matrix, population)
  
  NGM_eff = q * diag((1 - immunity) * population) %*% contact_matrix %*% diag(1 / population)
  
  return(NGM_eff)
}


#' Compute effective reproduction number R_eff
#'
#' Returns the dominant eigenvalue of the effective NGM.
#'
#' @inheritParams make_NGM
#' @return numeric, effective reproduction number
calc_reff = function(r0, 
                     contact_matrix = NULL, 
                     population = NULL, 
                     immunity = NULL,
                     symmetrize = FALSE){
  
  NGM_eff = make_NGM(r0 = r0,
                     contact_matrix = contact_matrix,
                     population = population,
                     immunity = immunity,
                     symmetrize = symmetrize)
  
  reff = max(Re(eigen(NGM_eff, only.values=TRUE)$values))
  return(reff)
}


#' Bootstrap R_eff to obtain uncertainty intervals
#'
#' Uses simulate_immunity() to perturb immunity and recomputes R_eff.
#'
#' @param replicates integer number of bootstrap replicates
#' @param delta numeric, parameter passed to simulate_immunity
#' @inheritParams make_NGM
#' @return named numeric vector with central, lower and upper bounds
bootstrap_reff = function(r0,
                          contact_matrix=NULL,
                          population=NULL,
                          immunity=NULL,
                          symmetrize=FALSE,
                          replicates=1000,
                          delta=0.2){
  
  central = calc_reff(r0,
                     contact_matrix=contact_matrix,
                     population=population,
                     immunity=immunity,
                     symmetrize=symmetrize)
  simulated_reff = replicate(replicates, {
    simulated_immunity = simulate_immunity(immunity, delta)
    calc_reff(r0,
              contact_matrix=contact_matrix,
              population=population,
              immunity=simulated_immunity,
              symmetrize=symmetrize)
  })
  ci = unname(quantile(simulated_reff, c(0.025, 0.975)))
  return(c(central = central, lower = ci[1], upper = ci[2]))
}


#' Calculate final outbreak size by age group
#'
#' Solve the final-size equations for a multi-group model using a fixed-point
#' iteration (final_size_solver).
#'
#' @inheritParams make_NGM
#' @return numeric vector of final sizes (proportion infected) with age-group names
calc_final_size = function(r0, 
                           contact_matrix = NULL, 
                           population = NULL, 
                           immunity = NULL,
                           symmetrize = FALSE){
  
  inputs = validate_inputs_(r0, contact_matrix, population, immunity, symmetrize)
  contact_matrix = inputs$contact_matrix
  population = inputs$population
  immunity = inputs$immunity
  
  q = calc_scale_factor_(r0, contact_matrix, population)
  
  A = q * contact_matrix %*% diag(1 - immunity)
  
  final_size = final_size_solver(A=A)
  names(final_size) = c("0-4", "5-14", "15-34", "35-54", "55+")
  return(final_size)
}



#' Solve final-size equations via fixed-point iteration
#'
#' Solve z = 1 - exp(-A %*% z) for z using simple fixed-point iteration.
#'
#' @param A numeric matrix in final-size equations
#' @param z0 optional numeric initial guess
#' @param tol numeric convergence tolerance
#' @param maxit integer maximum iterations
#' @return numeric vector z of final sizes (proportion infected)
final_size_solver <- function(A, 
                              z0 = NULL,
                              tol = 1e-10, 
                              maxit = 1e5) {
  
  # Initial guess (small outbreak in all groups)
  if (is.null(z0)) {
    z <- rep(1e-3, nrow(A))
  } else {
    z <- as.numeric(z0)
  }
  
  for (k in seq_len(maxit)) {
    z_new  <- 1 - exp(-as.vector(A %*% z))   # fixed-point map
    
    # Convergence check
    if (max(abs(z_new - z)) < tol) {
      return(z_new)
    }
    
    z <- z_new
  }
  
  warning("final_size_fixed_point: did not converge within maxit iterations.")
  z
}


#' Bootstrap final-size estimates
#'
#' Perturb immunity via simulate_immunity and recompute final sizes to obtain
#' uncertainty intervals.
#'
#' @inheritParams bootstrap_reff
#' @return list with central, lower and upper final-size vectors
bootstrap_final_size = function(r0,
                                 contact_matrix=NULL,
                                 population=NULL,
                                 immunity=NULL,
                                 symmetrize=FALSE,
                                 replicates=1000,
                                 delta=0.2){
  
  central = calc_final_size(r0,
                            contact_matrix=contact_matrix,
                            population=population,
                            immunity=immunity,
                            symmetrize=symmetrize)
  sim_final_size = replicate(replicates, {
    simulated_immunity = simulate_immunity(immunity, delta)
    calc_final_size(r0,
                    contact_matrix=contact_matrix,
                    population=population,
                    immunity=simulated_immunity,
                    symmetrize=symmetrize)
  })
  ci_lower = apply(sim_final_size, 1, function(x) unname(quantile(x, 0.025)))
  ci_upper = apply(sim_final_size, 1, function(x) unname(quantile(x, 0.975)))
  return(list(central = central, lower = ci_lower, upper = ci_upper))
}