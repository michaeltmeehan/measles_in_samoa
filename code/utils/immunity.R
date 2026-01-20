
#' immunity.R
#'
#' Utility functions for importing seropositivity and estimating
#' measles immunity from vaccination and serological data.
#'
#' These functions are simple helpers used by the analysis pipeline to
#' read grouped seropositivity, sample sizes, look up historical
#' vaccination coverage, estimate vaccine-derived immunity by age,
#' combine with serological data into age-groups, and simulate
#' observational draws.
#'

#' Import seropositivity proportions grouped by age for each year.
#'
#' Reads ./data/seropositivity_grouped.csv and returns a named list
#' where each element is a numeric vector of seropositivity
#' proportions for age groups "5-14", "15-34", "35-54", "55+" for
#' each observed year.
#'
#' @return A list indexed by year (as character) containing numeric
#'   vectors of length 4 with names c("5-14", "15-34", "35-54",
#'   "55+").
import_seropositivity = function(){
  seropositivity_data = read.csv("./data/seropositivity_grouped.csv")
  seropositivity = list()
  years = unique(seropositivity_data$year)
  for (yr in years){
    data_year = seropositivity_data %>% filter(year == yr)
    seropositivity[[as.character(yr)]] = with(data_year, positive / n)
    names(seropositivity[[as.character(yr)]]) = c("5-14", "15-34", "35-54", "55+")
  }
  return(seropositivity) 
}


#' Import grouped sample sizes for seropositivity data.
#'
#' Reads ./data/seropositivity_grouped.csv and returns a list of
#' integer vectors giving the sample sizes (n) for the age groups
#' c("5-14", "15-34", "35-54", "55+") for each year present in
#' the file.
#'
#' @return A list indexed by year (as character) containing integer
#'   vectors of length 4 with names c("5-14", "15-34", "35-54",
#'   "55+").
import_sample_size = function(){
  seropositivity_data = read.csv("./data/seropositivity_grouped.csv")
  sample_size = list()
  years = unique(seropositivity_data$year)
  for (yr in years){
    data_year = seropositivity_data %>% filter(year == yr)
    sample_size[[as.character(yr)]] = with(data_year, n)
    names(sample_size[[as.character(yr)]]) = c("5-14", "15-34", "35-54", "55+")
  }
  return(sample_size)
}


#' Lookup historical vaccine coverage for a birth cohort.
#'
#' Given the target assessment year and the age of the cohort, this
#' function returns the MCV1 and MCV2 coverage values applicable to
#' that birth cohort from a vaccine_coverage data.frame with columns
#' 'year', 'mcv1', and 'mcv2'.
#'
#' @param year Integer year of assessment.
#' @param age Integer age of the cohort in years.
#' @param vaccine_coverage data frame containing columns 'year',
#'   'mcv1' and 'mcv2'.
#' @return A named numeric vector c(mcv1=..., mcv2=...).
lookup_coverage = function(year, age, vaccine_coverage){
  mcv1_coverage = vaccine_coverage$mcv1[which(vaccine_coverage$year == year - age)]
  mcv2_coverage = vaccine_coverage$mcv2[which(vaccine_coverage$year == year - age + 1)]
  return(c("mcv1" = mcv1_coverage, "mcv2" = mcv2_coverage))
}


#' Estimate vaccine-derived immunity for infants under 1 year.
#'
#' Combines residual maternal immunity with expected protection from
#' MCV1 given the proportion vaccinated and the timing/efficacy
#' parameters.
#'
#' @param mcv1 Numeric coverage for MCV1 in newborn cohort.
#' @param vaccine_pars List containing maternal_immunity, mcv1_age,
#'   and mcv1_efficacy.
#' @return Numeric expected immunity proportion for age 0 (newborns).
estimate_vacc_immunity_under1 = function(mcv1, vaccine_pars){
  maternal_immunity = vaccine_pars$maternal_immunity
  mcv1_age = vaccine_pars$mcv1_age
  mcv1_efficacy = vaccine_pars$mcv1_efficacy
  
  # Newborns 0-1 year-olds: maternal antibodies + MCV1
  immunity = maternal_immunity + (1 - mcv1_age) * mcv1_efficacy * mcv1
  
  return(immunity)
}


#' Estimate vaccine-derived immunity for 1-2 year-olds.
#'
#' Accounts for partial exposure to MCV1 and the scheduled MCV2 at
#' mcv2_age (e.g. 15 months) using provided efficacy parameters.
#'
#' @param coverage Named numeric vector with elements 'mcv1' and
#'   'mcv2' giving coverage applicable to the cohort.
#' @param vaccine_pars List containing mcv1_age, mcv2_age,
#'   mcv1_efficacy, and mcv2_efficacy (maternal_immunity present but
#'   not used here except implicitly).
#' @return Numeric expected immunity proportion for ages 1-2.
estimate_vacc_immunity1to2 = function(coverage, vaccine_pars){
  maternal_immunity = vaccine_pars$maternal_immunity
  mcv1_age = vaccine_pars$mcv1_age
  mcv2_age = vaccine_pars$mcv2_age
  mcv1_efficacy = vaccine_pars$mcv1_efficacy
  mcv2_efficacy = vaccine_pars$mcv2_efficacy
  
  # 1-2 year-olds: MCV1 + MCV2 (at 15 months)
  immunity = (mcv2_age - 1) * mcv1_efficacy * coverage["mcv1"] +
    (2 - mcv2_age) * (1 - (1 - mcv1_efficacy * coverage["mcv1"]) * (1 - mcv2_efficacy * coverage["mcv2"]))
  
  return(immunity)
}

#' Estimate vaccine-derived immunity for ages over 2 years.
#'
#' Assumes independent contributions from MCV1 and MCV2 given their
#' efficacies and coverage; returns the complement of the product of
#' remaining susceptibility.
#'
#' @param coverage Named numeric vector c(mcv1=..., mcv2=...).
#' @param vaccine_pars List containing mcv1_efficacy and mcv2_efficacy.
#' @return Numeric expected immunity proportion for ages >2.
estimate_vacc_immunity_over2 = function(coverage, vaccine_pars){
  return(1 - (1 - vaccine_pars$mcv1_efficacy * coverage["mcv1"]) * (1 - vaccine_pars$mcv2_efficacy * coverage["mcv2"]))
}

#' Estimate age-specific vaccine-derived immunity for a given year.
#'
#' Produces a numeric vector of immunity probabilities for ages from
#' 0 up to 4 + max(year - 2024, 0) using vaccine coverage and
#' parameter inputs. Newborns (0), 1-2 year olds and older ages are
#' handled by specialized helper functions above.
#'
#' @param year Integer year of assessment.
#' @param vaccine_coverage data frame containing yearly 'year', 'mcv1'
#'   and 'mcv2' columns used to lookup cohort coverage.
#' @param vaccine_pars List of vaccine timing/efficacy parameters.
#' @return Numeric vector of immunity proportions indexed by age from
#'   0 to the maximum age considered.
estimate_vacc_immunity = function(year, vaccine_coverage, vaccine_pars){

  ages = 0:(4 + max(year - 2024, 0))
  
  immunity = vector(mode="numeric", length=length(ages))
  
  # Newborns
  coverage = vaccine_coverage$mcv1[which(vaccine_coverage$year == year)]
  immunity[1] = estimate_vacc_immunity_under1(coverage, vaccine_pars)
  
  # 1-2 year-olds
  coverage = lookup_coverage(year, 1, vaccine_coverage)
  immunity[2] = estimate_vacc_immunity1to2(coverage, vaccine_pars)
  
  # Over 2 year-olds
  for (age in 2:(length(ages)-1)){
    coverage = lookup_coverage(year, age, vaccine_coverage)
    immunity[age + 1] = estimate_vacc_immunity_over2(coverage, vaccine_pars)
  }
  
  return(immunity)
}



#' Calculate combined immunity by age-groups.
#'
#' Uses vaccine-derived immunity for young ages and seropositivity for
#' older groups to return a named vector for age-groups: "0-4",
#' "5-14", and the remaining seropositivity groups ("15-34",
#' "35-54","55+").
#'
#' @param year Integer year of assessment.
#' @param vaccine_coverage data frame used by estimate_vacc_immunity.
#' @param vaccine_pars List of vaccine parameters.
#' @param seropositivity List as returned by import_seropositivity.
#' @return Named numeric vector giving immunity for "0-4",
#'   "5-14", "15-34", "35-54", and "55+".
calc_immunity = function(year, vaccine_coverage, vaccine_pars, seropositivity){
  vacc_immunity = estimate_vacc_immunity(year, vaccine_coverage, vaccine_pars)
  sero_immunity = unlist(seropositivity[[as.character(min(year, 2024))]])
  
  under5_immunity = mean(vacc_immunity[1:5])
  a5to14_immunity = mean(c(vacc_immunity[-(1:5)], rep(sero_immunity[1], 10 - (year - 2024))))
  
  return(c("0-4" = under5_immunity, "5-14" = a5to14_immunity, sero_immunity[2:4]))
}



#' Simulate observed immunity measurements.
#'
#' For age-group 0-4 this samples a continuous jittered value around
#' the provided immunity with relative bound delta. For the other
#' groups it draws binomial observations with sample size n and
#' returns proportions.
#'
#' @param immunity Numeric vector length 5 giving true immunity for
#'   groups "0-4", "5-14", "15-34", "35-54", "55+".
#' @param n Integer sample size used for binomial draws for groups
#'   2:5.
#' @param delta Numeric relative jitter for the first group (default
#'   0.2).
#' @return Numeric vector length 5 of simulated observed immunity
#'   proportions.
simulate_immunity = function(immunity, n, delta = 0.2){
  
  simulated_immunity = vector(mode="numeric", length=5)
  simulated_immunity[1] = runif(1, immunity[1] * (1 - delta), min(immunity[1] * (1 + delta), 1))
  simulated_immunity[2:5] = rbinom(n=4, size=n, prob=immunity[2:5]) / n
  return(simulated_immunity)
}
