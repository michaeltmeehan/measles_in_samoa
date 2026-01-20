import_seropositivity = function(){
  seropositivity_data = read.csv("./Data/seropositivity_grouped.csv")
  seropositivity = list()
  years = unique(seropositivity_data$year)
  for (yr in years){
    data_year = seropositivity_data %>% filter(year == yr)
    seropositivity[[as.character(yr)]] = with(data_year, positive / n)
    names(seropositivity[[as.character(yr)]]) = c("5-14", "15-34", "35-54", "55+")
  }
  return(seropositivity) 
}


import_sample_size = function(){
  seropositivity_data = read.csv("./Data/seropositivity_grouped.csv")
  sample_size = list()
  years = unique(seropositivity_data$year)
  for (yr in years){
    data_year = seropositivity_data %>% filter(year == yr)
    sample_size[[as.character(yr)]] = with(data_year, n)
    names(sample_size[[as.character(yr)]]) = c("5-14", "15-34", "35-54", "55+")
  }
  return(sample_size)
}


lookup_coverage = function(year, age, vaccine_coverage){
  mcv1_coverage = vaccine_coverage$mcv1[which(vaccine_coverage$year == year - age)]
  mcv2_coverage = vaccine_coverage$mcv2[which(vaccine_coverage$year == year - age + 1)]
  return(c("mcv1" = mcv1_coverage, "mcv2" = mcv2_coverage))
}


estimate_vacc_immunity_under1 = function(mcv1, vaccine_pars){
  maternal_immunity = vaccine_pars$maternal_immunity
  mcv1_age = vaccine_pars$mcv1_age
  mcv1_efficacy = vaccine_pars$mcv1_efficacy
  
  # Newborns 0-1 year-olds: maternal antibodies + MCV1
  immunity = maternal_immunity + (1 - mcv1_age) * mcv1_efficacy * mcv1
  
  return(immunity)
}


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

estimate_vacc_immunity_over2 = function(coverage, vaccine_pars){
  return(1 - (1 - vaccine_pars$mcv1_efficacy * coverage["mcv1"]) * (1 - vaccine_pars$mcv2_efficacy * coverage["mcv2"]))
}

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



calc_immunity = function(year, vaccine_coverage, vaccine_pars, seropositivity){
  vacc_immunity = estimate_vacc_immunity(year, vaccine_coverage, vaccine_pars)
  sero_immunity = unlist(seropositivity[[as.character(min(year, 2024))]])
  
  under5_immunity = mean(vacc_immunity[1:5])
  a5to14_immunity = mean(c(vacc_immunity[-(1:5)], rep(sero_immunity[1], 10 - (year - 2024))))
  
  return(c("0-4" = under5_immunity, "5-14" = a5to14_immunity, sero_immunity[2:4]))
}



simulate_immunity = function(immunity, n, delta = 0.2){
  
  simulated_immunity = vector(mode="numeric", length=5)
  simulated_immunity[1] = runif(1, immunity[1] * (1 - delta), min(immunity[1] * (1 + delta), 1))
  simulated_immunity[2:5] = rbinom(n=4, size=n, prob=immunity[2:5]) / n
  return(simulated_immunity)
}
