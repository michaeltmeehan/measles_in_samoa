r0_estimates = data.frame(
  r0_min = 12,
  r0_max = 18,
  ymin = -Inf,
  ymax = Inf
)


vaccine_pars = list(
  maternal_immunity = 0.5 * 0.5, # Infants afforded 50% protection for 6 months
  mcv1_age = 0.75, # 9 months
  mcv1_efficacy = 0.84,
  mcv2_age = 1.25, # 15 months
  mcv2_efficacy = 0.925
)


# Relative uncertainty in under 5 immunity
delta = 0.1 


burden_pars = list(
  ifr = c("0-4" = 0.0075, "5-14" = 0.004, "15-34" = 0.0016, "35-54" = 0.0016, "55+" = 0.0016)
)


forecast_pars = list(
  "improved" = c("mcv1" = 0.95, "mcv2" = 0.8),
  "reduced" = c("mcv1" = 0.65, "mcv2" = 0.4)
)
