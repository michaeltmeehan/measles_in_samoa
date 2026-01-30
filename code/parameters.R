
## -----------------------------------------------------------------------------
# parameters.R
#
# Description:
#   Centralised parameter definitions for the measles modelling code used in the
#   Samoa project. This file contains default estimates for R0, vaccine-related
#   parameters, burden assumptions (IFR by age group), and forecast scenarios.
#
# Usage:
#   Source this file at the start of analysis scripts to import the parameter
#   objects: r0_estimates, vaccine_pars, delta, burden_pars, forecast_pars.
#
# Objects provided:
#   - r0_estimates: data.frame with r0_min, r0_max and plotting y-limits (ymin/ymax)
#   - vaccine_pars: list of vaccine and maternal immunity parameters
#   - delta: scalar; relative uncertainty in under-5 immunity
#   - burden_pars: list containing infection fatality ratios (IFR) by age group
#   - forecast_pars: list of named scenarios with assumed routine coverage
#
# Notes:
#   - Values here are project defaults and may be overridden by scripts that
#     source this file. Keep this file under version control and update the
#     header when parameters are changed.
## -----------------------------------------------------------------------------

# References:
#   Anderson, R.M., May, R.M.: Directly transmitted infections diseases: Control
#   by vaccination. Science 215(4536), 1053–1060 (1982)
#   https://doi.org/10.1126/science.7063839
#   https://www.science.org/doi/pdf/10.1126/science.7063839
#
#   Anderson, R.M., May, R.M.: Age-related changes in the rate of disease transmis-
#   sion: implications for the design of vaccination programmes. Journal of Hygiene
#   94(3), 365–436 (1985) https://doi.org/10.1017/S002217240006160X


r0_estimates = data.frame(
  r0_min = 12,
  r0_max = 18,
  ymin = -Inf,
  ymax = Inf
)



#   Uzicanin, A., Zimmerman, L.: Field effectiveness of live attenuated measles-
#   containing vaccines: A review of published literature. The Journal of Infectious
#   Diseases 204(1), 133–149 (2011) https://doi.org/10.1093/infdis/jir102

vaccine_pars = list(
  maternal_immunity = 0.5 * 0.5, # Infants afforded 50% protection for 6 months
  mcv1_age = 0.75, # 9 months
  mcv1_efficacy = 0.84,
  mcv2_age = 1.25, # 15 months
  mcv2_efficacy = 0.925
)


# Relative uncertainty in under 5 immunity
delta = 0.05 


burden_pars = list(
  ifr = c("0-4" = 0.0075, "5-14" = 0.004, "15-34" = 0.0016, "35-54" = 0.0016, "55+" = 0.0016)
)


forecast_pars = list(
  "improved" = c("mcv1" = 0.95, "mcv2" = 0.8),
  "reduced" = c("mcv1" = 0.65, "mcv2" = 0.4)
)


