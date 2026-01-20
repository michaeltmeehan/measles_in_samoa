## demography.R
## Utility functions for demographic data used in the measles_in_samoa project
## Population counts are taken from the 2024 Samoan census.

#' Import population by age group (Samoa, 2024 census)
#'
#' Returns a named numeric vector of population counts by age group
#' as used in the analysis. Values originate from the 2024 Samoan census.
#'
#' @return Named numeric vector with age groups as names and counts as values.
#' @examples
#' import_population()
#' @export
import_population <- function() {
  population <- c(
    "0-4" = 33174,
    "5-14" = 50294,
    "15-34" = 57706,
    "35-54" = 40018,
    "55+" = 24160
  )
  return(population)
}