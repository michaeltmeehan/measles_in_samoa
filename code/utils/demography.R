## demography.R
## Utility functions for demographic data used in the measles_in_samoa project
## Population counts are taken from the 2024 Samoan census.

#' Import population by age group (Samoa, 2021 census)
#' source: United Nations World Population Prospects 2024
#' Returns a named numeric vector of population counts by age group
#' as used in the analysis. Values originate from the 2021 Samoan census.
#'
#' @return Named numeric vector with age groups as names and counts as values.
#' @examples
#' import_population()
#' @export
import_population <- function() {
  population <- c(
    "0-4" = 29000,
    "5-14" = 53000,
    "15-34" = 63000,
    "35-54" = 42000,
    "55+" = 26000
  )
  return(population)
}