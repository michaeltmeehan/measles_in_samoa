
#' vaccination.R
#'
#' Utility functions for importing and forecasting measles vaccine coverage
#' for the Samoa project.
#'
#' The file contains helpers to read historical coverage from
#' ./data/samoan_vaccine_coverage.csv and to produce simple scenario-driven
#' forecasts for MCV1/MCV2 coverage.


#' Import vaccine coverage
#'
#' Read historical vaccine coverage from ./data/samoan_vaccine_coverage.csv.
#' The CSV is expected to contain columns 'year', 'mcv1' and 'mcv2' where
#' mcv1/mcv2 are percentages (0-100). This function converts those to
#' proportions (0-1).
#'
#' @return A data.frame with columns year, mcv1, mcv2 where mcv1/mcv2 are in [0,1].
import_vaccine_coverage = function(){
  coverage = read.csv("./data/samoan_vaccine_coverage.csv")
  coverage$mcv1 = coverage$mcv1 / 100
  coverage$mcv2 = coverage$mcv2 / 100
  return(coverage)
}


#' Forecast vaccine coverage
#'
#' Create a simple forecast of vaccine coverage for years 2025-2030 based on a
#' scenario. Supported scenarios:
#'  - "as-is": uses the most recent observed coverage
#'  - "improved": uses values provided in forecast_pars[["improved"]]
#'  - "reduced": uses values provided in forecast_pars[["reduced"]]
#'
#' @param scenario Character. One of "as-is", "improved", or "reduced".
#' @param forecast_pars A named list containing scenario parameter vectors
#'   with elements "mcv1" and "mcv2" (proportions, 0-1). Required for
#'   "improved" and "reduced" scenarios.
#' @return A data.frame with historical rows (from import_vaccine_coverage)
#'   and additional forecast rows for years 2025-2030. The data.frame is
#'   arranged in descending order by year.
forecast_vaccine_coverage = function(scenario="as-is", forecast_pars){
  coverage = import_vaccine_coverage()
  if(scenario == "as-is"){
  forecast_coverage = head(coverage[order(coverage$year, decreasing=TRUE), ], 1)[,-1]
  }else if(scenario == "improved"){
    forecast_coverage = data.frame(mcv1 = forecast_pars[[scenario]]["mcv1"], mcv2 = forecast_pars[[scenario]]["mcv2"])
  }else if(scenario == "reduced"){
    forecast_coverage = data.frame(mcv1 = forecast_pars[[scenario]]["mcv1"], mcv2 = forecast_pars[[scenario]]["mcv1"])
    }else{
      stop("Unknown scenario")
    }
  years = 2025:2030
  out = data.frame(year = years,
                        mcv1 = rep(forecast_coverage$mcv1, length(years)),
                        mcv2 = rep(forecast_coverage$mcv2, length(years))) %>%
    rbind(coverage) %>%
    arrange(desc(year))
  return(out)
}
