import_vaccine_coverage = function(){
  coverage = read.csv("./Data/samoan_vaccine_coverage.csv")
  coverage$mcv1 = coverage$mcv1 / 100
  coverage$mcv2 = coverage$mcv2 / 100
  return(coverage)
}


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
