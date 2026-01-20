# main.R
# Purpose: Run measles burden estimation and forecasting for Samoa.
# - Loads required packages, parameters, and utility functions.
# - Imports data (population, seropositivity, contact matrices, vaccine coverage).
# - Calculates burden across years and R0 values; visualizes results and forecasts scenarios.
# Usage: Run from the project root (here::here()); requires supporting scripts in ./code/.

# Set working directory to project root
setwd(here::here())

# Install and load required packages
source("./code/required_packages.R")

# Source parameters
source("./code/parameters.R")

# Source utilities
file_list = list.files(path = "./code/utils/", pattern = "*.R", full.names = TRUE)
invisible(lapply(file_list, source))



#### Data imports ####
population = import_population()
seropositivity = import_seropositivity()
contact_matrix = import_contact_matrix()
vaccine_coverage = import_vaccine_coverage()
sample_size = import_sample_size()

# forecasted_coverage = forecast_vaccine_coverage(scenario = "as-is")




#### Main calculation loop ####
years = c(2018, 2019, 2023, 2024)
r0_values = c(12, 15, 18, 20)

burden = burden_year_r0(years,
                     r0_values, 
                     contact_matrix,
                     population, 
                     vaccine_coverage,
                     vaccine_pars,
                     seropositivity,
                     sample_size,
                     burden_pars, 
                     replicates=10, 
                     delta=delta, 
                     symmetrize=FALSE)


# Add r0 = 0 results to reff dataframe for plotting
reff_r0_0 = expand_grid(
  Year = years,
  R0 = 0
) %>%
  mutate(
    central = 0,
    lower = 0,
    upper = 0
  )

rbind(
  reff_r0_0,
  burden$reff %>% select(Year, R0, central, lower, upper)
) %>% arrange(Year) -> reff_results


#### Visualize results ####
reff_results %>%
  dplyr::filter(R0 %in% c(0,max(r0_values))) %>%
  ggplot() +
  geom_line(aes(x=R0, y=central, col=factor(Year), group=Year), lwd=0.8) +
  geom_ribbon(aes(x=R0, y=central, fill=factor(Year), ymin=lower, ymax=upper), alpha=0.15) +
  geom_rect(data=r0_estimates, aes(xmin=r0_min, xmax=r0_max, ymin=ymin, ymax=ymax), alpha=0.2, fill="grey") +
  scale_y_continuous(limits=c(0, 12.01), breaks=seq(0, 12, by=2)) +
  labs(x = expression(paste("Basic reproduction number, ", R[0])),
       y = expression(paste("Effective reproduction number, ", R[e], sep=" ")),
       col = "Year",
       fill = "Year"
  ) +
  theme_minimal() +
  scale_color_npg() +
  scale_fill_npg() +
  scale_x_continuous(breaks=seq(0,max(r0_values),by=5)) +
  geom_hline(yintercept=1, linetype="dashed", color="black", lwd=0.8) +
  custom_theme



#### Forecasting ####
year = 2030
r0 = 15
scenarios = c("as-is", "improved", "reduced")

forecasted_burden = forecast_burden(year,
                                    scenarios,
                                    r0, 
                                    contact_matrix,
                                    population,
                                    vaccine_pars,
                                    seropositivity,
                                    sample_size,
                                    burden_pars,
                                    forecast_pars,
                                    replicates=1000, 
                                    delta=0.1, 
                                    symmetrize=FALSE)

burden[-c(1,2)] %>%
  bind_rows(.id = "Metric") %>%
  filter(Year == 2024, R0 == 15) %>%
  mutate(Scenario = Year) %>%
  select(Metric, central, lower, upper, age_group, Scenario) %>%
  rbind(
    forecasted_burden[-c(1,2)] %>%
  bind_rows(.id = "Metric")
  ) %>%
  ggplot(aes(x=age_group, y=central, fill=factor(Scenario), col=factor(Scenario))) +
  geom_bar(stat="identity", position=position_dodge(), alpha=0.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(0.9), width=0.25, lwd=0.8) +
  facet_grid(Metric ~ ., scales = "free") +
  theme_minimal() +
  scale_fill_lancet() +
  scale_color_lancet()  +
  labs(
    title = "",
    x = "Age (years)",
    y = "Burden",
    color = "Scenario",
    fill = "Scenario"
  ) +
  custom_theme

