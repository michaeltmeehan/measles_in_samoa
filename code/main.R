# main.R
# Purpose: Run measles burden estimation and forecasting for Samoa.
# - Loads required packages, parameters, and utility functions.
# - Imports data (population, seropositivity, contact matrices, vaccine coverage).
# - Calculates burden across years and R0 values; visualizes results and forecasts scenarios.
# Usage: Run from the project root (here::here()); requires supporting scripts in ./code/.

#### Setup ####
if (!"here" %in% rownames(installed.packages())){
  install.packages("here")
}

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



#### Transmission risk estimates (reff) ####
years = c(2018, 2019, 2023, 2024)
r0_range = c("min" = 0, "max" = 20)

out = expand.grid(Year = years, R0 = r0_range)

reff = data.frame(central = numeric(0), lower = numeric(0), upper = numeric(0))
for (i in seq_len(nrow(out))){
  immunity = calc_immunity(out$Year[i], vaccine_coverage, vaccine_pars, seropositivity)
  reff = rbind(reff, bootstrap_reff(
    r0 = out$R0[i],
    contact_matrix = contact_matrix,
    population = population,
    immunity = immunity,
    replicates = 1000,
    n = sample_size[[as.character(out$Year[i])]],
    delta = delta
    ) %>% as.list() %>% as.data.frame()
  )
}

out = cbind(out, reff)


ggplot(out) +
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

ggsave("./figs/reff_vs_r0.png", width=6, height=4, dpi=600)

# Scale for r0 = 12 and r0 = 18
cbind(Year = out[-(1:4), 1], out[-(1:4), -1] * 12 / max(r0_range))
cbind(Year = out[-(1:4), 1], out[-(1:4), -1] * 18 / max(r0_range))




#### Forecasting ####
year = 2030
r0 = 15
scenarios = c("baseline", "as-is", "improved", "reduced")

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
                                    delta=delta, 
                                    symmetrize=FALSE)

forecasted_burden[2:4] %>%
  bind_rows(.id = "Metric") %>%
  mutate(Metric = factor(Metric, levels=c("final_size", "infections", "deaths"),
                         labels=c("Final size", "Infections", "Deaths")),
         Scenario = factor(Scenario, 
                           levels=c("baseline", "as-is", "improved", "reduced"),
                           labels = c("Baseline (2024)", "As-is", "Improved", "Reduced"))) %>%
  ggplot(aes(x=age_group, y=central, fill=factor(Scenario), col=factor(Scenario))) +
  geom_bar(stat="identity", position=position_dodge(), alpha=0.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(0.9), width=0.25, lwd=0.8) +
  facet_grid(Metric ~ ., scales = "free") +
  theme_minimal() +
  scale_fill_npg() +
  scale_color_npg()  +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(
        labels = percent_format(accuracy = 1),
        limits = c(0, 1)
      ),
      scale_y_continuous(
        limits = c(0, 8e3)
      ),
      scale_y_continuous(
        limits = c(0, 60)
      )
    )
  ) +
  labs(
    title = "",
    x = "Age (years)",
    y = "",
    color = "Scenario",
    fill = "Scenario"
  ) +
  custom_theme

ggsave("./figs/forecasted_burden.png", width=6, height=7, dpi=600)
