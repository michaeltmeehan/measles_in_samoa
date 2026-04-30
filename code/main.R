# main.R
# Purpose: Run measles burden estimation and forecasting for Samoa.
# - Loads required packages, parameters, and utility functions.
# - Imports data (population, seropositivity, contact matrices, vaccine coverage).
# - Calculates burden across years and R0 values; visualizes results and forecasts scenarios.
# Usage: Run from the project root (here::here()); requires supporting scripts in ./code/.

set.seed(123)

#### Setup ####
if (!"here" %in% rownames(installed.packages())){
  install.packages("here")
}

# Set working directory to project root
# setwd(here::here())

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


#### Vaccine coverage ####
ggplot(vaccine_coverage, aes(x=year)) +
  geom_line(aes(y=mcv1, col="MCV1"), lwd=0.8) +
  geom_line(aes(y=mcv2, col="MCV2"), lwd=0.8) +
  labs(x = "Year", y = "Vaccine coverage", col = "Measles vaccine") +
  theme_minimal() +
  scale_color_npg() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1), breaks = seq(0, 1, by=0.2)) +
  custom_theme

ggsave("./figs/vaccine_coverage.png", width=6, height=3, dpi=600)

#### Transmission risk estimates (reff) ####
years = c(2018, 2019, 2023, 2024)
r0_range = c("min" = 0, "max" = 20)

out = expand.grid(Year = years, R0 = r0_range)

reff = data.frame(central = numeric(0), lower = numeric(0), upper = numeric(0))
for (i in seq_len(nrow(out))){
  immunity = calc_immunity(out$Year[i], vaccine_coverage, vaccine_pars, seropositivity)
  cat("immunity in under 5s: ", round(immunity[1], 2), " in year ", out$Year[i], "\n")
  reff = rbind(reff, bootstrap_reff(
    r0 = out$R0[i],
    contact_matrix = contact_matrix,
    population = population,
    immunity = immunity,
    replicates = 10000,
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
  scale_x_continuous(breaks=seq(0,max(r0_range),by=5)) +
  geom_hline(yintercept=1, linetype="dashed", color="black", lwd=0.8) +
  custom_theme

ggsave("./figs/reff_vs_r0.png", width=6, height=4, dpi=600)

# Scale for r0 = 12 and r0 = 18
round(cbind(Year = out[-(1:4), 1], out[-(1:4), -1] * 12 / max(r0_range)), 2)
round(cbind(Year = out[-(1:4), 1], out[-(1:4), -1] * 18 / max(r0_range)), 1)




#### Forecasting ####
years = 2026:2030
r0 = 15
scenarios = c("baseline", "as-is", "improved", "reduced")

set.seed(123)
forecasted_burden = lapply(years, function(year){
  
                    forecast_burden(year,
                                    scenarios,
                                    r0, 
                                    contact_matrix,
                                    population,
                                    vaccine_pars,
                                    seropositivity,
                                    sample_size,
                                    burden_pars,
                                    forecast_pars,
                                    replicates=10000, 
                                    delta=delta, 
                                    symmetrize=FALSE) %>%
                      # burden[2:4] %>% 
                        bind_rows(.id ="Metric") %>%
                      mutate(Metric = factor(Metric, levels=c("final_size", "infections", "deaths", "total_infections", "total_deaths"),
                                             labels=c("Final size", "Infections", "Deaths", "Total infections", "Total deaths")),
                             Scenario = factor(Scenario, 
                                               levels=c("baseline", "improved", "as-is", "reduced"),
                                               labels = c("Baseline (2024)", "Improved", "As-is", "Reduced")))
                    }
                  )
names(forecasted_burden) = years

forecasted_burden = forecasted_burden %>%
  bind_rows(.id = "Year")

forecasted_burden %>%
  filter(Scenario != "Baseline (2024)", Metric %in% c("Infections", "Deaths"), Year %in% c(2026, 2028, 2030)) %>%
  ggplot(aes(x=age_group, y=central, fill=factor(Scenario), col=factor(Scenario))) +
  geom_bar(stat="identity", position=position_dodge(), alpha=0.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(0.9), width=0.25, lwd=0.8) +
  facet_grid(Metric ~ Year, scales = "free", switch = "y") +
  theme_minimal() +
  scale_fill_npg() +
  scale_color_npg()  +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(
        limits = c(0, 1e4)
      ),
      scale_y_continuous(
        limits = c(0, 200)
      )
    )
  ) +
  labs(
    title = "",
    x = "Age (years)",
    y = "",
    color = "    Vaccination\ncoverage scenario",
    fill = "    Vaccination\ncoverage scenario"
  ) +
  custom_theme +
  theme(
    strip.placement = "outside",
    strip.background = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) 

ggsave("./figs/forecasted_infections_deaths.png", width=7, height=4, dpi=600)

forecasted_burden %>%
  filter(Scenario != "Baseline (2024)", Metric %in% c("Total infections", "Total deaths"), Year %in% c(2026, 2030)) %>%
  select(-age_group) %>%
  mutate(across(where(is.numeric), ~ round(.x)))



forecasted_burden %>%
  filter(Scenario != "Baseline (2024)", Metric %in% c("Infections", "Deaths"), Year %in% c(2026, 2030)) %>%
  group_by(Metric, Scenario, Year) %>%
  summarise(
    total_central = sum(central),
    child_central = sum(central[age_group %in% c("0-4")]),
    proportion = round(child_central / total_central * 100)
  ) %>%
  arrange(proportion)




### 2019 counterfactual (without preventive measures) ###
forecast_burden(2019,
                c("as-is"),
                r0, 
                contact_matrix,
                population,
                vaccine_pars,
                seropositivity,
                sample_size,
                burden_pars,
                forecast_pars,
                replicates=10000, 
                delta=delta, 
                symmetrize=FALSE)
