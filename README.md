
# measles_in_samoa

R scripts and utilities for estimating measles transmission risk, burden
and simple forecasts for Samoa. This project provides age-structured
transmission tools (next-generation matrices, effective R, final-size
calculations), bootstrapped uncertainty, and simple vaccine coverage
forecasting to produce infection and death burden estimates by age.

Contents
 - Project overview
 - Requirements
 - Repo layout
 - Data expectations / formats
 - Key functions and workflow
 - Usage examples
 - Notes and caveats


Project overview
---------------
This repository contains an R-based analytical pipeline to:
 - import demographic, serological and vaccination data for Samoa
 - estimate age-specific immunity (vaccine-derived + serology)
 - construct age-structured next-generation matrices and compute
	 effective reproduction numbers (R_eff)
 - solve multi-group final-size equations to estimate infections
	 and derive deaths using an input infection fatality ratio (IFR)
 - bootstrap the pipeline to provide uncertainty intervals
 - run simple vaccine coverage scenario forecasts and compare
	 resulting burden projections

Requirements
------------
The project relies on R (>= 3.6) and the packages listed in
code/required_packages.R. The script installs missing packages
automatically. Primary packages used include:
- tidyverse (dplyr, tidyr, purrr, readr)
- ggplot2
- ggsci
- here

Repository layout
-----------------
Key files and directories:

- code/main.R
	- Orchestrates the analysis: imports data, computes burden for
		a grid of years and R0 values, and produces figures and forecasts.
- code/required_packages.R
	- Installs and loads required R packages.
- code/parameters.R
	- Project parameters (vaccine timings, efficacies, IFRs, forecast
		parameters). This file is expected to be present in ./code/.
- code/utils/
	- mixing.R        : contact matrix provider (5x5 age groups)
	- immunity.R      : imports serology and computes vaccine-derived
											immunity & combined age-group immunity
	- transmission.R  : NGM construction, R_eff, final-size solver and
											bootstrap helpers
	- burden.R        : wrappers to compute infections/deaths and to
											run bootstrapped scenarios (burden_year_r0,
											forecast_burden)
	- vaccination.R   : import and simple forecasting of vaccine
											coverage (historical + scenarios)
	- plots.R         : shared ggplot2 theme and plotting helpers

Data expectations
-----------------
Place data in a ./data/ directory at the project root. The code
expects the following CSV files with these columns:

- ./data/seropositivity_grouped.csv
	- Columns: year, positive, n
	- For each year there should be four rows corresponding to age
		groups in this order: 5-14, 15-34, 35-54, 55+. The import helpers
		map those four rows to the named vector c("5-14","15-34",
		"35-54","55+").

- ./data/samoan_vaccine_coverage.csv
	- Columns: year, mcv1, mcv2
	- mcv1 and mcv2 are percentages (0-100). The importer converts
		them to proportions (0-1).

Parameters
----------
The file code/parameters.R is used by main.R and the utils. Typical
contents include vaccine timing/efficacy (mcv1_age, mcv2_age,
mcv1_efficacy, mcv2_efficacy, maternal_immunity), IFR values by age,
and forecast_pars used by vaccination.R for "improved"/"reduced"
scenarios.

Key functions and workflow
--------------------------
Most core functionality is implemented in code/utils/:

- mixing.R
	- import_contact_matrix(): returns a 5x5 contact matrix with age
		groups "0-4","5-14","15-34","35-54","55+".

- immunity.R
	- import_seropositivity(), import_sample_size(): read grouped
		serology proportions and sample sizes.
	- estimate_vacc_immunity(): builds age-specific vaccine-derived
		immunity for young ages using historical coverage.
	- calc_immunity(year, vaccine_coverage, vaccine_pars, seropositivity)
		returns a named vector of immunity for age-groups: "0-4",
		"5-14","15-34","35-54","55+." This is used as input to the
		transmission functions.
	- simulate_immunity(): used to perturb immunity for bootstrapping.

- transmission.R
	- make_NGM(), calc_reff(), calc_final_size(), final_size_solver():
		build the next-generation matrix (scaled to R0), compute R_eff
		and solve final-size equations z = 1 - exp(-A z).
	- bootstrap_reff(), bootstrap_final_size(): bootstrap R_eff and
		final-size estimates by perturbing immunity.

- burden.R
	- calc_burden(): high-level wrapper returning reff, final_size,
		infections and deaths for a single scenario.
	- bootstrap_burden(): generates central and bootstrapped
		uncertainty intervals for reff, final_size, infections and deaths
		for a given year and R0.
	- burden_year_r0(): run bootstrap_burden() across a grid of years
		and R0 values and combine results.
	- forecast_burden(): generate vaccine coverage forecasts for each
		scenario and compute burden across scenarios.

Usage examples
--------------
1) From an interactive R session (project root):

	 source("./code/required_packages.R")
	 source("./code/parameters.R")
	 source("./code/main.R")

	 The main.R script sources utilities, imports data and runs the
	 default analysis (see the top of main.R for the configured years,
	 R0 grid, forecasting year and scenarios).

2) Run a specific function from R, e.g. compute burden across years:

	 library(here) # main.R uses here::here() to set the working directory
	 source("./code/required_packages.R")
	 source("./code/parameters.R")
	 sourceDir <- list.files("./code/utils", pattern="\\.R$", full.names=TRUE)
	 invisible(lapply(sourceDir, source))

	 years = c(2018, 2019, 2023, 2024)
	 r0_values = c(12,15,18,20)
	 burden = burden_year_r0(years, r0_values, import_contact_matrix(), 
													 import_population(), import_vaccine_coverage(), vaccine_pars,
													 import_seropositivity(), import_sample_size(), burden_pars,
													 replicates=1000, delta=0.1)

Notes and caveats
-----------------
- The code assumes specific row ordering and counts in the seropositivity
	CSV (4 rows per year for age groups 5-14, 15-34, 35-54, 55+).
- forecast_vaccine_coverage() implements simple deterministic
	scenario forecasts (2025-2030). Review code/ vaccination.R if you
	require more sophisticated forecasting.
- The IFRs, vaccine efficacy/timing and other model parameters are
	defined in code/parameters.R and should be reviewed carefully before
	reuse or publication.

Contact
-------
Repository: https://github.com/michaeltmeehan/measles_in_samoa

If you need assistance adapting the pipeline for other settings or
data, inspect the helpers in code/utils/ and adjust parameters and
data formats accordingly.

