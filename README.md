# Code and Data Repository for "Statistically Validated Indices for COVID-19 Public Health Policies"

This repository hosts code and data necessary to reproduce the results in:

Kubinec et al. "Statistically Validated Indices for COVID-19 Public Health Policies."

The R code files are as follows:

-   `ag_dataset.R`: Loads the original CoronaNet and OxCGRT data, creates a wide version of the CoronaNet dataset and merges in the OxCGRT indicators. Creates one dataset for each index type.

-   `recode_city.R`: A script which cleans city names in CoronaNet to merge with with a population database.

-   `miss_province_pop.R`: A script which cleans province names in CoronaNet to merge them with population data.

-   `create_times_long.R`: A script which chooses the indicators that go in each index.

-   `run_indices.R` and `run_indices_aws.R`: Load the data files and estimate an index, save the result in `coronanet/`.

-   `analyze_indices.R`: load the index model objects in `coronanet/` and create diagnostic and descriptive plots saved in `plots/`. Save the estimated country index scores in `indices/`.

-   `inference.R`: given the country index scores in `indices/`, impute data and fit `brms` models predicting the index values given a set of predictors. The predictor data files are in `indices`.

Some relevant data files:

-   `coronanet/wide_data*`: list of CoronaNet data by index in wide format.

-   `coronanet/coronanet_internal_allvars.RDS`: the original CoronaNet data in long format.

-   `indices/[index]_time_data.csv`: the index scores by country in un-scaled (original posterior estimate) format

-   `indices/[index]_time_data_scaled.csv`: the index scores by country in scaled (0 - 100) format. This version is also released via the CoronaNet project separately.
