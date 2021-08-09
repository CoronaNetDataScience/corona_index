# Code and Data Repository for "Statistically Validated Indices for COVID-19 Public Health Policies"

## NOTE: The indices have been updated to April 30th. To access the original indices used to reproduce the paper, use the index files with  `jan15` in the title in the `indices` folder.

This repository hosts code and data necessary to reproduce the results in:

Kubinec et al. "Statistically Validated Indices for COVID-19 Public Health Policies."

For a description of the available data and indices, [please see this blog post](http://www.robertkubinec.com/post/err_in_vars/). For information about the files in this repo (which includes the data along with R code), read on.

## Indices

The estimated values for each index are in the `indices` folder. They are available as both R RDS files and as CSV files. The files are of the type `INDEX_time_data` and `INDEX_time_data_scaled`, where the scaled version has a minimum value of 0 and a maximum value of 100, and the unscaled version has the original standardized model estimates. The scaled data are the data type used in the paper linked above. Each file has four columns:

-   `med_est`: the posterior median estimate (most likely value)
-   `high_est`: the 95% posterior quantile (upper uncertainty interval)
-   `low_est`: the 5% posterior quantile (the lower uncertainty interval)
-   `sd_est`: the standard deviation of the posterior estimate

To incorporate measurement uncertainty, it is advisable to estimate models for `med_est`, `high_est`, and `low_est` to see if measurement error affects substantive results. The `sd_est` field can be used to directly incorporate measurement error via errors-in-variables models.

To identify a particular index, the `INDEX` codes are as follows:

-   `biz`: Business Restrictions
-   `sd`: Social Distancing Policies
-   `hr`: Health Resources
-   `hm2`: Health Monitoring
-   `school`: School Restrictions
-   `mask`: Mask Policies

The file `all_indices.csv` has all of the indices appended by row with an additional column `Index` to identify each index. If you do not want to bother with the Github repository, you can also download this file in CSV form from [this link](https://drive.google.com/uc?export=download&id=1dMCTVPrf-tJyhv_uxr0yAQO-Elx0QOCG).

## Indicators

In addition, the original indicators are available in the `coronanet` folder. Each indexes' indicators are in a separate file, `index_long_model_INDEX.rds`, where `INDEX` is a relevant code as in the list above. The files are in long form with one row for each policy - country combination, and the indicators are listed in the `item` column. Each index file also includes all of the Oxford CGRT data used in estimation, along with two additional fields: `man1` - `man3` and `voluntary`, which reflect the level of policy enforcement for CoronaNet data (i.e., `voluntary=1` if the policy did not have mandatory compliance).

If you want to a download a file with all the indicators in wide form (i.e., one row per country per all policies as columns), you can use [this link](https://drive.google.com/uc?id=1lorcowHNnF0Vl6pxBjMdjTC4yPhHBLJI&export=download).

## R Code

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
