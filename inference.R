# run cross-sectional and time-varying models

require(dplyr)
require(tidyr)
require(stringr)
require(forcats)
require(ggthemes)
require(ggplot2)
require(readr)
require(missRanger)
require(brms)
require(lubridate)
require(tidybayes)
require(modelsummary)
require(readxl)

# helper functions for brms

glance.brmsfit <- function(x) {
  
  this_sum <- summary(x)
  print("Estimating R2")
  this_R2 <- try(loo_R2(x))
  print("Estimating LOO")
  this_loo <- try(loo(x))
  
  if('try_error' %in% class(this_R2)) {
    r2 <- NA
  } else {
    r2 <- this_R2[,"Estimate"]
  }
  
  if('try_error' %in% class(this_loo)) {
    tl <- NA
  } else {
    tl <- this_loo$estimates[3,1]
  }
  
  out <- tibble::tibble(`$R^2$` =r2,
                        `LOO-IC`=tl,
                        `Chains`=this_sum$chains,
                        `Iter`=this_sum$iter,
                        `Warmup`=this_sum$warmup)
  return(out)
}

tidy.brmsfit <- function(x) {
  
  get_d <- summary(x)$fixed

   tibble::as_tibble(get_d,rownames="term") %>% 
    dplyr::select(term,
           estimate="Estimate",
           std.error="Est.Error",
           conf.low="l-95% CI",
           conf.high="u-95% CI",
           rhat="Rhat")
  
}

load_data <- T
run_mod <- T

# load time-varying estimates

all_mods_data <- lapply(list.files("indices/","time\\_data\\_scaled\\.rds",
                                   full.names = T),readRDS) %>% 
  bind_rows(.id="modtype")


all_mods_data$modtype <- fct_recode(all_mods_data$modtype,
                                    "biz" = "1",
                                    "hm2" = "2",
                                    "hr" = "3",
                                    "masks" = "4",
                                    "school" = "5",
                                    "sd" = "6")

# expand and fill

expand_mod <- group_by(all_mods_data,country,modtype) %>% 
  expand(date_policy=seq(ymd("2020-01-01"),max(all_mods_data$date_policy),
                         by="1 day"))

all_mods_data <- left_join(expand_mod,all_mods_data,by=c("country","modtype","date_policy")) %>% 
  group_by(country,modtype) %>% 
  arrange(country,date_policy) %>% 
  fill(med_est,sd_est,high_est,low_est,.direction="down")

all_mods_med <- select(all_mods_data,country,date_policy,modtype,med_est) %>% 
  mutate(modtype=paste0("med_",modtype)) %>% 
  spread(key="modtype",value="med_est")

all_mods_sd <- select(all_mods_data,country,date_policy,modtype,sd_est) %>% 
  mutate(modtype=paste0("sd_",modtype)) %>% 
  spread(key="modtype",value="sd_est")

if(load_data) {
  
  # ecdc
  
  jhu_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
    #filter(is.na(`Province/State`)) %>% 
    mutate(country=recode(`Country/Region`,
                          `Cabo Verde`="Cape Verde",
                          `Congo (Brazzaville)`="Republic of the Congo",
                          `Congo (Kinshasa)`="Democratic Republic of the Congo",
                          `Cote d'Ivoire`="Ivory Coast",
                          `Korea, South`="South Korea",
                          `Taiwan*`="Taiwan",
                          `Burma`="Myanmar",
                          `Holy See`="Vatican",
                          `US`="United States of America",
                          `Timor-Leste`="Timor Leste",
                          `West Bank and Gaza`="Palestine")) %>% 
    select(-`Country/Region`,-Lat,-Long) %>% 
    gather(key="date_policy",value="cases",-country,-`Province/State`) %>% 
    mutate(date_policy=lubridate::mdy(date_policy)) %>% 
    group_by(country,date_policy) %>% 
    summarize(cases=ifelse(any(is.na(`Province/State`)),cases[is.na(`Province/State`)],
                               sum(cases)))
  
  jhu_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
    mutate(country=recode(`Country/Region`,
                          `Cabo Verde`="Cape Verde",
                          `Congo (Brazzaville)`="Republic of the Congo",
                          `Congo (Kinshasa)`="Democratic Republic of the Congo",
                          `Cote d'Ivoire`="Ivory Coast",
                          `Korea, South`="South Korea",
                          `Taiwan*`="Taiwan",
                          `Burma`="Myanmar",
                          `Holy See`="Vatican",
                          `US`="United States of America",
                          `Timor-Leste`="Timor Leste",
                          `West Bank and Gaza`="Palestine")) %>% 
    select(-`Country/Region`,-Lat,-Long) %>% 
    gather(key="date_policy",value="deaths",-country,-`Province/State`) %>% 
    mutate(date_policy=lubridate::mdy(date_policy)) %>% 
    group_by(country,date_policy) %>% 
    summarize(deaths=ifelse(any(is.na(`Province/State`)),deaths[is.na(`Province/State`)],
                           sum(deaths)))
  
  # load in our goodies
  
  cross_sections <- readRDS("indices/merged_predictors.rds") %>% 
    select(country,area:pop_tot_log) %>% 
    distinct
  
  time_vary_fb <- read_csv("indices/fb_surv.csv") %>% 
    select(country,date_policy="date",
           contact="data.percent_contact",
           mask="data.percent_mask",
           anxious='data.pct_anxious_7d',
           finance="data.percent_finance")
  
  google <- readRDS("indices/gmob.rds") %>% 
    filter(is.na(sub_region_1), is.na(sub_region_2), is.na(metro_area)) %>% 
    select(country,matches("baseline"),date_policy="date")
  
  # merge into one dataset and probably impute
  
  combine_data <- left_join(time_vary_fb,all_mods_med,
                            by=c("country","date_policy")) %>% 
    left_join(google,by=c("country","date_policy")) %>% 
    left_join(cross_sections,by="country") %>% 
    left_join(jhu_cases,by=c("country","date_policy")) %>% 
    left_join(jhu_deaths,by=c("country","date_policy")) %>% 
    filter(!is.na(med_biz),!is.na(med_masks),!is.na(med_sd))
    
  
  # need to merge in indices to impute
  
  # impute 5 big ones
  
  over_five <- lapply(1:5,function(i) {
    
    missRanger(combine_data,pmm.k=10L)
  })
  
  saveRDS(over_five,"indices/over_five.rds")
  
} else {
  
  over_five <- readRDS("indices/over_five.rds")
  
}

# bind and merge

combine_dv <- lapply(over_five, function(o) {
    o %>% 
      left_join(all_mods_sd,
              by=c('country',"date_policy")) %>% 
      mutate(density=as.numeric(scale(exp(pop_tot_log)/area)),
           fdi_prop=as.numeric(scale((fdi/exp(pop_tot_log))/gdp_pc)),
           cases_per_cap=as.numeric(scale(cases/exp(pop_tot_log))),
           deaths_per_cap=as.numeric(scale(deaths/exp(pop_tot_log))))
  
      }) 

if(run_mod) {
  
  # try out a combined model
  
  biz_mod <- brm_multiple(brmsformula(med_biz | mi(sd_biz) ~ trade + finance + state_fragility + bureaucracy_corrupt +
                               retail_and_recreation_percent_change_from_baseline +
                               workplaces_percent_change_from_baseline +
                               grocery_and_pharmacy_percent_change_from_baseline +
                               parks_percent_change_from_baseline +
                               contact + 
                               anxious +
                               gdp_pc +
                               fdi_prop +
                               pandemic_prep +
                               woman_leader +
                               polity +
                               gini +
                                cases_per_cap +
                                 deaths_per_cap +
                               density,decomp="QR",center=TRUE),
                 data=combine_dv,
                 backend="cmdstanr",
                 chains=1,threads=threading(16),
                 iter=1000,
                 cores=16)
  
  saveRDS(biz_mod,"biz_mod.rds")
  
  school_mod <- brm_multiple(brmsformula(med_school | mi(sd_school) ~ trade + finance + state_fragility + bureaucracy_corrupt +
                                  retail_and_recreation_percent_change_from_baseline +
                                  workplaces_percent_change_from_baseline +
                                  grocery_and_pharmacy_percent_change_from_baseline +
                                  parks_percent_change_from_baseline +
                                  contact + 
                                  anxious +
                                  gdp_pc +
                                  fdi_prop +
                                  pandemic_prep +
                                  woman_leader +
                                  polity +
                                  gini +
                                    cases_per_cap +
                                    deaths_per_cap +
                                  density,decomp="QR",center=TRUE),
                    data=combine_dv,
                    backend="cmdstanr",
                    chains=1,threads=threading(16),
                    iter=1000,
                    cores=16)
  
  saveRDS(school_mod,"school_mod.rds")
  
  sd_mod <- brm_multiple(brmsformula(med_sd | mi(sd_sd) ~ trade + finance + state_fragility + bureaucracy_corrupt +
                              retail_and_recreation_percent_change_from_baseline +
                              workplaces_percent_change_from_baseline +
                              grocery_and_pharmacy_percent_change_from_baseline +
                              parks_percent_change_from_baseline +
                              contact + 
                              anxious +
                              gdp_pc +
                              fdi_prop +
                              pandemic_prep +
                              woman_leader +
                              polity +
                              gini +
                                cases_per_cap +
                                deaths_per_cap +
                              density,decomp="QR",center=TRUE),
                data=combine_dv,
                backend="cmdstanr",
                chains=1,threads=threading(16),
                iter=1000,
                cores=16)
  
  saveRDS(sd_mod,"sd_mod.rds")
  
} else {
  
  sd_mod <- readRDS("sd_mod.rds")
  biz_mod <- readRDS("biz_mod.rds")
  school_mod <- readRDS("school_mod.rds")
  
}



modelsummary(list(Business=biz_mod,
                  `Social Distancing`=sd_mod,
                  Schools=school_mod),
                  coef_omit="Observation",
             coef_rename=c("b_Intercept"="Intercept",
                           "b_trade"="Trade",
                           "b_finance"="Finance",
                           "b_state_fragility"="State Fragility",
                           "b_bureaucracy_corrupt"="Bureaucracy Corrupt",
                           "b_retail_and_recreation_percent_change_from_baseline"='Retail Mobility',
                           "b_workplaces_percent_change_from_baseline"="Workplace Mobility",
                           "b_grocery_and_pharmacy_percent_change_from_baseline"="Grocery Mobility",
                           "b_parks_percent_change_from_baseline"="Parks Mobility",
                           "b_contact"="Facebook % Contact",
                           "b_anxious"="Facebook % Anxious",
                           "b_gdp_pc"="GDP Per Capita",
                           "b_fdi_prop"="% FDI",
                           "b_pandemic_prep"="Pandemic Preparedness",
                           "b_woman_leader"="Woman Leader",
                           "b_polity"="Polity Score",
                           "b_gini"="Gini Index",
                           "b_cases_per_cap"="COVID-19 Cases",
                           "b_deaths_per_cap"="COVID-19 Deaths",
                           "b_density"="Population Density"),
             statistic=c("conf.int"),
             escape=F,
             booktabs=T,
             output="latex",
             gof_map=tibble(raw=c("$R^2$","LOO-IC","Iter","Warmup","Chains"),
                            clean=c("$R^2$","LOO-IC","Iter","Warmup","Chains"),
                            fmt=c(2,0,0,0,0))) %>% 
            kable_styling(latex_options = c("striped","hold_position")) %>% 
            kableExtra::save_kable("mod_table.tex")
