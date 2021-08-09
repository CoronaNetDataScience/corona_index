# run cross-sectional and time-varying models
.libPaths("~/other_R_libs2")
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
require(kableExtra)

num_cores <- parallel::detectCores()

# whether to generate tables/figures
paper_output <- FALSE

# run everything from scratch

load_data <- T
run_mod <- T

# helper functions for brms

glance.brmsfit <- function(x,...) {
  
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

tidy.brmsfit <- function(x,...) {
  
  get_d <- summary(x)$fixed

   tibble::as_tibble(get_d,rownames="term") %>% 
    dplyr::select(term,
           estimate="Estimate",
           std.error="Est.Error",
           conf.low="l-95% CI",
           conf.high="u-95% CI",
           rhat="Rhat")
  
}

# load time-varying estimates

all_mods_data <- lapply(list.files("indices/","time\\_data\\_scaledjan15\\.rds",
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
  
  # raw data 
  #   jhu_cases <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") 
  
  jhu_cases <- read_csv("indices/jhu_cases.csv") %>% 
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
  
  cross_sections <- readRDS("indices/predictors_with_electoraldata.rds") %>% 
    select(country,area:pop_tot_log,gdppc2019,humanrights,lastelection,nextelection) %>% 
    distinct %>% 
    select(-gdp_pc) %>% 
    mutate(lastelection=ymd(lastelection),
           nextelection=ymd(nextelection),
           lastelection=case_when(country=="China"~ymd("2018-01-01"),
                                  country %in% c("Brunei","Eritrea")~min(lastelection,na.rm=T),
                                  TRUE~lastelection),
           nextelection=case_when(country=="China"~ymd("2023-01-01"),
                                  country %in% c("Brunei","Eritrea","Yemen","Saudi Arabia")~max(nextelection,na.rm=T),
                                  TRUE~nextelection))
  
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
  
  # indicator for days to  election
  
  combine_data <- group_by(combine_data,country) %>% 
    mutate(days_to_elec=case_when((lastelection - date_policy < nextelection - date_policy) & (lastelection - date_policy >0) ~ lastelection - date_policy,
                               (lastelection - date_policy) == 0 ~ 0,
                               (nextelection - date_policy) == 0 ~ 0,
                               TRUE ~ nextelection - date_policy),
           days_to_elec=as.numeric(days_to_elec))
  
  
  # need to recode instances where gender of person in power changed
  
  combine_data <- mutate(combine_data,
                         woman_leader=case_when(country == "Austria" & date_policy < ymd("2020-01-07") ~ 1,
                                                country == "Austria" & date_policy >= ymd("2020-01-07") ~ 0,
                                                country == "Belgium" & date_policy < ymd("2020-10-01") ~  1,
                                                country == "Belgium" & date_policy >= ymd("2020-10-01") ~  0,
                                                country == "Bolivia" & date_policy < ymd("2020-11-08") ~ 1,
                                                country == "Bolivia" & date_policy >= ymd("2020-11-08") ~ 0,
                                                country == "Togo" & date_policy < ymd("2020-09-28") ~ 1,
                                                country == "Togo" & date_policy >= ymd("2020-09-28") ~ 0,
                                                country == "Marshall Islands" & date_policy < ymd("2020-01-13") ~ 1,
                                                country == "Marshall Islands" & date_policy >= ymd("2020-01-13") ~ 0,
                                                country == "Myanmar" & date_policy < ymd("2020-04-01") ~ 1,
                                                country == "Myanmar" & date_policy >= ymd("2020-04-01") ~ 0,
                                                country == "Estonia" & date_policy < ymd("2021-01-26") ~ 0,
                                                country == "Estonia" & date_policy >= ymd("2021-01-26") ~ 1,
                                                country == "Moldova" & date_policy < ymd("2020-12-24") ~ 0,
                                                country == "Moldova" & date_policy >= ymd("2020-12-24") ~ 1,
                                                country == "Samoa" & date_policy < ymd("2020-05-24") ~ 0,
                                                country == "Samoa" & date_policy >= ymd("2020-05-24") ~ 1,
                                                country == "Tanzania" & date_policy < ymd("2021-03-19") ~ 0,
                                                country == "Tanzania" & date_policy >= ymd("2021-03-19") ~ 1,
                                                country == "Lithuania" & date_policy < ymd("2020-12-11") ~ 0,
                                                country == "Lithuania" & date_policy >= ymd("2020-12-11") ~ 1,
                                                country == "Gabon" & date_policy < ymd("2020-07-16") ~ 0,
                                                country == "Gabon" & date_policy >= ymd("2020-07-16") ~ 1,
                                                country == "San Marino" & date_policy < ymd("2020-10-01") ~ 0.5,
                                                country == "San Marino" & date_policy >= ymd("2020-10-01") ~ 0))
  
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
           fdi_prop=as.numeric(scale((fdi/exp(pop_tot_log))/gdppc2019)),
          gdp_pc=as.numeric(scale(gdppc2019)),
           cases_per_cap=as.numeric(scale(cases/exp(pop_tot_log))),
           deaths_per_cap=as.numeric(scale(deaths/exp(pop_tot_log))),
          mean_sd = (sd_school + sd_biz + sd_sd)/3)
  
      }) 

if(run_mod) {
  
  # business restrictions
  
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
                               density +
                                 days_to_elec,decomp="QR",center=TRUE),
                 data=combine_dv,
                 backend="cmdstanr",
                 chains=1,threads=threading(num_cores),
                 iter=1000,
                 cores=num_cores)
  
  saveRDS(biz_mod,"biz_mod_rr.rds")
  
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
                    chains=1,threads=threading(num_cores),
                    iter=1000,
                    cores=num_cores)
  
  saveRDS(school_mod,"school_mod_rr.rds")
  
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
                chains=1,threads=threading(num_cores),
                iter=1000,
                cores=num_cores)
  
  saveRDS(sd_mod,"sd_mod_rr.rds")
  
  # combined model, average SDs
  
  sd_mod <- brm_multiple(brmsformula(mvbind(med_biz,med_school,med_sd) | mi(mean_sd) ~ trade + finance + state_fragility + bureaucracy_corrupt +
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
                         chains=1,threads=threading(num_cores),
                         iter=1000,
                         cores=num_cores)
  
  saveRDS(sd_mod,"sd_mod_rr.rds")
  
} else {
  
  sd_mod <- readRDS("sd_mod_rr.rds")
  biz_mod <- readRDS("biz_mod_rr.rds")
  school_mod <- readRDS("school_mod_rr.rds")
  
}

if(paper_output) {
  
  modelsummary(list(Business=biz_mod,
                    `Social Distancing`=sd_mod,
                    Schools=school_mod),
               coef_omit="Observation",
               coef_map=c("cases_per_cap"="COVID-19 Cases",
                          "deaths_per_cap"="COVID-19 Deaths",
                          "contact"="Facebook Personal Contact",
                          "finance"="Facebook Financial Anxiety",
                          "anxious"="Facebook General Anxiety",
                          "retail_and_recreation_percent_change_from_baseline"='Retail Mobility',
                          "workplaces_percent_change_from_baseline"="Workplace Mobility",
                          "grocery_and_pharmacy_percent_change_from_baseline"="Grocery Mobility",
                          "parks_percent_change_from_baseline"="Parks Mobility",
                          "density"="Population Density",
                          "gdp_pc"="GDP Per Capita",
                          "fdi_prop"="FDI",
                          "trade"="Trade",
                          "state_fragility"="State Fragility",
                          "bureaucracy_corrupt"="Bureaucracy Corrupt",
                          "pandemic_prep"="Pandemic Preparedness",
                          "woman_leader"="Woman Leader",
                          "polity"="Polity Score",
                          "gini"="Gini Index",
                          "days_to_elec"="Days to Election"),
               title="Results of Regression of Social, Political and Economic Covariates on Index Scores",
               statistic="({conf.low}, {conf.high})",
               gof_map=tibble(raw=c("$R^2$","LOO-IC"),
                              clean=c("$R^2$","LOO-IC"),
                              fmt=c(2,0)),
               escape=F,
               label="results",
               booktabs=T,
               output="latex") %>% 
    kable_styling(latex_options = c("striped","hold_position"),
                  font_size = 9) %>% 
    pack_rows("Time-varying", 1, 18) %>%
    pack_rows("Cross-sectional",19, 38) %>% 
    row_spec(seq(2,38,by=2),font_size=7,italic = T) %>% 
    footnote(general="Coefficients are the posterior median values and the uncertainty intervals are the 5% to 95% posterior density intervals. Results marginalize across 5 imputed datasets.",
             threeparttable = T) %>% 
    save_kable("mod_table.tex")
  
  
  # do some posterior predictions
  
  
  # FB Predictions ----------------------------------------------------------
  
  fb_biz <- conditional_effects(biz_mod,c("finance","contact","anxious"))
  fb_sd <- conditional_effects(sd_mod,c("finance","contact","anxious"))
  fb_school <- conditional_effects(school_mod,c("finance","contact","anxious"))
  
  fb_biz <- bind_rows(list(`Financial Anxiety`=fb_biz$finance,
                           `Personal Contact`=fb_biz$contact,
                           `General Anxiety`=fb_biz$anxious),.id="var")
  
  fb_sd <- bind_rows(list(`Financial Anxiety`=fb_sd$finance,
                          `Personal Contact`=fb_sd$contact,
                          `General Anxiety`=fb_sd$anxious),.id="var")
  
  fb_school <- bind_rows(list(`Financial Anxiety`=fb_school$finance,
                              `Personal Contact`=fb_school$contact,
                              `General Anxiety`=fb_school$anxious),.id="var")
  
  fb_fx <- bind_rows(list(Business=fb_biz,
                          Schools=fb_school,
                          `Social Distancing`=fb_sd),.id="index")
  
  label_value2 = function(labels) {
    label_value(labels = labels, multi_line = FALSE)
  }
  
  fb_fx %>% 
    ggplot(aes(y=estimate__,x=effect1__)) +
    geom_ribbon(aes(ymin=lower__,
                    ymax=upper__),alpha=0.5) +
    geom_line(colour="blue") +
    theme_tufte() +
    scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
    facet_wrap(vars(var,index),scales="free",labeller = label_value2) +
    labs(x="Weighted Percent Reporting via Facebook Survey",
         y="Predicted Index Score",
         caption=str_wrap("Estimates are predictions with other variables held at their means. Shaded intervals are the 5% to 95% posterior quantiles.",
                          width=75))
  
  ggsave("fb_fx.png")
  
  
  
  # Cross-section Predictions -----------------------------------------------
  
  cs_biz <- conditional_effects(biz_mod,c("gini","bureaucracy_corrupt","polity"))
  cs_sd <- conditional_effects(sd_mod,c("gini","bureaucracy_corrupt","polity"))
  cs_school <- conditional_effects(school_mod,c("gini","bureaucracy_corrupt","polity"))
  
  cs_biz <- bind_rows(list(`Gini`=cs_biz$gini,
                           `Corruption`=cs_biz$bureaucracy_corrupt,
                           `Democracy (Polity)`=cs_biz$polity),.id="var")
  
  cs_sd <- bind_rows(list(`Gini`=cs_sd$gini,
                          `Corruption`=cs_sd$bureaucracy_corrupt,
                          `Democracy (Polity)`=cs_sd$polity),.id="var")
  
  cs_school <- bind_rows(list(`Gini`=cs_school$gini,
                              `Corruption`=cs_school$bureaucracy_corrupt,
                              `Democracy (Polity)`=cs_school$polity),.id="var")
  
  cs_fx <- bind_rows(list(Business=cs_biz,
                          Schools=cs_school,
                          `Social Distancing`=cs_sd),.id="index")
  
  label_value2 = function(labels) {
    label_value(labels = labels, multi_line = FALSE)
  }
  
  cs_fx %>% 
    ggplot(aes(y=estimate__,x=effect1__)) +
    geom_ribbon(aes(ymin=lower__,
                    ymax=upper__),alpha=0.5) +
    geom_line(colour="blue") +
    theme_tufte() +
    facet_wrap(vars(var,index),scales="free_y",labeller = label_value2) +
    labs(x="Value of Standardized Predictor",
         y="Predicted Index Score",
         caption=str_wrap("Estimates are predictions with other variables held at their means. Shaded intervals are the 5% to 95% posterior quantiles.",
                          width=75))
  
  ggsave("cs_fx.png")
  
  # make a combined index file
  
  all_mods_data <- mutate(all_mods_data,
                          modtype=fct_recode(modtype,`Business Restrictions`="biz",
                                             `Health Monitoring`='hm2',
                                             `Health Resources`='hr',
                                             `Social Distancing`="sd",
                                             `Mask Policies`="masks",
                                             `School Restrictions`='school'))
  
  write_csv(all_mods_data,"indices/all_indices.csv")
  saveRDS(all_mods_data,'indices/all_indices.rds')
  
  # make a combined data file
  
  all_inds <- lapply(list.files(path="coronanet/",pattern="model",full.names=T),
                     readRDS) %>% 
    bind_rows %>% 
    select(country,item,date_policy,pop_out) %>% 
    group_by(country,item,date_policy) %>% 
    summarize(pop_out=max(pop_out))
  
  all_inds_weighted <- all_inds %>% 
    ungroup %>% 
    spread(key="item",value="pop_out")
  
  saveRDS(all_inds_weighted, "coronanet/all_inds_weighted.rds")
  write_csv(all_inds_weighted,"coronanet/all_inds_weighted.csv")
  
  
}





