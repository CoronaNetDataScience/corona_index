# run cross-sectional and time-varying models

require(cmdstanr)

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
paper_output <- T

# whether to model inter-score correlation

#model_cor <- Sys.getenv("MODELCOR")

model_cor <- "no"

# run everything from scratch

load_data <- F
run_mod <- T
mult_pull <- F

# load time-varying estimates

# all_mods_data <- lapply(list.files("indices/","time\\_data\\_scaledjan15\\.rds",
#                                    full.names = T),readRDS) %>% 
#   bind_rows(.id="modtype")

all_mods_data <- lapply(list.files("indices/","time\\_data\\.rds",
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

# all_inds <- lapply(list.files(path="coronanet/",pattern="long\\_model",full.names=T),
#                    readRDS) %>% 
#   bind_rows %>% 
#   select(country,item,date_policy,pop_out) %>% 
#   group_by(country,item,date_policy) %>% 
#   summarize(pop_out=max(pop_out))
# 
# all_inds_weighted <- all_inds %>% 
#   ungroup %>% 
#   spread(key="item",value="pop_out")
# 
# saveRDS(all_inds_weighted, "indices/all_inds_weighted.rds")
# write_csv(all_inds_weighted,"indices/all_inds_weighted.csv")

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
  
  # original data
  # read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  
  jhu_deaths <- read_csv("indices/jhu_deaths.csv") %>% 
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
  
  combine_dv_noimpute <- combine_dv
  
  saveRDS(combine_dv_noimpute, "data/non_imputed_data.rds")
  
  # need to merge in indices to impute
  
  # impute 5 big ones
  
  over_five <- lapply(1:5,function(i) {
    
    missRanger(combine_data,pmm.k=10L)
  })
  
  saveRDS(over_five,"indices/over_five.rds")
  
} else {
  
  over_five <- readRDS("indices/over_five.rds")
  
  combine_dv_noimpute <- readRDS("data/non_imputed_data.rds")
  
}

# bind and merge

combine_dv <- lapply(over_five, function(o) {

    o %>% 
      left_join(all_mods_sd,
              by=c('country',"date_policy")) %>% 
      ungroup %>% 
      mutate(density=as.numeric(scale(exp(pop_tot_log)/area)),
           fdi_prop=as.numeric(scale((fdi/exp(pop_tot_log))/gdppc2019)),
          gdp_pc=as.numeric(scale(gdppc2019)),
           cases_per_cap=as.numeric(scale(cases/exp(pop_tot_log))),
           deaths_per_cap=as.numeric(scale(deaths/exp(pop_tot_log))),
          mean_sd = (sd_school + sd_biz + sd_sd)/3)
  
      }) 


combine_dv_noimpute <- left_join(combine_dv_noimpute, all_mods_sd,
                                 by=c('country',"date_policy")) %>% 
  ungroup %>% 
  mutate(density=as.numeric(scale(exp(pop_tot_log)/area)),
         fdi_prop=as.numeric(scale((fdi/exp(pop_tot_log))/gdppc2019)),
         gdp_pc=as.numeric(scale(gdppc2019)),
         cases_per_cap=as.numeric(scale(cases/exp(pop_tot_log))),
         deaths_per_cap=as.numeric(scale(deaths/exp(pop_tot_log))),
         mean_sd = (sd_school + sd_biz + sd_sd)/3) %>% 
  mutate_at(vars(matches("med")), ~ as.numeric(scale(.)))


# Predicting contact rates ------------------------------------------------

biz_mi <- bf(med_biz | mi(sd_biz) ~ 0,family=gaussian())
hm2_mi <- bf(med_hm2 | mi(sd_hm2) ~ 0,family=gaussian())
sd_mi <- bf(med_sd | mi(sd_sd) ~ 0,family=gaussian())
school_mi <- bf(med_school | mi(sd_school) ~ 0,family=gaussian())
mask_mi <- bf(med_masks | mi(sd_masks) ~ 0,family=gaussian())
hr_mi <- bf(med_hr | mi(sd_hr) ~ 0,family=gaussian())

test_set <- sample_n(combine_dv[[1]],2000)
test_set$date_policy_fac <- as.character(test_set$date_policy)
combine_dv_noimpute$date_policy_fac <- as.character(combine_dv_noimpute$date_policy)

library(ordbetareg)

if(run_mod) {
  
  # make code and edit it a bit
  
  if(model_cor=="yes") {
    
    contact_mod_data <- make_standata(bf(contact ~ cases_per_cap + deaths_per_cap +
                                           me(med_biz,sdx = sd_biz) +
                                           me(med_hm2,sdx=sd_hm2) +
                                           me(med_sd,sdx = sd_sd) +
                                           me(med_school,sdx = sd_school) +
                                           me(med_masks,sdx = sd_masks) +
                                           me(med_hr,sdx = sd_hr) +
                                           date_policy_fac) +
                                        set_mecor(TRUE), 
                                      prior=prior(normal(0,1),class="meanme") + 
                                        prior(exponential(1),class="sdme") +
                                        prior(normal(0,5),class="b"),
                                      data=test_set,
                                      chains=1,threads=parallel::detectCores()/2,max_treedepth=12,
                                      warmup = 1000,iter = 500,
                                      backend="cmdstanr")
    
    # use centered parameterization for latent variables because of large number 
    
    contact_mod_code <- cmdstan_model("me_ovb_contact_cov_centered.stan",
                                      cpp_options = list(stan_threads = TRUE))
    
    contact_mod <- contact_mod_code$sample(data=contact_mod_data,
                                           seed=638825,
                                           refresh=100,
                                           chains=4,iter_warmup=500,
                                           iter_sampling=500,
                                           max_treedepth=12,
                                           parallel_chains=4,
                                           adapt_delta=0.95,
                                           threads_per_chain=as.integer(floor(parallel::detectCores()/4)))
    
    contact_mod_samp <- contact_mod$draws(variables=c("b","bsp","Omega"))
    
    saveRDS(contact_mod, "coronanet/contact_mod_noimpute.rds")
    saveRDS(contact_mod_samp, "coronanet/contact_mod_noimpute_samp.rds")
    
  } else {
    
    contact_mod_data <- make_standata(bf(contact ~ cases_per_cap + deaths_per_cap +
                                           me(med_biz,sdx = sd_biz) +
                                           me(med_hm2,sdx=sd_hm2) +
                                           me(med_sd,sdx = sd_sd) +
                                           me(med_school,sdx = sd_school) +
                                           me(med_masks,sdx = sd_masks) +
                                           me(med_hr,sdx = sd_hr) +
                                           date_policy_fac) +
                                        set_mecor(TRUE), 
                                      prior=prior(normal(0,1),class="meanme") + 
                                        prior(exponential(1),class="sdme") +
                                        prior(normal(0,5),class="b"),
                                      data=combine_dv_noimpute,
                                      chains=1,threads=parallel::detectCores(),max_treedepth=12,
                                      warmup = 1000,iter = 1500,
                                      backend="cmdstanr")
    
    contact_mod_code <- cmdstan_model("me_model_contact.stan",
                                      cpp_options = list(stan_threads = TRUE))
    
    contact_mod <- contact_mod_code$sample(data=contact_mod_data,
                                           seed=638825,
                                           refresh=100,
                                           chains=4,iter_warmup=500,
                                           iter_sampling=500,
                                           max_treedepth=12,
                                           threads_per_chain=as.integer(floor(parallel::detectCores()/4)))
    
    #contact_mod_samp <- contact_mod$draws(variables=c("b","bsp"))
    
    saveRDS(contact_mod, "coronanet/contact_mod_noimpute_nocor.rds")
    
    
  }
  
  
  
} else {
  
  contact_mod <- readRDS("coronanet/contact_mod_noimpute.rds")
  
}





library(modelsummary)
library(kableExtra)
library(posterior)

contact_mod_sum <- contact_mod$draws(variables = c("b[1]","b[2]",
                                                   "bsp")) %>% 
  summarize_draws()


contact_mod_sum %>% 
  mutate(variable=recode(variable,
                         `b[1]`="Cases Per Capita",
                         `b[2]`="Deaths Per Capita",
                         `bsp[1]`="Business Restrictions",
                         `bsp[2]`="Health Management",
                         `bsp[3]`="Social Distancing",
                         `bsp[4]`="School Restrictions",
                         `bsp[5]`="Masks",
                         `bsp[6]`="Health Resources"),
         Estimate=paste0(round(median,digits=3)," (",round(q5,digits=3),
                         ", ",
                         round(q95,digits=3),
                         ")"),
         rhat=round(rhat,digits=3)) %>% 
  select(Variable="variable",`Posterior Median and 5% -95% Interval`="Estimate",
         rhat) %>% 
  kable(format="latex",caption="Estimates of Regression of Contact Rates on Policy Intensity Scores",
        label="contact_mod",
        booktabs=T,
        align = c("l", "r", "r")) %>% 
  kable_styling(latex_options = c("striped","hold_position")) %>% 
     footnote(general="Coefficients are the posterior median values and the uncertainty intervals are the 5% to 95% posterior density intervals. Cases per capita and deaths per capita were both standardized within country. Not shown are day fixed effects and the estimated posterior values of the intensity scores incorporating measurement error. The R-hat statistic measures the ability of the independent MCMC runs of the sampler to converge, with values less than or equal to 1 being preferred.",
              threeparttable = T) %>% 
  save_kable("contact_mod_table.tex",keep_tex=T)

# visualize covariance matrix

# viz_cov_sum<- subset_draws(contact_mod,
#                                variable="corme_1",
#                                regex=T) %>% 
#   summarize_draws() %>% 
#   mutate(all_match=str_extract_all(variable,"biz|hm2|sd|hr|mask|school"),
#          Var1=sapply(all_match, function(x) x[1]),
#          Var2=sapply(all_match, function(x) x[2]),
#          Var1=recode(Var1,
#                      biz="Business Restrictions",
#                      sd="Social Distancing",
#                      school="School Restrictions",
#                      mask="Mask Policies",
#                      hm2="Health Monitoring",
#                      hr="Health Resources"),
#          Var2=recode(Var2,
#                      biz="Business Restrictions",
#                      sd="Social Distancing",
#                      school="School Restrictions",
#                      mask="Mask Policies",
#                      hm2="Health Management",
#                      hr="Health Resources"))
# 
# all_combos <- expand(viz_cov_sum, Var1, Var2)
# 
# all_combos <- left_join(all_combos,viz_cov_sum) %>% 
#   filter(Var1!=Var2)
# 
# all_combos2 <- all_combos %>% 
#   select(median,Var1="Var2",
#          Var2="Var1")
# 
# all_combos <- bind_rows(all_combos,
#                         all_combos2) %>% 
#   filter(!is.na(median))
# 
# all_combos %>% 
#   ggplot(aes(x=Var1,y=Var2,fill=median)) +
#   geom_tile() +
#   scale_fill_viridis_c(name="Correlation") +
#   theme_tufte() +
#   theme(axis.text.x =element_text(angle=90)) +
#   labs(x="",y="") +
#   theme(text=element_text(family=""))
# 
# ggsave("contact_mod_corr.pdf",width=6,height=4)


# drop this model

# if(run_mod) {
#   
#   # business restrictions
#   
#   biz_mod <- brm_multiple(brmsformula(med_biz | mi(sd_biz) ~ trade + finance + state_fragility + bureaucracy_corrupt +
#                                retail_and_recreation_percent_change_from_baseline +
#                                workplaces_percent_change_from_baseline +
#                                grocery_and_pharmacy_percent_change_from_baseline +
#                                parks_percent_change_from_baseline +
#                                contact +
#                                anxious +
#                                gdp_pc +
#                                fdi_prop +
#                                pandemic_prep +
#                                woman_leader +
#                                polity +
#                                gini +
#                                 cases_per_cap +
#                                  deaths_per_cap +
#                                density +
#                                  days_to_elec +
#                                  humanrights,decomp="QR",center=TRUE),
#                  data=combine_dv,
#                  backend="cmdstanr",
#                  chains=1,threads=threading(num_cores),
#                  iter=1000,
#                  cores=num_cores,
#                  max_treedepth=12)
# 
#   saveRDS(biz_mod,"biz_mod_rr.rds")
# 
#   school_mod <- brm_multiple(brmsformula(med_school | mi(sd_school) ~ trade + finance + state_fragility + bureaucracy_corrupt +
#                                   retail_and_recreation_percent_change_from_baseline +
#                                   workplaces_percent_change_from_baseline +
#                                   grocery_and_pharmacy_percent_change_from_baseline +
#                                   parks_percent_change_from_baseline +
#                                   contact +
#                                   anxious +
#                                   gdp_pc +
#                                   fdi_prop +
#                                   pandemic_prep +
#                                   woman_leader +
#                                   polity +
#                                   gini +
#                                     cases_per_cap +
#                                     deaths_per_cap +
#                                   density +
#                                     days_to_elec +
#                                     humanrights,decomp="QR",center=TRUE),
#                     data=combine_dv,
#                     backend="cmdstanr",
#                     chains=1,threads=threading(num_cores),
#                     iter=1000,
#                     cores=num_cores,
#                     max_treedepth=12)
# 
#   saveRDS(school_mod,"school_mod_rr.rds")
# 
#   sd_mod <- brm_multiple(brmsformula(med_sd | mi(sd_sd) ~ trade + finance + state_fragility + bureaucracy_corrupt +
#                               retail_and_recreation_percent_change_from_baseline +
#                               workplaces_percent_change_from_baseline +
#                               grocery_and_pharmacy_percent_change_from_baseline +
#                               parks_percent_change_from_baseline +
#                               contact +
#                               anxious +
#                               gdp_pc +
#                               fdi_prop +
#                               pandemic_prep +
#                               woman_leader +
#                               polity +
#                               gini +
#                                 cases_per_cap +
#                                 deaths_per_cap +
#                               density +
#                                 days_to_elec +
#                                 humanrights,decomp="QR",center=TRUE),
#                 data=combine_dv,
#                 backend="cmdstanr",
#                 chains=1,threads=threading(num_cores),
#                 iter=1000,
#                 cores=num_cores,
#                 max_treedepth=12)
# 
#   # saveRDS(sd_mod,"sd_mod_rr.rds")
#   
#   # combined model, average SDs
#   
#   combine_form <- mvbrmsformula(bf(med_biz | mi(sd_biz) ~ trade + finance + state_fragility + bureaucracy_corrupt +
#                                   retail_and_recreation_percent_change_from_baseline +
#                                   workplaces_percent_change_from_baseline +
#                                   grocery_and_pharmacy_percent_change_from_baseline +
#                                   parks_percent_change_from_baseline +
#                                   contact + 
#                                   anxious +
#                                   gdp_pc +
#                                   fdi_prop +
#                                   pandemic_prep +
#                                   woman_leader +
#                                   polity +
#                                   gini +
#                                   cases_per_cap +
#                                   deaths_per_cap +
#                                   density +
#                                   days_to_elec +
#                                   humanrights,
#                                 decomp="QR",center=TRUE),
#                                 bf(med_school | mi(sd_school) ~ trade + finance + state_fragility + bureaucracy_corrupt +
#                                      retail_and_recreation_percent_change_from_baseline +
#                                      workplaces_percent_change_from_baseline +
#                                      grocery_and_pharmacy_percent_change_from_baseline +
#                                      parks_percent_change_from_baseline +
#                                      contact + 
#                                      anxious +
#                                      gdp_pc +
#                                      fdi_prop +
#                                      pandemic_prep +
#                                      woman_leader +
#                                      polity +
#                                      gini +
#                                      cases_per_cap +
#                                      deaths_per_cap +
#                                      density +
#                                      days_to_elec +
#                                      humanrights,
#                                    decomp="QR",center=TRUE),
#                           bf(med_sd | mi(sd_sd) ~ trade + finance + state_fragility + bureaucracy_corrupt +
#                                retail_and_recreation_percent_change_from_baseline +
#                                workplaces_percent_change_from_baseline +
#                                grocery_and_pharmacy_percent_change_from_baseline +
#                                parks_percent_change_from_baseline +
#                                contact + 
#                                anxious +
#                                gdp_pc +
#                                fdi_prop +
#                                pandemic_prep +
#                                woman_leader +
#                                polity +
#                                gini +
#                                cases_per_cap +
#                                deaths_per_cap +
#                                density +
#                                days_to_elec +
#                                humanrights,
#                              decomp="QR",center=TRUE),
#                           rescor=T)
#   
#   mult_mod <- brm_multiple(combine_form,
#                          data=combine_dv,
#                          backend="cmdstanr",
#                          chains=1,threads=threading(num_cores),
#                          iter=1000,
#                          cores=num_cores,
#                          max_treedepth=12)
#   
#   saveRDS(mult_mod,"multivariate_mod_rr.rds")
#   
# } else {
#   
#   sd_mod <- readRDS("sd_mod_rr.rds")
#   biz_mod <- readRDS("biz_mod_rr.rds")
#   school_mod <- readRDS("school_mod_rr.rds")
#   mult_mod <- readRDS("multivariate_mod_rr.rds")
#   
# }

if(paper_output) {
  
  # old table/model without correlated errors
  
  # modelsummary(list(Business=biz_mod,
  #                   `Social Distancing`=sd_mod,
  #                   Schools=school_mod),
  #              coef_omit="Observation",
  #              coef_map=c("cases_per_cap"="COVID-19 Cases",
  #                         "deaths_per_cap"="COVID-19 Deaths",
  #                         "contact"="Facebook Personal Contact",
  #                         "finance"="Facebook Financial Anxiety",
  #                         "anxious"="Facebook General Anxiety",
  #                         "retail_and_recreation_percent_change_from_baseline"='Retail Mobility',
  #                         "workplaces_percent_change_from_baseline"="Workplace Mobility",
  #                         "grocery_and_pharmacy_percent_change_from_baseline"="Grocery Mobility",
  #                         "parks_percent_change_from_baseline"="Parks Mobility",
  #                         "days_to_elec"="Days to Election",
  #                         "density"="Population Density",
  #                         "gdp_pc"="GDP Per Capita",
  #                         "fdi_prop"="FDI",
  #                         "trade"="Trade",
  #                         "state_fragility"="State Fragility",
  #                         "bureaucracy_corrupt"="Bureaucracy Corrupt",
  #                         "pandemic_prep"="Pandemic Preparedness",
  #                         "woman_leader"="Woman Leader",
  #                         "polity"="Polity Score",
  #                         "gini"="Gini Index",
  #                         "humanrights"="Human Rights"),
  #              title="Results of Regression of Social, Political and Economic Covariates on Index Scores",
  #              statistic="({conf.low}, {conf.high})",
  #              gof_map=tibble(raw=c("$R^2$","LOO-IC"),
  #                             clean=c("$R^2$","LOO-IC"),
  #                             fmt=c(2,0)),
  #              escape=F,
  #              label="results",
  #              booktabs=T,
  #              output="latex") %>% 
  #   kable_styling(latex_options = c("striped","hold_position"),
  #                 font_size = 9) %>% 
  #   pack_rows("Time-varying", 1, 20) %>%
  #   pack_rows("Cross-sectional",21, 42) %>% 
  #   row_spec(seq(2,40,by=2),font_size=7,italic = T) %>% 
  #   footnote(general="Coefficients are the posterior median values and the uncertainty intervals are the 5% to 95% posterior density intervals. Results marginalize across 5 imputed datasets.",
  #            threeparttable = T) %>% 
  #   save_kable("mod_table_old.tex")
  
  # new multivariate model /w correlated errors
  
  # require(gtsummary)
  # require(tidybayes)
  # require(stringr)
  # 
  # if(mult_pull) {
  #   
  #   tidy_out <- gather_draws(mult_mod,`b_.*`, regex=T) %>% 
  #     median_qi
  #   
  #   saveRDS(tidy_out, "data/mult_tidy_out.rds")
  #   
  #   mult_loo <- loo(mult_mod)
  #   
  #   saveRDS(mult_loo, "data/mult_loo.rds")
  #   
  #   mult_R2 <- bayes_R2(mult_mod)
  #   
  #   saveRDS(mult_R2, "data/mult_R2.rds")
  #   
  # } else {
  #   
  #   mult_loo <- readRDS("data/mult_loo.rds")
  #   mult_R2 <- readRDS("data/mult_R2.rds")
  #   tidy_out <- readRDS("data/mult_tidy_out.rds")
  #   
  # }
  # 
  # 
  # 
  # # make a 3 column table
  # 
  # tidy_out <- mutate(tidy_out, type=str_extract(.variable,
  #                                               pattern="(?<=b\\_med)[a-z]+"),
  #                             .variable=str_extract(.variable,
  #                                                   pattern="(?<=med[a-z]{1,6}\\_)[a-zA-Z]+|Intercept"))
  # 
  # tidy_wide  <- select(tidy_out,.value, .variable,type) %>% spread(key="type",value=".value") %>% 
  #   mutate(stat="coef") %>% 
  #   mutate_at(c("biz","school","sd"), ~as.character(round(.,3)))
  # 
  # tidy_ci <- mutate(tidy_out,.value=paste0("(",round(.lower,3),", ",round(.upper,3),")")) %>% 
  #   select(.value, .variable,type) %>% spread(key="type",value=".value") %>% 
  #   mutate(stat="ci")
  # 
  # tidy_combine <- bind_rows(tidy_wide, tidy_ci) %>% 
  #   mutate(.variable=fct_recode(.variable,
  #                               "Facebook General Anxiety" = "anxious",
  #                               "Bureaucracy Corrupt" = "bureaucracy",
  #                               "COVID-19 Cases" = "cases",
  #                               "Facebook Personal Contact" = "contact",
  #                               "Days to Election" = "days",
  #                               "COVID-19 Deaths" = "deaths",
  #                               "Population Density" = "density",
  #                               "FDI" = "fdi",
  #                               "Facebook Financial Anxiety" = "finance",
  #                               "GDP Per Capita" = "gdp",
  #                               "Gini Index" = "gini",
  #                               "Grocery Mobility" = "grocery",
  #                               "Human Rights" = "humanrights",
  #                               "Pandemic Preparedness" = "pandemic",
  #                               "Parks Mobility" = "parks",
  #                               "Polity Score" = "polity",
  #                               "Retail Mobility" = "retail",
  #                               "State Fragility" = "state",
  #                               "Trade" = "trade",
  #                               "Woman Leader" = "woman",
  #                               "Workplace Mobility" = "workplaces"))
  # 
  # # add loo / R2
  # 
  # loo_tb <- tibble(biz=as.character(round(mult_loo$looic,0)),
  #                  sd=as.character(round(mult_loo$looic,0)),
  #                  school=as.character(round(mult_loo$looic,0)),
  #                  .variable="LOO-IC",
  #                  stat='coef')
  # 
  # r2_tb <- tibble(biz=as.character(round(mult_R2[1,1],3)),
  #                 sd=as.character(round(mult_R2[3,1],3)),
  #                 school=as.character(round(mult_R2[2,1],3)),
  #                 .variable="R2",
  #                 stat='coef')
  # 
  # tidy_combine <- bind_rows(tidy_combine,loo_tb) %>% 
  #   bind_rows(r2_tb)
  # 
  # 
  # # format and export
  # 
  # tidy_combine %>% 
  #   mutate(.variable=fct_relevel(
  #     .variable,
  #     "COVID-19 Deaths", "COVID-19 Cases", "Facebook Financial Anxiety",
  #     "Facebook General Anxiety", "Facebook Personal Contact", "Retail Mobility",
  #     "Parks Mobility", "Grocery Mobility", "Workplace Mobility", "Days to Election",
  #     "FDI", "GDP Per Capita", "Bureaucracy Corrupt", "Gini Index",
  #     "Human Rights", "Pandemic Preparedness", "Polity Score", "Population Density",
  #     "State Fragility", "Trade", "Woman Leader", "Intercept", "LOO-IC",
  #     "R2"
  #   )) %>% 
  #   arrange(.variable,desc(stat)) %>% 
  #   mutate(.variable=ifelse(stat=="coef",as.character(.variable),"")) %>% 
  #   select(-stat,Variable=".variable",
  #          `Business Restrictions`="biz",
  #          `Social Distancing`="sd",
  #          `School Restrictions`="school") %>% 
  #   kable(caption="Results of Regression of Social, Political and Economic Covariates on Index Scores",
  #         linesep = "",booktabs=T,format="latex",
  #         align=c('lccc')) %>% 
  #   kable_styling(latex_options = c("striped","hold_position"),
  #                 font_size = 9) %>% 
  #   pack_rows("Time-varying", 1, 20) %>%
  #   pack_rows("Cross-sectional",21, 44) %>% 
  #   row_spec(seq(2,44,by=2),font_size=7,italic = T) %>% 
  #   footnote(general="Coefficients are the posterior median values and the uncertainty intervals are the 5% to 95% posterior density intervals. Results marginalize across 5 imputed datasets. LOO-IC values are identical across models due to the use of a single multivariate Normal distribution.",
  #            threeparttable = T) %>% 
  #   save_kable("mod_table.tex",keep_tex=T)
  # 
  # # make a correlation table
  # 
  # cor_dv <- VarCorr(mult_mod)$residual__$cor
  # 
  # cor_dv <- cbind(cor_dv[,1,1],cor_dv[,1,2],cor_dv[,1,3])
  # 
  # row.names(cor_dv) <- c("Business","Schools","Social\nDistancing")
  # 
  # colnames(cor_dv) <- row.names(cor_dv)
  # 
  # require(ggcorrplot)
  # 
  # ggcorrplot(cor_dv,show.legend=F,lab=T)
  # 
  # ggsave("mult_mod_corr.png",scale=0.7)
  # 
  # # do some posterior predictions
  # 
  # 
  # # FB Predictions ----------------------------------------------------------
  # 
  # fb_biz <- conditional_effects(mult_mod,c("finance","contact","anxious"))
  # 
  # finance <- bind_rows(lapply(fb_biz[1:3], select, cond="finance", estimate__, upper__, lower__),
  #                      .id="index") %>% 
  #   mutate(var="Financial Anxiety")
  # 
  # contact <- bind_rows(lapply(fb_biz[4:6], select, cond="contact", estimate__, upper__, lower__),
  #                      .id="index") %>% 
  #   mutate(var="Personal Contacts")
  # 
  # anxious <- bind_rows(lapply(fb_biz[7:9], select, cond="anxious", estimate__, upper__, lower__),
  #                      .id="index") %>% 
  #   mutate(var="General Anxiety")
  # 
  # fb_fx <- bind_rows(finance,contact,anxious) %>% 
  #   mutate(index=case_when(grepl(x=index,pattern="biz")~"DV: Business",
  #                          grepl(x=index,pattern="sd")~"DV: Distancing",
  #                          grepl(x=index,pattern="school")~"DV: School"))
  # 
  # 
  # 
  # label_value2 = function(labels) {
  #   label_value(labels = labels, multi_line = FALSE)
  # }
  # 
  # fb_fx %>% 
  #   ggplot(aes(y=estimate__,x=cond)) +
  #   geom_ribbon(aes(ymin=lower__,
  #                   ymax=upper__),alpha=0.5) +
  #   geom_line(colour="blue") +
  #   theme_tufte() +
  #   scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
  #   facet_grid(cols = vars(var),rows = vars(index),labeller = label_value2,scales = "free_x") +
  #   labs(x="Weighted Percent Reporting via Facebook Survey",
  #        y="Predicted Index Score",
  #        caption=str_wrap("Estimates are predictions with other variables held at their means. Shaded intervals are the 5% to 95% posterior quantiles.",
  #                         width=75))
  # 
  # ggsave("plots/fb_fx.png")
  # ggsave("plots/extended_data_figure_5.pdf")
  # 
  # 
  # 
  # # Cross-section Predictions -----------------------------------------------
  # 
  # cs_all <- conditional_effects(mult_mod,c("humanrights","polity","trade"))
  # 
  # 
  # days <- bind_rows(lapply(cs_all[1:3], select, cond="humanrights", estimate__, upper__, lower__),
  #                      .id="index") %>% 
  #   mutate(var="Human Rights")
  # 
  # polity <- bind_rows(lapply(cs_all[4:6], select, cond="polity", estimate__, upper__, lower__),
  #                      .id="index") %>% 
  #   mutate(var="Polity IV (Democracy)")
  # 
  # trade <- bind_rows(lapply(cs_all[7:9], select, cond="trade", estimate__, upper__, lower__),
  #                      .id="index") %>% 
  #   mutate(var="Trade % GDP")
  # 
  # cs_fx <- bind_rows(days,trade,polity) %>% 
  #   mutate(index=case_when(grepl(x=index,pattern="biz")~"DV: Business",
  #                          grepl(x=index,pattern="sd")~"DV: Distancing",
  #                          grepl(x=index,pattern="school")~"DV: School"))
  # 
  # 
  # label_value2 = function(labels) {
  #   label_value(labels = labels, multi_line = FALSE)
  # }
  # 
  # cs_fx %>% 
  #   ggplot(aes(y=estimate__,x=cond)) +
  #   geom_ribbon(aes(ymin=lower__,
  #                   ymax=upper__),alpha=0.5) +
  #   geom_line(colour="blue") +
  #   theme_tufte() +
  #   #scale_x_continuous(labels=scales::percent_format(accuracy=1)) +
  #   facet_grid(cols = vars(var),rows = vars(index),labeller = label_value2,scales = "free_x") +
  #   labs(x="Weighted Percent Reporting via Facebook Survey",
  #        y="Predicted Index Score",
  #        caption=str_wrap("Estimates are predictions with other variables held at their means. Shaded intervals are the 5% to 95% posterior quantiles.",
  #                         width=75))
  # 
  # ggsave("cs_fx.png")
  # ggsave("plots/extended_data_figure_6.pdf")
  
  
}





