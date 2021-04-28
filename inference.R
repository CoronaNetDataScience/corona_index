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
require(kableExtra)

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
                           "gini"="Gini Index"),
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

