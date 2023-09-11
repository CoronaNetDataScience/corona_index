# run cross-sectional and time-varying models
#.libPaths("~/other_R_libs2")

require(cmdstanr)

#set_cmdstan_path("/home/rmk7/cmdstan")

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

# run everything from scratch

load_data <- F
run_mod <- F
mult_pull <- F

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
  fill(med_est,sd_est,high_est,low_est,.direction="down") %>% 
  mutate(modtype=recode(modtype,
                        biz="Business",
                        hm2="Health Monitoring",
                        hr="Health Resources",
                        masks="Masks",
                        school="Schools",
                        sd="Social Distancing")) %>% 
  rename(med_estimate="med_est",
         low_estimate="low_est",
         high_estimate="high_est",
         SD_estimate="sd_est",
         index="modtype")

all_mods_data_scaled <- lapply(list.files("indices/","time\\_data\\_scaled\\.rds",
                                   full.names = T),readRDS) %>% 
  bind_rows(.id="modtype")


all_mods_data_scaled$modtype <- fct_recode(all_mods_data_scaled$modtype,
                                    "biz" = "1",
                                    "hm2" = "2",
                                    "hr" = "3",
                                    "masks" = "4",
                                    "school" = "5",
                                    "sd" = "6")

# expand and fill

expand_mod_scaled <- group_by(all_mods_data_scaled,country,modtype) %>% 
  expand(date_policy=seq(ymd("2020-01-01"),max(all_mods_data$date_policy),
                         by="1 day"))

all_mods_data_scaled <- left_join(expand_mod_scaled,all_mods_data_scaled,by=c("country","modtype","date_policy")) %>% 
  group_by(country,modtype) %>% 
  arrange(country,date_policy) %>% 
  fill(med_est,sd_est,high_est,low_est,.direction="down") %>% 
  mutate(modtype=recode(modtype,
                        biz="Business",
                        hm2="Health Monitoring",
                        hr="Health Resources",
                        masks="Masks",
                        school="Schools",
                        sd="Social Distancing")) %>% 
  rename(med_estimate="med_est",
         low_estimate="low_est",
         high_estimate="high_est",
         SD_estimate="sd_est",
         index="modtype")

# create tables in AWS

con <-   dbConnect(RPostgres::Postgres(), user = "corona",
                   password = "Publi$$This", port = 5432,
                   dbname = "master",
                   host = "niehaususer.ccecwurg6k9l.us-east-2.rds.amazonaws.com")

dbWriteTable(con, name=Id(schema="external",table="policy_intensity"), all_mods_data,
             overwrite=T)
dbWriteTable(con, name=Id(schema="external",table="policy_intensity_scaled"), 
             all_mods_data_scaled,
             overwrite=T)

dbGetQuery(con, "grant select on external.policy_intensity to web_anon;")

dbGetQuery(con, "grant select on external.policy_intensity_scaled to web_anon;")

