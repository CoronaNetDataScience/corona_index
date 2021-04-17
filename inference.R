# run cross-sectional and time-varying models

require(dplyr)
require(tidyr)
require(forcats)
require(ggthemes)
require(ggplot2)
require(readr)
require(missRanger)
require(brms)


load_data <- F

# load time-varying estimates

all_mods_data <- lapply(list.files("indices/","time\\_data\\.rds",
                                   full.names = T),readRDS) %>% 
  bind_rows(.id="modtype")


all_mods_data$modtype <- fct_recode(all_mods_data$modtype,
                                    "biz" = "1",
                                    "hm2" = "2",
                                    "hr" = "3",
                                    "masks" = "5",
                                    "school" = "6",
                                    "sd" = "7"
)

all_mods_med <- select(all_mods_data,country,date_policy,modtype,med_est) %>% 
  mutate(modtype=paste0("med_",modtype)) %>% 
  fill(med_est,.direction="down") %>% 
  spread(key="modtype",value="med_est")

all_mods_sd <- select(all_mods_data,country,date_policy,modtype,sd_est) %>% 
  mutate(modtype=paste0("sd_",modtype)) %>% 
  fill(sd_est,.direction="down") %>% 
  spread(key="modtype",value="sd_est")

if(load_data) {
  
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
  
  combine_data <- left_join(time_vary_fb,google,by=c("country","date_policy")) %>% 
    left_join(cross_sections,by="country") %>% 
    left_join(all_mods_med,by=c("country","date_policy"))
  
  # need to merge in indices to impute
  
  # impute 5 big ones
  
  over_five <- lapply(1:5,function(i) {
    
    missRanger(combine_data,pmm.k=10L)
  })
  
  saveRDS(over_five,"indices/over_five.rds")
  
}

# bind and merge

combine_dv <- bind_rows(over_five,.id="imputed") %>% 
  left_join(all_mods_med,
                        by=c("country","date_policy")) %>% 
  left_join(all_mods_sd,
            by=c('country',"date_policy"))

# try out a combined model

biz_mod <- brm(med_biz | mi(sd_biz) ~ fdi + trade + finance + state_fragility + bureaucracy_corrupt,
              data=combine_dv,
              backend="cmdstanr",
              chains=1,threads=threading(4),
              iter=500,
              cores=3)




