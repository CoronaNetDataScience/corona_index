# Run a set of indices
# Using idealdstan as the back-end
# Robert Kubinec
# December 15th, 2020

# Assumes Oxford Tracker is at user home directory
# clone from https://github.com/OxCGRT/covid-policy-tracker.git

# the type of index we are creating

model_type <- Sys.getenv("MODELTYPE")
nchains <- Sys.getenv("NCHAINS")
time <- Sys.getenv("TIME")
run <- Sys.getenv("RUN")

libpaths <- switch(model_type,
                      sd="/home/rmk7/other_R_libs_cor1",
                      biz="/home/rmk7/other_R_libs_cor2",
                      ht="/home/rmk7/other_R_libs_cor3",
                      hm="/home/rmk7/other_R_libs_cor4",
                   hm2="/home/rmk7/other_R_libs_cor4",
                      mask="/home/rmk7/other_R_libs_cor5",
                      hr="/home/rmk7/other_R_libs_cor6",
                      school="/home/rmk7/other_R_libs_cor7")

.libPaths(libpaths)
cmdstanr::set_cmdstan_path("/home/rmk7/cmdstan")
  
require(idealstan)
require(ggplot2)
require(tidyr)
require(dplyr)
require(lubridate)
require(readr)
require(stringr)
require(readxl)
require(ggthemes)

compile_data <- F

# recompile/reload data (takes a while to aggregate policy counts)

if(compile_data) {
  source("RCode/ag_dataset.R")
} else {
  
  if(model_type=="hm2") {
    
    # collapse categories
    
    index_long <- bind_rows(filter(readRDS("coronanet/index_long_model_hm.rds")),
                            filter(readRDS("coronanet/index_long_model_ht.rds"),
                                           item!="ox_test",
                                           !(country %in% c("European Union",
                                                            "Liechtenstein"))))
    
  } else {
    
    index_long <- readRDS(paste0("coronanet/index_long_model_",model_type,".rds"))
  }

}

source("create_items_long.R")

filter_list <- switch(model_type,
                      sd=sd_items,
                      biz=biz_items,
                      ht=ht_items,
                      hm=hm_items,
                      hm2=hm2_items,
                      mask=mask_items,
                      hr=hr_items,
                      school=school_items)

# whether to use boundary-avoiding prior

if(model_type %in% c("ht","hm","hm2","hr")) {
  
  boundary_prior <- list(beta=5)
  
  max_treedepth <- 12
  
} else if(model_type=="mask") {
  
  boundary_prior <- list(beta=5)
  
  max_treedepth <- 12
  
} else {
  
  max_treedepth <- 12
  
  boundary_prior <- list(beta=5)
  
}

# which items to restrict for each model -- generally just one up

restrict_list <- switch(model_type,
                        sd=c("social_distance","number_mass"),
                        biz=c("biz_hours","biz_meeting"),
                        ht=c("ht_type_pcr","ht_portal_sms"),
                        hm=c("hm_home_visit","hm_telephone"),
                        hm2=c("ht_type_pcr","ht_portal_sms"),
                        mask=c("mask_public","mask_transport"),
                        hr=c("hr_ventilator","hr_syringe"),
                        school=c("primary_school","school_clean"))

  #pos_discrim <- model_type %in% c("biz","mask","hm")

  

  # countries <- c("United States of America","Germany","Brazil","Switzerland","Israel","France",
  #                #"Italy","Argentina","Brazil","Russia","United Kingdom",
  #                #"Nigeria","Egypt","United Arab Emirates",
  #                "Norway","Venezuela")
  
to_make <- index_long %>% 
  filter(item %in% filter_list) %>% 
  #filter(country %in% countries) %>% 
  #date_policy<ymd("2020-05-01")) %>% 
  group_by(item) %>% 
  mutate(model_id=case_when(item=="ox_health_invest"~9,
                            model_type=="sd" & grepl(x=item,pattern="ox")~5,
                            grepl(x=item,pattern="ox")~3,
                            TRUE~9),
         var_cont=ifelse(model_id>5,pop_out,0)) %>% 
  group_by(item) %>% 
  mutate(var=ifelse(model_id %in% c(3,5) & min(var,na.rm=T)==0,var+1,var),
         min_item=ifelse(model_id==9,min(var_cont,na.rm=T),
                         min(var,na.rm=T))) %>% 
  ungroup %>% 
  mutate(ra_num=as.numeric(scale(ra_num))) %>% 
  group_by(item) %>% 
  mutate(var=ifelse(is.na(var) & !grepl(x=item,pattern="ox"),min(var,na.rm=T),var),
         var_cont=ifelse(is.na(var_cont) & item!="ox_health_invest",min(var_cont,na.rm=T),var_cont),
         var_cont=ifelse(item=="ox_health_invest",as.numeric(scale(var_cont)),var_cont)) %>% 
  group_by(country,item,date_policy) %>% 
  mutate(n_dup=n()) %>% 
  ungroup
  
  # check for unique values
  
un_vals <- group_by(to_make,item) %>% 
  summarize(n_un=length(unique(var_cont)),
            n_vals=sum(var>0)/n(),
            model_id=model_id[1])

# convert to binary if number of unique values less than 20

to_make <- group_by(to_make,item) %>% 
  mutate(model_id=case_when((item %in% un_vals$item[un_vals$model_id==9 & un_vals$n_vals<.1]) & max(var_cont)<1.5 ~ 1,
                            TRUE~model_id),
         var=case_when((item %in% un_vals$item[un_vals$model_id==9 & un_vals$n_vals<.1])  & max(var_cont)<1.5 ~ round(var_cont),
                       TRUE~var))
  
  # non-zero entries
  
  sum(to_make$var!=0)
  
  # check country scores
  
  country_score <- group_by(to_make,country,date_policy) %>% 
    summarize(score=sum(var[model_id!=9]) + sum(var_cont[model_id==9])) %>% 
    group_by(country) %>% 
    mutate(var_out=var(score)) %>% 
    group_by(country) %>% 
    arrange(country,date_policy) %>% 
    mutate(score_diff=score - lag(score))
  
  #country_score %>% ggplot(aes(y=score_diff,x=date_policy)) + geom_line(aes(group=country))
  
  sum(country_score$score_diff>0,na.rm=T)/length(unique(to_make$country))
  
  
  # countries that show no change over time
  
  no_change <- ungroup(country_score) %>% 
    group_by(country) %>% 
    filter(all(score_diff==0,na.rm=T)) %>% 
    distinct(country)
  
  low_change <- ungroup(country_score) %>% 
    group_by(country) %>% 
    filter(sum(score_diff[score_diff>0],na.rm=T)<3) %>% 
    distinct(country)
  
  days_no_change <- group_by(country_score,date_policy) %>% 
    filter(all(score_diff==0,na.rm=T)) %>% 
    distinct(date_policy) %>% 
    filter(date_policy>ymd("2020-01-01"))
  
  
  # filter out no changes
  
  check_items <- group_by(to_make,item) %>% 
    summarize(n_country_cont=length(unique(country[(var_cont>0)])),
              n_country_var=length(unique(country[(var>0)])))
  
  # if masks, remove countries with no changes
  
  if(model_type=="mask") {
    
    to_make <- anti_join(to_make,no_change, by="country")

  }
  
  # remove countries that aren't in the Oxford data
  
  to_ideal <- to_make %>% 
    anti_join(days_no_change,by="date_policy") %>% 
    distinct %>% 
    mutate(var=as.integer(var)) %>% 
    filter(!(country %in% c("Samoa","Solomon Islands","Saint Kitts and Nevis",
                            "Liechtenstein","Montenegro","Northern Cyprus",
                            "North Macedonia","Nauru","Equatorial Guinea",
                            "Luxembourg","Malta","North Korea")),
           date_policy < ymd("2021-01-15"),
           !(item %in% c("allow_ann_event","postpone_rec_event","mask_preschool"))) %>% 
    distinct %>% 
            id_make(
            outcome_disc="var",
            outcome_cont="var_cont",
            person_id="country",
            item_id="item",time_id="date_policy")
  
  # note no missing data :)
  
  # determine cores
  
  # if(floor(length(unique(to_ideal@score_matrix$person_id))/parallel::detectCores())<1) {
  #   
  #   grainsize <- 1
  #   
  # } else {
  #   
  #   grainsize <- floor(length(unique(to_ideal@score_matrix$person_id))/parallel::detectCores())
  # }
  
  grainsize <- 1
  print(model_type)
  print(nchains)
  print(grainsize)
  activity_fit <- to_ideal %>% 
                    id_estimate(vary_ideal_pts=time,
                              ncores=parallel::detectCores(),
                              nchains=as.numeric(nchains),niters=400,
                              save_warmup=TRUE,
                              warmup=500,grainsize = grainsize,
                              boundary_prior=boundary_prior,
                              gpu=FALSE,save_files = "/scratch/rmk7/coronanet_csvs",
                              fixtype="prefix",pos_discrim = F,
                              restrict_ind_high=restrict_list[1],
                              restrict_ind_low=restrict_list[2],
                              restrict_sd_low=3,
                              map_over_id = "persons",
                              #adapt_delta=0.95,
                              max_treedepth=max_treedepth,het_var = T,
                              fix_high=1,
                              fix_low=0,
                              restrict_var = (model_type!="mask"),time_center_cutoff = 50,
                              time_sd=.1,
                              restrict_sd_high=.001,
                              id_refresh = 100,
                              const_type="items") 
  
  saveRDS(activity_fit,paste0("/scratch/rmk7/coronanet/activity_fit_rw",model_type,"_",time,"_run_",run,".rds"))
  
  get_all_discrim <- filter(activity_fit@summary,grepl(x=variable,pattern="reg\\_full"))
  
  get_all_discrim$id <- levels(activity_fit@score_data@score_matrix$item_id)
  
  get_all_discrim %>% 
    ggplot(aes(y=mean,x=reorder(id,mean))) +
    geom_pointrange(aes(ymin=lower,ymax=upper)) +
    theme_tufte() +
    coord_flip() +
    labs(x="Items",y="Level of Discrimination") +
    ggtitle("Discrimination parameters from model")
  
  ggsave(paste0("/scratch/rmk7/coronanet/discrim_",model_type,"_",time,"_run_",run,".png"))
  
  id_plot_legis_dyn(activity_fit,use_ci=T) + ylab(paste0(model_type," Index")) + guides(color="none") +
    ggtitle("CoronaNet Social Distancing Index",
            subtitle="Posterior Median Estimates with 5% - 95% Intervals")
  
  ggsave(paste0("/scratch/rmk7/coronanet/index_",model_type,"_",time,"_run_",run,".png"))
  
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
  country_est <- as_tibble(activity_fit@time_varying) %>% 
    mutate(iter=1:n()) %>% 
    gather(key="param",value="estimate",-iter) %>% 
    mutate(estimate=plogis(scale(estimate))*100) %>% 
    group_by(param) %>% 
    summarize(median_est=median(estimate),
              high_est=quantile(estimate,.95),
              low_est=quantile(estimate,.05))
  
  country_est <- mutate(country_est,
                        date_policy=as.numeric(str_extract(param,"(?<=\\[)[0-9]+")),
                        country=as.numeric(str_extract(param,"[0-9]+(?=\\])")),
                        date_policy=unique(activity_fit@score_data@score_matrix$time_id)[date_policy],
                        country=levels(activity_fit@score_data@score_matrix$person_id)[country]) %>% 
    select(country,date_policy,distancing_index_consensus_est="median_est",
           distancing_index_low_est="low_est",
           distancing_index_high_est="high_est")
  
  write_csv(country_est,paste0("/scratch/rmk7/coronanet/",model_type,"_",time,"_run_",run,"_index_est.csv"))

    
    # country_names <- read_xlsx("data/ISO WORLD COUNTRIES.xlsx",sheet = "ISO-names")
    # 
    # clean_comp <- mutate(clean_comp,
    #                      country=recode(country,Czechia="Czech Republic",
    #                                     `Hong Kong`="China",
    #                                     Macau="China",
    #                                     `United States`="United States of America",
    #                                     `Bahamas`="The Bahamas",
    #                                     `Tanzania`="United Republic of Tanzania",
    #                                     `North Macedonia`="Macedonia",
    #                                     `Micronesia`="Federated States of Micronesia",
    #                                     `Timor Leste`="East Timor",
    #                                     `Republic of the Congo`="Republic of Congo",
    #                                     `Cabo Verde`="Cape Verde",
    #                                     `Eswatini`="Swaziland",
    #                                     `Serbia`="Republic of Serbia",
    #                                     `Guinea-Bissau`="Guinea Bissau"))
    # 
    # # merge in ISOs
    # 
    # clean_comp <- left_join(clean_comp, country_names,by=c(country="ADMIN"))
    # 
    # # filter out the EU
    # 
    # clean_comp <- filter(clean_comp, !is.na(ISO_A3))
    # 
    # # merge deaths/cases
    # ecdc$dateRep <- as_date(ecdc$dateRep)
    # clean_comp <- left_join(clean_comp,ecdc,by=c("ISO_A3"="countryterritoryCode","date_start"="dateRep"))
    # 
    # # filter out some tiny islands
    # clean_comp <- group_by(clean_comp) %>% 
    #   filter(!all(is.na(cases)))
    # # assume no reported cases = 0


  
  # activity_fit <- as_cmdstan_fit("corona_distance_1.csv")
  # 
  # rstan_obj <- rstan::read_stan_csv("corona_distance_1.csv")
  # 
  # out_array <- activity_fit@stan_samples$post_warmup_draws
  # class(out_array) <- "array"
  #launch_shinystan(as.shinystan(out_array))





# all_lev <- unique(to_make$country)
# 
# # get country-level estimates
# 
# country_est <- filter(activity_fit@summary,grepl(x=variable,pattern="AR"))

# get_stan_mods <- lapply(activity_fit,function(s) s@stan_samples)
# 
# combine_stan_mods <- rstan::sflist2stanfit(get_stan_mods)
# 
# activity_fit[[1]]@stan_samples <- combine_stan_mods 
# 
# saveRDS(activity_fit2,"data/activity_fit_collapse.rds")



# convert to 100 points



# country_est %>% 
#   ggplot(aes(y=distancing_index_consensus_est,
#              x=date_policy)) +
#   geom_ribbon(aes(ymin=distancing_index_low_est,
#                   ymax=distancing_index_high_est),
#               colour="tan",
#               alpha=0.3) +
#   geom_line(aes(group=country),colour="blue") +
#   theme_tufte()
