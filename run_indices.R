# Run a set of indices
# Using idealdstan as the back-end
# Robert Kubinec
# December 15th, 2020

# Assumes Oxford Tracker is at user home directory
# clone from https://github.com/OxCGRT/covid-policy-tracker.git

.libPaths("/home/rmk7/other_R_libs")
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

# the type of index we are creating

model_type <- Sys.getenv("MODELTYPE")
nchains <- Sys.getenv("NCHAINS")
time <- Sys.getenv("TIME")

compile_data <- F

# recompile/reload data (takes a while to aggregate policy counts)

if(compile_data) {
  source("RCode/ag_dataset.R")
} else {
  index_long <- readRDS(paste0("coronanet/index_long_model_",model_type,".rds"))
}

source("create_items_long.R")

filter_list <- switch(model_type,
                      sd=sd_items,
                      biz=biz_items,
                      ht=ht_items,
                      hm=hm_items,
                      mask=mask_items,
                      hr=hr_items,
                      school=school_items)

# which items to restrict for each model -- generally just one up

restrict_list <- switch(model_type,
                        sd=c("social_distance","number_mass"),
                        biz=c("biz_essential","biz_mask"),
                        ht=c("ht_type_pcr","ht_portal_sms"),
                        hm=c("hm_home_visit","hm_telephone"),
                        mask=c("mask_public","mask_transport"),
                        hr=c("hr_ventilator","hr_syringe"),
                        school=c("primary_school","school_distance"))

  

  countries <- c("United States of America","Germany","Brazil","Switzerland","Israel","France",
                 #"Italy","Argentina","Brazil","Russia","United Kingdom",
                 #"Nigeria","Egypt","United Arab Emirates",
                 "Norway","Venezuela")
  
  to_make <- index_long %>% 
    filter(item %in% filter_list) %>% 
    #filter(country %in% countries) %>% 
           #date_policy<ymd("2020-05-01")) %>% 
    group_by(item) %>% 
    mutate(model_id=case_when(grepl(x=item,pattern="ox")~5,
                              TRUE~9),
           var_cont=ifelse(model_id>5,pop_out,0)) %>% 
    group_by(item) %>% 
    mutate(var=ifelse(model_id==5 & min(var,na.rm=T)==0,var+1,var),
           min_item=ifelse(model_id==9,min(var_cont,na.rm=T),
                           min(var,na.rm=T))) %>% 
    ungroup %>% 
    mutate(ra_num=as.numeric(scale(ra_num))) %>% 
    group_by(item) %>% 
    mutate(var=ifelse(is.na(var),min(var,na.rm=T),var),
           var_cont=ifelse(is.na(var_cont),min(var_cont,na.rm=T),var_cont)) %>% 
    group_by(country,item,date_policy) %>% 
    mutate(n_dup=n()) %>% 
    ungroup
  
  # check for unique values
  
  un_vals <- group_by(to_make,item) %>% 
    summarize(n_un=length(unique(var_cont)),
              model_id=model_id[1])
  
  # convert to binary if number of unique values less than 20
  
  to_make <- mutate(ungroup(to_make), model_id=case_when(item %in% un_vals$item[un_vals$model_id==9 & un_vals$n_un<20] ~ 1,
                                                TRUE~model_id),
                    var=case_when(item %in% un_vals$item[un_vals$model_id==9 & un_vals$n_un<20] ~ round(var),
                                  TRUE~var))
  
  # check country scores
  
  country_score <- group_by(to_make,country,date_policy) %>% 
    summarize(score=sum(var[model_id!=9]) + sum(var_cont[model_id==9])) %>% 
    group_by(country) %>% 
    mutate(var_out=var(score)) %>% 
    group_by(country) %>% 
    arrange(country,date_policy) %>% 
    mutate(score_diff=score - lag(score))
  
  # filter out no changes
  
  check_items <- group_by(to_make,country,date_policy) %>% 
    mutate(all_min=all(ifelse(model_id==9,min_item==var_cont,
                                 min_item==var))) %>% 
    group_by(item) %>% 
    mutate(all_equal=ifelse(model_id==9,length(unique(var_cont))==1,
                                length(unique(var))==1))
  
  to_ideal <- to_make %>% 
    mutate(var=as.integer(var)) %>% 
    filter(country!="Samoa") %>% 
    distinct %>% 
            id_make(
            outcome_disc="var",
            outcome_cont="var_cont",
            person_id="country",
            item_id="item",time_id="date_policy")
  
  # note no missing data :)
  
  # determine cores
  
  if(floor(length(unique(to_ideal@score_matrix$person_id))/parallel::detectCores())<1) {
    
    grainsize <- 1
    
  } else {
    
    grainsize <- floor(length(unique(to_ideal@score_matrix$person_id))/parallel::detectCores())
  }
  print(nchains)
  print(grainsize)
  activity_fit <- id_estimate(to_ideal,vary_ideal_pts=time,
                              ncores=parallel::detectCores(),
                              nchains=as.numeric(nchains),niters=400,
                              warmup=300,grainsize = grainsize,
                              within_chain="threads",
                              gpu=FALSE,save_files = "/scratch/rmk7/coronanet",
                              fixtype="prefix",pos_discrim = F,
                              restrict_ind_high=restrict_list[1],
                              restrict_ind_low=restrict_list[2],
                              restrict_sd_low=3,
                              mpi_export=getwd(),
                              map_over_id = "persons",
                              max_treedepth=10,het_var = T,
                              fix_high=1,
                              fix_low=0,
                              restrict_var = T,time_center_cutoff = 50,
                              time_sd=.1,
                              restrict_sd_high=.0001,
                              id_refresh = 100,
                              const_type="items") 
  
  saveRDS(activity_fit,paste0("/scratch/rmk7/coronanet/activity_fit_rw",model_type,"_",time,".rds"))
  
  get_all_discrim <- filter(activity_fit@summary,grepl(x=variable,pattern="reg\\_full"))
  
  get_all_discrim$id <- levels(activity_fit@score_data@score_matrix$item_id)
  
  get_all_discrim %>% 
    ggplot(aes(y=mean,x=reorder(id,mean))) +
    geom_pointrange(aes(ymin=lower,ymax=upper)) +
    theme_tufte() +
    coord_flip() +
    labs(x="Items",y="Level of Discrimination") +
    ggtitle("Discrimination parameters from model")
  
  ggsave(paste0("/scratch/rmk7/coronanet/discrim_",model_type,"_",time,".png"))
  
  id_plot_legis_dyn(activity_fit,use_ci=T) + ylab(paste0(model_type," Index")) + guides(color="none") +
    ggtitle("CoronaNet Social Distancing Index",
            subtitle="Posterior Median Estimates with 5% - 95% Intervals")
  
  ggsave(paste0("/scratch/rmk7/coronanet/index_",model_type,"_",time,".png"))
  
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
  
  write_csv(country_est,paste0("/scratch/rmk7/coronanet/",model_type,"_",time,"_index_est.csv"))

    
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
