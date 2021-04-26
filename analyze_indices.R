# load, plot and analyze indices

require(idealstan)
require(ggplot2)
require(tidyverse)
require(ggthemes)
require(cmdstanr)
require(posterior)
require(stringr)
require(posterior)

plot_countries <- c("United States of America","Brazil","China","United Arab Emirates",
                    "South Africa","Turkey","Singapore","South Korea",
                    "Australia")

source('apply_draws.R')
source('summarize_draws_mc.R')


# Business ----------------------------------------------------------------

biz_mod <- readRDS("coronanet/activity_fit_rwbiz_random_walk_run_2.rds")

all_mods <- lapply(list.files(path = "coronanet/",
                              pattern="biz\\_random\\_walk\\_run\\_[1-4].*rds",
                              full.names=T),readRDS)

all_mods_mat <-  lapply(all_mods, function(c) c@stan_samples$draws())

biz_draws <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))

biz_sum <- summarize_draws_mc(biz_draws,"median","quantile2",posterior::rhat)

all_mods_mat <-  lapply(all_mods, function(c) as_draws_array(c@time_varying))

rm(all_mods)

biz_time <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))

biz_time <- apply_draws(biz_time,FUN=function(c) as.numeric(scale(c)),MARGIN=c(1:2))

sum_time <- summarize_draws_mc(biz_time,"median","quantile2",posterior::rhat)

rm(all_mods_mat)

gc()

get_all_discrim <- filter(biz_sum,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(biz_mod@score_data@score_matrix$item_id)

get_all_discrim$id_rec <- fct_recode(get_all_discrim$id,
                                     "Other Regulations" = "biz_cond_other",
                                     "Contact Tracing" = "biz_cont_trace",
                                     "Essential Businesses" = "biz_essential",
                                     "Health Certification" = "biz_health_cert",
                                     "Health Questionnaire" = "biz_health_q",
                                     "Hours Restricted" = "biz_hours",
                                     "Hygiene Requirements" = "biz_hygiene",
                                     "Masks" = "biz_mask",
                                     "Meetings Restricted" = "biz_meeting",
                                     "Non-essential Businesses" = "biz_nonessential",
                                     "Number of Customers" = "biz_num_cust",
                                     "All Sectors" = "biz_restrict_all",
                                     "Commercial" = "biz_restrict_comm",
                                     "Construction" = "biz_restrict_construct",
                                     "Agriculture" = "biz_restrict_farm",
                                     "Finance" = "biz_restrict_finance",
                                     "Grocery" = "biz_restrict_grocery",
                                     "Grooming" = "biz_restrict_groom",
                                     "Health Care" = "biz_restrict_health",
                                     "Hotels" = "biz_restrict_hotel",
                                     "ICT" = "biz_restrict_info",
                                     "Insurance" = "biz_restrict_insurance",
                                     "Mining" = "biz_restrict_mining",
                                     "No Sector" = "biz_restrict_na",
                                     "Other Sector" = "biz_restrict_other",
                                     "Pharmacy" = "biz_restrict_pharmacy",
                                     "Publishing" = "biz_restrict_publish",
                                     "Restaurants" = "biz_restrict_rest",
                                     "Retail" = "biz_restrict_retail",
                                     "Shops" = "biz_restrict_shop",
                                     "Delivery"="biz_delivery",
                                     "Takeout"="biz_takeaway",
                                     "Telecom" = "biz_restrict_telecom",
                                     "Transport" = "biz_restrict_transport",
                                     "Warehouses" = "biz_restrict_warehouse",
                                     "Water" = "biz_restrict_water",
                                     "Social Distancing" = "biz_social_distance",
                                     "Store Size" = "biz_store_size",
                                     "Temperature Checks" = "biz_temp",
                                     "Work at Home" = "biz_work_home",
                                     "Oxford Closing Workplaces" = "ox_workplace_close")

biz_rhat <- sum_time %>% 
  #filter(grepl(x=variable,pattern="sigma\\_reg|L\\_tp1")) %>% 
  ggplot(aes(x=`posterior::rhat`)) +
  geom_histogram() +
  geom_vline(xintercept=1.2,linetype=2,colour="blue") +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("Business Restrictions")

saveRDS(biz_rhat,"coronanet/biz_rhat.rds")

biz <- get_all_discrim %>% 
  ggplot(aes(y=median,x=reorder(id_rec,median))) +
  geom_pointrange(aes(ymin=q5,ymax=q95)) +
  theme_tufte() +
  geom_hline(yintercept=1,linetype=2) +
  coord_flip() +
  labs(x="",y="Level of Discrimination") +
  ggtitle("Business")

biz

saveRDS(biz,"coronanet/biz_discrim_object.rds")

ggsave("plots/biz_discrim.png")

biz_time_data_scaled <- biz_time %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  group_by(.iteration) %>% 
  mutate(estimate=plogis(estimate)*100,
         country=levels(biz_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(biz_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

biz_time_data <- biz_time %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(country=levels(biz_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(biz_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

# make it complete

expand_index <- group_by(biz_time_data,country) %>% 
  expand(date_policy=seq(ymd("2020-01-01"),max(biz_time_data$date_policy),
                         by="1 day"))

biz_time_data <- left_join(expand_index,biz_time_data,
                           by=c("country","date_policy")) %>% 
  fill(med_est,high_est,low_est,sd_est,.direction="downup")

biz_time_data_scaled <- left_join(expand_index,biz_time_data_scaled,
                           by=c("country","date_policy")) %>% 
  fill(med_est,high_est,low_est,sd_est,.direction="downup")

saveRDS(biz_time_data,"indices/biz_time_data.rds")
saveRDS(biz_time_data_scaled,"indices/biz_time_data_scaled.rds")
write_csv(biz_time_data,"indices/biz_time_data.csv")
write_csv(biz_time_data_scaled,"indices/biz_time_data_scaled.csv")

sample_plot_dates <- group_by(biz_time_data_scaled,country) %>% 
  sample_n(1)

biz_time <- biz_time_data_scaled %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="#FFFFB3",fontface="bold",
            data=sample_plot_dates,check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("Business Restrictions") + 
  theme(axis.text.x=element_blank())

biz_time

saveRDS(biz_time,"coronanet/biz_mod_plot_object.rds")

ggsave("plots/biz_mod_plot.png")

biz_time_single <- biz_time_data_scaled %>% 
  filter(country %in% plot_countries) %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="black",fontface="bold",
            data=filter(sample_plot_dates,country %in% plot_countries),
            check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("Business Restrictions") + 
  theme(axis.text.x=element_blank())

biz_time_single

saveRDS(biz_time_single,"coronanet/biz_mod_plot_single_object.rds")

ggsave("plots/biz_mod_plot_single.png")

rm(biz_mod)


# Mask --------------------------------------------------------------------

# mask_mod <- readRDS("coronanet/activity_fit_rwmask_random_walk_run_1.rds")
# 
# all_mods <- lapply(list.files(path = "coronanet/",
#                               pattern="mask\\_random\\_walk\\_run\\_[0-4].*rds",
#                               full.names=T),readRDS)
# 
# all_mods_mat <-  lapply(all_mods, function(c) c@stan_samples$draws())
# 
# mask_draws <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))
# 
# mask_sum <- summarize_draws_mc(mask_draws,"median","quantile2",posterior::rhat)
# 
# all_mods_mat <-  lapply(all_mods, function(c) as_draws_array(c@time_varying))
# 
# rm(all_mods)
# 
# mask_time <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))
# 
# mask_time <- apply_draws(mask_time,FUN=function(c) as.numeric(scale(c)),MARGIN=c(1:2))
# 
# sum_time <- summarize_draws_mc(mask_time,"median","quantile2",posterior::rhat)
# 
# rm(all_mods_mat)
# 
# gc()
# 
# get_all_discrim <- filter(mask_sum,grepl(x=variable,pattern="reg\\_full"))
# 
# get_all_discrim$id <- levels(mask_mod@score_data@score_matrix$item_id)
# 
# get_all_discrim$id_rec <- fct_recode(get_all_discrim$id,
#                                      "Businesses" = "mask_business",
#                                      "Everywhere" = "mask_everywhere",
#                                      "Higher Ed" = "mask_higher_ed",
#                                      "Preschool" = "mask_preschool",
#                                      "Primary School" = "mask_primary_school",
#                                      "In Public" = "mask_public",
#                                      "Secondary School" = "mask_sec_school",
#                                      "Mass Transport" = "mask_transport",
#                                      "Unspecified Conditions" = "mask_unspec",
#                                      "Oxford Mask" = "ox_mask"
# )
# 
# mask_rhat <- sum_time %>% 
#   #filter(grepl(x=variable,pattern="sigma\\_reg|L\\_tp1")) %>% 
#   ggplot(aes(x=`posterior::rhat`)) +
#   geom_histogram() +
#   geom_vline(xintercept=1.2,linetype=2,colour="blue") +
#   theme_tufte() +
#   labs(x="",y="") +
#   ggtitle("Masks")
# 
# 
# saveRDS(mask_rhat,"coronanet/mask_rhat.rds")
# 
# mask <- get_all_discrim %>% 
#   ggplot(aes(y=median,x=reorder(id_rec,median))) +
#   geom_pointrange(aes(ymin=q5,ymax=q95)) +
#   theme_tufte() +
#   geom_hline(yintercept=1,linetype=2) +
#   coord_flip() +
#   labs(x="",y="Level of Discrimination") +
#   ggtitle("Masks")
# 
# mask
# 
# saveRDS(mask,"coronanet/mask_discrim_object.rds")
# 
# ggsave("plots/mask_discrim.png")
# 
# mask_time_data_scaled <- mask_time %>% as_draws_df() %>% 
#   gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
#   group_by(.iteration) %>% 
#   mutate(estimate=plogis(estimate)*100,
#          country=levels(mask_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
#          date_policy=unique(mask_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
#   group_by(date_policy,country) %>% 
#   summarize(med_est=quantile(estimate,.5),
#             high_est=quantile(estimate,.95),
#             low_est=quantile(estimate,.05),
#             sd_est=sd(estimate))
# 
# mask_time_data <- mask_time %>% as_draws_df() %>% 
#   gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
#   mutate(country=levels(mask_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
#          date_policy=unique(mask_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
#   group_by(date_policy,country) %>% 
#   summarize(med_est=quantile(estimate,.5),
#             high_est=quantile(estimate,.95),
#             low_est=quantile(estimate,.05),
#             sd_est=sd(estimate))
# 
# mask_time_data <- left_join(expand_index,mask_time_data,
#                            by=c("country","date_policy")) %>% 
#   fill(med_est,high_est,low_est,sd_est,.direction="downup")
# 
# mask_time_data_scaled <- left_join(expand_index,mask_time_data_scaled,
#                                   by=c("country","date_policy")) %>% 
#   fill(med_est,high_est,low_est,sd_est,.direction="downup")
# 
# saveRDS(mask_time_data,"indices/mask_time_data.rds")
# saveRDS(mask_time_data_scaled,"indices/mask_time_data_scaled.rds")
# write_csv(mask_time_data,"indices/mask_time_data.csv")
# write_csv(mask_time_data_scaled,"indices/mask_time_data_scaled.csv")
# 
# sample_plot_dates <- group_by(mask_time_data_scaled,country) %>% 
#   sample_n(1)
# 
# mask_time <- mask_time_data_scaled %>% 
#   ggplot(aes(y=med_est,x=date_policy)) +
#   geom_line(colour="#8DD3C7",aes(group=country)) +
#   geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
#   geom_text(aes(label=country),colour="#FFFFB3",fontface="bold",
#             data=sample_plot_dates,check_overlap = T,size=2) +
#   theme_tufte() +
#   labs(x="",y="Index Score") +
#   ggtitle("Mask") + 
#   theme(axis.text.x=element_blank())
# 
# mask_time
# 
# saveRDS(mask_time,"coronanet/mask_plot_object.rds")
# 
# ggsave("plots/mask_mod_plot.png")
# 
# mask_time_single <- mask_time_data_scaled %>% 
#   filter(country %in% plot_countries) %>% 
#   ggplot(aes(y=med_est,x=date_policy)) +
#   geom_line(colour="#8DD3C7",aes(group=country)) +
#   geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
#   geom_text(aes(label=country),colour="black",fontface="bold",
#             data=filter(sample_plot_dates,country %in% plot_countries),
#             check_overlap = T,size=2) +
#   theme_tufte() +
#   labs(x="",y="Index Score") +
#   ggtitle("Mask") + 
#   theme(axis.text.x=element_blank())
# 
# mask_time_single
# 
# saveRDS(mask_time_single,"coronanet/mask_plot_single_object.rds")
# 
# ggsave("plots/mask_mod_plot_single.png")
# 
# rm(mask_mod)


# Health monitoring -------------------------------------------------------

# hm2_mod <- readRDS("coronanet/activity_fit_rwhm2_random_walk_run_1.rds")
# 
# all_mods <- lapply(list.files(path = "coronanet/",
#                               pattern="hm2\\_random\\_walk\\_run\\_[1-4].*rds",
#                               full.names=T),readRDS)
# 
# all_mods_mat <-  lapply(all_mods, function(c) c@stan_samples$draws())
# 
# hm2_draws <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))
# 
# hm2_sum <- summarize_draws_mc(hm2_draws,"median","quantile2",posterior::rhat)
# 
# all_mods_mat <-  lapply(all_mods, function(c) as_draws_array(c@time_varying))
# 
# rm(all_mods)
# 
# hm2_time <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))
# 
# hm2_time <- apply_draws(hm2_time,FUN=function(c) as.numeric(scale(c)),MARGIN=c(1:2))
# 
# sum_time <- summarize_draws_mc(hm2_time,"median","quantile2",posterior::rhat)
# 
# rm(all_mods_mat)
# 
# gc()
# 
# get_all_discrim <- filter(hm2_mod@summary,grepl(x=variable,pattern="reg\\_full"))
# 
# get_all_discrim$id <- levels(hm2_mod@score_data@score_matrix$item_id)
# 
# get_all_discrim$id_rec <- fct_recode(get_all_discrim$id,
#                                      "Certification" = "hm2_cert",
#                                      "Home Visits" = "hm2_home_visit",
#                                      "Buses" = "hm2_loc_buses",
#                                      "Nursing" = "hm2_loc_nursing",
#                                      "Other Locations" = "hm2_loc_other",
#                                      "Subway" = "hm2_loc_subway",
#                                      "Trains" = "hm2_loc_trains",
#                                      "Other Monitoring" = "hm2_other_mon",
#                                      "Questionnaires" = "hm2_q",
#                                      "Other In person" = "hm2_snap_other",
#                                      "Temperature" = "hm2_snap_temp",
#                                      "Human Contact Tracing" = "hm2_stra_contact_human",
#                                      "Mobile Contact Tracing" = "hm2_stra_contact_phone",
#                                      "Other Tracing" = "hm2_stra_other",
#                                      "Wearable Tracking" = "hm2_stra_wearable",
#                                      "Bluetooth Tracking" = "hm2_tech_bluetooth",
#                                      "GPS Tracking" = "hm2_tech_gps",
#                                      "QR Codes"="hm2_tech_qr",
#                                      "Other Tracking" = "hm2_tech_other",
#                                      "Phone Calls" = "hm2_telephone"
# )
# 
# hm2_rhat <- sum_time %>% 
#   ggplot(aes(x=`posterior::rhat`)) +
#   geom_histogram() +
#   geom_vline(xintercept=1.2,linetype=2,colour="blue") +
#   theme_tufte() +
#   labs(x="",y="") +
#   ggtitle("Health Monitoring")
# 
# saveRDS(hm2_rhat,"coronanet/hm2_rhat.rds")
# 
# hm2 <- get_all_discrim %>%
#   ggplot(aes(y=mean,x=reorder(id_rec,mean))) +
#   geom_pointrange(aes(ymin=lower,ymax=upper)) +
#   theme_tufte() +
#   geom_hline(yintercept=1,linetype=2) +
#   coord_flip() +
#   labs(x="",y="Level of Discrimination") +
#   ggtitle("Health Monitoring")
# 
# hm2
# 
# saveRDS(hm2,"coronanet/hm2_discrim_object.rds")
# 
# ggsave("plots/hm2_discrim.png")
# 
# hm2_time_data_scaled <- hm2_time %>% as_draws_df() %>%
#   gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>%
#   group_by(.iteration) %>% 
#   mutate(estimate=plogis(estimate)*100,
#          country=levels(hm2_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
#          date_policy=unique(hm2_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>%
#   group_by(date_policy,country) %>%
#   summarize(med_est=quantile(estimate,.5),
#             high_est=quantile(estimate,.95),
#             low_est=quantile(estimate,.05),
#             sd_est=sd(estimate))
# 
# hm2_time_data <- hm2_time %>% as_draws_df() %>%
#   gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>%
#   mutate(country=levels(hm2_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
#          date_policy=unique(hm2_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>%
#   group_by(date_policy,country) %>%
#   summarize(med_est=quantile(estimate,.5),
#             high_est=quantile(estimate,.95),
#             low_est=quantile(estimate,.05),
#             sd_est=sd(estimate))
# 
# hm2_time_data <- left_join(expand_index,hm2_time_data,
#                            by=c("country","date_policy")) %>%
#   fill(med_est,high_est,low_est,sd_est,.direction="downup")
# 
# hm2_time_data_scaled <- left_join(expand_index,hm2_time_data_scaled,
#                                   by=c("country","date_policy")) %>%
#   fill(med_est,high_est,low_est,sd_est,.direction="downup")
# 
# saveRDS(hm2_time_data,"indices/hm2_time_data.rds")
# saveRDS(hm2_time_data_scaled,"indices/hm2_time_data_scaled.rds")
# write_csv(hm2_time_data,"indices/hm2_time_data.csv")
# write_csv(hm2_time_data_scaled,"indices/hm2_time_data_scaled.csv")
# 
# sample_plot_dates <- group_by(hm2_time_data_scaled,country) %>%
#   sample_n(1)
# 
# hm2_time <- hm2_time_data_scaled %>%
#   ggplot(aes(y=med_est,x=date_policy)) +
#   geom_line(colour="#8DD3C7",aes(group=country)) +
#   geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
#   geom_text(aes(label=country),colour="#FFFFB3",fontface="bold",
#             data=sample_plot_dates,check_overlap = T,size=2) +
#   theme_tufte() +
#   labs(x="",y="Index Score") +
#   ggtitle("Health Monitoring")
# 
# hm2_time
# 
# saveRDS(hm2_time,"coronanet/hm2_plot_object.rds")
# 
# ggsave("plots/hm2_mod_plot.png")
# 
# hm2_time_single <- hm2_time_data_scaled %>%
#   filter(country %in% plot_countries) %>%
#   ggplot(aes(y=med_est,x=date_policy)) +
#   geom_line(colour="#8DD3C7",aes(group=country)) +
#   geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
#   geom_text(aes(label=country),colour="black",fontface="bold",
#             data=filter(sample_plot_dates,country %in% plot_countries),
#             check_overlap = T,size=2) +
#   theme_tufte() +
#   labs(x="",y="Index Score") +
#   ggtitle("Health Monitoring")
# 
# hm2_time_single
# 
# saveRDS(hm2_time_single,"coronanet/hm2_plot_single_object.rds")
# 
# ggsave("plots/hm2_mod_plot_single.png")
# 
# rm(hm2_mod)

# social distance ---------------------------------------------------------

sd_mod <- readRDS("coronanet/activity_fit_rwsd_random_walk_run_1.rds")

all_mods <- lapply(list.files(path = "coronanet/",
                              pattern="sd\\_random\\_walk\\_run\\_[1-4].*rds",
                              full.names=T),readRDS)

all_mods_mat <-  lapply(all_mods, function(c) c@stan_samples$draws())

sd_draws <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))

sd_sum <- summarize_draws_mc(sd_draws,"median","quantile2",posterior::rhat)

all_mods_mat <-  lapply(all_mods, function(c) as_draws_array(c@time_varying))

rm(all_mods)

sd_time <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))

sd_time <- apply_draws(sd_time,FUN=function(c) as.numeric(scale(c)),MARGIN=c(1:2))

sum_time <- summarize_draws_mc(sd_time,"median","quantile2",posterior::rhat)

rm(all_mods_mat)

gc()

get_all_discrim <- filter(sd_sum,grepl(x=variable,pattern="reg\\_full"))


get_all_discrim$id <- levels(sd_mod@score_data@score_matrix$item_id)

get_all_discrim$id_rec <- fct_recode(get_all_discrim$id,
                                     "Allow Annual Events" = "allow_ann_event",
                                     "Distancing in Buses" = "buses",
                                     "Cancel Annual Events" = "cancel_annual_event",
                                     "Curfew" = "curfew_length",
                                     "Other" = "distance_other",
                                     "No Audiences" = "event_no_audience",
                                     "All Internal Restrictions" = "int_restrict_all",
                                     "Restrict Internal Border" = "int_restrict_border",
                                     "Restrict Buses" = "int_restrict_buses",
                                     "Restrict Crusies" = "int_restrict_cruises",
                                     "Restrict Ferries" = "int_restrict_ferries",
                                     "Restrict Internal Flights" = "int_restrict_flights",
                                     "Other Internal Restriction" = "int_restrict_NA",
                                     "Internal Ports" = "int_restrict_ports",
                                     "Restrict Trains" = "int_restrict_trains",
                                     "Number at Mass Gathering" = "number_mass",
                                     "Other Transportation" = "other_transport",
                                     "Oxford Internal Restrictions" = "ox_internal",
                                     "Oxford Mass Gathering" = "ox_mass_gathering",
                                     "Oxford Public Events" = "ox_pub_events",
                                     "Oxford Public Transport" = "ox_public_transport",
                                     "Oxford Stay Home" = "ox_stay_home",
                                     "Postpone Annual Event" = "postpone_ann_event",
                                     "Postpone Recreational Event" = "postpone_rec_event",
                                     "Prison Population" = "prison_pop",
                                     "Stay at Home" = "social_distance",
                                     "Distancing in Subways" = "subways")

sd_rhat <- sum_time %>% 
  ggplot(aes(x=`posterior::rhat`)) +
  geom_histogram() +
  geom_vline(xintercept=1.2,linetype=2,colour="blue") +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("Social Distancing")

saveRDS(sd_rhat,"coronanet/sd_rhat.rds")

sd <- get_all_discrim %>% 
  ggplot(aes(y=median,x=reorder(id_rec,median))) +
  geom_pointrange(aes(ymin=q5,ymax=q95)) +
  theme_tufte() +
  geom_hline(yintercept=1,linetype=2) +
  coord_flip() +
  labs(x="",y="Level of Discrimination") +
  ggtitle("Social Distancing")

sd

saveRDS(sd,"coronanet/sd_discrim_object.rds")

ggsave("plots/sd_discrim.png")

sd_time_data_scaled <- sd_time %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  group_by(.iteration) %>% 
  mutate(estimate=plogis(estimate)*100,
         country=levels(sd_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(sd_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

sd_time_data <- sd_time %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(country=levels(sd_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(sd_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

saveRDS(sd_time_data,"indices/sd_time_data.rds")
saveRDS(sd_time_data_scaled,"indices/sd_time_data_scaled.rds")
write_csv(sd_time_data,"indices/sd_time_data.csv")
write_csv(sd_time_data_scaled,"indices/sd_time_data_scaled.csv")

sample_plot_dates <- group_by(sd_time_data_scaled,country) %>% 
  sample_n(1)

sd_time <- sd_time_data_scaled %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="#FFFFB3",fontface="bold",
            data=sample_plot_dates,check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("Social Distancing")

sd_time

saveRDS(sd_time,"coronanet/sd_plot_object.rds")

ggsave("plots/sd_mod_plot.png")

sd_time_single <- sd_time_data_scaled %>% 
  filter(country %in% plot_countries) %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="black",fontface="bold",
            data=filter(sample_plot_dates,country %in% plot_countries),
            check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("Social Distancing")

sd_time_single

saveRDS(sd_time_single,"coronanet/sd_plot_single_object.rds")

ggsave("plots/sd_mod_plot_single.png")

rm(sd_mod)


# schools -----------------------------------------------------------------

school_mod <- readRDS("coronanet/activity_fit_rwschool_random_walk_run_1.rds")

all_mods <- lapply(list.files(path = "coronanet/",
                              pattern="school\\_random\\_walk\\_run\\_[0-4].*rds",
                              full.names=T),readRDS)

all_mods_mat <-  lapply(all_mods, function(c) c@stan_samples$draws())

school_draws <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))

school_sum <- summarize_draws_mc(school_draws,"median","quantile2",posterior::rhat)

all_mods_mat <-  lapply(all_mods, function(c) as_draws_array(c@time_varying)) 

rm(all_mods)

school_time <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))

# standardize by iteration

school_time <- apply_draws(school_time,FUN=function(c) as.numeric(scale(c)),MARGIN=c(1:2))

sum_time <- summarize_draws_mc(school_time,"median","quantile2",posterior::rhat)

rm(all_mods_mat)

gc()

get_all_discrim <- filter(school_sum,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(school_mod@score_data@score_matrix$item_id)

get_all_discrim$id_rec <- fct_recode(get_all_discrim$id,
                                     "Higher Ed" = "higher_ed",
                                     "Oxford School Close" = "ox_school_close",
                                     "Preschool" = "preschool",
                                     "Primary School" = "primary_school",
                                     "Sanitation" = "school_clean",
                                     "Social Distancing" = "school_distance",
                                     "Health Monitoring" = "school_health_monitoring",
                                     "Health Questionnaire" = "school_health_q",
                                     "Masks" = "school_mask",
                                     "Maximum Number Students" = "school_num",
                                     "Other" = "school_other",
                                     "School Event"="school_event",
                                     "School Hours"="school_hours",
                                     "Provisions for Students" = "school_special_student",
                                     "Provisions for Teachers" = "school_special_teacher",
                                     "Temperature Checks" = "school_temp",
                                     "Only Certain Students" = "school_type_pers",
                                     "Secondary School" = "secondary_school")

school_rhat <- sum_time %>% 
  ggplot(aes(x=`posterior::rhat`)) +
  geom_histogram() +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("School Restrictions")

 
saveRDS(school_rhat,"coronanet/school_rhat.rds")

school <- get_all_discrim %>% 
  ggplot(aes(y=median,x=reorder(id_rec,median))) +
  geom_pointrange(aes(ymin=q5,ymax=q95)) +
  theme_tufte() +
  geom_hline(yintercept=1,linetype=2) +
  coord_flip() +
  labs(x="",y="Level of Discrimination") +
  ggtitle("Schools")

school

saveRDS(school,"coronanet/school_discrim_object.rds")

ggsave("plots/school_discrim.png")

school_time_data_scaled <- school_time %>% as_draws_df %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  group_by(.iteration) %>% 
  mutate(estimate=plogis(estimate)*100,
         country=levels(school_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(school_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

school_time_data <- school_time %>% as_draws_df %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(country=levels(school_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(school_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

saveRDS(school_time_data,"indices/school_time_data.rds")
saveRDS(school_time_data_scaled,"indices/school_time_data_scaled.rds")
write_csv(school_time_data,"indices/school_time_data.csv")
write_csv(school_time_data_scaled,"indices/school_time_data_scaled.csv")

sample_plot_dates <- group_by(school_time_data_scaled,country) %>% 
  sample_n(1)

school_time <- school_time_data_scaled %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="#FFFFB3",fontface="bold",
            data=sample_plot_dates,check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("School Restrictions")

school_time

saveRDS(school_time,"coronanet/school_plot_object.rds")

ggsave("plots/school_mod_plot.png")

school_time_single <- school_time_data_scaled %>% 
  filter(country %in% plot_countries) %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="black",fontface="bold",
            data=filter(sample_plot_dates,country %in% plot_countries),
            check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("School Restrictions")

school_time_single

saveRDS(school_time_single,"coronanet/school_plot_single_object.rds")

ggsave("plots/school_mod_plot_single.png")


rm(school_mod)
rm(school_time)
rm(school_time_data)
rm(school_time_data_scaled)
rm(school_sum)
rm(school)
rm(school_draws)

gc()

# health resources --------------------------------------------------------

hr_mod <- readRDS("coronanet/activity_fit_rwhr_random_walk_run_1.rds")

all_mods <- lapply(list.files(path = "coronanet/",
                              pattern="hr\\_random\\_walk\\_run\\_[0-4].*rds",
                              full.names=T),readRDS)

all_mods_mat <-  lapply(all_mods, function(c) c@stan_samples$draws())

hr_draws <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))

hr_sum <- summarize_draws_mc(hr_draws,"median","quantile2",posterior::rhat)

all_mods_mat <-  lapply(all_mods, function(c) as_draws_array(c@time_varying))

rm(all_mods)

hr_time <- do.call(bind_draws, c(all_mods_mat,list(along="chain")))

school_time <- apply_draws(hr_time,FUN=function(c) as.numeric(scale(c)),MARGIN=c(1:2))

sum_time <- summarize_draws_mc(hr_time,"median","quantile2",posterior::rhat)

rm(all_mods_mat)

gc()

get_all_discrim <- filter(hr_sum,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(hr_mod@score_data@score_matrix$item_id)

get_all_discrim$id_rec <- fct_recode(get_all_discrim$id,
                                     "Cold Storage" = "hr_cold_storage",
                                     "Doctors" = "hr_doctors",
                                     "Pharmaceuticals" = "hr_drugs",
                                     "Dry Ice" = "hr_dry_ice",
                                     "Other Facilities" = "hr_facilities",
                                     "Hospitals" = "hr_hospitals",
                                     "Health Insurance" = "hr_insurance",
                                     "Mask Production" = "hr_masks",
                                     "Nurses" = "hr_nurses",
                                     "Other Infrastructure" = "hr_other_infra",
                                     "Other Materials" = "hr_other_mat",
                                     "Other Staff" = "hr_other_staff",
                                     "PCR Test Production" = "hr_pcr",
                                     "PPE" = "hr_ppe",
                                     "Quarantine Funding" = "hr_quarantine",
                                     "Sanitizer" = "hr_sanitizer",
                                     "Syringe Production" = "hr_syringe",
                                     "Targeted at Staff" = "hr_target_staff",
                                     "Targeted at Supply" = "hr_target_supply",
                                     "Test Kits" = "hr_test_kit",
                                     "General Testing" = "hr_testing",
                                     "Ventilators" = "hr_ventilator",
                                     "Recruiting Volunteers" = "hr_volunteers",
                                     "Oxford Health Investment" = "ox_health_invest")

hr_rhat <- hr_time %>% 
  ggplot(aes(x=`posterior::rhat`)) +
  geom_histogram() +
  geom_vline(xintercept=1.2,linetype=2,colour="blue") +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("Health Resources")

saveRDS(hr_rhat,"coronanet/hr_rhat.rds")

hr <- get_all_discrim %>% 
  ggplot(aes(y=median,x=reorder(id_rec,median))) +
  geom_pointrange(aes(ymin=q5,ymax=q95)) +
  theme_tufte() +
  geom_hline(yintercept=1,linetype=2) +
  coord_flip() +
  labs(x="",y="Level of Discrimination") +
  ggtitle("Health Resources")

hr

saveRDS(hr,"coronanet/hr_discrim_object.rds")

ggsave("plots/hr_discrim.png")

hr_time_data_scaled <- hr_time %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  group_by(.iteration) %>% 
  mutate(estimate=plogis(estimate)*100,
         country=levels(hr_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(hr_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

hr_time_data <- hr_time %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(country=levels(hr_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(hr_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

hr_time_data <- left_join(expand_index,hr_time_data,
                           by=c("country","date_policy")) %>% 
  fill(med_est,high_est,low_est,sd_est,.direction="downup")

hr_time_data_scaled <- left_join(expand_index,hr_time_data_scaled,
                                  by=c("country","date_policy")) %>% 
  fill(med_est,high_est,low_est,sd_est,.direction="downup")

saveRDS(hr_time_data,"indices/hr_time_data.rds")
saveRDS(hr_time_data_scaled,"indices/hr_time_data_scaled.rds")
write_csv(hr_time_data,"indices/hr_time_data.csv")
write_csv(hr_time_data_scaled,"indices/hr_time_data_scaled.csv")

sample_plot_dates <- group_by(hr_time_data_scaled,country) %>% 
  sample_n(1)

hr_time <- hr_time_data_scaled %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="#FFFFB3",fontface="bold",
            data=sample_plot_dates,check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="Index Score") +
  ggtitle("Health Resources")

hr_time

saveRDS(hr_time,"coronanet/hr_plot_object.rds")

ggsave("plots/hr_mod_plot.png")

hr_time_single <- hr_time_data_scaled %>% 
  filter(country %in% plot_countries) %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="black",fontface="bold",
            data=filter(sample_plot_dates,country %in% plot_countries),
            check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="Index Score") +
  ggtitle("Health Resources")

hr_time_single

saveRDS(hr_time_single,"coronanet/hr_plot_single_object.rds")

ggsave("plots/hr_mod_plot_single.png")

rm(hr_mod)
rm(hr_time)
rm(hr_time_data)
rm(hr_time_data_scaled)
rm(hr_sum)
rm(hr)
rm(hr_draws)

gc()

# combine plots -----------------------------------------------------------

require(patchwork)

# first combined all trajectories

(mask_time + hm2_time + biz_time) / (hr_time + school_time + sd_time)

ggsave("plots/combine_plot.png")

(mask_time_single + hm2_time_single + biz_time_single) / (hr_time_single + school_time_single + sd_time_single)

ggsave("plots/combine_plot_single.png")

# and discimrinations

(mask + hm2) / (hr + school)

ggsave("plots/discrim1.png")

(sd + biz) 

ggsave("plots/discrim2.png")

(mask_rhat + hm2_rhat + biz_rhat) / (hr_rhat + school_rhat + sd_rhat)

ggsave("plots/rhat.png")


