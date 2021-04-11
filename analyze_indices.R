# load, plot and analyze indices

require(idealstan)
require(ggplot2)
require(tidyverse)
require(ggthemes)
require(cmdstanr)
require(posterior)
require(stringr)

plot_countries <- c("United States of America","Brazil","China","United Arab Emirates",
                    "South Africa","Turkey","Singapore","South Korea",
                    "Australia")


# Business ----------------------------------------------------------------

biz_mod <- readRDS("coronanet/activity_fit_rwbiz_random_walk_run_1.rds")

get_all_discrim <- filter(biz_mod@summary,grepl(x=variable,pattern="reg\\_full"))

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
                                     "Takeout"="biz_takeout",
                                     "Telecom" = "biz_restrict_telecom",
                                     "Transport" = "biz_restrict_transport",
                                     "Warehouses" = "biz_restrict_warehouse",
                                     "Water" = "biz_restrict_water",
                                     "Social Distancing" = "biz_social_distance",
                                     "Store Size" = "biz_store_size",
                                     "Temperature Checks" = "biz_temp",
                                     "Work at Home" = "biz_work_home",
                                     "Oxford Closing Workplaces" = "ox_workplace_close")

biz_rhat <- id_plot_rhats(biz_mod) +
  ggtitle("") +
  labs(caption="")

saveRDS(biz_rhat,"coronanet/biz_rhat.rds")

biz <- get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id_rec,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  geom_hline(yintercept=1,linetype=2) +
  coord_flip() +
  labs(x="",y="Level of Discrimination") +
  ggtitle("Business")

biz

saveRDS(biz,"coronanet/biz_discrim_object.rds")

ggsave("biz_discrim.png")

biz_time_data_scaled <- biz_mod@time_varying %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(estimate=plogis(scale(estimate))*100,
         country=levels(biz_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(biz_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

biz_time_data <- biz_mod@time_varying %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(country=levels(biz_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(biz_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

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

ggsave("biz_mod_plot.png")

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

ggsave("biz_mod_plot_single.png")

rm(biz_mod)


# Mask --------------------------------------------------------------------

mask_mod <- readRDS("coronanet/activity_fit_rwmask_random_walk_run_1.rds")

get_all_discrim <- filter(mask_mod@summary,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(mask_mod@score_data@score_matrix$item_id)

get_all_discrim$id_rec <- fct_recode(get_all_discrim$id,
                                     "Businesses" = "mask_business",
                                     "Everywhere" = "mask_everywhere",
                                     "Higher Ed" = "mask_higher_ed",
                                     "Preschool" = "mask_preschool",
                                     "Primary School" = "mask_primary_school",
                                     "In Public" = "mask_public",
                                     "Secondary School" = "mask_sec_school",
                                     "Mass Transport" = "mask_transport",
                                     "Unspecified Conditions" = "mask_unspec",
                                     "Oxford Mask" = "ox_mask"
)

mask_rhat <- id_plot_rhats(mask_mod) +
  ggtitle("") +
  labs(caption="")

saveRDS(mask_rhat,"coronanet/mask_rhat.rds")

mask <- get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id_rec,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  geom_hline(yintercept=1,linetype=2) +
  coord_flip() +
  labs(x="",y="Level of Discrimination") +
  ggtitle("Masks")

mask

saveRDS(mask,"coronanet/mask_discrim_object.rds")

ggsave("mask_discrim.png")

mask_time_data_scaled <- mask_mod@time_varying %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(estimate=plogis(scale(estimate))*100,
         country=levels(mask_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(mask_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

mask_time_data <- mask_mod@time_varying %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(country=levels(mask_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(mask_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

saveRDS(mask_time_data,"indices/mask_time_data.rds")
saveRDS(mask_time_data_scaled,"indices/mask_time_data_scaled.rds")
write_csv(mask_time_data,"indices/mask_time_data.csv")
write_csv(mask_time_data_scaled,"indices/mask_time_data_scaled.csv")

sample_plot_dates <- group_by(mask_time_data_scaled,country) %>% 
  sample_n(1)

mask_time <- mask_time_data_scaled %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="#FFFFB3",fontface="bold",
            data=sample_plot_dates,check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="Index Score") +
  ggtitle("Mask") + 
  theme(axis.text.x=element_blank())

mask_time

saveRDS(mask_time,"coronanet/mask_plot_object.rds")

ggsave("mask_mod_plot.png")

mask_time_single <- mask_time_data_scaled %>% 
  filter(country %in% plot_countries) %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="black",fontface="bold",
            data=filter(sample_plot_dates,country %in% plot_countries),
            check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="Index Score") +
  ggtitle("Mask") + 
  theme(axis.text.x=element_blank())

mask_time_single

saveRDS(mask_time_single,"coronanet/mask_plot_single_object.rds")

ggsave("mask_mod_plot_single.png")

rm(mask_mod)


# Health management -------------------------------------------------------

hm_mod <- readRDS("coronanet/activity_fit_rwhm_random_walk.rds")

get_all_discrim <- filter(hm_mod@summary,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(hm_mod@score_data@score_matrix$item_id)

get_all_discrim$id_rec <- fct_recode(get_all_discrim$id,
                                     "Certification" = "hm_cert",
                                     "Home Visits" = "hm_home_visit",
                                     "Buses" = "hm_loc_buses",
                                     "Nursing" = "hm_loc_nursing",
                                     "Other Locations" = "hm_loc_other",
                                     "Subway" = "hm_loc_subway",
                                     "Trains" = "hm_loc_trains",
                                     "Other Monitoring" = "hm_other_mon",
                                     "Questionnaires" = "hm_q",
                                     "Other In person" = "hm_snap_other",
                                     "Temperature" = "hm_snap_temp",
                                     "Human Contact Tracing" = "hm_stra_contact_human",
                                     "Mobile Contact Tracing" = "hm_stra_contact_phone",
                                     "Other Tracing" = "hm_stra_other",
                                     "Wearable Tracking" = "hm_stra_wearable",
                                     "Bluetooth Tracking" = "hm_tech_bluetooth",
                                     "GPS Tracking" = "hm_tech_gps",
                                     "QR Codes"="hm_tech_qr",
                                     "Other Tracking" = "hm_tech_other",
                                     "Phone Calls" = "hm_telephone"
)

hm_rhat <- id_plot_rhats(hm_mod) +
  ggtitle("") +
  labs(caption="")

saveRDS(hm_rhat,"coronanet/hm_rhat.rds")

hm <- get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id_rec,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  geom_hline(yintercept=1,linetype=2) +
  coord_flip() +
  labs(x="",y="Level of Discrimination") +
  ggtitle("Health Monitoring")

hm

saveRDS(hm,"coronanet/hm_discrim_object.rds")

ggsave("hm_discrim.png")

hm_time_data_scaled <- hm_mod@time_varying %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(estimate=plogis(scale(estimate))*100,
         country=levels(hm_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(hm_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

hm_time_data <- hm_mod@time_varying %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(country=levels(hm_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(hm_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

saveRDS(hm_time_data,"indices/hm_time_data.rds")
saveRDS(hm_time_data_scaled,"indices/hm_time_data_scaled.rds")
write_csv(hm_time_data,"indices/hm_time_data.csv")
write_csv(hm_time_data_scaled,"indices/hm_time_data_scaled.csv")

sample_plot_dates <- group_by(hm_time_data_scaled,country) %>% 
  sample_n(1)

hm_time <- hm_time_data_scaled %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="#FFFFB3",fontface="bold",
            data=sample_plot_dates,check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="Index Score") +
  ggtitle("Health Monitoring")

hm_time

saveRDS(hm_time,"coronanet/hm_plot_object.rds")

ggsave("hm_mod_plot.png")

hm_time_single <- hm_time_data_scaled %>% 
  filter(country %in% plot_countries) %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="black",fontface="bold",
            data=filter(sample_plot_dates,country %in% plot_countries),
            check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="Index Score") +
  ggtitle("Health Monitoring")

hm_time_single

saveRDS(hm_time_single,"coronanet/hm_plot_single_object.rds")

ggsave("hm_mod_plot_single.png")

rm(hm_mod)

# Health tech -------------------------------------------------------------

ht_mod <- readRDS("coronanet/activity_fit_rwht_random_walk_run_1.rds")

get_all_discrim <- filter(ht_mod@summary,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(ht_mod@score_data@score_matrix$item_id)

get_all_discrim$id_rec <- fct_recode(get_all_discrim$id,
                                     "No Test Subsidy" = "ht_cost_all_pay",
                                     "Business Pay for Tests" = "ht_cost_biz",
                                     "Free Testing" = "ht_cost_free_all",
                                     "Free Testing for Vulnerable" = "ht_cost_free_subset",
                                     "Other Test Cost Policies" = "ht_cost_other",
                                     "Test Partly Free" = "ht_cost_partly_free",
                                     "Test Free for Symptomatic" = "ht_cost_symptomatic",
                                     "Door to Door Testing" = "ht_door2door",
                                     "Drive In Testing" = "ht_drivein",
                                     "Entire Population" = "ht_entire_pop",
                                     "Fixed" = "ht_fixed",
                                     "Clinic" = "ht_loc_clinic",
                                     "Hospital" = "ht_loc_hospital",
                                     "Other Locations" = "ht_loc_other",
                                     "Pharmacy" = "ht_loc_pharmacy",
                                     "Private" = "ht_loc_private",
                                     "Mobile Testing" = "ht_mobile",
                                     "Other" = "ht_other",
                                     "App Portal" = "ht_portal_app",
                                     "Email Portal" = "ht_portal_email",
                                     "Other Portal" = "ht_portal_other",
                                     "Paper Portal" = "ht_portal_paper",
                                     "Phone Portal" = "ht_portal_phone",
                                     "SMS Portal" = "ht_portal_sms",
                                     "Antibody" = "ht_type_antibody",
                                     "Antigen" = "ht_type_antigen",
                                     "Other Test Types" = "ht_type_other",
                                     "PCR" = "ht_type_pcr",
                                     "Oxford Test" = "ox_test")

ht_rhat <- id_plot_rhats(ht_mod) +
  ggtitle("") +
  labs(caption="")

saveRDS(ht_rhat,"coronanet/ht_rhat.rds")

ht <- get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id_rec,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  geom_hline(yintercept=1,linetype=2) +
  coord_flip() +
  labs(x="",y="Level of Discrimination") +
  ggtitle("Health Testing")

ht

saveRDS(ht,"coronanet/ht_discrim_object.rds")

ggsave("ht_discrim.png")

ht_time_data_scaled <- ht_mod@time_varying %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(estimate=plogis(scale(estimate))*100,
         country=levels(ht_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(ht_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

ht_time_data <- ht_mod@time_varying %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(country=levels(ht_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(ht_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

saveRDS(ht_time_data,"indices/ht_time_data.rds")
saveRDS(ht_time_data_scaled,"indices/ht_time_data_scaled.rds")
write_csv(ht_time_data,"indices/ht_time_data.csv")
write_csv(ht_time_data_scaled,"indices/ht_time_data_scaled.csv")

sample_plot_dates <- group_by(ht_time_data_scaled,country) %>% 
  sample_n(1)

ht_time <- ht_time_data_scaled %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="#FFFFB3",fontface="bold",
            data=sample_plot_dates,check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("Health Testing") + 
  theme(axis.text.x=element_blank())

ht_time

saveRDS(ht_time,"coronanet/ht_plot_object.rds")

ggsave("ht_mod_plot.png")

ht_time_single <- ht_time_data_scaled %>% 
  filter(country %in% plot_countries) %>% 
  ggplot(aes(y=med_est,x=date_policy)) +
  geom_line(colour="#8DD3C7",aes(group=country)) +
  geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
  geom_text(aes(label=country),colour="black",fontface="bold",
            data=filter(sample_plot_dates,country %in% plot_countries),
            check_overlap = T,size=2) +
  theme_tufte() +
  labs(x="",y="") +
  ggtitle("Health Testing") + 
  theme(axis.text.x=element_blank())

ht_time_single

saveRDS(ht_time_single,"coronanet/ht_plot_single_object.rds")

ggsave("ht_mod_plot_single.png")

rm(ht_mod)

# social distance ---------------------------------------------------------

sd_mod <- readRDS("coronanet/activity_fit_rwsd_random_walk_run_1.rds")

get_all_discrim <- filter(sd_mod@summary,grepl(x=variable,pattern="reg\\_full"))

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

sd_rhat <- id_plot_rhats(sd_mod) +
  ggtitle("") +
  labs(caption="")

saveRDS(sd_rhat,"coronanet/sd_rhat.rds")

sd <- get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id_rec,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  geom_hline(yintercept=1,linetype=2) +
  coord_flip() +
  labs(x="",y="Level of Discrimination") +
  ggtitle("Social Distancing")

sd

saveRDS(sd,"coronanet/sd_discrim_object.rds")

ggsave("sd_discrim.png")

sd_time_data_scaled <- sd_mod@time_varying %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(estimate=plogis(scale(estimate))*100,
         country=levels(sd_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(sd_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

sd_time_data <- sd_mod@time_varying %>% as_draws_df() %>% 
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

ggsave("sd_mod_plot.png")

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

ggsave("sd_mod_plot_single.png")

rm(sd_mod)


# schools -----------------------------------------------------------------

school_mod <- readRDS("coronanet/activity_fit_rwschool_random_walk_run_1.rds")

get_all_discrim <- filter(school_mod@summary,grepl(x=variable,pattern="reg\\_full"))

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
                                     "Provisions for Students" = "school_special_student",
                                     "Provisions for Teachers" = "school_special_teacher",
                                     "Temperature Checks" = "school_temp",
                                     "Only Certain Students" = "school_type_pers",
                                     "Secondary School" = "secondary_school")

school_rhat <- id_plot_rhats(school_mod) +
  ggtitle("") +
  labs(caption="")

saveRDS(school_rhat,"coronanet/school_rhat.rds")

school <- get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id_rec,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  geom_hline(yintercept=1,linetype=2) +
  coord_flip() +
  labs(x="",y="Level of Discrimination") +
  ggtitle("Schools")

school

saveRDS(school,"coronanet/school_discrim_object.rds")

ggsave("school_discrim.png")

school_time_data_scaled <- school_mod@time_varying %>% as_draws_df() %>% 
  gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
  mutate(estimate=plogis(scale(estimate))*100,
         country=levels(school_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
         date_policy=unique(school_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
  group_by(date_policy,country) %>% 
  summarize(med_est=quantile(estimate,.5),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05),
            sd_est=sd(estimate))

school_time_data <- school_mod@time_varying %>% as_draws_df() %>% 
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

ggsave("school_mod_plot.png")

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

ggsave("school_mod_plot_single.png")


rm(school_mod)

# health resources --------------------------------------------------------

# hr_mod <- readRDS("coronanet/activity_fit_rwhr_random_walk.rds")
# 
# get_all_discrim <- filter(hr_mod@summary,grepl(x=variable,pattern="reg\\_full"))
# 
# get_all_discrim$id <- levels(hr_mod@score_data@score_matrix$item_id)
# 
# get_all_discrim$id_rec <- fct_recode(get_all_discrim$id,
#                                      "Higher Ed" = "higher_ed",
#                                      "Oxford School Close" = "ox_school_close",
#                                      "Preschool" = "preschool",
#                                      "Primary School" = "primary_school",
#                                      "Sanitation" = "school_clean",
#                                      "Social Distancing" = "school_distance",
#                                      "Health Monitoring" = "school_health_monitoring",
#                                      "Health Questionnaire" = "school_health_q",
#                                      "Masks" = "school_mask",
#                                      "Maximum Number Students" = "school_num",
#                                      "Other" = "school_other",
#                                      "Provisions for Students" = "school_special_student",
#                                      "Provisions for Teachers" = "school_special_teacher",
#                                      "Temperature Checks" = "school_temp",
#                                      "Only Certain Students" = "school_type_pers",
#                                      "Secondary School" = "secondary_school")
# 
# hr <- get_all_discrim %>% 
#   ggplot(aes(y=mean,x=reorder(id_rec,mean))) +
#   geom_pointrange(aes(ymin=lower,ymax=upper)) +
#   theme_tufte() +
#   geom_hline(yintercept=1,linetype=2) +
#   coord_flip() +
#   labs(x="",y="Level of Discrimination") +
#   ggtitle("hrs")
# 
# hr
# 
# ggsave("hr_discrim.png")
# 
# hr_time_data_scaled <- hr_mod@time_varying %>% as_draws_df() %>% 
#   gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
#   mutate(estimate=plogis(scale(estimate))*100,
#          country=levels(hr_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
#          date_policy=unique(hr_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
#   group_by(date_policy,country) %>% 
#   summarize(med_est=quantile(estimate,.5),
#             high_est=quantile(estimate,.95),
#             low_est=quantile(estimate,.05),
#             sd_est=sd(estimate))
# 
# hr_time_data <- hr_mod@time_varying %>% as_draws_df() %>% 
#   gather(key="variable",value="estimate",-.chain,-.iteration,-.draw) %>% 
#   mutate(country=levels(hr_mod@score_data@score_matrix$person_id)[as.numeric(str_extract(variable,"[0-9]+(?=\\])"))],
#          date_policy=unique(hr_mod@score_data@score_matrix$time_id)[as.numeric(str_extract(variable,"(?<=\\[)[0-9]+"))]) %>% 
#   group_by(date_policy,country) %>% 
#   summarize(med_est=quantile(estimate,.5),
#             high_est=quantile(estimate,.95),
#             low_est=quantile(estimate,.05),
#             sd_est=sd(estimate))
# 
# saveRDS(hr_time_data,"indices/hr_time_data.rds")
# saveRDS(hr_time_data_scaled,"indices/hr_time_data_scaled.rds")
# write_csv(hr_time_data,"indices/hr_time_data.csv")
# write_csv(hr_time_data_scaled,"indices/hr_time_data_scaled.csv")
# 
# sample_plot_dates <- group_by(hr_time_data_scaled,country) %>% 
#   sample_n(1)
# 
# hr_time <- hr_time_data_scaled %>% 
#   ggplot(aes(y=med_est,x=date_policy)) +
#   geom_line(colour="#8DD3C7",aes(group=country)) +
#   geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
#   geom_text(aes(label=country),colour="#FFFFB3",fontface="bold",
#             data=sample_plot_dates,check_overlap = T,size=2) +
#   theme_tufte() +
#   labs(x="",y="") +
#   ggtitle("hr Restrictions")
# 
# hr_time
# 
# ggsave("hr_mod_plot.png")
# 
# hr_time_single <- hr_time_data_scaled %>% 
#   filter(country %in% plot_countries) %>% 
#   ggplot(aes(y=med_est,x=date_policy)) +
#   geom_line(colour="#8DD3C7",aes(group=country)) +
#   geom_ribbon(aes(ymin=low_est,ymax=high_est,group=country),alpha=0.25) +
#   geom_text(aes(label=country),colour="black",fontface="bold",
#             data=filter(sample_plot_dates,country %in% plot_countries),
#             check_overlap = T,size=2) +
#   theme_tufte() +
#   labs(x="",y="") +
#   ggtitle("hr Restrictions")
# 
# hr_time_single
# 
# ggsave("hr_mod_plot_single.png")

# combine plots -----------------------------------------------------------

require(patchwork)

# first combined all trajectories

(mask_time + ht_time + biz_time) / (hm_time + school_time + sd_time)

ggsave("combine_plot.png")

(mask_time + ht_time + biz_time) / (hm_time + school_time + sd_time)

ggsave("combine_plot_single.png")

# and discimrinations

(mask + ht) / (hm + school)

ggsave("discrim1.png")

(sd + biz) 

ggsave("discrim2.png")

(mask_rhat + ht_rhat + biz_rhat) / (hm_rhat + school_rhat + sd_rhat)

ggsave("combine_plot_single.png")


