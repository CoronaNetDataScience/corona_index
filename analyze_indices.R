# load, plot and analyze indices

require(idealstan)
require(ggplot2)
require(tidyverse)
require(ggthemes)


biz_mod <- readRDS("coronanet/activity_fit_rwbiz_random_walk.rds")

get_all_discrim <- filter(biz_mod@summary,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(biz_mod@score_data@score_matrix$item_id)

get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  coord_flip() +
  labs(x="Items",y="Level of Discrimination") +
  ggtitle("Business Discrimination")

ggsave("biz_discrim.png")

id_plot_legis_dyn(biz_mod) + ggtitle("Business Index")

ggsave("biz_mod_plot.png")

mask_mod <- readRDS("coronanet/activity_fit_rwmask_random_walk.rds")

get_all_discrim <- filter(mask_mod@summary,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(mask_mod@score_data@score_matrix$item_id)

get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  coord_flip() +
  labs(x="Items",y="Level of Discrimination") +
  ggtitle("Mask Discrimination")

ggsave("mask_discrim.png")

id_plot_legis_dyn(mask_mod) + ggtitle("Mask Index")

ggsave("mask_mod.png")

hm_mod <- readRDS("coronanet/activity_fit_rwhm_random_walk.rds")

get_all_discrim <- filter(hm_mod@summary,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(hm_mod@score_data@score_matrix$item_id)

get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  coord_flip() +
  labs(x="Items",y="Level of Discrimination") +
  ggtitle("Health Management Discrimination")

ggsave("hm_discrim.png")

id_plot_legis_dyn(hm_mod) + ggtitle("Health Management Index")

ggsave("hm_mod.png")

ht_mod <- readRDS("coronanet/activity_fit_rwht_random_walk.rds")

get_all_discrim <- filter(ht_mod@summary,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(ht_mod@score_data@score_matrix$item_id)

get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  coord_flip() +
  labs(x="Items",y="Level of Discrimination") +
  ggtitle("Health Testing Discrimination")

ggsave("ht_discrim.png")

id_plot_legis_dyn(ht_mod) + ggtitle("Health Testing Index")

ggsave("ht_mod.png")

sd_mod <- readRDS("coronanet/activity_fit_rwsd_random_walk.rds")

get_all_discrim <- filter(sd_mod@summary,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(sd_mod@score_data@score_matrix$item_id)

get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  coord_flip() +
  labs(x="Items",y="Level of Discrimination") +
  ggtitle("Social Distancing Disrimination")

ggsave("sd_discrim.png")

id_plot_legis_dyn(sd_mod) + ggtitle("Social Distancing Index")

ggsave("sd_mod.png")

school_mod <- readRDS("coronanet/activity_fit_rwschool_random_walk.rds")

get_all_discrim <- filter(school_mod@summary,grepl(x=variable,pattern="reg\\_full"))

get_all_discrim$id <- levels(school_mod@score_data@score_matrix$item_id)

get_all_discrim %>% 
  ggplot(aes(y=mean,x=reorder(id,mean))) +
  geom_pointrange(aes(ymin=lower,ymax=upper)) +
  theme_tufte() +
  coord_flip() +
  labs(x="Items",y="Level of Discrimination") +
  ggtitle("School Discrimination")

ggsave("school_discrim.png")

id_plot_legis_dyn(school_mod) + ggtitle("School Index")

ggsave("school_mod.png")

