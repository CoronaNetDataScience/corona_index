# add some missing provincial population data

require(readr)
require(dplyr)
require(readxl)
require(stringr)

miss_prov <- read_xlsx("need_to_code.xlsx",sheet="need_to_code") %>% 
  filter(province!="NA")

add_prov <- read_xlsx("need_to_code.xlsx",sheet="to_merge") %>% 
  mutate(province=str_remove_all(province,"\\[.*"),
         province=str_trim(province,side="both"))

miss_prov <- left_join(miss_prov,add_prov,by=c("country","province")) %>% 
  mutate(pop=coalesce(pop,pop2)) %>% 
  select(-pop2,province_pop="pop",country,province) %>% 
  filter(!is.na(province_pop))



