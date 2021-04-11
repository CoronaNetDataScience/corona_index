# create data for activity index / aggregated data set
# Robert Kubinec JUne 24th

require(idealstan)
require(ggplot2)
require(tidyr)
require(dplyr)
require(lubridate)
require(readr)
require(stringr)
require(readxl)
require(RPostgres)


# setup -------------------------------------------------------------------

# what type of model to run
model_type <- Sys.getenv("MODELTYPE")

# load datasets

# con <- dbConnect("PostgreSQL",dbname="master",
#                  host="niehaususer.ccecwurg6k9l.us-east-2.rds.amazonaws.com",
#                  user= "corona",
#                  password="Corona7et")

# download.file("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-06-24.xlsx",
#               "/scratch/rmk7/coronanet/ecdc.xlsx")

ecdc <- read_xlsx("coronanet/ecdc.xlsx")

# load cleaned file

clean <- readRDS("coronanet/coronanet_internal_allvars.RDS")


clean_mass <- function(col) {
  
  # clean and get numbers
  
  col2 <- str_remove_all(col,"[,\\.]")
  all_nums <- str_extract_all(col2, "[0-9]+(?<!%)")
  
  all_nums <- sapply(all_nums, function(x) {
    if(any(is.na(x)) || length(x)==0) {
      return(NA)
    } else {
      if(length(x)==1) {
        return(as.numeric(x))
      } else {
        x <- as.numeric(x)
        if(!all(is.na(max(x)))) {
          return(x[which(x==max(x))][1])
        } else {
          return(x[1])
        }
      }
    }
  })
  
  all_nums
  
  
}

range01 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm =T)-min(x,na.rm=T))}


# make new index from clean -----------------------------------------------



index <- filter(
  clean,
  !type %in% c(
    "COVID-19 Vaccines",
    "Declaration of Emergency",
    "New Task Force, Bureau or Administrative Configuration",
    "Other Policy Not Listed Above",
    "Anti-Disinformation Measures",
    "Restriction and Regulation of Government Services"
  ),
  #!grepl(x=type_mass_gathering,pattern="capacity"),!is.na(date_start)
) %>%
  select(
    record_id,
    policy_id,
    description,
    date_start,
    date_end,
    country,
    compliance,
    type,
    target_geog_level,
    target_city,
    target_province,
    target_other,
    target_who_what,
    target_who_gen,
    target_direction,
    travel_mechanism,
    institution_conditions,
    institution_status,
    institution_cat,
    type_sub_cat,
    type_mass_gathering,
    type_curfew_start,
    type_curfew_end,
    domestic_policy,
    init_country_level,
    matches("type_health")
  ) %>%
  mutate(
    curfew_length = (as_datetime(as.numeric(type_curfew_end)) + days(1)) - as_datetime(as.numeric(type_curfew_start)),
    curfew_length = ifelse(curfew_length > 24, curfew_length - 24, curfew_length),
    curfew_length = curfew_length / 24,
    preschool = case_when(
      as.numeric(
        type_sub_cat == "Preschool or childcare facilities (generally for children ages 5 and below)"
      ) & as.numeric(grepl(x = institution_status,
                           pattern =
                             "closed/locked down")) ~ 3,
      as.numeric(
        type_sub_cat == "Preschool or childcare facilities (generally for children ages 5 and below)"
      ) & as.numeric(grepl(x = institution_status,
                           pattern =
                             "open with conditions")) ~ 2,
      TRUE ~
        1
    ),
    primary_school = case_when(
      as.numeric(
        type_sub_cat == "Primary Schools (generally for children ages 10 and below)"
      ) & as.numeric(grepl(x = institution_status,
                           pattern =
                             "closed/locked down")) ~ 3,
      as.numeric(
        type_sub_cat == "Primary Schools (generally for children ages 10 and below)"
      ) & as.numeric(grepl(x = institution_status,
                           pattern =
                             "open with conditions")) ~ 2,
      TRUE ~
        1
    ),
    secondary_school = case_when(
      as.numeric(
        type_sub_cat == "Secondary Schools (generally for children ages 10 to 18)"
      ) & as.numeric(grepl(x = institution_status,
                           pattern =
                             "closed/locked down")) ~ 3,
      as.numeric(
        type_sub_cat == "Secondary Schools (generally for children ages 10 to 18)"
      ) & as.numeric(grepl(x = institution_status,
                           pattern =
                             "open with conditions")) ~ 2,
      TRUE ~
        1
    ),
    higher_ed = case_when(
      type_sub_cat %in% c(
        "Higher education institutions (i.e. degree granting institutions)",
        "Higher education (i.e. degree granting institutions)"
      ) & as.numeric(grepl(x = institution_status,
                           pattern =
                             "closed/locked down")) ~ 3,
      type_sub_cat %in% c(
        "Higher education institutions (i.e. degree granting institutions)",
        "Higher education (i.e. degree granting institutions)"
      )  & as.numeric(grepl(x = institution_status,
                            pattern =
                              "open with conditions")) ~ 2,
      TRUE ~
        1
    ),
    school_clean = as.numeric(
      grepl(x = institution_conditions,
            pattern = "cleaning and sanitary")
    ),
    # school_distance = as.numeric(grepl(x = institution_conditions,
    #                                    pattern =
    #                                      "Keeping a distance")),
    # school_mask = as.numeric(grepl(x = institution_conditions,
    #                                pattern = "Mask wearing")),
    school_other = as.numeric(grepl(x = institution_conditions,
                                    pattern = "Other conditions")),
    school_num = as.numeric(grepl(x = institution_conditions,
                                  pattern = "Number of people")),
    school_type_pers = as.numeric(grepl(x = institution_conditions,
                                        pattern = "Types of people")),
    school_event=as.numeric(grepl(x = institution_conditions,
                                  pattern = "School event cancelled")),
    school_hours=as.numeric(grepl(x = institution_conditions,
                                  pattern = "Physical classroom hours")),
    school_health_q = as.numeric(
      grepl(x = institution_conditions,
            pattern = "Health Questionnaire")
    ),
    school_special_student = as.numeric(
      grepl(x = institution_conditions,
            pattern = "Special provisions for all students")
    ),
    school_special_teacher = as.numeric(
      grepl(x = institution_conditions,
            pattern = "Special provisions exist for how teaching")
    ),
    school_temp = as.numeric(grepl(x = institution_conditions,
                                   pattern = "Temperature")),
    school_health_monitoring = as.numeric(
      grepl(x = institution_conditions,
            pattern = "Other Health Monitoring")
    ),
    biz_restrict_all = case_when(type_sub_cat == "All or unspecified businesses" & grepl(x = institution_status, pattern =
                                                                                           "closed/locked down") ~ 3,
                                 type_sub_cat == "All or unspecified businesses" & grepl(x = institution_status, pattern =
                                                                                           "open with conditions") ~ 2,
                                 TRUE~1),
    biz_restrict_rest = case_when(type_sub_cat %in% c(
      "Restaurants", "Restaurants/Bars", "Bars") & grepl(x = institution_status, pattern =
                                                           "closed/locked down") ~ 3,
      type_sub_cat %in% c(
        "Restaurants", "Restaurants/Bars", "Bars") & grepl(x = institution_status, pattern =
                                                             "open with conditions") ~ 2,
      TRUE~1),
    biz_restrict_comm = case_when(type_sub_cat == "Commercial Businesses" & grepl(x = institution_status, pattern =
                                                                                    "closed/locked down") ~ 3,
                                  type_sub_cat == "Commercial Businesses" & grepl(x = institution_status, pattern =
                                                                                    "open with conditions") ~ 2,
                                  TRUE~1),
    biz_restrict_retail = case_when(type_sub_cat == "Retail Businesses" & grepl(x = institution_status, pattern =
                                                                                  "closed/locked down") ~ 3,
                                    type_sub_cat == "Retail Businesses" & grepl(x = institution_status, pattern =
                                                                                  "open with conditions") ~ 2,
                                    TRUE~1),
    biz_restrict_shop = case_when(type_sub_cat == "Shopping Centers" & grepl(x = institution_status, pattern =
                                                                               "closed/locked down") ~ 3,
                                  type_sub_cat == "Shopping Centers" & grepl(x = institution_status, pattern =
                                                                               "open with conditions") ~ 2,
                                  TRUE~1),
    biz_restrict_groom = case_when(type_sub_cat == "Personal Grooming Businesses (e.g. hair salons)" & grepl(x = institution_status, pattern =
                                                                                                               "closed/locked down") ~ 3,
                                   type_sub_cat == "Personal Grooming Businesses (e.g. hair salons)" & grepl(x = institution_status, pattern =
                                                                                                               "open with conditions") ~ 2,
                                   TRUE~1),
    biz_restrict_other = case_when(type_sub_cat == "Other Businesses" & grepl(x = institution_status, pattern =
                                                                                "closed/locked down") ~ 3,
                                   type_sub_cat == "Other Businesses" & grepl(x = institution_status, pattern =
                                                                                "open with conditions") ~ 2,
                                   TRUE~1),
    biz_restrict_grocery = case_when(type_sub_cat == "Supermarkets/grocery stores" & grepl(x = institution_status, pattern =
                                                                                             "closed/locked down") ~ 3,
                                     type_sub_cat == "Supermarkets/grocery stores" & grepl(x = institution_status, pattern =
                                                                                             "open with conditions") ~ 2,
                                     TRUE~1),
    biz_restrict_telecom = case_when(type_sub_cat == "Telecommunications" & grepl(x = institution_status, pattern =
                                                                                    "closed/locked down") ~ 3,
                                     type_sub_cat == "Telecommunications" & grepl(x = institution_status, pattern =
                                                                                    "open with conditions") ~ 2,
                                     TRUE~1),
    biz_restrict_info = case_when(type_sub_cat == "Information service activities" & grepl(x = institution_status, pattern =
                                                                                             "closed/locked down") ~ 3,
                                  type_sub_cat == "Information service activities" & grepl(x = institution_status, pattern =
                                                                                             "open with conditions") ~ 2,
                                  TRUE~1),
    biz_restrict_publish = case_when(type_sub_cat == "Publishing activities" & grepl(x = institution_status, pattern =
                                                                                       "closed/locked down") ~ 3,
                                     type_sub_cat == "Publishing activities" & grepl(x = institution_status, pattern =
                                                                                       "open with conditions") ~ 2,
                                     TRUE~1),
    biz_restrict_construct = case_when(type_sub_cat == "Construction" & grepl(x = institution_status, pattern =
                                                                                "closed/locked down") ~ 3,
                                       type_sub_cat == "Construction" & grepl(x = institution_status, pattern =
                                                                                "open with conditions") ~ 2,
                                       TRUE~1),
    biz_restrict_farm = case_when(type_sub_cat == "Agriculture; forestry and fishing" & grepl(x = institution_status, pattern =
                                                                                                "closed/locked down") ~ 3,
                                  type_sub_cat == "Agriculture; forestry and fishing" & grepl(x = institution_status, pattern =
                                                                                                "open with conditions") ~ 2,
                                  TRUE~1),
    biz_restrict_transport = case_when(type_sub_cat == "Transportation (land; water and air)" & grepl(x = institution_status, pattern =
                                                                                                        "closed/locked down") ~ 3,
                                       type_sub_cat == "Transportation (land; water and air)" & grepl(x = institution_status, pattern =
                                                                                                        "open with conditions") ~ 2,
                                       TRUE~1),
    biz_restrict_hotel = case_when(type_sub_cat == "Paid lodgings (e.g. hotels; motels)" & grepl(x = institution_status, pattern =
                                                                                                   "closed/locked down") ~ 3,
                                   type_sub_cat == "Paid lodgings (e.g. hotels; motels)" & grepl(x = institution_status, pattern =
                                                                                                   "open with conditions") ~ 2,
                                   TRUE~1),
    biz_restrict_warehouse = case_when(type_sub_cat == "Warehousing and support activities for transportation" & grepl(x = institution_status, pattern =
                                                                                                                         "closed/locked down") ~ 3,
                                       type_sub_cat == "Warehousing and support activities for transportation" & grepl(x = institution_status, pattern =
                                                                                                                         "open with conditions") ~ 2,
                                       TRUE~1),
    biz_restrict_health = case_when(type_sub_cat == "Private health offices" & grepl(x = institution_status, pattern =
                                                                                       "closed/locked down") ~ 3,
                                    type_sub_cat == "Private health offices" & grepl(x = institution_status, pattern =
                                                                                       "open with conditions") ~ 2,
                                    TRUE~1),
    biz_restrict_pharmacy = case_when(type_sub_cat == "Pharmacies" & grepl(x = institution_status, pattern =
                                                                             "closed/locked down") ~ 3,
                                      type_sub_cat == "Pharmacies" & grepl(x = institution_status, pattern =
                                                                             "open with conditions") ~ 2,
                                      TRUE~1),
    biz_restrict_water = case_when(type_sub_cat == "Water supply; sewerage; waste management and remediation activities" & grepl(x = institution_status, pattern =
                                                                                                                                   "closed/locked down") ~ 3,
                                   type_sub_cat == "Water supply; sewerage; waste management and remediation activities" & grepl(x = institution_status, pattern =
                                                                                                                                   "open with conditions") ~ 2,
                                   TRUE~1),
    biz_restrict_finance = case_when(type_sub_cat == "Financial service activities except insurance and pension funding" & grepl(x = institution_status, pattern =
                                                                                                                                   "closed/locked down") ~ 3,
                                     type_sub_cat == "Financial service activities except insurance and pension funding" & grepl(x = institution_status, pattern =
                                                                                                                                   "open with conditions") ~ 2,
                                     TRUE~1),
    biz_restrict_mining = case_when(type_sub_cat == "Mining and quarrying" & grepl(x = institution_status, pattern =
                                                                                     "closed/locked down") ~ 3,
                                    type_sub_cat == "Mining and quarrying" & grepl(x = institution_status, pattern =
                                                                                     "open with conditions") ~ 2,
                                    TRUE~1),
    biz_restrict_insurance = case_when(type_sub_cat == "Insurance; reinsurance; and pension funding except compulsory social security" & grepl(x = institution_status, pattern =
                                                                                                                                                 "closed/locked down") ~ 3,
                                       type_sub_cat == "Insurance; reinsurance; and pension funding except compulsory social security" & grepl(x = institution_status, pattern =
                                                                                                                                                 "open with conditions") ~ 2,
                                       TRUE~1),
    biz_restrict_na =  case_when(is.na(
      type_sub_cat == "Pharmacies" &
        type == "Restriction and Regulation of Businesses"
    ) & grepl(x = institution_status, pattern = "closed/locked down") ~ 3,
    is.na(
      type_sub_cat == "Pharmacies" &
        type == "Restriction and Regulation of Businesses"
    )  & grepl(x = institution_status, pattern = "open with conditions") ~ 2,
    TRUE~1),
    biz_nonessential = as.numeric(grepl(x = institution_cat, pattern =
                                          "Non-Essential Businesses")),
    biz_essential = as.numeric(institution_cat == "Essential Businesses"),
    biz_hygiene = as.numeric(grepl(x = institution_conditions, pattern =
                                     "Hygiene")),
    biz_hours = as.numeric(grepl(x = institution_conditions, pattern =
                                   "business hours")),
    biz_work_home = as.numeric(
      grepl(x = institution_conditions, pattern = "work at home policies|Maximum number of employees")
    ),
    biz_meeting = as.numeric(grepl(x = institution_conditions, pattern =
                                     "business meetings")),
    # disappeared, perhaps a coding error
    # biz_social_distance = as.numeric(grepl(x = institution_conditions, pattern =
    #                                          "1.5 meters")),
    biz_takeaway=as.numeric(grepl(x = institution_conditions, pattern =
                                    "Takeaway")),
    biz_delivery=as.numeric(grepl(x = institution_conditions, pattern =
                                    "Delivery")),
    # biz_mask = as.numeric(grepl(x = institution_conditions, pattern =
    #                               "Mask")),
    biz_temp = as.numeric(grepl(x = institution_conditions, pattern =
                                  "Temperature")),
    biz_health_cert = as.numeric(
      grepl(x = institution_conditions, pattern = "Health Certificate")
    ),
    biz_health_q = as.numeric(
      grepl(x = institution_conditions, pattern = "Health Questionnaire")
    ),
    biz_num_cust = as.numeric(
      grepl(x = institution_conditions, pattern = "number of customers")
    ),
    biz_store_size = as.numeric(grepl(x = institution_conditions, pattern =
                                        "Size of store")),
    biz_cont_trace = as.numeric(grepl(x = institution_conditions, pattern =
                                        "Contact tracing")),
    biz_cond_other = as.numeric(grepl(x = institution_conditions, pattern =
                                        "Other condition")),
    hr_cold_storage = as.numeric(grepl(x = type_sub_cat, pattern =
                                         "Cold storage")),
    hr_doctors = as.numeric(grepl(x = type_sub_cat, pattern = "Doctors")),
    hr_dry_ice = as.numeric(grepl(x = type_sub_cat, pattern = "Dry ice")),
    hr_sanitizer = as.numeric(grepl(x = type_sub_cat, pattern = "Sanitizer")),
    hr_insurance = as.numeric(grepl(x = type_sub_cat, pattern = "Insurance")),
    hr_facilities = as.numeric(grepl(x = type_sub_cat, pattern =
                                       "Facilities")),
    hr_volunteers = as.numeric(grepl(x = type_sub_cat, pattern =
                                       "Volunteers")),
    hr_hospitals = as.numeric(grepl(x = type_sub_cat, pattern = "Hospitals")),
    hr_masks = as.numeric(grepl(x = type_sub_cat, pattern = "Masks")),
    hr_drugs = as.numeric(grepl(x = type_sub_cat, pattern = "Drugs")),
    hr_nurses = as.numeric(grepl(x = type_sub_cat, pattern = "Nurses")),
    hr_other_infra = as.numeric(
      grepl(x = type_sub_cat, pattern = "Other Health Infrastructure|Unspecified Health Infrastructure")
    ),
    hr_other_mat = as.numeric(
      grepl(x = type_sub_cat, pattern = "Other Health Materials|Unspecified Health Materials")
    ),
    hr_other_staff = as.numeric(
      grepl(x = type_sub_cat, pattern = "Other Heath Staff|Unspecified Health Staff")
    ),
    hr_ppe = as.numeric(grepl(x = type_sub_cat, pattern = "Protective")),
    hr_testing = as.numeric(grepl(x = type_sub_cat, pattern = "Public Testing")),
    hr_syringe = as.numeric(grepl(x = type_sub_cat, pattern = "Syringes")),
    hr_quarantine = as.numeric(grepl(x = type_sub_cat, pattern =
                                       "Temporary Quarantine")),
    hr_pcr = as.numeric(grepl(x = type_sub_cat, pattern = "Thermal cyclers")),
    hr_ventilator = as.numeric(grepl(x = type_sub_cat, pattern =
                                       "Ventilators")),
    hr_test_kit = as.numeric(grepl(x = type_sub_cat, pattern = "Test Kits")),
    hr_target_staff = as.numeric(grepl(x = target_who_what, pattern =
                                         "Health Staff")),
    hr_target_supply = as.numeric(grepl(x = target_who_what, pattern =
                                          "Health-Related Supplies")),
    hm_home_visit = as.numeric(grepl(x = type_health_mon_hum, pattern =
                                       "Home visits")),
    hm_other_mon = as.numeric(
      grepl(x = type_health_mon_hum, pattern = "Other human health monitoring strategy")
    ),
    hm_telephone = as.numeric(grepl(x = type_health_mon_hum, pattern =
                                      "Telephone calls")),
    hm_loc_nursing = as.numeric(grepl(x = type_health_mon_loc, pattern =
                                        "Nursing Homes")),
    hm_loc_other = as.numeric(
      grepl(x = type_health_mon_loc, pattern = "Other Health Monitoring Location|Other Public Transportation")
    ),
    hm_loc_subway = as.numeric(grepl(x = type_health_mon_loc, pattern =
                                       "Subways/Trams")),
    hm_loc_buses = as.numeric(grepl(x = type_health_mon_loc, pattern =
                                      "Buses")),
    hm_loc_trains = as.numeric(grepl(x = type_health_mon_loc, pattern =
                                       "Trains")),
    hm_loc_nursing = as.numeric(grepl(x = type_health_mon_loc, pattern =
                                        "Nursing Homes")),
    hm_cert = as.numeric(grepl(x = type_health_mon_snap, pattern =
                                 "Health Certificate")),
    hm_q = as.numeric(grepl(x = type_health_mon_snap, pattern = "Questionnaire")),
    hm_snap_other = as.numeric(grepl(x = type_health_mon_snap, pattern =
                                       "Other")),
    hm_snap_temp = as.numeric(grepl(x = type_health_mon_snap, pattern =
                                      "Temperature")),
    hm_stra_contact_human = as.numeric(
      grepl(x = type_health_mon_stra, pattern = "Contact tracing through human teams")
    ),
    hm_stra_contact_phone = as.numeric(
      grepl(x = type_health_mon_stra, pattern = "Contact tracing through smart phones")
    ),
    hm_stra_other = as.numeric(grepl(x = type_health_mon_stra, pattern =
                                       "Other")),
    hm_stra_wearable = as.numeric(grepl(x = type_health_mon_stra, pattern =
                                          "Wearable technology")),
    hm_tech_bluetooth = as.numeric(grepl(x = type_health_mon_tech, pattern =
                                           "Bluetooth")),
    hm_tech_gps = as.numeric(grepl(x = type_health_mon_tech, pattern =
                                     "GPS")),
    hm_tech_qr = as.numeric(grepl(x = type_health_mon_tech, pattern =
                                    "QR")),
    hm_tech_other = as.numeric(grepl(x = type_health_mon_tech, pattern =
                                       "Other")),
    ht_door2door = as.numeric(grepl(x = type_sub_cat, pattern = "Door-to-door")),
    ht_drivein = as.numeric(grepl(x = type_sub_cat, pattern = "Drive-in")),
    ht_fixed = as.numeric(grepl(x = type_sub_cat, pattern = "Fixed Health Testing")),
    ht_entire_pop = as.numeric(grepl(x = type_sub_cat, pattern =
                                       "entire population")),
    ht_mobile = as.numeric(grepl(x = type_sub_cat, pattern = "Mobile Health Testing")),
    ht_other = as.numeric(grepl(x = type_sub_cat, pattern = "Other Health Testing")),
    ht_type_antibody = as.numeric(
      grepl(x = type_health_test_cat, pattern = "Antibody/serological")
    ),
    ht_type_antigen = as.numeric(grepl(x = type_health_test_cat, pattern =
                                         "Antigen")),
    ht_type_other = as.numeric(grepl(x = type_health_test_cat, pattern =
                                       "Not specified|Other")),
    ht_type_pcr = as.numeric(grepl(x = type_health_test_cat, pattern =
                                     "PCR test")),
    ht_type_antibody = as.numeric(
      grepl(x = type_health_test_cat, pattern = "Antibody/serological")
    ),
    ht_portal_email = as.numeric(grepl(x = type_health_test_res, pattern =
                                         "Email")),
    ht_portal_sms = as.numeric(grepl(x = type_health_test_res, pattern =
                                       "Mobile text")),
    ht_portal_app = as.numeric(grepl(x = type_health_test_res, pattern =
                                       "app or website")),
    ht_portal_other = as.numeric(grepl(x = type_health_test_res, pattern =
                                         "Other|Not specified")),
    ht_portal_paper = as.numeric(grepl(x = type_health_test_res, pattern =
                                         "Paper")),
    ht_portal_phone = as.numeric(grepl(x = type_health_test_res, pattern =
                                         "Phone call")),
    ht_cost_free_all = as.numeric(
      grepl(x = type_health_test_eco, pattern = "Testing is free for all individuals")
    ),
    ht_cost_free_subset = as.numeric(
      grepl(x = type_health_test_eco, pattern = "Testing is free for a subset of the population")
    ),
    ht_cost_partly_free = as.numeric(
      grepl(x = type_health_test_eco, pattern = "Testing is partially subsidized by the government")
    ),
    ht_cost_biz = as.numeric(
      grepl(x = type_health_test_eco, pattern = "Business employees and employers")
    ),
    ht_cost_other = as.numeric(
      grepl(x = type_health_test_eco, pattern = "No information provided")
    ),
    ht_cost_all_pay = as.numeric(
      grepl(x = type_health_test_eco, pattern = "All Individuals must pay full cost")
    ),
    ht_cost_symptomatic = as.numeric(grepl(x = type_health_test_eco, pattern =
                                             "Symptomatic people")),
    ht_loc_clinic = as.numeric(grepl(x = type_health_test_loc, pattern =
                                       "Health Clinics")),
    ht_loc_private = as.numeric(
      grepl(x = type_health_test_loc, pattern = "Private doctors offices")
    ),
    ht_loc_hospital = as.numeric(grepl(x = type_health_test_loc, pattern =
                                         "Hospitals")),
    ht_loc_other = as.numeric(grepl(x = type_health_test_loc, pattern =
                                      "Not specified|Other")),
    ht_loc_pharmacy = as.numeric(grepl(x = type_health_test_loc, pattern =
                                         "Pharmacies")),
    social_distance = as.numeric(
      type_sub_cat == "Keeping a distance of at least 6 feet or 1.5 meters apart"
    ),
    mask_public = as.numeric(type_sub_cat == "Wearing Masks inside public buildings"),
    mask_everywhere = as.numeric(
      type_sub_cat %in% c(
        "Wearing Masks in all public spaces/everywhere",
        "Wearing Masks in all indoor spaces",
        "Wearing Masks inside public or commercial building"
      )
    ),
    mask_business = as.numeric(
      type_sub_cat == "Wearing Masks inside private businesses (e.g. supermarkets)"
    ),
    mask_primary_school = as.numeric(
      type_sub_cat == "Wearing Masks inside Primary Schools (generally for children ages 10 and below)"
    ),
    mask_sec_school = as.numeric(
      type_sub_cat == "Wearing Masks inside Secondary Schools (generally for children ages 10 to 18)"
    ),
    mask_transport = as.numeric(type_sub_cat == "Wearing Masks inside Public transportation"),
    mask_unspec = as.numeric(
      type_sub_cat %in% c("Unspecified Mask Wearing Policy",
                          "Wearing masks")
    ),
    mask_preschool = as.numeric(
      type_sub_cat == "Wearing Masks inside Preschools or childcare facilities (generally for children age 5 and below)"
    ),
    mask_higher_ed = as.numeric(
      type_sub_cat == "Wearing Masks inside Higher education institutions (i.e. degree granting institutions)"
    ),
    buses = as.numeric(type_sub_cat == "Restrictions on ridership of buses"),
    other_transport = as.numeric(
      type_sub_cat == "Restrictions ridership of other forms of public transportation (please include details in the text entry)"
    ),
    private_transport = as.numeric(
      type_sub_cat == "Restrictions on  private vehicles in public circulation"
    ),
    subways = as.numeric(type_sub_cat == "Restrictions on ridership of subways and trams"),
    distance_other = as.numeric(
      type_sub_cat == "Keep a distance of some other distance not listed above. Please note the distance in meters in the text entry."
    ),
    number_mass = clean_mass(type_mass_gathering),
    number_mass = 1 - range01(number_mass),
    #cancel_rec_event=as.numeric(grepl(x=type_sub_cat,pattern="Cancellation of a recreational or commercial event")),
    cancel_annual_event = as.numeric(
      grepl(x = type_sub_cat, pattern = "Cancellation of a recreational or commercial event")
    ),
    prison_pop = as.numeric(grepl(x = type_sub_cat, pattern = "Prison population reduced")),
    #other_mass=as.numeric(grepl(x=type_sub_cat,pattern="Prison population reduced") | is.na(type_sub_cat)),
    postpone_ann_event = as.numeric(
      grepl(x = type_sub_cat, pattern = "Postponement of a recreational or commercial event")
    ),
    postpone_rec_event = as.numeric(
      grepl(x = type_sub_cat, pattern = "Postponement of an annually recurring event")
    ),
    private_event = as.numeric(
      grepl(x = type_sub_cat, pattern = "Events at private residencies restricted")
    ),
    allow_ann_event = as.numeric(
      grepl(x = type_sub_cat, pattern = "Cancellation of an annually recurring event")
    ),
    event_no_audience = as.numeric(
      grepl(x = type_sub_cat, pattern = "allowed to occur but no audience is allowed")
    ),
    int_restrict_flights = as.numeric(grepl(x = travel_mechanism, pattern =
                                              "Flights")),
    int_restrict_border = as.numeric(grepl(x = travel_mechanism, pattern =
                                             "Land Border")),
    int_restrict_all = as.numeric(grepl(x = travel_mechanism, pattern =
                                          "All kinds of transport")),
    int_restrict_NA = as.numeric(grepl(x = travel_mechanism, pattern =
                                         "Not Applicable")),
    int_restrict_cruises = as.numeric(grepl(x = travel_mechanism, pattern =
                                              "Cruises")),
    int_restrict_ferries = as.numeric(grepl(x = travel_mechanism, pattern =
                                              "Ferries")),
    int_restrict_ports = as.numeric(grepl(x = travel_mechanism, pattern =
                                            "Seaports")),
    int_restrict_trains = as.numeric(grepl(x = travel_mechanism, pattern =
                                             "Trains")),
    int_restrict_buses = as.numeric(grepl(x = travel_mechanism, pattern =
                                            "Buses")),
    date_end = as_date(ifelse(
      is.na(date_end), today() - days(5), date_end
    )),
    type_sub_cat = ifelse(
      grepl(x = type_sub_cat, pattern = "[Qq]uarantine"),
      NA_character_,
      type_sub_cat
    ),
    date_start = as_date(ifelse(
      date_start > date_end, date_end, date_start
    )),
    voluntary = grepl(x = compliance, pattern = "Voluntary/Recommended but No Penalties"),
    man1 = grepl(x = compliance, pattern = "Mandatory \\(Unspecified/Implied\\)"),
    man2 = grepl(x = compliance, pattern = "Mandatory with Fines"),
    man3 = grepl(x = compliance, pattern = "Mandatory with Legal Penalties \\(Jail Time\\)"),
    compliance = case_when(man3 ~ 3,
                           man2 ~ 2,
                           man1 ~ 1,
                           voluntary ~ 0,
                           TRUE ~ NA_real_)
  ) %>%
  ungroup


# make data time-complete -------------------------------------------------



# loop over each variable and make a separate series 
# need to convert a hybrid wide/long system to just long

rm(clean)

# load list of variables

source("create_items.R")

filter_list <- list(sd=sd_items,
                    biz=biz_items,
                    ht=ht_items,
                    hm=hm_items,
                    mask=mask_items,
                    hr=hr_items,
                    school=school_items)

# loop over type of index

lapply(names(filter_list), function(type) {

print(paste("now on list",type))

this_vars <- c(filter_list[[type]],"voluntary","man1","man2",
               "man3")

index_long <- lapply(this_vars, function(a) {
  
  if(grepl(x=a,pattern="ox")) {
    return(NULL)
  }
  
  print(paste("Now on ",a))
  
  this_data <- select(index,policy_id,record_id,date_start,date_end,one_of(a)) %>% 
    ungroup
  
  names(this_data) <- c("policy_id","record_id","date_start","date_end","var")
  
  this_data <- filter(this_data,!is.na(var),var>0 | var<0) %>% 
    mutate(item=a)
  
  # make a time series
  
  this_data %>% 
    filter(!is.na(date_start)) %>% 
    group_by(record_id,policy_id) %>% 
    distinct %>% 
    mutate(date_policy = list(seq(date_start, date_end, by='1 day'))) %>%
    unnest(cols=c(date_policy)) %>% 
    ungroup
  
})

index_long <- bind_rows(index_long)

#parallel::detectCores()

# merge in other covariates

index_long <- left_join(ungroup(index_long),distinct(select(index,record_id,
                                                            policy_id,
                                                            country,
                                                            compliance,
                                                            init_country_level,
                                                            target_city,
                                                            target_province,
                                                            target_other)),
                        by=c("record_id","policy_id"))

#index_long <- mutate(index_long,item=paste0(item,init_country_level))

# aggregate border restrictions

# merge in city/provincial population data

source("recode_city.R")

# merge in province pop data

province_pop <- read_delim("coronanet/coronanet_population.csv",delim = ";") %>% 
  select(-X1) %>% 
  distinct %>% 
  filter(!is.na(province)) %>% 
  select(country,province,province_pop=population_total)

# add missing data

source("miss_province_pop.R")

province_pop <- anti_join(province_pop,miss_prov,by=c('country',"province"))

province_pop <- bind_rows(province_pop,miss_prov) %>% 
  distinct

# merge city and province population data

wb_pop_country <- read_csv("wb_country_pop.csv") %>% 
  select(country="Country Name",
         country_pop="2015 [YR2015]") %>% 
  filter(country_pop!="...") %>% 
  mutate(country_pop=as.numeric(country_pop),
         country=recode(country,
                        `United States`="United States of America",
                        `Brunei Darussalam`="Brunei",
                        `Cabo Verde`="Cape Verde",
                        `Congo, Dem. Rep.`="Democratic Republic of the Congo",
                        `Gambia, The`="Gambia",
                        `Iran, Islamic Rep.`="Iran",
                        `Korea, Dem. People’s Rep.`="North Korea",
                        `Czech Republic`="Czechia",
                        `Russian Federation`="Russia",
                        `St. Kitts and Nevis`="Saint Kitts and Nevis",
                        `Korea, Rep.`="South Korea",
                        `Timor-Leste`="Timor Leste",
                        `Venezuela, RB`="Venezuela",
                        `Kyrgyz Republic`="Kyrgyzstan",
                        `Bahamas, The`="Bahamas",
                        `Hong Kong SAR, China`="Hong Kong",
                        `Cote d'Ivoire`="Ivory Coast",
                        `Lao PDR`="Laos",
                        `Micronesia, Fed. Sts.`="Micronesia",
                        `West Bank and Gaza`="Palestine",
                        `Congo, Rep.`="Republic of the Congo",
                        `St. Lucia`="Saint Lucia",
                        `Egypt, Arab Rep.`="Egypt",
                        `St. Vincent and the Grenadines`="Saint Vincent and the Grenadines",
                        `Slovak Republic`="Slovakia",
                        `Syrian Arab Republic`="Syria",
                        `Yemen, Rep.`="Yemen")) %>% 
  bind_rows(tibble(country=c("Taiwan","Vatican","Macau","Northern Cyprus"),
                   country_pop=c(23816775,
                                 825,
                                 640445,
                                 326000)))
# add taiwan, vatican, Macau,

index_long <- left_join(index_long,combine_pop_sum,by="target_city") %>% 
  left_join(wb_pop_country,by="country") %>% 
  left_join(province_pop,by=c("country",c("target_province"="province"))) %>% 
  filter((init_country_level=="Municipal" & !is.na(combine_pop_city)) | (init_country_level=="Provincial" & !is.na(province_pop) ) | init_country_level=="National")

index_long <- group_by(index_long,country,item,date_policy,init_country_level) %>% 
  mutate(var=case_when(!grepl(x=item,pattern="number|curfew") & init_country_level %in% c("Municipal","Provincial")~1,
                       grepl(x=item,pattern="number|curfew") & init_country_level %in% c("Municipal","Provincial")~mean(var,na.rm=T),
                       TRUE~var)) %>% 
  ungroup %>% 
         mutate(population=case_when(init_country_level=="Municipal" & var>0~combine_pop_city,
                              init_country_level=="Provincial" & var>0~province_pop,
                              init_country_level=="National" & var>0~1,
                              TRUE~0))



# need to calculate proportions of provinces/cities

# province_data <- read_csv("country_region_clean.csv") %>% 
#   mutate(Country=recode(Country,
#                         `United States`="United States of America",
#                         `Paletsine`="Palestine")) %>% 
#   gather(key="prov_num",value="province",-ISO2,-Country) %>% 
#   group_by(Country) %>% 
#   summarize(n_prov=length(unique(province)))
# 
# city_data <- read_csv("world-cities_csv.csv") %>% 
#   mutate(country=recode(country,
#                         `United States`="United States of America",
#                         `Czech Republic`="Czechia",
#                         Macedonia="North Macedonia",
#                         `Palestinian Territory`="Palestine",
#                         Swaziland="Eswatini")) %>% 
#   group_by(country) %>% 
#   summarize(n_city=length(unique(geonameid)))
# 
# index_long <- left_join(index_long,province_data,by=c("country"="Country")) %>% 
#   left_join(city_data)

index_long <- mutate(ungroup(index_long),
                     population=case_when(init_country_level=="Municipal"~population/country_pop,
                                          init_country_level=="Provincial"~population/country_pop,
                                          TRUE~population)) %>% 
  # get rid of any possible overlapping records, such as reductions in prison population
  # that I have not yet been able to take apart
  distinct(country,item,date_policy,var,init_country_level,population) %>% 
  group_by(country,item,date_policy) %>% 
  # average multiple indicators
  summarize(pop_out=sum(population*var,na.rm=T),
            var=sum(var,na.rm=T))

# make it complete

expand_index <- group_by(index_long,country,item) %>% 
  expand(date_policy=seq(ymd("2020-01-01"),as_date(today() - days(5)),
                         by="1 day"))

# get rid of duplicates due to sum function used earlier
# compliance should be equal to -1 if var = 0 to indicate no policy in effect

index_long <- left_join(ungroup(expand_index),
                        ungroup(index_long),by=c("country","item","date_policy")) %>% 
  group_by(country,item) %>% 
  distinct
#fill(compliance,.direction=c("downup")) %>% 
# select(-record_id,-policy_id,-init_country_level,-matches('target'),-date_start,
#        -date_end) %>% 

# mutate(compliance=ifelse(var==0,0,compliance))

# merge in RA work data

ra_work <- read_csv("certificate.csv") %>% 
  mutate(length_work=end-start)

# need to make table with total RAs per country

ra_country <- lapply(unique(index_long$country), function(c) {
  
  c1 <- switch(c,`Cape Verde`="Cabo Verde",
               `United States of America`="United States",
               Palestine="Israel",
               c)
  
  tibble(country=c,
         ra_num=sum(ra_work$length_work[grepl(x=ra_work$country,pattern=c1)],na.rm=T))
  
}) %>% bind_rows

index_long <- left_join(index_long,ra_country,by="country")

# remove any duplicates

index_long <- distinct(index_long)

# add in oxford tracker data

oxford <- read_csv("~/covid-policy-tracker/data/OxCGRT_latest.csv") %>% 
  filter(Jurisdiction=="NAT_TOTAL") %>% 
  select(country="CountryName",
         ox_mass_gathering="C4_Restrictions on gatherings",
         ox_public_transport="C5_Close public transport",
         ox_pub_events="C3_Cancel public events",
         ox_stay_home="C6_Stay at home requirements",
         ox_internal="C7_Restrictions on internal movement",
         ox_school_close="C1_School closing",
         ox_workplace_close="C2_Workplace closing",
         ox_external="C8_International travel controls",
         ox_test="H2_Testing policy",
         ox_health_invest="H4_Emergency investment in healthcare",
         ox_mask="H6_Facial Coverings",
         date_policy="Date") %>% 
  mutate(country=recode(country,
                        `United States`="United States of America",
                        `Timor-Leste`="Timor Leste",
                        `Cote d'Ivoire`="Ivory Coast",
                        `Congo`="Republic of the Congo",
                        `Czech Republic`="Czechia",
                        `Kyrgyz Republic`="Kyrgyzstan",
                        `Slovak Republic`="Slovakia",
                        `Democratic Republic of Congo`="Democratic Republic of the Congo"),
         date_policy=ymd(as.character(date_policy))) %>% 
  gather(key="item",value="var",-date_policy,-country) %>% 
  group_by(item) %>% 
  mutate(ordered_id=length(unique(var[!is.na(var)])),
         pop_out=ifelse(item=="ox_health_invest",as.numeric(scale(var)),0))

# remove any not in our data

oxford <- semi_join(oxford,index_long,by="country") %>% 
  distinct

index_long_model <- bind_rows(index_long,oxford)

index_long_model <- filter(index_long_model, date_policy<max(oxford$date_policy))

# check for countries with few records

count_cont <- count(distinct(ungroup(index_long_model), country, item), country)

# get rid of any countries that don't have at least 5 distinct items coded

index_long_model <- anti_join(index_long_model, filter(count_cont, n<5),by="country")

fillin <- expand(ungroup(index_long_model),country,date_policy,item)

index_long_model <- right_join(index_long_model,fillin) %>% 
  mutate(ordered_id=coalesce(ordered_id,0)) %>% 
  group_by(country) %>% 
  mutate(ra_num=unique(ra_num[!is.na(ra_num)])) %>% 
  ungroup %>% 
  mutate(pop_out=coalesce(pop_out,0),
         var=coalesce(var,0))

saveRDS(index_long_model,paste0("coronanet/index_long_model_",type,".rds"))

rm(index_long_model)
rm(oxford)

fillin <- expand(ungroup(index_long),country,date_policy,item)

index_long <- right_join(index_long,fillin) %>% 
  group_by(country) %>% 
  mutate(ra_num=unique(ra_num[!is.na(ra_num)])) %>% 
  ungroup %>% 
  mutate(pop_out=coalesce(pop_out,0),
         var=coalesce(var,0))

index_long_var <- select(index_long,-pop_out) %>% spread(key="item",value="var")

index_long_pop <- select(index_long,-var) %>% spread(key="item",value="pop_out")

saveRDS(index_long_var,paste0("coronanet/wide_data_binary_",type,".rds"))

saveRDS(index_long_pop,paste0("coronanet/wide_data_pop_weighted_",type,".rds"))

})


