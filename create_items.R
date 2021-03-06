# define all the items


sd_items <- c("allow_ann_event","buses","cancel_annual_event",
              "curfew_length","distance_other",
              "event_no_audience","int_restrict_all","int_restrict_border",
              "int_restrict_buses","int_restrict_cruises","int_restrict_ferries",
              "int_restrict_flights","int_restrict_NA","int_restrict_ports",
              "int_restrict_trains","number_mass","other_transport","postpone_ann_event",
              "postpone_rec_event","prison_pop","social_distance" ,"subways",
              "ox_mass_gathering","ox_public_transport","ox_pub_events",
              "ox_stay_home","ox_internal","other_transport")

biz_items <- c(names(index)[grepl(x=names(index),
                                             pattern="biz\\_")],"ox_workplace_close" )

ht_items <- c(names(index)[grepl(x=names(index),
                                            pattern="ht\\_")],"ox_test")

hm_items <- names(index)[grepl(x=names(index),
                                          pattern="hm\\_")]

hm2_items <- c(names(index)[grepl(x=names(index),
                                            pattern="hm|ht")],"ox_test")

mask_items <- c(names(index)[grepl(x=names(index),
                                              pattern="mask\\_")],"ox_mask")

hr_items <- c(names(index)[grepl(x=names(index),
                                            pattern="hr\\_")],"ox_health_invest")

school_items <- c(names(index)[grepl(x=names(index),pattern="school\\_")],
                  "preschool","primary_school","higher_ed","secondary_school",
                  "ox_school_close")