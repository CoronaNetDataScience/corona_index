# need to have some dedicated code as the cities is quite a task!
  
require(forcats)

# switch to Kaggle DB

  world_cities <- read_csv("data/worldcities.csv") %>% 
    filter(!is.na(population),population>0)

  # un_city_data <- read_csv("data/UNdata_Export_20210401_132806548.csv") %>% 
  #   group_by(City,`Country or Area`) %>% 
  #   filter(Year==max(Year),Sex=="Both Sexes") %>% 
  #   select(pop="Value",City) %>% 
  #   mutate(City_rec=str_remove_all(City,"\\(.+\\)|\\.|-"),
  #   City_rec=str_remove_all(City_rec,
  #                                  "[Dd]istrict|[Dd]istricts|([Tt]he)? ?[Cc]ity of|([Tt]he)? ?[Dd]epartments? of|([Tt]he)? ?[Rr]egency of|([Tt]he)? ?[Mm]unicipality of|([Tt]he)? ?[Vv]illage of|[Cc]ity|[Vv]illage"),
  #   City_rec=trimws(City_rec,which="both"),
  #   City_rec=str_to_lower(City_rec),
  #   City_rec=str_replace_all(City_rec,"[[:punct:]]",""),
  #   City_rec=str_remove_all(City_rec," "))
  # 
  # 
  # un_city_table <- read_csv("data/table08.csv") %>% 
  #   mutate(city2=str_extract(city,"(?<=\\().+(?=\\))|(?<=-).+")) %>% 
  #   select(city,city2,population) %>% 
  #   mutate(population=str_remove_all(population,",|\\.\\.\\."),
  #          population=as.numeric(population)) %>% 
  #   filter(!is.na(population)) %>% 
  #   mutate(City_rec=str_remove_all(city,"\\(.+\\)|\\.|-\\s.*$"),
  #          City_rec=str_remove_all(City_rec,
  #                                  "[Dd]istrict|[Dd]istricts|([Tt]he)? ?[Cc]ity of|([Tt]he)? ?[Dd]epartments? of|([Tt]he)? ?[Rr]egency of|([Tt]he)? ?[Mm]unicipality of|([Tt]he)? ?[Vv]illage of|[Cc]ity|[Vv]illage"),
  #          City_rec=trimws(City_rec,which="both"),
  #          City_rec=str_to_lower(City_rec),
  #          City_rec=str_replace_all(City_rec,"[[:punct:]]",""),
  #          City_rec=str_remove_all(City_rec," "),
  #          City_rec2=str_remove_all(city2,"\\(.+\\)|\\.|-\\s.*$"),
  #          City_rec2=str_remove_all(City_rec2,
  #                                  "[Dd]istrict|[Dd]istricts|([Tt]he)? ?[Cc]ity of|([Tt]he)? ?[Dd]epartments? of|([Tt]he)? ?[Rr]egency of|([Tt]he)? ?[Mm]unicipality of|([Tt]he)? ?[Vv]illage of|[Cc]ity|[Vv]illage"),
  #          City_rec2=trimws(City_rec2,which="both"),
  #          City_rec2=str_to_lower(City_rec2),
  #          City_rec2=str_replace_all(City_rec2,"[[:punct:]]",""),
  #          City_rec2=str_remove_all(City_rec2," "))
  # 
  # 
  # oecd_city_data <- read_csv("data/cities_oecd.csv") %>% 
  #   group_by(`Metropolitan areas`) %>% 
  #   filter(Year==max(Year,na.rm=T),VAR=="T_T") %>% 
  #   mutate(City_rec=str_remove_all(`Metropolitan areas`,"\\(.+\\)|\\.|-"),
  #          City_rec=str_remove_all(City_rec,
  #                                  "[Dd]istrict|[Dd]istricts|([Tt]he)? ?[Cc]ity of|([Tt]he)? ?[Dd]epartments? of|([Tt]he)? ?[Rr]egency of|([Tt]he)? ?[Mm]unicipality of|([Tt]he)? ?[Vv]illage of|[Cc]ity|[Vv]illage"),
  #          City_rec=trimws(City_rec,which="both"),
  #          City_rec=str_to_lower(City_rec),
  #          City_rec=str_replace_all(City_rec,"[[:punct:]]",""),
  #          City_rec=str_remove_all(City_rec," "))
  
  city_list <- distinct(index,city,country)
  
  city_list <- left_join(city_list, 
                         world_cities, 
                         by=c("city","country"))
  
  ## Recoding city_list$target_city into city_list$target_city_rec
  city_list$target_city_rec <- fct_recode(city_list$target_city,
                                          "Abu Keer" = "\"Abu Keer\"",
                                          "Wien"="Vienna",
                                          "Moskva"="Moscow",
                                          "The Seven Wells" = "\"The Seven Wells\" village",
                                          "remove" = "(information not contained in the source)",
                                          "remove" = "36 districts",
                                          "remove" = "58 Kirchberg – Remich – Merzig (D) und 159 Luxemburg – Losheim (D),Schengen, dann Besch, nach Nennig",
                                          "Abaiang" = "Abaiang island council",
                                          "Abidjan,Abengourou,Aboisso,Bondoukou,Bouaké,Bouna; Daloa,Gagnoa,Korhogo,Man,Odienné,San Pédro,Yamoussoukro" = "Abidjan; Abengourou; Aboisso; Bondoukou; Bouaké; Bouna; Daloa; Gagnoa; Korhogo; Man; Odienné; San Pédro; Yamoussoukro",
                                          "Abu Dhabi" = "Abu Dahbi City",
                                          "Acevedo" = "Acevedo (Miranda)",
                                          "remove" = "Afghanistan's Torkham border with Pakistan",
                                          "Ahmedabad, Vadodara, Surat, Rajkot" = "Ahmedabad, Vadodara, Surat and Rajkot",
                                          "Akurana" = "Akurana in Kandy",
                                          "remove" = "All cities",
                                          "remove" = "All cities besides Eilat",
                                          "remove" = "All cities in Lombardia; Modena (Emilia-Romagna); Parma (Emilia-Romagna); Reggio-Emilia (Emilia-Romagna); Rimini (Emilia-Romagna); Pesaro (March); Urbino (Marche); Alessandria (Piemonte); Asti (Piemonte); Verbano-Cusio-Ossola (Piemonte); Vercelli (Piemonte); Padova (Veneto);Treviso (Veneto); and Venezia (Veneto).",
                                          "remove" = "All cities in Lombardia; Modena (Emilia-Romagna); Parma (Emilia-Romagna); Reggio-Emilia (Emilia-Romagna); Rimini (Emilia-Romagna); Pesaro (Marche); Urbino (Marche); Alessandria (Piemonte); Asti (Piemonte); Verbano-Cusio-Ossola (Piemonte); Vercelli (Piemonte); Padova (Veneto); Treviso (Veneto); and Venezia (Veneto).",
                                          "remove" = "All cities in South Carolina",
                                          "remove" = "All cities in Sudan that have inter-city bus services.",
                                          "remove" = "All except Rome",
                                          "remove" = "All execpt Accomack and Richmond",
                                          "remove" = "all municipalities",
                                          "remove" = "All municipalities",
                                          "remove" = "all municipalities except Skopje",
                                          "remove" = "All municipalities except Skopje",
                                          "remove" = "All municipalities in Bosnia and Herzegovina except Neum",
                                          "remove" = "All visitors not residing in Nay Pyi Taw",
                                          "remove" = "All; but Muscat specifically",
                                          "Almaty" = "Almaty city",
                                          "Almaty,Nur-Sultan city" = "Almaty city; Nur-Sultan city",
                                          "Alto Hospicio" = "Alto Hospicio (Iquique)",
                                          "Amsterdam" = "Amsterdam-Amstelland",
                                          "Seville, Jerez de la Frontera, Cordoba, Granada, El Puerto de Santa María and Cadiz" = "Andalucian towns of Seville, Jerez de la Frontera, Cordoba, Granada, El Puerto de Santa María and Cadiz",
                                          "Andorra la vella; Escaldes-Engordany; Encamp; Sant Julià de Lòria; La Massana; Santa Coloma, Ordino" = "Andorra la vella; Escaldes-Engordany; Encamp; Sant Julià de Lòria; La Massana; Santa Coloma, Ordino.",
                                          "remove" = "Antananarivo, other large cities across the country",
                                          "Antofagasta" = "Antofagasta (Antofagasta)",
                                          "Antofagasta" = "Antofagasta (El Loa)",
                                          "remove" = "Areas IV, VI and VII",
                                          "Atulugama" = "Atulugama village in Kalutara",
                                          "Baki" = "Bakı",
                                          "remove" = "Baltaysky District",
                                          "Barranquilla, Soledad, Sabanalarga" = "Barranquilla, soledad, Sabanalarga",
                                          "Beijing; Shanghai; Guangzhou; Tianjin; Shijiazhuang; Taiyuan" = "Beijing; Shanghai; Guangzhou; Tianjin; Shijiazhuang; Taiyuan and other 12 destination cities",
                                          "Bekasi" = "Bekasi (city)",
                                          "Ben Arous" = "Ben Arous, Tunisia",
                                          "remove" = "Benslimane Province",
                                          "Billings City" = "Billings City, Yellowstone County",
                                          "remove" = "Binh Duong province",
                                          "Birbhum" = "Birbhum District",
                                          "Bobonong" = "Bobonong is a town in the Central District of Botswana",
                                          "Bogura" = "Bogura municipality",
                                          "Canilla, Uspantán" = "Canilla as well as Uspantán",
                                          "City of Córdoba" = "City of Córdoba and its urban agglomeration",
                                          "remove" = "high-risk zones and middle-risk zones (China)",
                                          "Karon and Rawai" = "Karon and Rawai Sub-district Municipalities",
                                          "Kuwait" = "Kuwait municipality",
                                          "Medellín" = "Medellin",
                                          "remove" = "middle-risk zones (China)",
                                          "remove" = "municipal risk areas (over 50 new infections on 100.000 inhabitants)",
                                          "Abbadia San Salvatore, Asciano, Buonconvento, Casole d'Elsa, Castellina in Chianti, Castelnuovo Berardenga, Castiglione d'Orcia, Cetona, Chianciano Terme, Chiusdino, Chiusi, Colle di Val d'Elsa Gaiole in Chianti, Montalcino, Montepulciano, Monteriggioni, Monteroni d'Arbia, Monticiano, Murlo, Piancastagnaio, Pienza, Poggibonsi, Radda in Chianti, Radicofani, Radicondoli, Rapolano Terme, San Casciano dei Bagni, San Gimignano, San Quirico d'Orcia, Sarteano, Siena, Sinalunga, Sovicille, Torrita di Siena, Trequanda (Province of Siena)" = "Municipalities of Abbadia San Salvatore, Asciano, Buonconvento, Casole d'Elsa, Castellina in Chianti, Castelnuovo Berardenga, Castiglione d'Orcia, Cetona, Chianciano Terme, Chiusdino, Chiusi, Colle di Val d'Elsa Gaiole in Chianti, Montalcino, Montepulciano, Monteriggioni, Monteroni d'Arbia, Monticiano, Murlo, Piancastagnaio, Pienza, Poggibonsi, Radda in Chianti, Radicofani, Radicondoli, Rapolano Terme, San Casciano dei Bagni, San Gimignano, San Quirico d'Orcia, Sarteano, Siena, Sinalunga, Sovicille, Torrita di Siena, Trequanda (Province of Siena)",
                                          "N'Djamena" = "N'djari 40-edged neighbourhood (N'Djamena) + 4th arrondissement of the city of N'Djamena",
                                          "Nablus,Shechem" = "Nablus/Shechem",
                                          "Marg in Hermel" = "neighborhood of Marg in Hermel",
                                          "Prague" = "Prague; extended to all major cities with notable public transportation infrastructure in Czech Republic.",
                                          "remove" = "Region 5  to Region 6; between East Bank of Berbice to New Amsterdam",
                                          "remove" = "Seeduwa Police jurisdiction",
                                          "remove" = "settlements in the Kursk region",
                                          "remove" = "South FLY district",
                                          "remove" = "Tashkent Region",
                                          "Dammam" = "The Dammam 2nd Industrial City",
                                          "remove" = "The Ndyuka tribe",
                                          "N'Djamena" = "the townhall of N'Djamena annonced the measure",
                                          "remove" = "This policy only applies to the Princess Margaret Hospital in Nassau Bahamas.",
                                          "Tokyo" = "Tokyo Metropolitan Area",
                                          "Tokyo" = "Tokyo Metropolitan Government",
                                          "Tokyo" = "Tokyo; unspecified metropolitan areas",
                                          "remove" = "Unspecified",
                                          "remove" = "western side of the Berbice River Bridge",
                                          "Wuhan" = "Wuhan and related areas",
                                          "remove" = "\u0648\u0644\u0627\u064a\u0629 \u0627\u0644\u0639\u0627\u0645\u0631\u0627\u062a"
  )
  city_list$target_city_rec <- fct_explicit_na(city_list$target_city_rec, "remove")
  
  # now do some regex 
  
  city_list <- mutate(city_list,target_city_rec=str_remove_all(target_city_rec,"\\(.+\\)|\\.|-"),
                      target_city_rec=str_replace_all(target_city_rec,
                                                                ";| and | und ",
                                                                ","),
                      target_city_rec=str_replace_all(target_city_rec,", ",","),
                      target_city_rec=str_remove_all(target_city_rec,
                                                     "[Dd]istrict|[Dd]istricts|([Tt]he)? ?[Cc]ity of|([Tt]he)? ?[Dd]epartments? of|([Tt]he)? ?[Rr]egency of|([Tt]he)? ?[Mm]unicipality of|([Tt]he)? ?[Vv]illage of|[Cc]ity|[Vv]illage"),
                      target_city_rec=trimws(target_city_rec,which="both")) %>% 
    separate_rows(target_city_rec,sep=",") %>% 
    mutate(target_city_rec=str_to_lower(target_city_rec),
           target_city_rec=str_replace_all(target_city_rec,"[[:punct:]]",""),
           target_city_rec=str_remove_all(target_city_rec," "))
  

# first join with oecd data
  
combine_pop <- left_join(city_list,select(ungroup(oecd_city_data),
                                          City_rec,
                                          oecd_pop="Value"),
                         by=c("target_city_rec"="City_rec")) %>% 
              left_join(select(ungroup(un_city_table),City_rec,
                               un_tab_population1="population"),
                        by=c("target_city_rec"="City_rec")) %>% 
  left_join(select(ungroup(un_city_table),City_rec2,
                   un_tab_population2="population"),
            by=c("target_city_rec"="City_rec2")) %>% 
    left_join(select(un_city_data,un_data_pop='pop',
                     City_rec),by=c("target_city_rec"="City_rec")) %>% 
  mutate(combine_pop=case_when(!is.na(oecd_pop)~oecd_pop,
                              is.na(oecd_pop) & !is.na(un_tab_population1) & !is.na(un_data_pop) & un_data_pop>un_tab_population1 ~un_data_pop,
                              is.na(oecd_pop) & !is.na(un_tab_population1) & !is.na(un_data_pop) & un_tab_population1>un_data_pop ~un_tab_population1,
                              is.na(oecd_pop) & !is.na(un_data_pop)~un_data_pop,
                              is.na(oecd_pop) & !is.na(un_tab_population1)~un_tab_population1,
                              is.na(oecd_pop) & !is.na(un_tab_population2)~un_tab_population2))


# select biggest record for each city

combine_pop_sum <- group_by(combine_pop,target_city,target_city_rec) %>% 
  summarize(combine_pop=ifelse(n()>1,max(combine_pop,na.rm=T),combine_pop)) %>% 
  filter(target_city_rec!="remove") %>% 
  group_by(target_city) %>% 
  summarize(combine_pop_city=sum(combine_pop,na.rm=T)) %>% 
  filter(combine_pop_city!=0)
  
  