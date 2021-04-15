
library(dplyr)
library(tidyr)

coronanet <- readRDS("/Users/JBS548/Downloads/coronanet_internal_allvars.rds")[, c('country', 'province')]

coronanet[which(coronanet$province == 'Kano' & coronanet$country == "Sudan"),]$country <- "Nigeria" #this is probably a mistake as Kano is a state of Nigeria, not Sudan

population_data <- read.csv2('/Users/JBS548/Downloads/Subnational-PopulationData.csv', sep = ',')
population_data <- population_data[, c('Country.Name', 'Country.Code', 'Indicator.Name', 'Indicator.Code', 'X2016')]

population_data$Country.Name <- gsub('Congo,', 'Congo', population_data$Country.Name)
population_data$Country.Name <- gsub(', Arab Rep.', '', population_data$Country.Name)
population_data$Country.Name <- gsub(', Islamic Rep.', '', population_data$Country.Name)
population_data$Country.Name <- gsub('Korea, Rep.', 'South Korea', population_data$Country.Name)
population_data$Country.Name <- gsub(', RB', '', population_data$Country.Name)
population_data$Country.Name <- gsub(', Rep.', '', population_data$Country.Name)
population_data$Country.Name <- gsub(', The', '', population_data$Country.Name)

population_data[which(population_data$country == "Czech Republic"),]$country <- "Czechia"

population_data <- population_data %>% 
  separate(Country.Name, c("country", "province"), sep = ",")

population_data$province <- trimws(population_data$province, which = c("left"), whitespace = "[ \t\r\n]")

#population data added manually

#no population data for Libya
#no population data for Malaysia provinces
#no population data for Micronesia provinces
#no population data for Morocco provinces
#no population data for Norway provinces
#no population data for Oman provinces
#no population data for Pakistan provinces
#no population data for Russia provinces
#no population data for Swiss provinces
#no population data for USA provinces

andorra_data <- as.data.frame(cbind(country = c(rep(c("Andorra"), 4)),
                                    province = c(rep(c(NA, "Canillo"),2)),
                                    Country.Code = rep(c("AND"), 4),
                                    Indicator.Name = c(rep('Population (% of total)', 2),
                                                       rep('Population, total', 2)),
                                    Indicator.Code = c(rep('SP.POP.TOTL.ZS', 2),
                                                       rep('SP.POP.TOTL', 2)
                                    ),
                                    X2016 = c(100, (4124/77142)*100,
                                              77142, 4124)
))

australia_data <- as.data.frame(cbind(country = rep("Australia", 18),
                                      province = rep(c(NA, "Australian Capital Territory", "New South Wales", "Queensland", 
                                                   "Northern Territory", "Victoria", "Tasmania", "Western Australia", "South Australia"), 2),
                                      Country.Code = rep('AUL', 18),
                                      Indicator.Name = c(rep('Population (% of total)', 9),
                                                         rep('Population, total', 9)),
                                      Indicator.Code = c(rep('SP.POP.TOTL.ZS', 9),
                                                         rep('SP.POP.TOTL', 9)),
                                      X2016 = c(100,(431215/24511800)*100, (8166369/24511800)*100,
                                                (5184847/24511800)*100,(246500/24511800)*100, (6680648/24511800)*100,
                                                (541071/24511800)*100,(2667130/24511800)*100, (1770591/24511800)*100,
                                                
                                                173500, 431215, 8166369, 5184847, 246500, 6680648, 541071, 2667130, 1770591)))

azerbaijan_data <- as.data.frame(cbind(country = rep('Azerbaijan', 6),
                                       province = rep(c('Shaki', 'Qazax', 'Baki'),2),
                                       Country.Code = rep('AZE', 6),
                                       Indicator.Name = c(rep('Population (% of total)', 3),
                                                          rep('Population, total', 3)),
                                       Indicator.Code = c(rep('SP.POP.TOTL.ZS', 3),
                                                          rep('SP.POP.TOTL', 3)
                                       ),
                                       X2016 = c((173500/9762000)*100,(90800/9762000)*100, (2092400/9762000)*100,
                                                 173500, 90800, 2092400)
))

bosnia_data <- as.data.frame(cbind(country = rep('Bosnia and Herzegovina', 2),
                                       province = rep(c('Srpska'),2),
                                       Country.Code = rep('BIH', 2),
                                       Indicator.Name = c(rep('Population (% of total)', 1),
                                                          rep('Population, total', 1)),
                                       Indicator.Code = c(rep('SP.POP.TOTL.ZS', 1),
                                                          rep('SP.POP.TOTL', 1)
                                       ),
                                       X2016 = c((1327000/3517000)*100, 1327000)
))

canada_data <- as.data.frame(cbind(country = rep('Canada', 28),
                                   province = rep(c(NA, "New Brunswick", "Ontario", "Alberta", "Nova Scotia", "Newfoundland and Labrador",
                                                    "Quebec", "Nunavut", "Prince Edward Island", "British Columbia", "Yukon", "Manitoba",
                                                    "Northwest Territories", "Saskatchewan"),2),
                                   Country.Code = rep('CAN', 28),
                                   Indicator.Name = c(rep('Population (% of total)', 14),
                                                      rep('Population, total', 14)),
                                   Indicator.Code = c(rep('SP.POP.TOTL.ZS', 14),
                                                      rep('SP.POP.TOTL', 14)
                                   ),
                                   X2016 = c(100, (782078/37922003)*100, (14755211/37922003)*100,(4436258/37922003)*100,
                                             (979449/37922003)*100, (520438/37922003)*100, (8575944/37922003)*100,
                                             (39407/37922003)*100, (159819/37922003)*100, (5153039/37922003)*100,
                                             (42192/37922003)*100, (1380935/37922003)*100, (45136/37922003)*100,
                                             (1098352/37922003)*100,
                                             37922003, 782078, 14755211, 4436258, 979449, 520438, 8575944, 39407,
                                              159819, 5153039, 42192, 1380935, 45136, 1098352)
))

china_data <- as.data.frame(cbind(country = rep('China', 2),
                                    province = c(rep('Tibet', 2)),
                                    Country.Code = rep('CHN', 2),
                                    Indicator.Name = c('Population (% of total)', 'Population, total'),
                                    Indicator.Code = c('SP.POP.TOTL.ZS', 'SP.POP.TOTL'),
                                    X2016 = c((3506000/1378665000)*100, 3506000)
))

cyprus_data <- as.data.frame(cbind(country = rep('Cyprus', 6),
                                   province = rep(c(NA, 'Nicosia', 'Larnaka'),2),
                                   Country.Code = rep('CYP', 6),
                                   Indicator.Name = c(rep('Population (% of total)', 3),
                                                      rep('Population, total', 3)),
                                   Indicator.Code = c(rep('SP.POP.TOTL.ZS', 3),
                                                      rep('SP.POP.TOTL', 3)
                                   ),
                                   X2016 = c(100, (334120/880600)*100, (145365/880600)*100,
                                             880600, 334120, 145365)
))

czech_data <- as.data.frame(cbind(country = rep('Czechia', 6),
                                   province = rep(c(NA, "Hlavni mesto Praha", "Olomoucky"),2),
                                   Country.Code = rep('CZE', 6),
                                   Indicator.Name = c(rep('Population (% of total)', 3),
                                                      rep('Population, total', 3)),
                                   Indicator.Code = c(rep('SP.POP.TOTL.ZS', 3),
                                                      rep('SP.POP.TOTL', 3)
                                   ),
                                   X2016 = c(100, (1324277/10724008)*100, (632492/10724008)*100,
                                             10724008, 1324277, 632492)
))

denmark_data <- as.data.frame(cbind(country = c(rep(c("Denmark"), 2)),
                                  province = rep(NA,2),
                                  Country.Code = rep(c("DEN"), 2),
                                  Indicator.Name = c(rep('Population (% of total)', 1),
                                                     rep('Population, total', 1)),
                                  Indicator.Code = c(rep('SP.POP.TOTL.ZS', 1),
                                                     rep('SP.POP.TOTL', 1)
                                  ),
                                  X2016 = c(100, 
                                            5806000)
))


germany_data <- as.data.frame(cbind(country = rep('Germany', 34),
                                    province = rep(c(NA, "Baden-Wuerttemberg", "Brandenburg", "Bremen", "Thuringia", "Saxony", "North Rhine-Westphalia",
                                                     "Mecklenburg-Vorpommern", "Bavaria", "Lower Saxony", "Hamburg", "Saxony-Anhalt",
                                                     "Schleswig-Holstein", "Rheinland-Pfalz", "Saarland", "Hesse", "Berlin"), 2),
                                    Country.Code = rep('GRM', 34),
                                    Indicator.Name = c(rep('Population (% of total)', 17),
                                                       rep('Population, total', 17)),
                                    Indicator.Code = c(rep('SP.POP.TOTL.ZS', 17),
                                                       rep('SP.POP.TOTL', 17)),
                                    X2016 = c(100, (11100394/37922003)*100, (2521893/37922003)*100, (681202/37922003)*100,(2143145/37922003)*100,
                                              (4077937/37922003)*100, (17932651/37922003)*100, (1609675/37922003)*100,
                                              (13124737/37922003)*100, (7993448/37922003)*100, (1847253/37922003)*100,
                                              (2208321/37922003)*100, (2896712/37922003)*100, (4084844/37922003)*100,
                                              (990509/37922003)*100, (6288080/37922003)*100, (2521893/37922003)*100, 
                                              37922003, 11100394, 2521893, 681202, 2143145, 4077937, 17932651, 1609675, 13124737,
                                              7993448, 1847253, 2208321, 2896712, 4084844, 990509, 6288080, 2521893)))

india_data <- as.data.frame(cbind(country = rep('India', 4),
                                       province = rep(c('Ladakh', 'Telangana'),2),
                                       Country.Code = rep('IND', 4),
                                       Indicator.Name = c(rep('Population (% of total)', 2),
                                                          rep('Population, total', 2)),
                                       Indicator.Code = c(rep('SP.POP.TOTL.ZS', 2),
                                                          rep('SP.POP.TOTL', 2)
                                       ),
                                       X2016 = c((274289/1324171000)*100,
                                                 (35193978/1324171000)*100, 
                                                 274289, 
                                                 35193978)
))

japan_data <- as.data.frame(cbind(country = rep('Japan', 80),
                                    province = rep(c(NA, "Saitama", "Hiroshima", "Saga", "Okayama", "Fukuoka", "Tottori", 
                                                     "Chiba", "Hokkaido", "Fukushima", "Iwate", "Miyagi", "Akita", "Yamagata", "Okinawa",
                                                     "Shimane", "Aomori", "Tokyo", "Oita", "Osaka", "Aichi", "Gifu","Kanagawa",
                                                     "Fukui", "Wakayama", "Yamaguchi", "Nara", "Kagoshima", "Miyazaki", "Kumamoto",
                                                     "Ishikawa", "Mie", "Nagasaki", "Ibaraki", "Niigata", "Kyoto", "Hyogo",
                                                     "Nagano", "Yamanashi", "Shizuoka"), 2),
                                    Country.Code = rep('JPN', 80),
                                    Indicator.Name = c(rep('Population (% of total)', 40),
                                                       rep('Population, total', 40)),
                                    Indicator.Code = c(rep('SP.POP.TOTL.ZS', 40),
                                                       rep('SP.POP.TOTL', 40)),
                                    X2016 = c(100, (7261271/125480000)*100, (2844963/125480000)*100, (833245/125480000)*100,
                                              (1922181/125480000)*100, (5102871/125480000)*100, 
                                              (573648/125480000)*100, (6224027/125480000)*100, (5383579/125480000)*100, 
                                              (1913606/125480000)*100, (1279814/125480000)*100, (2334215/125480000)*100, 
                                              (1022839/125480000)*100, (1122957/125480000)*100, (1434138/125480000)*100, 
                                              (694188/125480000)*100, (1308649/125480000)*100,  (13513734/125480000)*100,
                                              (1166729/125480000)*100, (8838908/125480000)*100, (7484094/125480000)*100,
                                              (2032533/125480000)*100, (9127323/125480000)*100, (787099/125480000)*100,
                                              (963850/125480000)*100, (1405007/125480000)*100, (1365008/125480000)*100,
                                              (1648752/125480000)*100, (1104377/125480000)*100, (1786969/125480000)*100,
                                              (1154343/125480000)*100, (1815827/125480000)*100, (1377780/125480000)*100,
                                              (2917857/125480000)*100, (2305098/125480000)*100,
                                              (2610140/125480000)*100, (5536989/125480000)*100, (2099759/125480000)*100, 
                                              (835165/125480000)*100, (3701181/125480000)*100, 
                                              
                                              125480000, 7261271, 2844963, 833245, 1922181, 5102871 , 573648, 6224027,
                                              5383579, 1913606, 1279814, 2334215, 1122957, 1434138, 6288080, 694188, 1308649, 
                                              13513734, 1166729, 8838908, 7484094, 2032533, 9127323, 787099, 963850,
                                              1405007, 1365008, 1648752, 1104377, 1786969, 1154343, 1815827, 1377780,
                                              2917857, 2305098, 2610140, 5536989, 2099759, 835165, 3701181)))


kazakhstan_data <- as.data.frame(cbind(country = rep('Kazakhstan', 4),
                                  province = c(rep('Shymkent', 2), rep('Kyzlorda Region',2)),
                                  Country.Code = rep('KAZ', 4),
                                  Indicator.Name = rep(c('Population (% of total)', 'Population, total'), 2),
                                  Indicator.Code = rep(c('SP.POP.TOTL.ZS', 'SP.POP.TOTL'), 2),
                                  X2016 = c((1042218/17797000)*100, 1042218,
                                            (590000/17797000)*100, 590000)
))

lebanon_data <- as.data.frame(cbind(country = rep('Lebanon', 2),
                                       province = c(rep('Baalbek-Hermel', 2)),
                                       Country.Code = rep('LBN', 2),
                                       Indicator.Name = c('Population (% of total)', 'Population, total'),
                                       Indicator.Code = c('SP.POP.TOTL.ZS', 'SP.POP.TOTL'),
                                       X2016 = c((416427/6007000)*100, 416427)
))

macau_data <- as.data.frame(cbind(country = rep('Macau', 2),
                                   province = rep(c('Macau'),2),
                                   Country.Code = rep('MAC', 2),
                                   Indicator.Name = c(rep('Population (% of total)', 1),
                                                      rep('Population, total', 1)),
                                   Indicator.Code = c(rep('SP.POP.TOTL.ZS', 1),
                                                      rep('SP.POP.TOTL', 1)
                                   ),
                                   X2016 = c(100, 640445)
))

hk_data <- as.data.frame(cbind(country = rep('Hong Kong', 2),
                                  province = rep(c('Hong Kong'),2),
                                  Country.Code = rep('HK', 2),
                                  Indicator.Name = c(rep('Population (% of total)', 1),
                                                     rep('Population, total', 1)),
                                  Indicator.Code = c(rep('SP.POP.TOTL.ZS', 1),
                                                     rep('SP.POP.TOTL', 1)
                                  ),
                                  X2016 = c(100, 7507000)
))

mauritania_data <- as.data.frame(cbind(country = rep('Mauritania', 4),
                                       province = c(rep('Nouakchott Ouest', 2), rep('Nouakchott Nord',2)),
                                       Country.Code = rep('MRT', 4),
                                       Indicator.Name = rep(c('Population (% of total)', 'Population, total'), 2),
                                       Indicator.Code = rep(c('SP.POP.TOTL.ZS', 'SP.POP.TOTL'), 2),
                                       X2016 = c((165814/17797000)*100, 165814,
                                                 (366912/17797000)*100, 366912)
))

moldova_data <- as.data.frame(cbind(country = rep('Moldova', 2),
                                    province = c(rep('Transnistria', 2)),
                                    Country.Code = rep('MDA', 2),
                                    Indicator.Name = c('Population (% of total)', 'Population, total'),
                                    Indicator.Code = c('SP.POP.TOTL.ZS', 'SP.POP.TOTL'),
                                    X2016 = c((469000/3552000)*100, 469000)
))

morocco_data <- as.data.frame(cbind(country = rep('Morocco', 10),
                               province = rep(c(NA, "Casablanca-Settat", "Tanger-Tetouan-Al Hoceima", 
                                                "Rabat-Sale-Kenitra", "Fes-Meknes"), 2),
                               Country.Code = rep('MOR', 10),
                               Indicator.Name = c(rep('Population (% of total)', 5),
                                                  rep('Population, total', 5)),
                               Indicator.Code = c(rep('SP.POP.TOTL.ZS', 5),
                                                  rep('SP.POP.TOTL', 5)),
                               X2016 = c(100, (6861739/37241357)*100, (3556729/37241357)*100, (4580866/37241357)*100,
                                         (4236892/37241357)*100,
                                         
                                         37241357, 6861739, 3556729, 4580866, 4236892)))

nigeria_data <- as.data.frame(cbind(country = rep('Nigeria', 2),
                                    province = c(rep('FCT', 2)),
                                    Country.Code = rep('NGA', 2),
                                    Indicator.Name = c('Population (% of total)', 'Population, total'),
                                    Indicator.Code = c('SP.POP.TOTL.ZS', 'SP.POP.TOTL'),
                                    X2016 = c((2238800/185990000)*100, 2238800)
))

spain_data <- as.data.frame(cbind(country = rep('Spain', 30),
                               province = rep(c(NA, "Basque Country", "Catalonia", "Madrid", "Castille and Leon", "Valencia",
                                                "Asturias", "Navarre", "Andalusia", "Balearic Islands", "La Rioja", "Castille-La Mancha",
                                                "Canary Islands", "Galicia", "Extremadura"), 2),
                               Country.Code = rep('SPA', 30),
                               Indicator.Name = c(rep('Population (% of total)', 15),
                                                  rep('Population, total', 15)),
                               Indicator.Code = c(rep('SP.POP.TOTL.ZS', 15),
                                                  rep('SP.POP.TOTL', 15)),
                               X2016 = c(100, (2167166/46598049)*100, (7518903/46598049)*100, (6378297/46598049)*100,
                                         (2495689/46598049)*100, (4956427/46598049)*100, (1058975/46598049)*100, (636450/46598049)*100, 
                                         (8388875/46598049)*100, (1115841/46598049)*100, (315223/46598049)*100, (2075197/46598049)*100, 
                                         (2114845/46598049)*100, (2747226/46598049)*100, (1099605/46598049)*100, 
                                         
                                         46598049, 2167166, 7518903, 6378297, 2495689, 4956427, 1058975, 636450, 8388875, 1115841,
                                         315223, 2075197, 2114845, 2747226, 1099605)))

uk_data <- as.data.frame(cbind(country = rep('United Kingdom', 10),
                                  province = rep(c(NA, "Scotland", "Northern Ireland", "England", "Wales"), 2),
                                  Country.Code = rep('UK', 10),
                                  Indicator.Name = c(rep('Population (% of total)', 5),
                                                     rep('Population, total', 5)),
                                  Indicator.Code = c(rep('SP.POP.TOTL.ZS', 5),
                                                     rep('SP.POP.TOTL', 5)),
                                  X2016 = c(100, (5463300/66796807)*100, (1893667/66796807)*100, (56286961/66796807)*100,
                                            (3152879/66796807)*100,
                                            
                                            66796807, 5463300, 1893667, 56286961, 3152879)))


population_data <- rbind(population_data, australia_data, azerbaijan_data, cyprus_data, czech_data, india_data, kazakhstan_data, lebanon_data, mauritania_data, moldova_data,
                         nigeria_data, bosnia_data, canada_data, japan_data, germany_data, uk_data, spain_data, macau_data, hk_data,
                         morocco_data, china_data, denmark_data, andorra_data)

population_data_total <- population_data[ which(population_data$Indicator.Name == "Population, total"), ]

population_data_perc <- population_data[ which(population_data$Indicator.Name == "Population (% of total)"), ]

unmatched <- anti_join(coronanet, population_data_total, by = c('country', 'province'))

#Afghanistan mispellings

population_data_total$province[which(population_data_total$country == 'Afghanistan' & 
                                       population_data_total$province == "Ghor")] <- 'Ghowr'

population_data_total$province[which(population_data_total$country == 'Afghanistan' & 
                                       population_data_total$province == "Hilmand")] <- 'Helmand'

population_data_total$province[which(population_data_total$country == 'Afghanistan' & 
                                       population_data_total$province == "Sar-e-Pul")] <- 'Sar-e Pol'

population_data_total$province[which(population_data_total$country == 'Afghanistan' & 
                                       population_data_total$province == "Khost")] <- 'Khowst'

population_data_total$province[which(population_data_total$country == 'Afghanistan' & 
                                       population_data_total$province == "Jawzjan")] <- 'Jowzjan'

#Algeria mispellings

population_data_total$province[which(population_data_total$country == 'Algeria' & 
                                       population_data_total$province == "El-Tarf")] <- 'El Tarf'

population_data_total$province[which(population_data_total$country == 'Algeria' & 
                                       population_data_total$province == "Alger")] <- 'Algiers'

#Angola mispellings

population_data_total$province[which(population_data_total$country == 'Angola' & 
                                       population_data_total$province == "Cuanza Sul")] <- 'Kwanza Sul'

population_data_total$province[which(population_data_total$country == 'Angola' & 
                                       population_data_total$province == "Kuanza Norte")] <- 'Cuanza Norte'

population_data_total$province[which(population_data_total$country == 'Angola' & 
                                       population_data_total$province == "Lunda Norte")] <- 'Luanda Norte'

population_data_total$province[which(population_data_total$country == 'Angola' & 
                                       population_data_total$province == "Cuando Cubango")] <- 'Cuando Cobango'

#Argentina mispellings

population_data_total$province[which(population_data_total$country == 'Argentina' & 
                                       population_data_total$province == "Buenos Aires D.f.")] <- 'Buenos Aires F.D.'

population_data_total$province[which(population_data_total$country == 'Argentina' & 
                                       population_data_total$province == "Tierra Del Fuego")] <- 'Tierra del Fuego'

population_data_total$province[which(population_data_total$country == 'Argentina' & 
                                       population_data_total$province == "Santiago Del Estero")] <- 'Santiago del Estero'

#Armenia mispellings

population_data_total$province[which(population_data_total$country == 'Armenia' & 
                                       population_data_total$province == "Gergharkunik")] <- 'Gegharkunik'

#Bangladesh mispellings
 
population_data_total$province[which(population_data_total$country == 'Bangladesh' & 
                                       population_data_total$province == "Rajshahi")] <- 'Rajshahi Division'

#Bhutan mispellings

population_data_total$province[which(population_data_total$country == 'Bhutan' & 
                                       population_data_total$province == "Lhuentse")] <- 'Lhuntse'

population_data_total$province[which(population_data_total$country == 'Bhutan' & 
                                       population_data_total$province == "Tsirang")] <- 'Chirang'

#Bosnia mispellings

population_data_total$province[which(population_data_total$country == 'Bosnia and Herzegovina' & 
                                       population_data_total$province == "Federacija Bosne I Hercegovine")] <- 'Federation of B&H'


#Brazil mispellings

population_data_total$province[which(population_data_total$country == 'Brazil' & 
                                       population_data_total$province == "Rio De Janeiro")] <- 'Rio de Janeiro'

population_data_total$province[which(population_data_total$country == 'Brazil' & 
                                       population_data_total$province == "Rio Grande Do Sul")] <- 'Rio Grande do Sul'

population_data_total$province[which(population_data_total$country == 'Brazil' & 
                                       population_data_total$province == "Mato Grosso Do Sul")] <- 'Mato Grosso do Sul'

#Burkina Faso mispellings

population_data_total$province[which(population_data_total$country == 'Burkina Faso' & 
                                       population_data_total$province == "Sud-ouest")] <- 'Sud-Ouest'

population_data_total$province[which(population_data_total$country == 'Burkina Faso' & 
                                       population_data_total$province == "Centre-nord")] <- 'Centre-Nord'

population_data_total$province[which(population_data_total$country == 'Burkina Faso' & 
                                       population_data_total$province == "Hauts-bassins")] <- 'Hauts-Bassins'


#Cameroon mispellings

population_data_total$province[which(population_data_total$country == 'Cameroon' & 
                                       population_data_total$province == "Nord-Ouest")] <- 'North-West'

population_data_total$province[which(population_data_total$country == 'Cameroon' & 
                                       population_data_total$province == "Ouest")] <- 'West'

population_data_total$province[which(population_data_total$country == 'Cameroon' & 
                                       population_data_total$province == "Sud-Ouest")] <- 'South-West'


#Chile mispellings

population_data_total$province[which(population_data_total$country == 'Chile' & 
                                       population_data_total$province == "Metropolitana")] <- 'Santiago Metropolitan'

#China mispellings

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Hubei Sheng")] <- 'Hubei'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Hunan Sheng")] <- 'Hunan'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Guangdong Sheng")] <- 'Guangdong'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Sichuan Sheng")] <- 'Sichuan'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Gansu Sheng")] <- 'Gansu'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Shandong Sheng")] <- 'Shandong'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Xinjiang Uygur Zizhiqu")] <- 'Xinjiang'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Tianjin Shi")] <- 'Tianjin'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Fujian Sheng")] <- 'Fujian'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Shanghai Shi")] <- 'Shanghai'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Heilongjiang Sheng")] <- 'Heilongjiang'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Zhejiang Sheng")] <- 'Zhejiang'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Anhui Sheng")] <- 'Anhui'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Zhejiang Sheng")] <- 'Zhejiang'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Hebei Sheng")] <- 'Hebei'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Ningxia Huizu Zizhiqu")] <- 'Ningxia Hui Autonomous Region'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Jiangsu Sheng")] <- 'Jiangsu'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Henan Sheng")] <- 'Henan'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Jilin Sheng")] <- 'Jilin'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Guizhou Sheng")] <- 'Guizhou'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Yunnan Sheng")] <- 'Yunnan'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Nei Mongol Zizhiqu")] <- 'Inner Mongolia'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Liaoning Sheng")] <- 'Liaoning'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Chongqing Shi")] <- 'Chongqing'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Beijing Shi")] <- 'Beijing'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Jiangxi Sheng")] <- 'Jiangxi'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Guangxi Zhuangzu Zizhiqu")] <- 'Guangxi'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Hainan Sheng")] <- 'Hainan'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Qinghai Sheng")] <- 'Qinghai'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Shaanxi Sheng")] <- 'Shaanxi'

population_data_total$province[which(population_data_total$country == 'China' & 
                                       population_data_total$province == "Shanxi Sheng")] <- 'Shanxi'


#Colombia mispellings

population_data_total$province[which(population_data_total$country == 'Colombia' & 
                                       population_data_total$province == "Valle Del Cauca")] <- 'Valle del Cauca'

population_data_total$province[which(population_data_total$country == 'Colombia' & 
                                       population_data_total$province == "Bogota")] <- 'Bogota D.C.'

population_data_total$province[which(population_data_total$country == 'Colombia' & 
                                       population_data_total$province == "Norte De Santander")] <- 'Norte de Santander'

#Croatia misspellings

population_data_total$province[which(population_data_total$country == 'Croatia' & 
                                       population_data_total$province == "Zagreb")] <- 'City of Zagreb'

population_data_total$province[which(population_data_total$country == 'Croatia' & 
                                       population_data_total$province == "Dubrovnik-neretva")] <- 'Dubrovacko-Neretvanska'

population_data_total$province[which(population_data_total$country == 'Croatia' & 
                                       population_data_total$province == "Istra")] <- 'Istria'

#Cuba misspellings

population_data_total$province[which(population_data_total$country == 'Cuba' & 
                                       population_data_total$province == "Pinar Del Rio")] <- 'Pinar del Rio'

#Dominican Republic misspellings

population_data_total$province[which(population_data_total$country == 'Dominican Republic' & 
                                       population_data_total$province == "Distrito Nacional")] <- 'Nacional'

#Estonia misspellings

population_data_total$province[which(population_data_total$country == 'Estonia' & 
                                       population_data_total$province == "Harju")] <- 'Harjumaa'

population_data_total$province[which(population_data_total$country == 'Estonia' & 
                                       population_data_total$province == "Ida-Viru")] <- 'Ida-Virumaa'

population_data_total$province[which(population_data_total$country == 'Estonia' & 
                                       population_data_total$province == "Polva")] <- 'Polvamaa'

population_data_total$province[which(population_data_total$country == 'Estonia' & 
                                       population_data_total$province == "Valga")] <- 'Valgamaa'

population_data_total$province[which(population_data_total$country == 'Estonia' & 
                                       population_data_total$province == "Voru")] <- 'Vorumaa'

#Ethiopia misspellings

population_data_total$province[which(population_data_total$country == 'Ethiopia' & 
                                       population_data_total$province == "Oromia")] <- 'Oromiya'

population_data_total$province[which(population_data_total$country == 'Ethiopia' & 
                                       population_data_total$province == "Beneshangul Gumu")] <- 'Binshangul Gumuz'

#Finland misspellings

population_data_total$province[which(population_data_total$country == 'Finland' & 
                                       population_data_total$province == "Oromia")] <- 'Oromiya'

#Gambia misspellings

population_data_total$province[which(population_data_total$country == 'Gambia' & 
                                       population_data_total$province == "Basse")] <- "Upper River"

population_data_total$province[which(population_data_total$country == 'Gambia' & 
                                       population_data_total$province == "Brikama")] <- "Western"

#Georgia misspellings

population_data_total$province[which(population_data_total$country == 'Georgia' & 
                                       population_data_total$province == "Racha-Lechkhumi and Kvemo (lower) Svaneti")] <- 'Racha-Lechkhumi and Kvemo Svaneti'

population_data_total$province[which(population_data_total$country == 'Georgia' & 
                                       population_data_total$province == "Samergelo and Zemo (upper) Svaneti")] <- 'Samegrelo and Zemo Svaneti'

population_data_total$province[which(population_data_total$country == 'Georgia' & 
                                       population_data_total$province == "Tbilisi")] <- "T'bilisi"

#Guatemala misspellings

population_data_total$province[which(population_data_total$country == 'Guatemala' & 
                                       population_data_total$province == "Suchitepequez")] <- 'Suchitepeque'

#India misspellings

population_data_total$province[which(population_data_total$country == 'India' & 
                                       population_data_total$province == "Jammu/Kashmir")] <- 'Jammu and Kashmir'

population_data_total$province[which(population_data_total$country == 'India' & 
                                       population_data_total$province == "Orissa")] <- 'Odisha'

#Indonesia misspellings

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "DKI Jakarta")] <- 'Jakarta'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Jawa Tengah")] <- 'Central Java'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Jawa Timur")] <- 'East Java'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Jawa Barat")] <- 'West Java'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Nanggroe Aceh Darussalam")] <- 'Aceh'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Kalimantan Barat")] <- 'West Kalimantan'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Kalimantan Tengah")] <- 'Central Kalimantan'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Sulawesi Selatan")] <- 'South Sulawesi'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Nusa Tenggara Timur")] <- 'East Nusa Tenggara'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Sumatera Barat")] <- 'West Sumatra'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "DI Yogyakarta")] <- 'Yogyakarta'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Kalimantan Selatan")] <- 'South Kalimantan'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Sumatera Selatan")] <- 'South Sumatra'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Papua Barat")] <- 'West Papua'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Nusa Tenggara Barat")] <- 'West Nusa Tenggara'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Sulawesi Tengah")] <- 'Central Sulawesi'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Sumatera Utara")] <- 'North Sumatra'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Kepulauan Bangka-Belitung")] <- 'Bangka-Belitung Islands'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Sulawesi Tenggara")] <- 'Southeast Sulawesi'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Sulawesi Utara")] <- 'North Sulawesi'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Sulawesi Barat")] <- 'West Sulawesi'

population_data_total$province[which(population_data_total$country == 'Indonesia' & 
                                       population_data_total$province == "Kepulauan Riau")] <- 'Riau Islands'

#Iran misspellings

population_data_total$province[which(population_data_total$country == 'Iran' & 
                                       population_data_total$province == "Ghom")] <- 'Qom'


#Iraq misspellings

population_data_total$province[which(population_data_total$country == 'Iraq' & 
                                       population_data_total$province == "Erbil")] <- 'Arbil'

population_data_total$province[which(population_data_total$country == 'Iraq' & 
                                       population_data_total$province == "Basrah")] <- 'Basra'

population_data_total$province[which(population_data_total$country == 'Iraq' & 
                                       population_data_total$province == "Najaf")] <- 'An Najaf'

population_data_total$province[which(population_data_total$country == 'Iraq' & 
                                       population_data_total$province == "Thi-Qar")] <- 'Dhi Qar'

population_data_total$province[which(population_data_total$country == 'Iraq' & 
                                       population_data_total$province == "Kerbala")] <- "Muhafazat Karbala'"

population_data_total$province[which(population_data_total$country == 'Iraq' & 
                                       population_data_total$province == "Kerbala")] <- "Qom"


#Kazakhstan misspellings

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Kyzylordinskaya")] <- "Baikonur"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Astana City area")] <- "Nur-Sultan"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Akmolinskaya")] <- "Akmola Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Karagandinskaya")] <- "Karaganda Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Southern")] <- "Turkistan Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Almaty City area")] <- "Almaty"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Mangistauskaya")] <- "Mangystau Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Atyrauskaya")] <- "Atyau Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Pavlodarskaya")] <- "Pavlodar Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Western")] <- "West Kazakhstan Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Aktyubinskaya")] <- "Aktobe Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Jambylslkaya")] <- "Jambyl Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Northern")] <- "North Kazakhstan Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Almatinskaya")] <- "Almaty Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Kustanayskaya")] <- "Kostanay Region"

population_data_total$province[which(population_data_total$country == 'Kazakhstan' & 
                                       population_data_total$province == "Eastern")] <- "East Kazakhstan Region"

#Kenya misspellings

population_data_total$province[which(population_data_total$country == 'Kenya' & 
                                       population_data_total$province == "Nairobi")] <- "Nairobi Area"

#Lebanon misspellings

population_data_total$province[which(population_data_total$country == 'Lebanon' & 
                                       population_data_total$province == "North")] <- "Liban-Nord"

population_data_total$province[which(population_data_total$country == 'Lebanon' & 
                                       population_data_total$province == "Bekaa")] <- "Beqaa"

population_data_total$province[which(population_data_total$country == 'Lebanon' & 
                                       population_data_total$province == "Mount Lebanon")] <- "Mont-Liban"

population_data_total$province[which(population_data_total$country == 'Lebanon' & 
                                       population_data_total$province == "South")] <- "South Governorate"

#Lithuania misspellings

population_data_total$province[which(population_data_total$country == 'Lithuania' & 
                                       population_data_total$province == "Vilniaus")] <- "Vilnius"

#Mauritania misspellings

population_data_total$province[which(population_data_total$country == 'Mauritania' & 
                                       population_data_total$province == "Dakhlet-Nouadhibou")] <- "Dakhlet Nouadhibou"

population_data_total$province[which(population_data_total$country == 'Mauritania' & 
                                       population_data_total$province == "Tiris-Zemmour")] <- "Tiris Zemmour"

#Mexico misspellings

population_data_total$province[which(population_data_total$country == 'Mexico' & 
                                       population_data_total$province == "Distrito Federal")] <- "Mexico City"

#Mongolia misspellings

population_data_total$province[which(population_data_total$country == 'Mongolia' & 
                                       population_data_total$province == "Dornogovi")] <- "East Gobi Aymag"

#Myanmar misspellings

population_data_total$province[which(population_data_total$country == 'Myanmar' & 
                                       population_data_total$province == "Sagaing")] <- "Sagain"

population_data_total$province[which(population_data_total$country == 'Myanmar' & 
                                       population_data_total$province == "Naypyidaw")] <- "Nay Pyi Taw"

population_data_total$province[which(population_data_total$country == 'Myanmar' & 
                                       population_data_total$province == "Yangon")] <- "Rangoon"

population_data_total$province[which(population_data_total$country == 'Myanmar' & 
                                       population_data_total$province == "Kayar")] <- "Kayah"

population_data_total$province[which(population_data_total$country == 'Myanmar' & 
                                       population_data_total$province == "Ayeyawaddy")] <- "Ayeyarwady"

population_data_total$province[which(population_data_total$country == 'Myanmar' & 
                                       population_data_total$province == "Taninthayi")] <- "Tanintharyi"

#Nepal misspellings

population_data_total$province[which(population_data_total$country == 'Nepal' & 
                                       population_data_total$province == "Eastern")] <- "Province 1"

population_data_total$province[which(population_data_total$country == 'Nepal' & 
                                       population_data_total$province == "Central")] <- "Province 3"


#Philippines misspellings

population_data_total$province[which(population_data_total$country == 'Philippines' & 
                                       population_data_total$province == "Davao Region")] <- "Davao Oriental"

population_data_total$province[which(population_data_total$country == 'Philippines' & 
                                       population_data_total$province == "Calabarzon")] <- "Quezon"

population_data_total$province[which(population_data_total$country == 'Philippines' & 
                                       population_data_total$province == "Central Visayas")] <- "Cebu"

population_data_total$province[which(population_data_total$country == 'Philippines' & 
                                       population_data_total$province == "Western Visayas")] <- "Antique"


#Rwanda misspellings

population_data_total$province[which(population_data_total$country == 'Rwanda' & 
                                       population_data_total$province == "Western")] <- "Western Province"

population_data_total$province[which(population_data_total$country == 'Rwanda' & 
                                       population_data_total$province == "Kigali City")] <- "Kigali"


#South Korea misspellings

population_data_total$province[which(population_data_total$country == 'South Korea' & 
                                       population_data_total$province == "Taegu")] <- "Daegu"

population_data_total$province[which(population_data_total$country == 'South Korea' & 
                                       population_data_total$province == "Chollanam-do")] <- "Jeollanam-do"

population_data_total$province[which(population_data_total$country == 'South Korea' & 
                                       population_data_total$province == "Kyonggi-do")] <- "Gyeonggi-do"


#South Sudan misspellings

population_data_total$province[which(population_data_total$country == 'South Sudan' & 
                                       population_data_total$province == "Warab")] <- "Warrap"

population_data_total$province[which(population_data_total$country == 'South Sudan' & 
                                       population_data_total$province == "Northern Bahr El Ghazal")] <- "Northern Bahr el Ghazal"

population_data_total$province[which(population_data_total$country == 'South Sudan' & 
                                       population_data_total$province == "Jonglei")] <- "Jonglei, Lakes"


#Sri Lanka misspellings

population_data_total$province[which(population_data_total$country == 'Sri Lanka' & 
                                       population_data_total$province == "Eastern")] <- "Eastern Province"


#Sudan misspellings

population_data_total$province[which(population_data_total$country == 'Sudan' & 
                                       population_data_total$province == "Al Jazeera")] <- "Al Jazirah"

population_data_total$province[which(population_data_total$country == 'Sudan' & 
                                       population_data_total$province == "Sennar")] <- "Sinnar"

population_data_total$province[which(population_data_total$country == 'Sudan' & 
                                       population_data_total$province == "Northern Kordofan")] <- "North Kordofan"

population_data_total$province[which(population_data_total$country == 'Sudan' & 
                                       population_data_total$province == "Gadaref")] <- "Al Qadarif"


#Tajikistan misspellings

population_data_total$province[which(population_data_total$country == 'Tajikistan' & 
                                       population_data_total$province == "Sogd")] <- "Sughd"

#Tanzania misspellings

population_data_total$province[which(population_data_total$country == 'Tanzania' & 
                                       population_data_total$province == "Dar-es-salaam")] <- "Dar es Salaam"

#Thailand misspellings

population_data_total$province[which(population_data_total$country == 'Thailand' & 
                                       population_data_total$province == "Chonburi")] <- "Chon Buri"

#Tunisia misspellings

population_data_total$province[which(population_data_total$country == 'Tunisia' & 
                                       population_data_total$province == "Gabes")] <- "Qabis"

population_data_total$province[which(population_data_total$country == 'Tunisia' & 
                                       population_data_total$province == "Le Kef")] <- "Kef"

population_data_total$province[which(population_data_total$country == 'Tunisia' & 
                                       population_data_total$province == "Sousse")] <- "Susah"

population_data_total$province[which(population_data_total$country == 'Tunisia' & 
                                       population_data_total$province == "Nabeul")] <- "Nabul"

#Turkey misspellings

population_data_total$province[which(population_data_total$country == 'Turkey' & 
                                       population_data_total$province == "Kutahya")] <- "Kuetahya"

#Uzbekistan misspellings

population_data_total$province[which(population_data_total$country == 'Uzbekistan' & 
                                       population_data_total$province == "Tashkent city")] <- "Toshkent Shahri"

population_data_total$province[which(population_data_total$country == 'Uzbekistan' & 
                                       population_data_total$province == "Tashkent")] <- "Toshkent"

population_data_total$province[which(population_data_total$country == 'Uzbekistan' & 
                                       population_data_total$province == "Samarkand")] <- "Samarqand"

#Venezuela misspellings

population_data_total$province[which(population_data_total$country == 'Venezuela' & 
                                       population_data_total$province == "Distrito Capital")] <- "Distrito Federal"

#Vietnam misspellings

population_data_total$province[which(population_data_total$country == 'Vietnam' & 
                                       population_data_total$province == "Ho Chi Minh City")] <- "Ho Chi Minh"

population_data_total$province[which(population_data_total$country == 'Vietnam' & 
                                       population_data_total$province == "Ha Noi City")] <- "Hanoi"

population_data_total$province[which(population_data_total$country == 'Vietnam' & 
                                       population_data_total$province == "Da Nang City")] <- "Da Nang"


#Yemen misspellings

population_data_total$province[which(population_data_total$country == 'Yemen' & 
                                       population_data_total$province == "Hadramaut")] <- "Hadramawt"

population_data_total$province[which(population_data_total$country == 'Yemen' & 
                                       population_data_total$province == "Marib")] <- "Ma'rib"

population_data_total$province[which(population_data_total$country == 'Yemen' & 
                                       population_data_total$province == "Sana'a")] <- "Sanaa"

population_data_total$province[which(population_data_total$country == 'Yemen' & 
                                       population_data_total$province == "Taizz")] <- "Ta'izz"

population_data_total$province[which(population_data_total$country == 'Yemen' & 
                                       population_data_total$province == "Al Maharah")] <- "Al Mahrah"

population_data_total[which(population_data_total$country == 'Antigua and Barbuda'),]$province

unmatched <- anti_join(coronanet, population_data_total, by = c('country', 'province'))

as.data.frame(filter(unmatched, !country %in% c("Austria", 'Antigua and Barbuda', "Bahamas", "Bahrain", "Barbados", "Belgium", "Belize", "Botswana", "Brunei", "Cape Verde",
                                  "Central African Republic", "Chad", "Comoros",
                                  "Democratic Republic of the Congo", "Djibouti", "Dominica", "Equatorial Guinea",
                                  "Eritrea", "Eswatini", "European Union", "Fiji", "Finland", "France", "Gabon",
                                  "Greece", "Grenada",  "Guyana", "Haiti", "Iceland", "Ireland", "Israel", "Italy",
                                  "Ivory Coast", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Libya",
                                  "Liechtenstein", "Luxembourg", "Malaysia", "Maldives", "Malta", "Marshall Islands", 
                                  "Mauritius", "Micronesia", "Monaco", "Namibia", "Nauru", "Netherlands", 
                                  "New Zealand", "North Korea", "North Macedonia", "Northern Cyprus", "Norway", "Oman",
                                  "Pakistan", "Palau", "Palestine", "Papua New Guinea", "Paraguay", "Portugal", "Qatar",
                                  "Republic of the Congo", "Russia", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines",
                                  "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Serbia", "Seychelles",
                                  "Singapore", "Slovakia", "Solomon Islands", "Somalia", "Suriname", "Sweden",
                                  "Switzerland", "Syria", "Taiwan", "Timor Leste", "Tonga", "Turkmenistan", "Tuvalu", "Uganda",
                                  "Ukraine", "United Arab Emirates", "United States of America", "Vanuatu",
                                  "Vatican")))

coronanet_pop <- merge(coronanet, population_data_total, by = c('country', 'province'))

colnames(coronanet_pop)[6] <- 'population_total'

coronanet_population  <- merge(coronanet_pop, population_data_perc, by = c('country', 'province'))

colnames(coronanet_population)[10] <- 'population_perc'

write.csv2(coronanet_population, '/Users/JBS548/Downloads/coronanet_population.csv')


