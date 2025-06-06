
# cleaning/clean_anes_72_76.R

source("code/cleaning/utils.R")

clean_anes_1972_76 <- function(path) {
  anes7 <- read_and_zap(path)

  cleaned <- anes7 %>%
    mutate(id = row_number()) %>%
    select(id, 
           V742003, V764004, #weights
           V720294, V742406, V763369, #ages (1, 3, 4)
           V720022, #date1
           V720440, #date2
           V742023, #date3
           V763023, #date4
           V763525, #date5
           V720140, V742204, V763174, #partyid
           V720652, V742305, V763286, #liberal-conservative
           V720291, V763926, #stay home
           V720172, V720613, V742265, V763241, V763758, #job guar scale
           V720208, V763273, #health insurance scale
           V720629, V742296, V763264, #minority scale
           V720238, V763796, #abortion
           V720719, V742364, V763833, #democrats therm
           V720721, V742366, V763835, #republican therm
           V720720, V742365, V763832, #blacks therm
           V720718, V763846, #whites therm
           V720724, V742369, V763838, #conservatives therm
           V720709, V742358, V763823, #liberals therm
           V720707, V742356, V763821, #big business therm
           V720722, V742367, V763836, #unions therm
           V720717, V742362, V763831, #military therm 
           V720708, V742357, V763822, #poor people therm
           V720712, V763826, #catholics therm
           V720716, V763842, #jews therm
           V720714, V742360, V763828, #police therm
           V720725, V742370, V763839, #womens mvmt therm
           V720581, V742400, V763745, #trust
           V720582, V742401, V763746, #helpful
           V720583, V742402, V763747, #fair
           V720232, V742302, V763787, #eqrole
           V720093, V720574, V742233, V763166, #crooked
           V720089, V720570, V742229, V763162, #wastetax
           V720090, V720571, V742230, V763163, #trustgov
           V720091, V720572, V742231, V763164, #runfew
           #attention political campaigns (1:very much, 3:somewhat, 5:not much interested, na 8:9), 
           V720163, V763031, 
           #people like me have no say (1:agree, 5:disagree, 8:9 NA)
           V720269,	V720559, V742222, V763815,
           V720288, V763349, #so many vote (1,4) (1 agree, 5 dis, 8 - DK, 9 - NA)
           V720271, V720561, V742224, V763817, #too complicated (1,2,3,5) (1 - agree, 5 - disagree, 8 - DK, 9 - NA, 0 - NA)
           V720272, V720562, V742225, V763818, #pols dont care (1,2,3,5)
           V720094, V720575, V742234, V763741, #gov pay attn (1,2,3,5) (1 - good deal, 3 - some, 5 - not much) (0,8,9 - NA)
           V720096, V720577, V742236, V763743 #electmatter (1,2,3,5) (1 - good deal, 3 - some, 5 not much) (0,8,9, NA)
    ) %>%
    mutate(weight_2 = "1",
           weight_3 = ifelse(V742003 == 0, "1", as.character(V742003)),
           weight_4 = as.character(V764004),
           weight_5 = weight_4,
           age_1 = V720294, 
           age_3 = V742406, 
           age_4 = V763369,
           across(c(age_1, age_3, age_4),
                  ~ifelse(.x == 0, NA, .x)),
           age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3 - 2, age_1),
           age_1 = ifelse(is.na(age_1) & !is.na(age_4), age_4 - 4, age_1),
           age_2 = age_1,
           age_3 = ifelse(is.na(age_3), age_1 + 2, age_3),
           age_4 = ifelse(is.na(age_4), age_1 + 4, age_4),
           age_5 = age_4,
           age_1 = as.character(age_1),
           age_2 = as.character(age_2),
           age_3 = as.character(age_3),
           age_4 = as.character(age_4),
           age_5 = as.character(age_5)) %>% 
    mutate(md = sprintf("%04d", V720022),
           date_1 = paste("1972", substr(md, 1, 2), substr(md, 3, 4), sep = "-")) %>%
    mutate(md = sprintf("%04d", V720440),
           month = substr(md, 1, 2),
           year = ifelse(month %in% c("01", "02"), "1973", "1972"),
           date_2 = paste(year, month, substr(md, 3, 4), sep = "-"),
           date_2 = ifelse(date_2 == "1972-00-00", NA, date_2)) %>%
    select(-c(md, month, year)) %>%
    mutate(md = sprintf("%04d", V742023),
           month = substr(md, 1, 2),
           year = ifelse(month %in% c("01"), "1975", "1974"),
           date_3 = paste(year, month, substr(md, 3, 4), sep = "-"),
           date_3 = ifelse(date_3 == "1974-99-99", NA, date_3)) %>%
    select(-c(md, month, year)) %>%
    mutate(md = sprintf("%04d", V763023),
           date_4 = paste("1976", substr(md, 1, 2), substr(md, 3, 4), sep = "-"),
           date_4 = ifelse(date_4 == "1976-00-00", NA, date_4)) %>%
    select(-c(md)) %>%
    mutate(md = sprintf("%04d", V763525),
           month = substr(md, 1, 2),
           year = ifelse(month %in% c("01", "02"), "1977", "1976"),
           date_5 = paste(year, month, substr(md, 3, 4), sep = "-"),
           date_5 = ifelse(date_5 == "1976-00-00", NA, date_5)) %>%
    select(-c(md, month, year)) %>%
    mutate(across(c(V720140, V742204, V763174), ~ifelse(.x %in% c(7,8,9), NA, .x)),
           across(c(V720090, V720571, V742230, V763163), ~ifelse(.x == 7, 5, .x)),
           across(c(V720652, V742305, V763286,
                    V720172, V720613, V742265, V763241, V763758,
                    V720208, V763273,
                    V720629, V742296, V763264,
                    V720232, V742302, V763787,
                    V720094, V720575, V742234, V763741, 
                    V720096, V720577, V742236, V763743), 
                  ~ifelse(.x %in% c(0, 8,9), NA, .x)),
           across(c(V720581, V742400, V763745,
                    V720288, V763349, 
                    V720291, V763926,
                    V720582, V742401, V763746,
                    V720583, V742402, V763747,
                    V720271, V720561, V742224, V763817,
                    V720272, V720562, V742225, V763818,
                    V720269,	V720559, V742222, V763815),
                  ~ifelse(.x == 8, 3, ifelse(.x %in% c(0,9), NA, .x))),
           across(c(V720238, V763796,
                    V720093, V720574, V742233, V763166,
                    V720089, V720570, V742229, V763162,
                    V720090, V720571, V742230, V763163,
                    V720091, V720572, V742231, V763164,
                    V720163, V763031), 
                  ~ifelse(.x %in% c(0,7,8,9), NA, .x)),
           across(c(V720719, V742364, V763833,
                    V720721, V742366, V763835,
                    V720720, V742365, V763832,
                    V720718, V763846,
                    V720724, V742369, V763838,
                    V720709, V742358, V763823,
                    V720707, V742356, V763821,
                    V720722, V742367, V763836,
                    V720717, V742362, V763831,
                    V720708, V742357, V763822,
                    V720712, V763826,
                    V720716, V763842,
                    V720714, V742360, V763828,
                    V720725, V742370, V763839),
                  ~ifelse(.x %in% c(98,99), NA, .x))) %>% 
    mutate(partyid_1 = as.character(V720140), partyid_3 = as.character(V742204),
           partyid_4 = as.character(V763174),
           polviews_1 = as.character(V720652), polviews_3 = as.character(V742305),
           polviews_4 = as.character(V763286),
           trust_1 = as.character(V720581), trust_3 = as.character(V742400),
           trust_5 = as.character(V763745),
           helpful_1 = as.character(V720582), helpful_3 = as.character(V742401), 
           helpful_5 = as.character(V763746),
           fair_1 = as.character(V720583), fair_3 = as.character(V742402), 
           fair_5 = as.character(V763747),
           stayhome_1 = as.character(V720291), stayhome_5 = as.character(V763926),
           jobguar_1 = as.character(V720172), jobguar_2 = as.character(V720613), 
           jobguar_3 = as.character(V742265), jobguar_4 = as.character(V763241), 
           jobguar_5 = as.character(V763758),
           govins_1 = as.character(V720208), govins_4 = as.character(V763273),
           helpblk_2 = as.character(V720629), helpblk_3 = as.character(V742296), 
           helpblk_4 = as.character(V763264),
           abortion_1 = as.character(V720238), abortion_5 = as.character(V763796),
           ftdems_1 = as.character(V720719), ftdems_3 = as.character(V742364), 
           ftdems_5 = as.character(V763833),
           ftreps_1 = as.character(V720721), ftreps_3 = as.character(V742366), 
           ftreps_5 = as.character(V763835),
           ftblacks_1 = as.character(V720720), ftblacks_3 = as.character(V742365), 
           ftblacks_5 = as.character(V763832),
           ftwhites_1 = as.character(V720718), ftwhites_5 = as.character(V763846),
           ftcons_1 = as.character(V720724), ftcons_3 = as.character(V742369), 
           ftcons_5 = as.character(V763838),
           ftlibs_1 = as.character(V720709), ftlibs_3 = as.character(V742358), 
           ftlibs_5 = as.character(V763823),
           ftbiz_1 = as.character(V720707), ftbiz_3 = as.character(V742356), 
           ftbiz_5 = as.character(V763821),
           ftlabor_1 = as.character(V720722), ftlabor_3 = as.character(V742367), 
           ftlabor_5 = as.character(V763836),
           ftmil_1 = as.character(V720717), ftmil_3 = as.character(V742362), 
           ftmil_5 = as.character(V763831),
           ftpoor_1 = as.character(V720708), ftpoor_3 = as.character(V742357), 
           ftpoor_5 = as.character(V763822),
           ftcath_1 = as.character(V720712), ftcath_5 = as.character(V763826),
           ftjews_1 = as.character(V720716), ftjews_5 = as.character(V763842),
           ftcops_1 = as.character(V720714), ftcops_3 = as.character(V742360), 
           ftcops_5 = as.character(V763828),
           ftfelib_1 = as.character(V720725), ftfelib_3 = as.character(V742370), 
           ftfelib_5 = as.character(V763839),
           eqrole_1 = as.character(V720232), eqrole_3 = as.character(V742302), 
           eqrole_5 = as.character(V763787),
           crooked_1 = as.character(V720093), 
           crooked_2 = as.character(V720574), 
           crooked_3 = as.character(V742233), 
           crooked_4 = as.character(V763166),
           wastetax_1 = as.character(V720089), 
           wastetax_2 = as.character(V720570), 
           wastetax_3 = as.character(V742229), 
           wastetax_4 = as.character(V763162),
           trustgov_1 = as.character(V720090), 
           trustgov_2 = as.character(V720571), 
           trustgov_3 = as.character(V742230), 
           trustgov_4 = as.character(V763163),
           runfew_1 = as.character(V720091), 
           runfew_2 = as.character(V720572), 
           runfew_3 = as.character(V742231), 
           runfew_4 = as.character(V763164), 
           attentioncpg_1 = as.character(V720163), 
           attentioncpg_2 = as.character(V763031),
           ppllikeme_1 = as.character(V720269),	
           ppllikeme_2 = as.character(V720559), 
           ppllikeme_3 = as.character(V742222), 
           ppllikeme_4 = as.character(V763815),
           manyvote_1 = as.character(V720288),
           manyvote_4 = as.character(V763349), #so many vote (1,4) (1 agree, 5 dis, 8 - DK, 9 - NA)
           complicated_1 = as.character(V720271), 
           complicated_2 = as.character(V720561), 
           complicated_3 = as.character(V742224), 
           complicated_5 = as.character(V763817), #too complicated (1,2,3,5) (1 - agree, 5 - disagree, 8 - DK, 9 - NA, 0 - NA)
           dontcare_1 = as.character(V720272), 
           dontcare_2 = as.character(V720562), 
           dontcare_3 = as.character(V742225), 
           dontcare_5 = as.character(V763818), #pols dont care (1,2,3,5)
           polattn_1 = as.character(V720094), 
           polattn_2 = as.character(V720575), 
           polattn_3 = as.character(V742234), 
           polattn_5 = as.character(V763741), #gov pay attn (1,2,3,5) (1 - good deal, 3 - some, 5 - not much) (0,8,9 - NA)
           elexmatter_1 = as.character(V720096), 
           elexmatter_2 = as.character(V720577), 
           elexmatter_3 = as.character(V742236), 
           elexmatter_5 = as.character(V763743)) %>% 
    select(c(id, weight_2:weight_5, age_1:age_5, date_1:elexmatter_5)) %>% 
    pivot_longer(weight_2:elexmatter_5) %>%
    separate(name, into = c("measure", "wave")) %>%
    spread(measure, value) %>% 
    mutate(date = as.Date(date),
           across(c(weight, abortion:crooked, dontcare:wastetax),
                  ~as.numeric(.x)),
           partyid = (partyid/6)*100,
           across(c(polviews, jobguar, govins, helpblk,
                    eqrole),
                  ~(.x - 1)/6*100),
           across(c(trust, helpful, fair,
                    crooked, wastetax, trustgov, runfew,
                    attentioncpg, ppllikeme, manyvote,
                    complicated, dontcare, polattn, 
                    elexmatter),
                  ~(.x-1)/4*100),
           stayhome = (stayhome - 1)/4*100,
           abortion = (abortion-1)/3*100) %>%
    pivot_longer(c(abortion, attentioncpg:crooked, dontcare:wastetax)) %>%
    filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
    mutate(df = "1972-76 ANES")
  
  return(cleaned)
}
