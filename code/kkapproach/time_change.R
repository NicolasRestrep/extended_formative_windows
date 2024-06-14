
library(haven)
library(tidyverse)
library(broom)
library(broom.mixed)
library(lme4)

anes5 <- read_dta("~/Dropbox/data/anes/anes5660/anes_mergedfile_1956to1960.dta") 
anes7 <- read_dta("~/Dropbox/data/anes/anes7276/anes_mergedfile_1972to1976.dta")
anes8 <- read_dta("~/Dropbox/data/anes/anes1980/anes1980.dta")
anes90 <- read_dta("~/Dropbox/data/anes/anes9092/anes_mergedfile_1990to1992.dta")
anes9 <- read_dta("~/Dropbox/data/anes/anes9297/anes_mergedfile_1992to1997.dta") 
anes0 <- read_dta("~/Dropbox/data/anes/anes0004/anes_mergedfile_2000to2004.dta")

gss6 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta")
gss8 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel08.dta")
gss10 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel10.dta")
gss20 <- read_dta("~/Dropbox/data/gss2020panel/gss2020panel.dta")

anes16 <- read_sav("~/Dropbox/data/anes/anes1620.sav")
anes20 <- read_dta("~/Dropbox/data/anes/anes2022/anes2022")


anes5_long <- anes5 %>%
  mutate(id = 1:nrow(anes5)) %>%
  select(id, V560193,	V580309,	V600578,	V600837,
         V560088, V580360, V600657, V600835,
         V560295, V580472, V600688) %>%
  mutate(across(c(V560088, V580360, V600657, V600835), ~ifelse(.x %in% c(7,8,9), NA, .x)),
         across(c(V560295, V580472, V600688), ~ifelse(.x > 97, NA, .x)),
         age_1 = V560295,
         age_2 = V580472,
         age_3 = V600688,
         age_1 = ifelse(is.na(age_1) & age_2 < 98, age_2-2, age_1), 
         age_1 = ifelse(is.na(age_1) & age_3 < 98, age_3-4, age_1),
         age_2 = ifelse(is.na(age_2), age_1 + 2, age_2),
         age_3 = ifelse(is.na(age_3), age_1 + 4, age_3),
         age_1 = as.character(age_1), age_2 = as.character(age_2),
         age_3 = as.character(age_3),
         age_4 = age_3) %>%
  zap_labels() %>%
  mutate(date_1 = recode(V560193, "10"="1956-11-07", "11"="1956-11-08", "12"="1956-11-09", 
                         "13"="1956-11-10", "14"="1956-11-11", "15"="1956-11-12",
                         "16"="1956-11-13", "20"="1956-11-14", "21"="1956-11-15", 
                         "22"="1956-11-16", "23"="1956-11-17", "24"="1956-11-18",
                         "25"="1956-11-19", "26"="1956-11-20", "30"="1956-11-21", 
                         "31"="1956-11-22", "32"="1956-11-23", "33"="1956-11-24",
                         "34"="1956-11-25", "35"="1956-11-26", "36"="1956-11-27", 
                         "40"="1956-11-28", "41"="1956-11-29", "42"="1956-11-30",
                         "43"="1956-12-01", "44"="1956-12-02", "45"="1956-12-03", 
                         "46"="1956-12-04", "50"="1956-12-05", "51"="1956-12-06",
                         "52"="1956-12-07", "53"="1956-12-08", "54"="1956-12-09", 
                         "55"="1956-12-10", "56"="1956-12-11", "60"="1956-12-12",
                         "61"="1956-12-13", "62"="1956-12-14", "63"="1956-12-15", 
                         "64"="1956-12-16", "65"="1956-12-17", "66"="1956-12-18",
                         "70"="1956-12-19", "71"="1956-12-20", "72"="1956-12-21", 
                         "73"="1956-12-22", "74"="1956-12-23", "75"="1956-12-24",
                         "76"="1956-12-25", "80"="1956-12-26", "81"="1956-12-27", 
                         "82"="1956-12-28", "83"="1956-12-29", "84"="1956-12-30",
                         "85"="1956-12-31", "86"="1957-01-06", "87"="1957-01-07", 
                         "88"="1957-01-08", "89"="1957-01-09", "90"="1957-01-11",
                         "91"="1957-01-12", "92"="1957-01-13", "93"="1957-01-14", 
                         "94"="1957-01-15", "95"="1957-01-16", "96"="1957-01-17",
                         "97"="1957-01-18", "98"="1957-01-19", "99"=NA_character_),
         date_2 = recode(V580309, "1"="1958-11-05", "2"="1958-11-06", "3"="1958-11-07",
                         "4"="1958-11-08", "5"="1958-11-09", "6"="1958-11-10",
                         "7"="1958-11-11", "8"="1958-11-12", "9"="1958-11-13",
                         "10"="1958-11-14", "11"="1958-11-15", "12"="1958-11-16",
                         "13"="1958-11-17", "14"="1958-11-18", "15"="1958-11-19",
                         "16"="1958-11-20", "17"="1958-11-21", "18"="1958-11-22",
                         "19"="1958-11-23", "20"="1958-11-24", "21"="1958-11-25",
                         "22"="1958-11-26", "23"="1958-11-27", "24"="1958-11-28",
                         "25"="1958-11-29", "26"="1958-11-30", "27"="1958-12-01",
                         "28"="1958-12-02", "29"="1958-12-03", "30"="1958-12-04",
                         "31"="1958-12-05", "32"="1958-12-06", "33"="1958-12-07",
                         "34"="1958-12-08", "35"="1958-12-09", "36"="1958-12-10",
                         "37"="1958-12-11", "38"="1958-12-12", "39"="1958-12-13",
                         "40"="1958-12-14", "41"="1958-12-15", "42"="1958-12-16",
                         "43"="1958-12-17", "44"="1958-12-18", "45"="1958-12-19",
                         "46"="1958-12-20", "47"="1958-12-21", "48"="1958-12-22",
                         "49"="1958-12-23", "50"="1958-12-24", "51"="1958-12-25",
                         "99"=NA_character_),
         date_3 = recode(V600578, "1"="1960-09-12", "2"="1960-09-13", "3"="1960-09-14",
                         "4"="1960-09-15", "5"="1960-09-16", "6"="1960-09-17",
                         "7"="1960-09-18", "8"="1960-09-19", "9"="1960-09-20",
                         "10"="1960-09-21", "11"="1960-09-22", "12"="1960-09-23",
                         "13"="1960-09-24", "14"="1960-09-25", "15"="1960-09-26",
                         "16"="1960-09-17", "17"="1960-09-18", "18"="1960-09-19",
                         "19"="1960-09-20", "20"="1960-10-01", "21"="1960-10-02",
                         "22"="1960-10-03", "23"="1960-10-04", "24"="1960-10-05",
                         "25"="1960-10-06", "26"="1960-10-07", "27"="1960-10-08",
                         "28"="1960-10-09", "29"="1960-10-10", "30"="1960-10-11",
                         "31"="1960-10-12", "32"="1960-10-13", "33"="1960-10-14",
                         "34"="1960-10-15", "35"="1960-10-16", "36"="1960-10-17",
                         "37"="1960-10-18", "38"="1960-10-19", "39"="1960-10-20",
                         "40"="1960-10-21", "41"="1960-10-22", "42"="1960-10-23",
                         "43"="1960-10-24", "44"="1960-10-25", "45"="1960-10-26",
                         "46"="1960-10-27", "47"="1960-10-28", "48"="1960-10-29",
                         "49"="1960-10-30", "50"="1960-10-31", "51"="1960-11-01",
                         "52"="1960-11-02", "53"="1960-11-03", "54"="1960-11-04",
                         "55"="1960-11-05", "56"="1960-11-06", "57"="1960-11-07",
                         "99"=NA_character_),
         date_4 = recode(V600837, "9"="1960-11-09", "10"="1960-11-10", "11"="1960-11-11",
                         "12"="1960-11-12", "13"="1960-11-13", "14"="1960-11-14",
                         "15"="1960-11-15", "16"="1960-11-16", "17"="1960-11-17",
                         "18"="1960-11-18", "19"="1960-11-19", "20"="1960-11-20",
                         "21"="1960-11-21", "22"="1960-11-22", "23"="1960-11-23",
                         "24"="1960-11-24", "25"="1960-11-25", "26"="1960-11-26",
                         "27"="1960-11-27", "28"="1960-11-28", "29"="1960-11-29",
                         "30"="1960-11-30", "31"="1960-12-01", "32"="1960-12-02",
                         "33"="1960-12-03", "34"="1960-12-04", "35"="1960-12-05",
                         "36"="1960-12-06", "37"="1960-12-07", "38"="1960-12-08",
                         "39"="1960-12-09", "40"="1960-12-10", "41"="1960-12-11",
                         "42"="1960-12-12", "43"="1960-12-13", "44"="1960-12-14",
                         "45"="1960-12-15", "46"="1960-12-16", "47"="1960-12-17",
                         "48"="1960-12-18", "49"="1960-12-19", "50"="1960-12-20",
                         "51"="1960-12-21", "52"="1960-12-22", "53"="1960-12-23",
                         "54"="1960-12-24", "55"="1960-12-25", "56"="1960-12-26",
                         "0"=NA_character_),
         partyid_1 = as.character(V560088), 
         partyid_2 = as.character(V580360), 
         partyid_3 = as.character(V600657), 
         partyid_4 = as.character(V600835)) %>%
  select(id, age_1:date_4, partyid_1:partyid_4) %>%
  pivot_longer(age_1:partyid_4) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         partyid = as.numeric(partyid),
         partyid = (partyid/6)*100,
         age = as.numeric(age)) %>%
  pivot_longer(partyid) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         d3_value = lead(date,3) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2)),
         a3_value = abs(value - lead(value,3))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value, d3_value, 
             a1_value, a2_value, a3_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit()  %>%
  mutate(d = d/365.25)%>%
  ungroup()

anes7_long <- anes7 %>%
  mutate(id = 1:nrow(anes7)) %>%
  select(id, V720294, V742406, V763369,
         V720022, V720440, V742023, V763023, V763525,
         V720140, V742204, V763174,
         V720652, V742305, V763286,
         V720581, V742400, V763745,
         V720291, V763926,
         V720172, V720613, V742265, V763241, V763758,
         V720208, V763273,
         V720629, V742296, V763264,
         V720238, V763796,
         #group thermometers
         V720719, V742364, V763833, #democrats
         V720721, V742366, V763835, #republicans
         V720720, V742365, V763832, #blacks
         V720718, V763846, #whites
         V720724, V742369, V763838, #conservatives
         V720709, V742358, V763823, #liberals
         V720707, V742356, V763821, #big business
         V720722, V742367, V763836, #unions
         V720717, V742362, V763831, #military
         V720708, V742357, V763822, #poor people
         V720712, V763826, #catholics
         V720716, V763842, #jews
         V720714, V742360, V763828, #police
         V720725, V742370, V763839, #womens movement
         V720582, V742401, V763746,
         V720583, V742402, V763747,
         V720232, V742302, V763787,
         #crooked (1,3,5 - na:0, 8, 9)
         V720093, V720574, V742233, V763166,
         #wastetax (1,3,5 - na: 0,8,9)
         V720089, V720570, V742229, V763162,
         #trustgov (1,3,5,7 - convert 7 to 5 - na: 0, 8, 9)
         V720090, V720571, V742230, V763163,
         #big interest (1,5 - na: 0, 7, 8, 9)
         V720091, V720572, V742231, V763164
         ) %>%
  mutate(age_1 = V720294, 
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
                  V720581, V742400, V763745,
                  V720291, V763926,
                  V720172, V720613, V742265, V763241, V763758,
                  V720208, V763273,
                  V720629, V742296, V763264,
                  V720582, V742401, V763746,
                  V720583, V742402, V763747,
                  V720232, V742302, V763787), 
                ~ifelse(.x %in% c(0, 8,9), NA, .x)),
         across(c(V720238, V763796,
                  V720093, V720574, V742233, V763166,
                  V720089, V720570, V742229, V763162,
                  V720090, V720571, V742230, V763163,
                  V720091, V720572, V742231, V763164), 
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
         runfew_4 = as.character(V763164)) %>%
  select(c(id, age_1:age_5, date_1:runfew_4)) %>%
  pivot_longer(age_1:runfew_4) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(abortion:crooked, eqrole:wastetax),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, jobguar, govins, helpblk,
                  eqrole),
                ~(.x - 1)/6*100),
         across(c(trust, helpful, fair,
                  crooked, wastetax, trustgov, runfew),
                ~(.x-1)/4*100),
         stayhome = (stayhome - 1)/4*100,
         abortion = (abortion-1)/3*100) %>%
  pivot_longer(c(abortion, crooked, eqrole:wastetax)) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value, 
             a1_value, a2_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25)%>%
  ungroup()

anes8_long <- anes8 %>%
  mutate(id = 1:nrow(anes8)) %>%
  select(id, 
         VMP0325, VMP2404, VMP3386,
         VMP0578, VMP2425, VMP3571, VMP4105,
         VMP0191, VMP2212, VMP3212, VMP4022,
         VMP0121, VMP2125, VMP3213,
         VMP2192, VMP3284,
         VMP0144, VMP2159, VMP3254,
         VMP0133, VMP2137, VMP3234,
         VMP0052, VMP2052, VMP3109,
         VMP0053, VMP2053, VMP3110) %>% 
  mutate(across(c(VMP0325, VMP2404, VMP3386),
                ~ifelse(.x == 0, NA, .x)),
         age_1 = VMP0325,
         age_2 = VMP2404, 
         age_3 = VMP3386,
         age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2, age_1),
         age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3, age_1),
         age_2 = ifelse(is.na(age_2), age_1, age_2),
         age_3 = ifelse(is.na(age_3), age_1, age_3),
         age_4 = age_3,
         across(c(age_1:age_4), ~as.character(.x))) %>% 
  mutate(date_str = sprintf("%06d", as.integer(VMP0578)),
         date_1 = paste(paste0("19", substr(date_str, 5, 6)), substr(date_str, 1, 2), 
                        substr(date_str, 3, 4), sep = "-")) %>%
  select(-c(date_str)) %>%
  mutate(date_str = sprintf("%06d", as.integer(VMP2425)),
         date_2 = paste(paste0("19", substr(date_str, 5, 6)), substr(date_str, 1, 2), 
                        substr(date_str, 3, 4), sep = "-"),
         date_2 = ifelse(date_2 == "1999-99-99", NA, date_2)) %>%
  select(-c(date_str)) %>%
  mutate(date_str = sprintf("%06d", as.integer(VMP3571)),
         date_3 = paste(paste0("19", substr(date_str, 5, 6)), substr(date_str, 1, 2), 
                        substr(date_str, 3, 4), sep = "-"),
         date_3 = ifelse(date_3 %in% c("1900-00-00", "1999-99-99"), NA, date_3)) %>%
  select(-c(date_str)) %>%
  mutate(date_str = sprintf("%06d", as.integer(VMP4105)),
         date_4 = paste(paste0("19", substr(date_str, 5, 6)), substr(date_str, 1, 2), 
                        substr(date_str, 3, 4), sep = "-"),
         date_4 = ifelse(date_4 %in% c("1900-00-00", "1999-99-99"), NA, date_4)) %>%
  select(-c(date_str)) %>%
  mutate(across(c(VMP0191, VMP2212, VMP3212, VMP4022),
                ~ifelse(.x %in% c(7,8,9), NA, .x)),
         across(c(VMP0121, VMP2125, VMP3213,
                  VMP0144, VMP2159, VMP3254,
                  VMP0133, VMP2137, VMP3234), 
                ~ifelse(.x %in% c(0,8,9), NA, .x)),
         across(c(VMP2192, VMP3284),
                ~ifelse(.x %in% c(7,8,9), NA, .x)),
         across(c(VMP0052, VMP2052, VMP3109,
                  #republicans
                  VMP0053, VMP2053, VMP3110),
                ~ifelse(.x %in% c(998, 999), NA, .x))) %>%
  mutate(partyid_1 = as.character(VMP0191),
         partyid_2 = as.character(VMP2212),
         partyid_3 = as.character(VMP3212),
         partyid_4 = as.character(VMP4022),
         polviews_1 = as.character(VMP0121), 
         polviews_2 = as.character(VMP2125),
         polviews_3 = as.character(VMP3213),
         abortion_2 = as.character(VMP2192), 
         abortion_3 = as.character(VMP3284),
         spendserv_1 = as.character(VMP0144), 
         spendserv_2 = as.character(VMP2159), 
         spendserv_3 = as.character(VMP3254),
         defscale_1 = as.character(VMP0133), 
         defscale_2 = as.character(VMP2137), 
         defscale_3 = as.character(VMP3234),
         ftdems_1 = as.character(VMP0052), 
         ftdems_2 = as.character(VMP2052), 
         ftdems_3 = as.character(VMP3109),
         ftreps_1 = as.character(VMP0053), 
         ftreps_2 = as.character(VMP2053), 
         ftreps_3 = as.character(VMP3110)) %>%
  select(c(id, age_1:age_4, date_1:ftreps_3)) %>%
  pivot_longer(age_1:ftreps_3) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(age, abortion, defscale:spendserv),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, spendserv,
                  defscale),
                ~(.x-1)/6*100),
         abortion = (abortion-1)/3*100) %>%
  pivot_longer(c(abortion,defscale:spendserv)) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         d3_value = lead(date,3) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2)),
         a3_value = abs(value - lead(value,3))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value, d3_value,
             a1_value, a2_value, a3_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25)%>%
  ungroup()

anes90_long <- anes90 %>%
  mutate(id = 1:nrow(anes90)) %>%
  select(id, V900552, V923903,
         V900052, V900053, V912032, V923026, V923027, V925005, V925006,
         V900320, V912333, V923634,
         V900406, V912450, V923509,
         V912831, V926139,
         V900498, V912485, V923604,
         V900446, V923718,
         V900447, V923724,
         V900479, V923732,
         V900452, V912600, V923701,
         V900377, V923815,
         V900379, V923727,
         V900380, V923811,
         V900382, V923725,
         V900383, V923818,
         V900384, V923730,
         V900385, V923813,
         V900439, V912475, V923707,
         V900151, V912222, V923317,
         V900152, V912228, V923318,
         V900155, V912232, V925323,
         V912231, V925333,
         V900156, V912220, V925319,
         V900161, V912226, V925326,
         V900157, V925316,
         V912233, V925327,
         V900158, V912239, V925324,
         V900438, V923801,
         V900507, V926123,
         V900505, V926121,
         V900504, V912487, V926120,
         V900506, V912488, V926122) %>% 
  mutate(across(c(V900552, V923903), 
                ~ifelse(.x == 0, NA, .x)),
         age_1 = V900552,
         age_2 = age_1 + 1,
         age_3 = ifelse(is.na(V923903), age_1 + 2, V923903),
         age_4 = age_3,
         across(c(age_1:age_4), ~as.character(.x))) %>% 
  mutate(year = ifelse(V900052 %in% c(1,2), "1991", "1990"),
         date_1 = paste(year, sprintf("%02d", V900052), sprintf("%02d", V900053), sep = "-")) %>% 
  mutate(md = sprintf("%04d", V912032),
         date_2 = paste("1991", substr(md, 1, 2), substr(md, 3, 4), sep = "-"),
         date_2 = ifelse(date_2 == "1991-00-00", NA, date_2)) %>% 
  select(-c(md)) %>%
  mutate(date_3 = paste("1992", sprintf("%02d", V923026), sprintf("%02d", V923027),
                        sep = "-"),
         date_3 = ifelse(date_3 == "1992-00-00", NA, date_3)) %>% 
  mutate(year = ifelse(V925005 == 1, "1993", "1992"),
         date_4 = paste(year, sprintf("%02d", V925005), sprintf("%02d", V925006),
                        sep = "-"),
         date_4 = ifelse(date_4 == "1992-00-00", NA, date_4)) %>% 
  select(-year) %>% 
  mutate(across(c(V900504, V912487, V926120),
                ~ifelse(.x == 7, 5, .x)),
         across(c(V900320, V912333, V923634), ~ifelse(.x %in% c(7,8,9), NA, .x)),
         across(c(V900406, V912450, V923509,
                  V912831, V926139,
                  V900498, V912485, V923604,
                  V900446, V923718,
                  V900447, V923724,
                  V900452, V912600, V923701,
                  V900439, V912475, V923707,
                  V900438, V923801,
                  V900507, V926123,
                  V900505, V926121,
                  V900504, V912487, V926120,
                  V900506, V912488, V926122), 
                ~ifelse(.x %in% c(0,8, 9), NA, .x)),
         across(c(V900479, V923732,
                  V900377, V923815,
                  V900379, V923727,
                  V900380, V923811,
                  V900382, V923725,
                  V900383, V923818,
                  V900384, V923730,
                  V900385, V923813), ~ifelse(.x %in% c(0,6,7,8,9), NA, .x)),
         across(c(V900151, V912222, V923317,
           V900152, V912228, V923318,
           V900155, V912232, V925323,
           V912231, V925333,
           V900156, V912220, V925319,
           V900161, V912226, V925326,
           V900157, V925316,
           V912233, V925327,
           V900158, V912239, V925324),
           ~ifelse(.x %in% c(888,997,998,999), NA, .x))) %>%
  mutate(partyid_1 = as.character(V900320),
         partyid_2 = as.character(V912333), 
         partyid_3 = as.character(V923634),
         polviews_1 = as.character(V900406),
         polviews_2 = as.character(V912450),
         polviews_3 = as.character(V923509),
         trust_2 =as.character(V912831),
         trust_4 = as.character(V926139),
         stayhome_1 = as.character(V900498),
         stayhome_2 = as.character(V912485),
         stayhome_3 = as.character(V923604),
         jobguar_1 = as.character(V900446),
         jobguar_3 = as.character(V923718),
         helpblk_1 = as.character(V900447), 
         helpblk_3 = as.character(V923724),
         abortion_1 = as.character(V900479), 
         abortion_3 = as.character(V923732),
         spendserv_1 = as.character(V900452), 
         spendserv_2 = as.character(V912600), 
         spendserv_3 = as.character(V923701),
         natenvir_1 = as.character(V900377), 
         natenvir_3 = as.character(V923815),
         nataids_1 = as.character(V900379), 
         nataids_3 = as.character(V923727),
         natsoc_1 = as.character(V900380),
         natsoc_3 = as.character(V923811),
         natfood_1 = as.character(V900382), 
         natfood_3 = as.character(V923725),
         natschools_1 = as.character(V900383),
         natschools_3 = as.character(V923818),
         nathome_1 = as.character(V900384),
         nathome_3 = as.character(V923730),
         natchld_1 = as.character(V900385),
         natchld_3 = as.character(V923813),
         defscale_1 = as.character(V900439), 
         defscale_2 = as.character(V912475), 
         defscale_3 = as.character(V923707),
         ftdems_1 = as.character(V900151), 
         ftdems_2 = as.character(V912222), 
         ftdems_3 = as.character(V923317),
         ftreps_1 = as.character(V900152), 
         ftreps_2 = as.character(V912228), 
         ftreps_3 = as.character(V923318),
         ftblacks_1 = as.character(V900155), 
         ftblacks_2 = as.character(V912232), 
         ftblacks_4 = as.character(V925323),
         ftwhites_2 = as.character(V912231), 
         ftwhites_4 = as.character(V925333),
         ftcons_1 = as.character(V900156), 
         ftcons_2 = as.character(V912220), 
         ftcons_4 = as.character(V925319),
         ftlibs_1 = as.character(V900161), 
         ftlibs_2 = as.character(V912226), 
         ftlibs_4 = as.character(V925326),
         ftlabor_1 = as.character(V900157), 
         ftlabor_4 = as.character(V925316),
         fthisp_2 = as.character(V912233), 
         fthisp_4 = as.character(V925327),
         ftfelib_1 = as.character(V900158), 
         ftfelib_2 = as.character(V912239), 
         ftfelib_4 = as.character(V925324),
         eqrole_1 = as.character(V900438),
         eqrole_3 = as.character(V923801),
         crooked_1 = as.character(V900507), 
         crooked_4 = as.character(V926123),
         wastetax_1 = as.character(V900505), 
         wastetax_4 = as.character(V926121),
         trustgov_1 = as.character(V900504), 
         trustgov_2 = as.character(V912487), 
         trustgov_4 = as.character(V926120),
         runfew_1 = as.character(V900506), 
         runfew_2 = as.character(V912488), 
         runfew_4 = as.character(V926122)) %>%
  select(c(id, age_1:age_4, date_1:runfew_4)) %>%
  pivot_longer(age_1:runfew_4) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(age, abortion, crooked, defscale:wastetax),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, jobguar, helpblk,
                  spendserv, defscale, eqrole),
                ~(.x-1)/6*100),
         trust = (trust-1)*100,
         across(c(stayhome, crooked, wastetax, trustgov, runfew),
                ~(.x-1)/4*100),
         abortion = (abortion - 1)/3*100,
         across(c(natenvir, nataids, natsoc, natfood, natschools,
                  nathome, natchld),
                ~(.x -1)/2*100)) %>% 
  pivot_longer(c(abortion, crooked, defscale:wastetax)) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value,
             a1_value, a2_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25)%>%
  ungroup()

#8 waves of observation
anes9_long <- anes9 %>%
  mutate(id = 1:nrow(anes9)) %>%
  select(id, V923903, V941203, V960605, 
         V923026, V923027, V925005, V925006, V937014, V937015, 
         V940033, V940034, V952360, V960012, V960011, V960903, V960902,
         V970367,
         V923634, V937370, V940655, V952263a, V960420, V970106, #partyid
         V923509, V937204, V940839, V952253, V960365, V961269, V970302, #polviews
         V926139, V960567, V961258, V970011, #trust
         V923604, V941019, V960410, #stayhome
         V923718, V940930, V960483, #jobguar
         V923716, V940950, V960479, #govins
         V923724, V940936, V960487, V961210, V970193, #helpblk
         V923732, V941014, V960503, #abortion
         V923701, V940940, V960450, V970096, #spendserv
         V923815, V940817, V960561, #environment
         V923727, V940821, V960498, #aids
         V923811, V940819, V960560, #socsec
         V923725, V940822, V960496, #schools
         V923818, V940823, V960562, #food stamps
         V923730, V960501, #homeless
         V923813, V940824, V960564, #child care
         V923726, V940820, V960497, #welfare
         V923814, V940825, V960563,
         V923707, V940929, V960463,
         V923317, V937141, V940301, V960292,
         V923318, V937142, V940302, V960293,
         V925323, V940305, V961029, V970040,
         V925333, V940313, V961030, V970041, 
         V925319, V940306, V961031,
         V925326, V940311, V961032,
         V925322, V940314, V952115, V961034, V970056,
         V925316, V940307, V961033, V970055,
         V925328, V937146, V961027,
         V925327, V940304, V961037, V970042,
         V925335, V937145, V940318, V961042, V970054,
         V940312, V961035, 
         V925324, V940308, V961039,
         V960569, V961259, V970012,
         V923801, V940928, V960543, V970230,
         V926123, V941036, V961254,
         V926121, V941034, V961252,	V970321,
         V926120, V941033, V960566, V961251, V970013,
         V926122, V941035, V961253) %>%
  zap_labels() %>%
  mutate(across(c(V923903, V941203, V960605),
                ~ifelse(.x < 0 | .x > 95, NA, .x)),
         age_1 = ifelse(is.na(V923903) & !is.na(V941203), V941203 - 2, V923903),
         age_1 = ifelse(is.na(V923903) & !is.na(V960605), V960605 - 4, V923903),
         age_2 = age_1,
         age_3 = age_1 + 1,
         age_4 = ifelse(is.na(V941203), age_1 + 2, V941203),
         age_5 = age_4 + 1,
         age_6 = ifelse(is.na(V960605), age_1 + 4, V960605),
         age_7 = age_6,
         age_8 = age_7 + 1,
         across(age_1:age_8, ~as.character(.x))) %>%
  mutate(date_1 = paste(1992, sprintf("%02d", V923026), sprintf("%02d", V923027), sep = "-"),
         date_1 = ifelse(date_1 == "1992-NA-NA", NA, date_1),
         year = ifelse(V925005 %in% 1, 1993, 1992),
         date_2 = paste(year, sprintf("%02d", V925005), sprintf("%02d", V925006), sep = "-"),
         date_2 = ifelse(date_2 %in% c("1992-NA-NA"), NA, date_2),
         date_3 = paste(1993, sprintf("%02d", V937014), sprintf("%02d", V937015), sep = "-"),
         date_3 = ifelse(date_3 %in% c("1992-00-00", "1993-NA-NA"), NA, date_3),
         year = ifelse(V940033 == 1, 1995, 1994),
         date_4 = paste(year, sprintf("%02d", V940033), sprintf("%02d", V940034), sep = "-"),
         date_4 = ifelse(date_4 == "NA-NA-NA", NA, date_4),
         date_5 = paste(substr(V952360, 1, 4),substr(V952360, 5, 6),substr(V952360, 7, 8),
                        sep = "-"),
         date_5 = ifelse(date_5 %in% c("NA-NA-NA", "1e+0-8-"), NA, date_5),
         date_6 = paste(1996, sprintf("%02d", V960012), sprintf("%02d", V960011), sep = "-"),
         date_6 = ifelse(date_6 %in% c("1996-NA-NA"), NA, date_6),
         year = ifelse(V960903 == 1, 1997, 1996),
         date_7 = paste(year, sprintf("%02d", V960903), sprintf("%02d", V960902), sep = "-"),
         date_7 = ifelse(date_7 %in% c("NA-NA-NA", "1996-00-00"), NA, date_7),
         dm = sprintf("%04d", V970367),
         date_8 = paste(1997 ,substr(dm, 1, 2),substr(dm, 3, 4),
                        sep = "-"),
         date_8 = ifelse(date_8 %in% c("1997-00-NA"), NA, date_8),
         #Nat____ questions coded differently in different wave
         across(c(V960561, V960498, V960560, V960496,
                  V960562, V960501, V960564, V960497,
                  V960563), 
                ~recode(.x, "1"=1, "2"=3, "3"=2, "7"=NA_real_,
                        "8"=NA_real_, "9"=NA_real_)),
         across(c(V961254, V961252,	V970321),
                ~recode(.x, "1"=5, "5"=1, "5"=3,
                        "8"=NA_real_, "9"=NA_real_, "0"=NA_real_)),
         V960566 = recode(V960566, "1"=1, "2"=3, "3"=5, "7"=5,
                          "8"=NA_real_, "9"=NA_real_),
         V961251 = recode(V961251, "1"=1, "2"=3, "3"=5, 
                          "4"=7, "8"=NA_real_, "9"=NA_real_,
                          "0"=NA_real_),
         across(c(V926120, V941033, V960566, V961251, V970013),
                ~ifelse(.x == 7, 5, .x)),
         V961253 = recode(V961253, "1"=5, "5"=1, "0"=0, "8"=8, "9"=9),
         across(c(V923634, V937370, V940655, V952263a, V960420, V970106),
                ~ifelse(.x %in% c(7,8,9), NA, .x)),
         across(c(V923509, V937204, V940839, V952253, V960365, V961269, V970302,
                  V926139, V960567, V961258, V970011,
                  V923604, V941019, V960410,
                  V923718, V940930, V960483,
                  V923716, V940950, V960479,
                  V923724, V940936, V960487, V961210, V970193,
                  V923701, V940940, V960450, V970096,
                  V923707, V940929, V960463,
                  V960569, V961259, V970012,
                  V923801, V940928, V960543, V970230,
                  V926123, V941036, V961254,
                  V926121, V941034, V961252,	V970321,
                  V926120, V941033, V960566, V961251, V970013,
                  V926122, V941035, V961253), 
                       ~ifelse(.x %in% c(0,8,9, 96), NA, .x)),
         across(c(V923732, V941014, V960503,
                  V923815, V940817, V960561,
                  V923727, V940821, V960498,
                  V923811, V940819, V960560,
                  V923725, V940822, V960496,
                  V923818, V940823, V960562,
                  V923730, V960501,
                  V923813, V940824, V960564, 
                  V923726, V940820, V960497, #welfare
                  V923814, V940825, V960563),
                ~ifelse(.x %in% c(6,7,8,9), NA, .x)),
         across(c(V961258, V970011,
                  V961259, V970012), ~ifelse(.x == 5, 2, .x)),
         across(c(#democrats 997/998/999/888/996
           V923317, V937141, V940301, V960292,
           V923318, V937142, V940302, V960293,
           V925323, V940305, V961029, V970040,
           V925333, V940313, V961030, V970041, 
           V925319, V940306, V961031,
           V925326, V940311, V961032,
           V925322, V940314, V952115, V961034, V970056,
           V925316, V940307, V961033, V970055,
           V925328, V937146, V961027,
           V925327, V940304, V961037, V970042,
           V925335, V937145, V940318, V961042, V970054,
           V940312, V961035, 
           V925324, V940308, V961039),
           ~ifelse(.x %in% c(997,998,999,888,996), NA, .x)),
         partyid_1 = as.character(V923634),
         partyid_3 = as.character(V937370),
         partyid_4 = as.character(V940655),
         partyid_5 = as.character(V952263a),
         partyid_6 = as.character(V960420),
         partyid_8 = as.character(V970106),
         polviews_1 = as.character(V923509),
         polviews_3 = as.character(V937204),
         polviews_4 = as.character(V940839),
         polviews_5 = as.character(V952253),
         polviews_6 = as.character(V960365),
         polviews_7 = as.character(V961269),
         polviews_8 = as.character(V970302),
         trust_2 = as.character(V926139),
         trust_6 = as.character(V960567),
         trust_7 = as.character(V961258),
         trust_8 = as.character(V970011),
         stayhome_1 = as.character(V923604), 
         stayhome_4 = as.character(V941019), 
         stayhome_6 = as.character(V960410),
         jobguar_1 = as.character(V923718), 
         jobguar_4 = as.character(V940930), 
         jobguar_6 = as.character(V960483),
         govins_1 = as.character(V923716), 
         govins_4 = as.character(V940950), 
         govins_6 = as.character(V960479),
         helpblk_1 = as.character(V923724),
         helpblk_4 = as.character(V940936), 
         helpblk_6 = as.character(V960487), 
         helpblk_7 = as.character(V961210), 
         helpblk_8 = as.character(V970193),
         abortion_1 = as.character(V923732), 
         abortion_4 = as.character(V941014), 
         abortion_6 = as.character(V960503),
         spendserv_1 = as.character(V923701), 
         spendserv_4 = as.character(V940940), 
         spendserv_6 = as.character(V960450), 
         spendserv_8 = as.character(V970096),
         natenvir_1 = as.character(V923815), 
         natenvir_4 = as.character(V940817), 
         natenvir_6 = as.character(V960561),
         nataids_1 = as.character(V923727), 
         nataids_4 = as.character(V940821), 
         nataids_6 = as.character(V960498),
         natsoc_1 = as.character(V923811), 
         natsoc_4 = as.character(V940819), 
         natsoc_6 = as.character(V960560),
         natfood_1 = as.character(V923725), 
         natfood_4 = as.character(V940822), 
         natfood_6 = as.character(V960496),
         natschools_1 = as.character(V923818), 
         natschools_4 = as.character(V940823), 
         natschools_6 = as.character(V960562),
         nathome_1 = as.character(V923730), 
         nathome_6 = as.character(V960501),
         natchld_1 = as.character(V923813), 
         natchld_4 = as.character(V940824), 
         natchld_6 = as.character(V960564),
         natfare_1 = as.character(V923726), 
         natfare_4 = as.character(V940820), 
         natfare_6 = as.character(V960497),
         natcrime_1 = as.character(V923814), 
         natcrime_4 = as.character(V940825), 
         natcrime_6 = as.character(V960563),
         defscale_1 = as.character(V923707), 
         defscale_4 = as.character(V940929), 
         defscale_6 = as.character(V960463),
         ftdems_1 = as.character(V923317), 
         ftdems_3 = as.character(V937141), 
         ftdems_4 = as.character(V940301), 
         ftdems_6 = as.character(V960292),
         ftreps_1 = as.character(V923318), 
         ftreps_3 = as.character(V937142), 
         ftreps_4 = as.character(V940302), 
         ftreps_6 = as.character(V960293),
         ftblacks_2 = as.character(V925323), 
         ftblacks_4 = as.character(V940305), 
         ftblacks_7 = as.character(V961029), 
         ftblacks_8 = as.character(V970040),
         ftwhites_2 = as.character(V925333), 
         ftwhites_4 = as.character(V940313), 
         ftwhites_7 = as.character(V961030), 
         ftwhites_8 = as.character(V970041), 
         ftcons_2 = as.character(V925319), 
         ftcons_4 = as.character(V940306), 
         ftcons_7 = as.character(V961031),
         ftlibs_2 = as.character(V925326), 
         ftlibs_4 = as.character(V940311), 
         ftlibs_7 = as.character(V961032),
         ftbiz_2 = as.character(V925322), 
         ftbiz_4 = as.character(V940314), 
         ftbiz_5 = as.character(V952115), 
         ftbiz_7 = as.character(V961034), 
         ftbiz_8 = as.character(V970056),
         ftlabor_2 = as.character(V925316), 
         ftlabor_4 = as.character(V940307), 
         ftlabor_7 = as.character(V961033), 
         ftlabor_8 = as.character(V970055),
         ftmil_2 = as.character(V925328), 
         ftmil_3 = as.character(V937146), 
         ftmil_7 = as.character(V961027),
         fthisp_2 = as.character(V925327), 
         fthisp_4 = as.character(V940304), 
         fthisp_7 = as.character(V961037), 
         fthisp_8 = as.character(V970042),
         fthomo_2 = as.character(V925335), 
         fthomo_3 = as.character(V937145), 
         fthomo_4 = as.character(V940318), 
         fthomo_7 = as.character(V961042), 
         fthomo_8 = as.character(V970054),
         ftpoor_4 = as.character(V940312), 
         ftpoor_7 = as.character(V961035), 
         ftfelib_2 = as.character(V925324), 
         ftfelib_4 = as.character(V940308), 
         ftfelib_7 = as.character(V961039),
         fair_6 = as.character(V960569), 
         fair_7 = as.character(V961259), 
         fair_8 = as.character(V970012),
         eqrole_1 = as.character(V923801), 
         eqrole_4 = as.character(V940928), 
         eqrole_6 = as.character(V960543), 
         eqrole_8 = as.character(V970230),
         crooked_2 = as.character(V926123), 
         crooked_4 = as.character(V941036), 
         crooked_7 = as.character(V961254),
         wastetax_2 = as.character(V926121), 
         wastetax_4 = as.character(V941034), 
         wastetax_7 = as.character(V961252),
         wastetax_8 = as.character(V970321),
         trustgov_2 = as.character(V926120), 
         trustgov_4 = as.character(V941033), 
         trustgov_6 = as.character(V960566), 
         trustgov_7 = as.character(V961251), 
         trustgov_8 = as.character(V970013),
         #runfew (1 - all; 5 = big, NA: 0,8,9);
         runfew_2 = as.character(V926122), 
         runfew_4 = as.character(V941035), 
         runfew_7 = as.character(V961253)
         ) %>%
  select(c(id, age_1:age_8, date_1, date_2:date_7, date_8:runfew_7)) %>%
  pivot_longer(age_1:runfew_7) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(abortion:crooked, defscale:wastetax),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, jobguar, govins, helpblk,
                  spendserv, defscale, eqrole),
                ~(.x - 1)/6*100),
         across(c(trust, fair),
                ~(.x-1)*100),
         across(c(stayhome, crooked, wastetax, trustgov, runfew),
                ~(.x - 1)/4*100),
         abortion = (abortion-1)/3*100,
         across(c(natenvir, nataids, natsoc, natfood, natschools,
                  nathome, natchld, natfare, natcrime),
                ~(.x - 1)/2*100)) %>%
  pivot_longer(c(abortion, crooked, defscale:wastetax)) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         d3_value = lead(date,3) - date,
         d4_value = lead(date,4) - date,
         d5_value = lead(date,5) - date,
         d6_value = lead(date,6) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2)),
         a3_value = abs(value - lead(value,3)),
         a4_value = abs(value - lead(value,4)),
         a5_value = abs(value - lead(value,5)),
         a6_value = abs(value - lead(value,6))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value, d3_value, d4_value, d5_value, d6_value,
             a1_value, a2_value, a3_value, a4_value, a5_value, a6_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25) %>%
  ungroup()


anes0_long <- anes0 %>%
  mutate(id = 1:nrow(anes0)) %>%
  select(id, M000908,M023126X,M045193,
         M000008, M000130, M022012C, M024012C, M044013b,
         M000523, M023038X, M045058x,
         M000446, M001368, M023022,
         M001475, M025101, M045158,
         M001477, M025103, M045160,
         M001476, M025102, M045159, 
         M000513a, M023033, M045143,
         M000694, M001403, M045110,
         M000545, M001385, M045127,
         M000682, M023052, M025113, M045068,
         M000678, M023054, M025116, M045076,
         M000677, M023045, M025106, M045069, #aids
         M000681, M023055, M025117, M045077, #socsec
         M000683, M023047A, M025108A, M045071a, #schools
         M000685, M023049, M025109, M045073,
         M000676, M023046, M025107, M045070,
         M000684, M023048, M025109, M045072,
         M001308, M025055, M045023,
         M001309, M025056, M045024,
         M001310, M025057, M045025,
         M001311, M025058, M045026,
         M001313, M025060, M045028,
         M001312, M025059, M045027,
         M001306, M025053, M045021,
         M001316, M025063, M045031,
         M001321, M025067, M045035,
         M001314, M025061, M045029,
         M001323, M025068, M045036,
         M001324, M025069, M045037,
         #crooked (1quite, 35 -NA: 0,8,9)
         M001537, M025177, M045152,
         #wastetax (1lot, 35 - NA:0,8,9)
         M001535, M025175, M045150,
         #trustgov (123, recode 4, 0,8,9)
         M001534, M025174, M045149,
         #runfew (1few; 5all - NA: 0,8,9)
         M001536, M025176, M045151
         ) %>%
  zap_labels() %>%
  mutate(age_1 = ifelse(is.na(M000908) & !is.na(M023126X), M023126X - 2, M000908),
         age_1 = ifelse(is.na(M000908) & !is.na(M045193), M045193 - 4, M000908),
         age_2 = age_1,
         age_3 = ifelse(is.na(M023126X), age_1 + 2, M023126X),
         age_4 = age_3,
         age_5 = ifelse(is.na(M045193), age_1 + 4, M045193),
         across(age_1:age_5, ~as.character(.x))) %>%
  mutate(date_1 = paste(2000,substr(M000008, 1, 2),substr(M000008, 3, 4),
                        sep = "-"),
         date_2 = paste(2000,substr(M000130, 1, 2),substr(M000130, 3, 4),
                        sep = "-"),
         date_2 = ifelse(date_2 %in% c("2000-00-00"), NA, date_2),
         date_3 = paste(2002,substr(M022012C, 1, 2),substr(M022012C, 3, 4),
                        sep = "-"),
         date_3 = ifelse(date_3 == "2002--", NA, date_3),
         date_4 = paste(2002,substr(M024012C, 1, 2),substr(M024012C, 3, 4),
                        sep = "-"),
         date_4 = ifelse(date_4 == "2002--", NA, date_4),
         date_5 = paste(2004,substr(M044013b, 1, 2),substr(M044013b, 3, 4),
                        sep = "-"),
         date_5 = ifelse(date_5 == "2004--", NA, date_5),
         across(c(M001534, M025174, M045149),
                ~ifelse(.x == 4, 3, .x)),
         across(c(M000682, M000678, M000677, M000681, 
                  M000683, M000685, M000676, M000684), 
                ~recode(.x, "1"=1, "5"=2, "3"=3, "7"=NA_real_)),
         across(c(M023052, M025113, M045068,
                  M023054, M025116, M045076,
                  M023045, M025106, M045069,
                  M023055, M025117, M045077,
                  M023047A, M025108A, M045071a,
                  M023049, M025109, M045073, 
                  M023046, M025107, M045070, 
                  M023048, M025109, M045072),
                ~recode(.x, "1"=1, "2"=3, "3"=2)),
         across(c(M000523, M023038X, M045058x), 
                ~ifelse(.x %in% c(7,8,9), NA, .x)),
         across(c(M000446, M001368, M023022), 
                ~ifelse(.x %in% c(0,8,9,90), NA, .x)),
         across(c(M001475, M025101, M045158,
                  M000513a, M023033, M045143,
                  M000545, M001385, M045127,
                  M001477, M025103, M045160,
                  M001476, M025102, M045159,
                  M001537, M025177, M045152,
                  M001535, M025175, M045150,
                  M001534, M025174, M045149,
                  M001536, M025176, M045151), 
                ~ifelse(.x %in% c(0,8,9), NA, .x)),
         across(c(M000694, M001403, M045110),
                ~ifelse(.x %in% c(0,7,8,9), NA, .x)),
         across(c(
           M001308, M025055, M045023,
           M001309, M025056, M045024,
           M001310, M025057, M045025,
           M001311, M025058, M045026,
           M001313, M025060, M045028,
           M001312, M025059, M045027,
           M001306, M025053, M045021,
           M001316, M025063, M045031,
           M001321, M025067, M045035,
           M001314, M025061, M045029,
           M001323, M025068, M045036,
           M001324, M025069, M045037),
           ~ifelse(.x > 100, NA, .x)),
         partyid_1 = as.character(M000523),
         partyid_3 = as.character(M023038X),
         partyid_5 = as.character(M045058x),
         polviews_1 = as.character(M000446),
         polviews_2 = as.character(M001368),
         polviews_3 = as.character(M023022),
         trust_2 = as.character(M001475),
         trust_4 = as.character(M025101),
         trust_5 = as.character(M045158),
         helpful_2 = as.character(M001477), 
         helpful_4 = as.character(M025103), 
         helpful_5 = as.character(M045160),
         fair_2 = as.character(M001476), 
         fair_4 = as.character(M025102), 
         fair_5 = as.character(M045159),
         stayhome_1 = as.character(M000513a), 
         stayhome_3 = as.character(M023033), 
         stayhome_5 = as.character(M045143),
         abortion_1 = as.character(M000694), 
         abortion_2 = as.character(M001403), 
         abortion_5 = as.character(M045110),
         spendserv_1 = as.character(M000545), 
         spendserv_2 = as.character(M001385), 
         spendserv_5 = as.character(M045127),
         natenvir_1 = as.character(M000682), 
         natenvir_3 = as.character(M023052), 
         natenvir_4 = as.character(M025113), 
         natenvir_5 = as.character(M045068),
         nataid_1 = as.character(M000678), 
         nataid_3 = as.character(M023054), 
         nataid_4 = as.character(M025116), 
         nataid_5 = as.character(M045076),
         nataids_1 = as.character(M000677), 
         nataids_3 = as.character(M023045), 
         nataids_4 = as.character(M025106), 
         nataids_5 = as.character(M045069),
         natsoc_1 = as.character(M000681), 
         natsoc_3 = as.character(M023055), 
         natsoc_4 = as.character(M025117), 
         natsoc_5 = as.character(M045077),
         natschools_1 = as.character(M000683), 
         natschools_3 = as.character(M023047A), 
         natschools_4 = as.character(M025108A), 
         natschools_5 = as.character(M045071a),
         natchld_1 = as.character(M000685), 
         natchld_3 = as.character(M023049), 
         natchld_4 = as.character(M025109), 
         natchld_5 = as.character(M045073),
         natfare_1 = as.character(M000676), 
         natfare_3 = as.character(M023046), 
         natfare_4 = as.character(M025107), 
         natfare_5 = as.character(M045070),
         natcrime_1 = as.character(M000684), 
         natcrime_3 = as.character(M023048), 
         natcrime_4 = as.character(M025109), 
         natcrime_5 = as.character(M045072),
         ftblacks_2 = as.character(M001308), 
         ftblacks_4 = as.character(M025055), 
         ftblacks_5 = as.character(M045023),
         ftwhites_2 = as.character(M001309), 
         ftwhites_4 = as.character(M025056), 
         ftwhites_5 = as.character(M045024),
         ftcons_2 = as.character(M001310), 
         ftcons_4 = as.character(M025057), 
         ftcons_5 = as.character(M045025),
         ftlibs_2 = as.character(M001311), 
         ftlibs_4 = as.character(M025058), 
         ftlibs_5 = as.character(M045026),
         ftbiz_2 = as.character(M001313), 
         ftbiz_4 = as.character(M025060), 
         ftbiz_5 = as.character(M045028),
         ftlabor_2 = as.character(M001312), 
         ftlabor_4 = as.character(M025059), 
         ftlabor_5 = as.character(M045027),
         ftmil_2 = as.character(M001306), 
         ftmil_4 = as.character(M025053), 
         ftmil_5 = as.character(M045021),
         fthisp_2 = as.character(M001316), 
         fthisp_4 = as.character(M025063), 
         fthisp_5 = as.character(M045031),
         fthomo_2 = as.character(M001321), 
         fthomo_4 = as.character(M025067), 
         fthomo_5 = as.character(M045035),
         ftpoor_2 = as.character(M001314), 
         ftpoor_4 = as.character(M025061), 
         ftpoor_5 = as.character(M045029),
         ftcath_2 = as.character(M001323), 
         ftcath_4 = as.character(M025068), 
         ftcath_5 = as.character(M045036),
         ftjews_2 = as.character(M001324), 
         ftjews_4 = as.character(M025069), 
         ftjews_5 = as.character(M045037),
         crooked_2 = as.character(M001537),
         crooked_4 = as.character(M025177), 
         crooked_5 = as.character(M045152),
         wastetax_2 = as.character(M001535), 
         wastetax_4 = as.character(M025175), 
         wastetax_5 = as.character(M045150),
         trustgov_2 = as.character(M001534), 
         trustgov_4 = as.character(M025174), 
         trustgov_5 = as.character(M045149),
         runfew_2 = as.character(M001536), 
         runfew_4 = as.character(M025176), 
         runfew_5 = as.character(M045151)) %>% 
  select(c(id, age_1:age_5, date_1:runfew_5)) %>%
  pivot_longer(age_1:runfew_5) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(abortion:crooked, fair:wastetax),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, spendserv),
                ~(.x-1)/6*100),
         across(c(trust, fair, helpful,
                  stayhome, crooked, wastetax,
                  runfew),
                ~(.x - 1)/4*100),
         abortion = (abortion-1)/3*100,
         across(c(natenvir, nataid,
                  nataids, natsoc, natschools, natchld,
                  natfare, natcrime,
                  trustgov),
                ~(.x-1)/2*100)) %>%
  pivot_longer(c(abortion,crooked, fair:wastetax)) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         d3_value = lead(date,3) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2)),
         a3_value = abs(value - lead(value,3))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value, d3_value,
             a1_value, a2_value, a3_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25)%>%
  ungroup()

gss6_long <- gss6 %>%
  mutate(id = 1:nrow(gss6)) %>%
  select(id, age_1, age_2, age_3,
         dateintv_1, dateintv_2, dateintv_3,
         partyid_1, partyid_2, partyid_3,
         polviews_1, polviews_2, polviews_3,
         trust_1, trust_2, trust_3,
         fair_1, fair_2, fair_3,
         helpful_1, helpful_2, helpful_3,
         natenvir_1, natenvir_2, natenvir_3,
         nataid_1, nataid_2, nataid_3,
         natsoc_1, natsoc_2, natsoc_3,
         natchld_1, natchld_2, natchld_3,
         natfare_1, natfare_2, natfare_3,
         natcrime_1, natcrime_2, natcrime_3) %>%
  mutate(age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
         age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3 - 4, age_1),
         age_2 = ifelse(is.na(age_2), age_1 + 2, age_2),
         age_3 = ifelse(is.na(age_3), age_1 + 4, age_3),
         across(age_1:age_3, ~as.character(.x))) %>%
  mutate(date_1 = paste(2006,substr(dateintv_1, 1, 1),substr(dateintv_1, 2, 3),
                        sep = "-"),
         date_2 = paste(2008,substr(dateintv_2, 1, 1),substr(dateintv_2, 2, 3),
                        sep = "-"),
         date_2 = ifelse(date_2 == "2008-NA-NA", NA, date_2),
         date_3 = paste(2010,substr(dateintv_3, 1, 1),substr(dateintv_3, 2, 3),
                        sep = "-"),
         date_3 = ifelse(date_3 == "2010-NA-NA", NA, date_3),
         across(c(partyid_1, partyid_2, partyid_3), ~ifelse(.x %in% c(7), NA, .x)),
         across(c(trust_1, trust_2, trust_3,
                  fair_1, fair_2, fair_3,
                  helpful_1, helpful_2, helpful_3), 
                ~ifelse(.x == 3, 1.5, .x)),
         across(partyid_1:natcrime_3,
                ~as.character(.x))) %>%
  select(-c(dateintv_1, dateintv_2, dateintv_3)) %>%
  pivot_longer(age_1:date_3) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(age, fair:trust),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         polviews = (polviews-1)/6*100,
         across(c(trust, fair,helpful),
                ~(.x-1)*100),
         across(c(natenvir, nataid, natsoc, natchld,
                  natfare, natcrime),
                ~(.x-1)/2*100)) %>%
  pivot_longer(fair:trust) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value,
             a1_value, a2_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25)%>%
  ungroup()


gss8_long <- gss8 %>%
  mutate(id = 1:nrow(gss8)) %>%
  select(id, age_1, age_2, age_3, 
         dateintv_1, dateintv_2, dateintv_3,
         partyid_1, partyid_2, partyid_3,
         polviews_1, polviews_2, polviews_3,
         trust_1, trust_2, trust_3,
         fair_1, fair_2, fair_3,
         helpful_1, helpful_2, helpful_3,
         natenvir_1, natenvir_2, natenvir_3,
         nataid_1, nataid_2, nataid_3,
         natsoc_1, natsoc_2, natsoc_3,
         natchld_1, natchld_2, natchld_3,
         natfare_1, natfare_2, natfare_3,
         natcrime_1, natcrime_2, natcrime_3) %>%
  mutate(age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
         age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3 - 4, age_1),
         age_2 = ifelse(is.na(age_2), age_1 + 2, age_2),
         age_3 = ifelse(is.na(age_3), age_1 + 4, age_3),
         across(age_1:age_3, ~as.character(.x))) %>%
  mutate(date_1 = paste(2008,substr(dateintv_1, 1, 1),substr(dateintv_1, 2, 3),
                        sep = "-"),
         date_2 = paste(2010,substr(dateintv_2, 1, 1),substr(dateintv_2, 2, 3),
                        sep = "-"),
         date_2 = ifelse(date_2 == "2010-NA-NA", NA, date_2),
         date_3 = paste(2012,substr(dateintv_3, 1, 1),substr(dateintv_3, 2, 3),
                        sep = "-"),
         date_3 = ifelse(date_3 == "2012-NA-NA", NA, date_3),
         across(c(partyid_1, partyid_2, partyid_3), ~ifelse(.x %in% c(7), NA, .x)),
         across(c(trust_1, trust_2, trust_3,
                  fair_1, fair_2, fair_3,
                  helpful_1, helpful_2, helpful_3), ~ifelse(.x == 3, 1.5, .x)),
         across(partyid_1:natcrime_3,
                ~as.character(.x))) %>%
  select(-c(dateintv_1, dateintv_2, dateintv_3)) %>%
  pivot_longer(age_1:date_3) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(age, fair:trust),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         polviews = (polviews-1)/6*100,
         across(c(trust, fair,helpful),
                ~(.x-1)*100),
         across(c(natenvir, nataid,
                  natsoc, natchld, natfare,
                  natcrime),
                ~(.x-1)/2*100)) %>%
  pivot_longer(fair:trust) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value,
             a1_value, a2_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25)%>%
  ungroup()

gss10_long <- gss10 %>%
  mutate(id = 1:nrow(gss10)) %>%
  select(id, age_1, age_2, age_3,
         dateintv_1, dateintv_2, dateintv_3,
         partyid_1, partyid_2, partyid_3,
         polviews_1, polviews_2, polviews_3,
         trust_1, trust_2, trust_3,
         fair_1, fair_2, fair_3,
         helpful_1, helpful_2, helpful_3,
         natenvir_1, natenvir_2, natenvir_3,
         nataid_1, nataid_2, nataid_3,
         natsoc_1, natsoc_2, natsoc_3,
         natchld_1, natchld_2, natchld_3,
         natfare_1, natfare_2, natfare_3,
         natcrime_1, natcrime_2, natcrime_3) %>%
  mutate(age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
         age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3 - 4, age_1),
         age_2 = ifelse(is.na(age_2), age_1 + 2, age_2),
         age_3 = ifelse(is.na(age_3), age_1 + 4, age_3),
         across(age_1:age_3, ~as.character(.x))) %>%
  mutate(date_1 = paste(2010,substr(dateintv_1, 1, 1),substr(dateintv_1, 2, 3),
                        sep = "-"),
         date_2 = paste(2012,substr(dateintv_2, 1, 1),substr(dateintv_2, 2, 3),
                        sep = "-"),
         date_2 = ifelse(date_2 == "2012-NA-NA", NA, date_2),
         date_3 = paste(2014,substr(dateintv_3, 1, 1),substr(dateintv_3, 2, 3),
                        sep = "-"),
         date_3 = ifelse(date_3 == "2014-NA-NA", NA, date_3),
         across(c(partyid_1, partyid_2, partyid_3), ~ifelse(.x %in% c(7), NA, .x)),
         across(c(trust_1, trust_2, trust_3,
                  fair_1, fair_2, fair_3,
                  helpful_1, helpful_2, helpful_3), ~ifelse(.x == 3, 1.5, .x)),
         across(partyid_1:natcrime_3,
                ~as.character(.x))) %>%
  select(-c(dateintv_1, dateintv_2, dateintv_3)) %>%
  pivot_longer(age_1:date_3) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(age, fair:trust),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         polviews = (polviews-1)/6*100,
         across(c(trust, fair,helpful),
                ~(.x-1)*100),
         across(c(natenvir, nataid,
                  natsoc, natchld, natfare, natcrime),
                ~(.x-1)/2*100)) %>%
  pivot_longer(fair:trust) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value,
             a1_value, a2_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25)%>%
  ungroup()

gss20_long <- gss20 %>%
  mutate(id = 1:nrow(gss20)) %>%
  select(id, age_1a, age_1b, age_2,
         dateintv_1a, dateintv_1b, dateintv_2,
         partyid_1a, partyid_1b, partyid_2,
         polviews_1a, polviews_1b, polviews_2,
         trust_1a, trust_1b, trust_2,
         fair_1a, fair_1b, fair_2,
         helpful_1a, helpful_1b, helpful_2,
         natenvir_1a, natenvir_1b, natenvir_2,
         nataid_1a, nataid_1b, nataid_2,
         natsoc_1a, natsoc_1b, natsoc_2,
         natchld_1a, natchld_1b, natchld_2,
         natfare_1a, natfare_1b, natfare_2,
         natcrime_1a, natcrime_1b, natcrime_2) %>%
  mutate(age_1a = ifelse(is.na(age_1a) & !is.na(age_2), age_2 - 4, age_1a),
         age_1b = ifelse(is.na(age_1b) & !is.na(age_2), age_2 - 2, age_1b),
         age_2 = ifelse(is.na(age_2) & !is.na(age_1a), age_1a + 4, age_2),
         age_2 = ifelse(is.na(age_2) & !is.na(age_1b), age_1b + 2, age_2),
         across(age_1a:age_2, ~as.character(.x))) %>%
  mutate(md = sprintf("%04d", dateintv_1a),
         date_1a = paste(2016, substr(md, 1, 2),substr(md, 3, 4),
                        sep = "-"),
         date_1a = ifelse(date_1a == "2016-00-NA", NA, date_1a),
         md = sprintf("%04d", dateintv_1b),
         date_1b = paste(2018, substr(md, 1, 2),substr(md, 3, 4),
                        sep = "-"),
         date_1b = ifelse(date_1b == "2018-00-NA", NA, date_1b),
         md = sprintf("%04d", dateintv_2),
         date_2 = paste(2020, substr(md, 1, 2),substr(md, 3, 4),
                        sep = "-"),
         date_2 = ifelse(date_2 == "2020-00-NA", NA, date_2),
         across(c(partyid_1a, partyid_1b, partyid_2), 
                ~ifelse(.x %in% c(7,8,9), NA, .x)),
         across(c(trust_1a, trust_1b, trust_2,
                  fair_1a, fair_1b, fair_2,
                  helpful_1a, helpful_1b, helpful_2), 
                ~ifelse(.x == 3, 1.5, .x)),
         across(partyid_1a:natcrime_2,
                ~as.character(.x))) %>%
  select(id, age_1a:age_2, date_1a, date_1b, date_2, partyid_1a:natcrime_2) %>% 
  pivot_longer(age_1a:natcrime_2) %>%
  separate(name, into = c("measure", "wave")) %>%
  mutate(wave = recode(wave, "1a"="1", "1b"="2", "2"="3")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(age, fair:trust),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         polviews = (polviews-1)/6*100,
         across(c(trust, fair, helpful),
                ~(.x-1)*100),
         across(c(natenvir, nataid,
                  natsoc, natchld,
                  natfare, natcrime),
                ~(.x-1)/2*100)) %>%
  pivot_longer(fair:trust) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value,
             a1_value, a2_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25)%>%
  ungroup()

convert_to_date <- function(date_str) {
  year <- substr(date_str, 1, 4)
  month <- substr(date_str, 5, 6)
  day = substr(date_str, 7, 8)
  formatted <- as.character(paste(year,month,day,sep="-"))
  return(formatted)
}
anes16_long <- anes16 %>%
  mutate(id = 1:nrow(anes16)) %>%
  zap_labels() %>%
  select(id, V161267, V201507x,
         V164002, V165002, V203053, V203078,
         V161158x, V201231x,
         V161126, V162171, V201200,
         V161153, V201347,
         V161217, V201235,
         V161216, V201234,
         V161189, V201255,
         V161184, V201252,
         V161198, V201258,
         V161232, V201336,
         V161178, V201246,
         V161181, V201249,
         V161212, V201321,
         V161205, V201300,
         V161206, V201303,
         V161209, V201312,
         V161208, V201309,
         V161211, V201318,
         V161095, V201156,
         V161096, V201157,
         V162312, V202480,
         V162314, V202482,
         V162101, V202164,
         V162097, V202161,
         V162100, V202163,
         V162098, V202162,
         V162311, V202479,
         V162103, V202166,
         V162108, V202170,
         V162110, V202171) %>%
  mutate(age_1 = ifelse(V161267 < 0 & V201507x > 0, V201507x, ifelse(V161267 < 0, NA, V161267)),
         age_2 = age_1,
         age_3 = ifelse(!is.na(age_1) & V201507x < 0, age_1 + 4, V201507x),
         age_4 = age_3,
         across(age_1:age_4, ~as.character(.x))) %>%
  mutate(date_1 = convert_to_date(V164002),
         date_2 = convert_to_date(V165002),
         date_3 = convert_to_date(V203053),
         date_4 = convert_to_date(V203078),
         across(c(V161095, V201156,
                  V161096, V201157,
                  V162312, V202480,
                  V162314, V202482,
                  V162101, V202164,
                  V162097, V202161,
                  V162100, V202163,
                  V162098, V202162,
                  V162311, V202479,
                  V162103, V202166,
                  V162108, V202170,
                  V162110, V202171),
                ~ifelse(.x < 0 | .x > 100, NA, .x)),
         across(c(V161158x, V201231x,
                  V161126, V162171, V201200,
                  V161153, V201347,
                  V161217, V201235,
                  V161216, V201234, 
                  V161189, V201255,
                  V161184, V201252,
                  V161198, V201258,
                  V161178, V201246,
                  V161181, V201249),
                ~ifelse(.x < 0 | .x > 7, NA, .x)),
         across(c(V161232, V201336),
                ~ifelse(.x < 0 | .x > 4, NA, .x)),
         across(c(V161212, V201321,
                  V161205, V201300,
                  V161206, V201303,
                  V161209, V201312,
                  V161208, V201309,
                  V161211, V201318),
                ~ifelse(.x == 3, 1.5, ifelse(.x < 0, NA, .x))),
         partyid_1 = as.character(V161158x),
         partyid_3 = as.character(V201231x),
         polviews_1 = as.character(V161126),
         polviews_2 = as.character(V162171),
         polviews_3 = as.character(V201200),
         stayhome_1 = as.character(V161153), 
         stayhome_3 = as.character(V201347),
         wastetax_1 = as.character(V161217), 
         wastetax_3 = as.character(V201235),
         runfew_1 = as.character(V161216), 
         runfew_3 = as.character(V201234),
         jobguar_1 = as.character(V161189), 
         jobguar_3 = as.character(V201255),
         govins_1 = as.character(V161184), 
         govins_3 = as.character(V201252),
         helpblk_1 = as.character(V161198), 
         helpblk_3 = as.character(V201258),
         abortion_1 = as.character(V161232), 
         abortion_3 = as.character(V201336),
         spendserv_1 = as.character(V161178), 
         spendserv_3 = as.character(V201246),
         defscale_1 = as.character(V161181), 
         defscale_3 = as.character(V201249),
         natenvir_1 = as.character(V161212), 
         natenvir_3 = as.character(V201321),
         natsoc_1 = as.character(V161205), 
         natsoc_3 = as.character(V201300),
         natschools_1 = as.character(V161206), 
         natschools_3 = as.character(V201303),
         natfare_1 = as.character(V161209), 
         natfare_3 = as.character(V201312),
         natcrime_1 = as.character(V161208),
         natcrime_3 = as.character(V201309),
         natpoor_1 = as.character(V161211), 
         natpoor_3 = as.character(V201318),
         ftdems_1 = as.character(V161095), 
         ftdems_3 = as.character(V201156),
         ftreps_1 = as.character(V161096), 
         ftreps_3 = as.character(V201157),
         ftblacks_2 = as.character(V162312), 
         ftblacks_4 = as.character(V202480),
         ftwhites_2 = as.character(V162314), 
         ftwhites_4 = as.character(V202482),
         ftcons_2 = as.character(V162101), 
         ftcons_4 = as.character(V202164),
         ftlibs_2 = as.character(V162097), 
         ftlibs_4 = as.character(V202161),
         ftbiz_2 = as.character(V162100), 
         ftbiz_4 = as.character(V202163),
         ftlabor_2 = as.character(V162098), 
         ftlabor_4 = as.character(V202162),
         fthisp_2 = as.character(V162311), 
         fthisp_4 = as.character(V202479),
         fthomo_2 = as.character(V162103), 
         fthomo_4 = as.character(V202166),
         ftjews_2 = as.character(V162108), 
         ftjews_4 = as.character(V202170),
         ftcops_2 = as.character(V162110), 
         ftcops_4 = as.character(V202171)) %>% 
  select(id, age_1:age_4, date_1:ftlabor_4) %>%
  pivot_longer(age_1:ftlabor_4) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(abortion, age, defscale:wastetax),
                ~as.numeric(.x)),
         across(c(partyid, polviews, jobguar,
                  govins, helpblk, spendserv,
                  defscale),
                ~(.x-1)/6*100),
         across(c(stayhome, runfew),
                ~(.x-1)*100),
         across(c(wastetax,
                  natenvir, natsoc, natschools, natfare,
                  natcrime, natpoor),
                ~(.x-1)/2*100),
         across(c(abortion),
                ~(.x-1)/3*100)) %>%
  pivot_longer(c(abortion, defscale:wastetax)) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value,
             a1_value, a2_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25)%>%
  ungroup()

anes20_long <- anes20 %>%
  mutate(id = 1:nrow(anes20)) %>%
  select(id, profile_age,
         start, w2start, w3startdt,
         pid7x, w2pid7x, w3pid7x,
         lcself, w2lcself, w3lcself,
         hp_you, w2hp_you,
         ftdem, w2ftdem, w3ftdem,
         ftrep, w2ftrep, w3ftrep,
         w2ftgay, w3ftgay, 
         w2ftpolice, w3ftpolice) %>%
  mutate(age_1 = profile_age,
         age_2 = profile_age, 
         age_3 = profile_age + 2,
         across(age_1:age_3, ~as.character(.x))) %>%
  mutate(date_1 = paste(substr(start, 1, 4), substr(start, 5, 6),substr(start, 7, 8),
                        sep = "-"),
         date_2 = paste(substr(w2start, 1, 4), substr(w2start, 5, 6),substr(w2start, 7, 8),
                        sep = "-"),
         date_3 = as.character(w3startdt),
         across(c(pid7x, w2pid7x, w3pid7x), 
                ~ifelse(.x %in% c(-7, -6), NA, .x - 1)),
         across(c(lcself, w2lcself, w3lcself,
                  hp_you, w2hp_you),
                ~ifelse(.x %in% c(-7, -6, -1, 77, 98, 99), NA, .x-1)),
         across(c(ftdem, w2ftdem, w3ftdem,
                  ftrep, w2ftrep, w3ftrep,
                  w2ftgay, w3ftgay, 
                  w2ftpolice, w3ftpolice),
                ~ifelse(.x > 100 | .x < 0, NA, .x)),
         partyid_1 = as.character(pid7x), 
         partyid_2 = as.character(w2pid7x),
         partyid_3 = as.character(w3pid7x),
         polviews_1 = as.character(lcself),
         polviews_2 = as.character(w2lcself),
         polviews_3 = as.character(w3lcself),
         govins_1 = as.character(hp_you), 
         govins_2 = as.character(w2hp_you),
         ftdems_1 = as.character(ftdem),
         ftdems_2 = as.character(w2ftdem),
         ftdems_3 = as.character(w3ftdem),
         ftreps_1 = as.character(ftrep),
         ftreps_2 = as.character(w2ftrep),
         ftreps_3 = as.character(w3ftrep),
         fthomo_2 = as.character(w2ftgay),
         fthomo_3 = as.character(w3ftgay),
         ftcops_2 = as.character(w2ftpolice),
         ftcops_3 = as.character(w3ftpolice)) %>%
  select(id, age_1:age_3, date_1:ftcops_3) %>%
  pivot_longer(age_1:ftcops_3) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(age, ftcops:polviews),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, govins),
                ~(.x - 1)/6*100)) %>%
  pivot_longer(ftcops:polviews) %>%
  na.omit() %>% arrange(id, date) %>%
  group_by(id, name) %>%
  mutate(d1_value = lead(date) - date,
         d2_value = lead(date,2) - date,
         a1_value = abs(value - lead(value)),
         a2_value = abs(value - lead(value,2))) %>%
  pivot_longer(
    cols = c(d1_value, d2_value,
             a1_value, a2_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>%
  na.omit() %>%
  mutate(d = d/365.25)%>%
  ungroup()



#Combine the data sets
long_data <- bind_rows(anes5_long %>% mutate(df = "1956-60 ANES"),
          anes7_long %>% mutate(df = "1972-76 ANES"),
          anes8_long %>% mutate(df = "1980 ANES"),
          anes90_long %>% mutate(df = "1990-92 ANES"),
          anes9_long %>% mutate(df = "1992-97 ANES"),
          anes0_long %>% mutate(df = "2000-04 ANES"),
          gss6_long %>% mutate(df = "2006-10 GSS"),
          gss8_long %>% mutate(df = "2008-12 GSS"),
          gss10_long %>% mutate(df = "2010-14 GSS"),
          gss20_long %>% mutate(df = "2016-20 GSS"),
          anes16_long %>% mutate(df = "2016-20 ANES"),
          anes20_long %>% mutate(df = "2020-22 ANES")) %>%
  ungroup() %>%
  mutate(dec_diff = (as.numeric(difftime(date, min(date), units = "days")))/3652.5,
         d = as.numeric(d))

long_data %>%
  group_by(name, df) %>% 
  summarise(mean_d = mean(d), sd_d = sd(d), mean_a = mean(a),
            sd_a = sd(a)) %>% View()

m1 <- lm(a ~ d + df + dec_diff + I(d*dec_diff), data = long_data)




# Every year since someone was last observed increases their absolute change
# 1.1354 on average.
  
long_data %>%
  ggplot(aes(x = d, y = a, color = df)) +
  #geom_point(alpha = .1) + 
  geom_smooth(method = "lm", se = FALSE) + 
  #facet_wrap(~df) + 
  theme_bw() + 
  facet_wrap(~name) +
  labs(x = "Duration between observations",
       y = "Expected absolute difference",
       color = "Panel") + 
  scale_color_viridis_d()

long_data %>%
  group_by(name, df, set) %>%
  summarise(a = mean(a)) %>% ungroup() %>% 
  mutate(year = recode(df, "1956-60 ANES"=1956, "1972-76 ANES"=1972,
                       "1980 ANES"=1980, "1990-92 ANES"=1990, 
                       "1992-97 ANES"=1992, "2000-04 ANES"=2000,
                       "2006-10 GSS"=2006, "2008-12 GSS"=2008, 
                       "2010-14 GSS"=2010, "2016-20 GSS"=2016,
                       "2016-20 ANES"=2016, "2020-22 ANES"=2020)) %>%
  ggplot(aes(x = year, y = a)) + 
  geom_point() +
  facet_wrap(~name) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()

m1 <- lm(a ~ d + dec_diff + I(d*dec_diff) + name, 
         data = long_data)

m2 <- lmer(a ~ d + dec_diff + I(d*dec_diff) + (1 + d + dec_diff | name),
     data = long_data)

test <- tidy(m2) %>%
  filter(effect == "fixed") %>%
  mutate(fe = estimate) %>%
  select(term, fe)


augment(ranef(m2, condVar = TRUE)) %>%
  left_join(test, by = c("variable"="term")) %>%
  mutate(estimate = estimate + fe, lb = lb + fe, ub = ub + fe) %>%
  ggplot(aes(x = estimate, y = level, xmin = lb, xmax = ub)) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_vline(data = tidy(fixef(m2)) %>% mutate(variable = names, estimate = x),
             aes(xintercept = estimate), color = "firebrick", linetype = 2) +
  geom_linerange() + 
  geom_point() + 
  facet_grid(.~variable, scales = "free_x") + 
  theme_bw()



diff_function <- function(x) {
  m1 <- lm(a ~ d, data = x)
  return(tidy(m1))
}

long_data %>%
  group_by(df, name) %>%
  nest() %>%
  mutate(lm = map(data, diff_function)) %>%
  unnest(lm) %>% 
  #filter(name %in% c("partyid", "polviews")) %>%
  mutate(year = recode(df, "1956-60 ANES"=1956, "1972-76 ANES"=1972,
                       "1980 ANES"=1980, "1990-92 ANES"=1990, 
                       "1992-97 ANES"=1992, "2000-04 ANES"=2000,
                       "2006-10 GSS"=2006, "2008-12 GSS"=2008, 
                       "2010-12 GSS"=2010, "2016-20 GSS"=2016,
                       "2020-22 ANES"=2020)) %>%
  filter(name == "natenvir") %>%
  ggplot(aes(x = year, 
             # ymin = estimate - 1.96*std.error,
             # ymax = estimate + 1.96*std.error,
             y = estimate)) + 
  geom_point() + 
  #geom_linerange() + 
  theme_bw() + 
  geom_smooth(method = "lm") +
  labs(x = "Year", y = "Slope Coefficient",
       title = "Expected abs. difference for 1-year duration: Party ID & Polviews",
       subtitle = "Variable rescaled to 0-100") + 
  facet_grid(term~name, scales = "free_y")
  

anes90_long %>%
  filter(name == "trust") %>%
  group_by(wave) %>%
  summarise(mean = mean(value),
            mean_d = mean(d),
            sd_d = sd(d))

long_data %>%
  ggplot(aes(x = date, y = a)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~name) + 
  theme_bw()


lm_func <- function(x) {
  tidy(lm(a ~ date, data = x))
}

long_data %>%
  group_by(name) %>%
  nest() %>%
  mutate(t = map(data, lm_func)) %>%
  unnest(t) %>%
  filter(term == "date") %>%
  unnest(data) %>% 
  mutate(direction = ifelse(estimate < 0, "negative", "positive")) %>%
  ggplot(aes(x = date, y = a, color = direction)) + 
  facet_wrap(~reorder(name, estimate)) +
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(x = "Year", y = "Absolute difference",
       title = "Expected wave-to-wave absolute difference by observation year") + 
  scale_color_brewer(type = "qual") + 
  theme(legend.position = "none")


