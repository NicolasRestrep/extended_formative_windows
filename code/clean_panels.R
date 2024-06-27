#########################################
#### Cleaning for Stability Analysis ####
#########################################

# Packages ====
library(haven)
library(tidyverse)
library(broom)
library(broom.mixed)
library(lme4)

# Help Functions ====
convert_to_date <- function(date_str) {
  year <- substr(date_str, 1, 4)
  month <- substr(date_str, 5, 6)
  day = substr(date_str, 7, 8)
  formatted <- as.character(paste(year,month,day,sep="-"))
  return(formatted)
}

# Since we have different paths, just comment out whichever one you're not using

# Kevin Read in Data ====
#Load in ANES panels
anes5 <- read_dta("~/Dropbox/data/anes/anes5660/anes_mergedfile_1956to1960.dta") 
anes7 <- read_dta("~/Dropbox/data/anes/anes7276/anes_mergedfile_1972to1976.dta")
anes8 <- read_dta("~/Dropbox/data/anes/anes1980/anes1980.dta")
anes90 <- read_dta("~/Dropbox/data/anes/anes9092/anes_mergedfile_1990to1992.dta")
anes9 <- read_dta("~/Dropbox/data/anes/anes9297/anes_mergedfile_1992to1997.dta") 
anes0 <- read_dta("~/Dropbox/data/anes/anes0004/anes_mergedfile_2000to2004.dta")
anes16 <- read_sav("~/Dropbox/data/anes/anes1620.sav")
anes20 <- read_dta("~/Dropbox/data/anes/anes2022/anes2022")

#Load in GSS panels
gss6 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta")
gss8 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel08.dta")
gss10 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel10.dta")
gss20 <- read_dta("~/Dropbox/data/gss2020panel/gss2020panel.dta")


# Nico Read in Data ====
# anes5 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_1956to1960.dta")
# anes7 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_1972to1976.dta")
# anes8 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes1980.dta")
# anes90 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_1990to1992.dta")
# anes9 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_1992to1997.dta")
# anes0 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_2000to2004.dta")
# anes16 <- read_sav("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_2016to2020.sav")
# anes20 <- read_csv("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_2022.csv")
# 
# gss6 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/gss/gsspanel06.dta")
# gss8 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/gss/gsspanel08.dta")
# gss10 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/gss/gsspanel10.dta")
# gss20 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/gss/gsspanel20.dta")

### 1956-60 ANES Panel
anes5_long <- anes5 %>%
  mutate(id = 1:nrow(anes5)) %>%
  zap_labels() %>%
  select(id, V600569, #weight
    V560193, # Interview date 1
    V580309, # Interview date 2
    V600578, # Interview date 3
    V600837, # Interview date 4
    
    V560088, # Party ID 1 
    V580360, # Party ID 2
    V600657, # Party ID 3
    V600835, # Party ID 4 
    
    V560295, # Age 1 
    V580472, # Age 2
    V600688, # Age 3
    
    V560035, V580323, V600622, # stayhome 
    
    V560097, # Pay attention campaign 1
    V580373, # Pay attention campaign 2
    V600664, # Pay attention campaign 3
    
    V560108, # People like me, no say 1
    V600673, # People like me, no say 3
    
    V560111, #so many vote, 1
    V600676, #so many vote, 3
    
    V560112, #pol too complicated 1
    V600677, #pol too complicated 3
    
    V560115, #officials dont care 1
    V600680 #officials dont care 3
         ) %>%
  mutate(weight_4 = as.character(V600569),
         weight_3 = weight_4,
         weight_2 = weight_4,
         across(c(V560088, V580360, V600657, V600835), ~ ifelse(.x %in% c(7, 8, 9), NA, .x)),
         across(c(V560295, V580472, V600688), ~ ifelse(.x > 97, NA, .x)),
         across(c(V560097, V580373, V600664), ~ ifelse(.x %in% c(8,9), NA, .x)), 
         across(c(V560035, V580323, V600622),
                ~recode(.x, 
                        "1"=1, "2"=1, "3"=1.5, "4"=2, "5"=2, "7"=NA_real_,
                        "0"=NA_real_, "8"=NA_real_, "9"=NA_real_)),
         across(c(V560108, V600673, V560111, V600676, V560112, V600677,
                  V560115, V600680),
                ~ifelse(.x == 8, 3, ifelse(.x == 9, NA, .x))),
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
         partyid_4 = as.character(V600835),
         stayhome_1 = as.character(V560035), 
         stayhome_2 = as.character(V580323), 
         stayhome_3 = as.character(V600622),
         attentioncpg_1 = as.character(V560097), 
         attentioncpg_2 = as.character(V580373), 
         attentioncpg_3 = as.character(V600664), 
         ppllikeme_1 = as.character(V560108), 
         ppllikeme_3 = as.character(V600673),
         manyvote_1 = as.character(V560111), #so many vote, 1
         manyvote_3 = as.character(V600676), #so many vote, 3
         complicated_1 = as.character(V560112), #pol too complicated 1
         complicated_3 = as.character(V600677), #pol too complicated 3
         dontcare_1 = as.character(V560115), #officials dont care 1
         dontcare_3 = as.character(V600680) #officials dont care 3
         ) %>%
  select(id, weight_2, weight_3, weight_4, age_1:date_4, partyid_1:dontcare_3) %>% 
  pivot_longer(weight_2:dontcare_3) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>% 
  mutate(date = as.Date(date),
         across(c(weight, age, attentioncpg:complicated, dontcare:stayhome),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         stayhome = (stayhome-1)*100,
         across(c(attentioncpg, ppllikeme, complicated,
                  dontcare, manyvote), 
                ~(.x-1)/4*100)) %>%
  pivot_longer(c(attentioncpg:complicated, dontcare:stayhome)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%  
  mutate(df = "1956-60 ANES")

### 1972-76 ANES Panel
#Does not include weights that compensate
#for sampling design/non-response
anes7_long <- anes7 %>%
  mutate(id = 1:nrow(anes7)) %>%
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


# Clean Data 1980 ====
# Do not appear to be weights for this
anes8_long <- anes8 %>%
  mutate(id = 1:nrow(anes8)) %>%
  select(
    id,
    VMP0325,
    VMP2404,
    VMP3386,
    VMP0578,
    VMP2425,
    VMP3571,
    VMP4105,
    VMP0191,
    VMP2212,
    VMP3212,
    VMP4022,
    VMP0121,
    VMP2125,
    VMP3213,
    VMP2192,
    VMP3284,
    VMP0144,
    VMP2159,
    VMP3254,
    VMP0133,
    VMP2137,
    VMP3234,
    VMP0052,
    VMP2052,
    VMP3109,
    VMP0053,
    VMP2053,
    VMP3110,
    # attention political campaigns
    VMP0005,
    VMP2001,
    VMP3001,
    VMP4001
  ) %>%
  mutate(
    across(c(VMP0325, VMP2404, VMP3386), ~ ifelse(.x == 0, NA, .x)),
    age_1 = VMP0325,
    age_2 = VMP2404,
    age_3 = VMP3386,
    age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2, age_1),
    age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3, age_1),
    age_2 = ifelse(is.na(age_2), age_1, age_2),
    age_3 = ifelse(is.na(age_3), age_1, age_3),
    age_4 = age_3,
    across(c(age_1:age_4), ~ as.character(.x))
  ) %>%
  mutate(
    date_str = sprintf("%06d", as.integer(VMP0578)),
    date_1 = paste(
      paste0("19", substr(date_str, 5, 6)),
      substr(date_str, 1, 2),
      substr(date_str, 3, 4),
      sep = "-"
    )
  ) %>%
  select(-c(date_str)) %>%
  mutate(
    date_str = sprintf("%06d", as.integer(VMP2425)),
    date_2 = paste(
      paste0("19", substr(date_str, 5, 6)),
      substr(date_str, 1, 2),
      substr(date_str, 3, 4),
      sep = "-"
    ),
    date_2 = ifelse(date_2 == "1999-99-99", NA, date_2)
  ) %>%
  select(-c(date_str)) %>%
  mutate(
    date_str = sprintf("%06d", as.integer(VMP3571)),
    date_3 = paste(
      paste0("19", substr(date_str, 5, 6)),
      substr(date_str, 1, 2),
      substr(date_str, 3, 4),
      sep = "-"
    ),
    date_3 = ifelse(date_3 %in% c("1900-00-00", "1999-99-99"), NA, date_3)
  ) %>%
  select(-c(date_str)) %>%
  mutate(
    date_str = sprintf("%06d", as.integer(VMP4105)),
    date_4 = paste(
      paste0("19", substr(date_str, 5, 6)),
      substr(date_str, 1, 2),
      substr(date_str, 3, 4),
      sep = "-"
    ),
    date_4 = ifelse(date_4 %in% c("1900-00-00", "1999-99-99"), NA, date_4)
  ) %>%
  select(-c(date_str)) %>%
  mutate(
    across(c(VMP0191, VMP2212, VMP3212, VMP4022), ~ ifelse(.x %in% c(7, 8, 9), NA, .x)),
    across(
      c(
        VMP0121,
        VMP2125,
        VMP3213,
        VMP0144,
        VMP2159,
        VMP3254,
        VMP0133,
        VMP2137,
        VMP3234, 
        VMP0005,
        VMP2001,
        VMP3001,
        VMP4001
      ),
      ~ ifelse(.x %in% c(0, 8, 9), NA, .x)
    ),
    across(c(VMP2192, VMP3284), ~ ifelse(.x %in% c(7, 8, 9), NA, .x)),
    across(
      c(VMP0052, VMP2052, VMP3109, #republicans
        VMP0053, VMP2053, VMP3110),
      ~ ifelse(.x %in% c(998, 999), NA, .x)
    )
  ) %>%
  mutate(
    partyid_1 = as.character(VMP0191),
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
    ftreps_3 = as.character(VMP3110), 
    attentioncpg_1 = as.character(VMP0005),
    attentioncpg_2 = as.character(VMP2001),
    attentioncpg_3 = as.character(VMP3001),
    attentioncpg_4 = as.character(VMP4001)
  ) %>%
  select(c(id, age_1:age_4, date_1:attentioncpg_4)) %>%
  pivot_longer(age_1:attentioncpg_4) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(age, abortion, defscale:spendserv),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, spendserv,
                  defscale),
                ~(.x-1)/6*100),
         abortion = (abortion-1)/3*100, 
         attentioncpg = as.numeric(attentioncpg), 
         attentioncpg = (attentioncpg-1)/4*100) %>%
  pivot_longer(c(abortion,attentioncpg, defscale:spendserv)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
  mutate(df = "1980 ANES") %>%
  mutate(weight = 1)

# Clean Data 1990-92 ANES ====

anes90_long <- anes90 %>%
  mutate(id = 1:nrow(anes90)) %>%
  zap_labels() %>%
  select(id,
         V923009,
         V900552, V923903,
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
         V900506, V912488, V926122, 
         # Attention political campaigns(1,3)
         V900062,	V923101, 
         # People like me Likert (1-4)
         V900509,	V912489, V926102,
         # Less gvtment 
         V900333,	V925729, 
         # Preferential hiring
         V900463,	V912558, V925935, 
         # Death Penalty 
         V900477,	V925933, 
         # Immigrants decrease 
         V912619,	V926235, 
         # T: asian-americans
         V912235,	V925339, 
         # T: Feminists
         V912240,	V925317, 
         # T: welfare
         V900159, V925318, 
         # T: environmentalists 
         V912230,	V925329, 
         # T: illegal immigrants 
         V912234, V925331, 
         # Equal Op Success 
         V900426, V926024, 
         # Equal rights too far 
         V900427,	V912705,	V926025, 
         # Not everyone chance 
         V900428, V926029, 
         # Worry less equality
         V900429,	V926026, 
         # More chances are not a problem
         V900430, V926027, 
         # If people were treated equally
         V900431,	V912703, V926028, 
         # Moral relativity 
         V900501,	V912702, V926115, 
         # Traditional family ties 
         V900502, V926117, 
         # Slavery difficulty
         V900523, V926129, 
         # Irish and italians worked 
         V900521, V926126, 
         # Matter of hard work
         V900522, V926128, 
         # Religion Important 
         V900511, V923820,
         # complicated (1,4 . 5-pt sclae)
         V900510, V926104,
         # dont care (1,4. 5-pt scale)
         V900508, V926103) %>% 
   mutate(weight_3 = as.character(V923009),
         weight_4 = weight_3,
         weight_2 = weight_3,
          across(c(V900552, V923903), 
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
                  V900506, V912488, V926122, 
                  V900333, V925729, 
                  V900463,	V912558, V925935, 
                  V900477,	V925933, 
                  V912619,	V926235, 
                  V900426, V926024, 
                  V900427,	V912705,	V926025, 
                  V900428, V926029, 
                  V900429, V926026, 
                  V900430, V926027, 
                  V900431, V912703, V926028, 
                  V900501,	V912702, V926115, 
                  V900502, V926117, 
                  V900523, V926129, 
                  V900521, V926126, 
                  V900511, V923820,
                  V900522, V926128,
                  V900062, V923101), 
                ~ifelse(.x %in% c(0,8, 9), NA, .x)),
         across(c(V900510, V926104,
                  V900508, V926103,
                  V900509, V912489, V926102),
           ~recode(.x, "1"=1, "2"=1, "3"=3, "4"=5, "5"=5,
                   "8"=3, "9"=NA_real_, "0"=NA_real_)),
         V912558 = ifelse(V912558 == 3, NA, V912558),
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
                  V900158, V912239, V925324, 
                  V912235, V925339, 
                  V912240, V925317, 
                  V900159, V925318, 
                  V912230, V925329, 
                  V912234, V925331),
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
         runfew_4 = as.character(V926122), 
         attentioncpg_1 = as.character(V900062),	
         attentioncpg_3 = as.character(V923101), 
         ppllikeme_1 = as.character(V900509),	
         ppllikeme_2 = as.character(V912489), 
         ppllikeme_4 = as.character(V926102), 
         lessgvt_1 = as.character(V900333), 
         lessgvt_4 = as.character(V925729), 
         prefhiring_1 = as.character(V900463),	
         prefhiring_2 = as.character(V912558), 
         prefhiring_4 = as.character(V925935), 
         cappun_1 = as.character(V900477),	
         cappun_4 = as.character(V925933), 
         letin1a_2 = as.character(V912619),	
         letin1a_4 = as.character(V926235),
         ftasianamericans_2 = as.character(V912235),	
         ftasianamericans_4 = as.character(V925339), 
         ftfeminists_2 = as.character(V912240),
         ftfeminists_4 = as.character(V925317), 
         ftwelfare_1 = as.character(V900159),
         ftwelfare_4 = as.character(V925318), 
         ftenvironmental_2 = as.character(V912230), 
         ftenvironmental_4 = as.character(V925329), 
         ftillimmigrants_2 = as.character(V912234), 
         ftillimmigrants_4 = as.character(V925331), 
         eqoppsuccess_1 = as.character(V900426), 
         eqoppsuccess_4 = as.character(V926024), 
         eqrightstoofar_1 = as.character(V900427),	
         eqrightstoofar_2 = as.character(V912705),	
         eqrightstoofar_4 = as.character(V926025), 
         noteveryonechance_1 = as.character(V900428), 
         noteveryonechance_4 = as.character(V926029), 
         worrylesseq_1 = as.character(V900429),	
         worrylesseq_4 = as.character(V926026), 
         unequalnoprob_1 = as.character(V900430), 
         unequalnoprob_4 = as.character(V926027), 
         equallessprob_2 = as.character(V912703), 
         equallessprob_1 = as.character(V900431),	
         equallessprob_4 = as.character(V926028), 
         moralrel_1 = as.character(V900501),	
         moralrel_2 = as.character(V912702), 
         moralrel_4 = as.character(V926115), 
         tradfamily_1 = as.character(V900502), 
         tradfamily_4 = as.character(V926117), 
         slavediff_1 = as.character(V900523), 
         slavediff_4 = as.character(V926129), 
         wrkwayup_1 = as.character(V900521), 
         wrkwayup_4 = as.character(V926126), 
         matterhrdwrk_1 = as.character(V900522), 
         matterhrdwrk_4 = as.character(V926128), 
         religimp_1 = as.character(V900511), 
         religimp_3 = as.character(V923820),
         complicated_1 = as.character(V900510), 
         complicated_4 = as.character(V926104),
         dontcare_1 = as.character(V900508), 
         dontcare_4 = as.character(V926103)) %>%
  select(c(id, weight_2, weight_3, weight_4, age_1:age_4, date_1:dontcare_4)) %>%
  pivot_longer(weight_2:dontcare_4) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>% 
  mutate(date = as.Date(date),
         across(c(weight, abortion:crooked, defscale:wrkwayup),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, jobguar, helpblk,
                  spendserv, defscale, eqrole),
                ~(.x-1)/6*100),
         trust = (trust-1)*100,
         across(c(stayhome, crooked, wastetax, trustgov, runfew, 
                  attentioncpg, ppllikeme, prefhiring, cappun, 
                  letin1a, eqoppsuccess, eqrightstoofar, noteveryonechance,
                  worrylesseq, unequalnoprob, equallessprob, 
                  moralrel, tradfamily, slavediff, wrkwayup, 
                  matterhrdwrk, religimp,
                  complicated, dontcare),
                ~(.x-1)/4*100),
         abortion = (abortion - 1)/3*100,
         across(c(natenvir, nataids, natsoc, natfood, natschools,
                  nathome, natchld, lessgvt),
                ~(.x -1)/2*100)) %>% 
  pivot_longer(c(abortion, attentioncpg:crooked, defscale:wastetax, worrylesseq:wrkwayup)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
  mutate(df = "1990-92 ANES")
#Missing specific weights for 
#wave pairs 90/91. If people have the panel weight
#they get that wave

# Clean Data 1992-97 ====

anes9_long <- anes9 %>%
  mutate(id = 1:nrow(anes9)) %>%
  select(id, 
         V923008a, V923008b, V940006a, 
         V960005a, V960005b,#weights
         V923903, V941203, V960605, 
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
         V926122, V941035, V961253, 
         # Attention Pol campaigns, 
         V923101, V940124, V960201, 
         # People like me 
         V926102,	V941038, V960568, V961245, V970014, 
         # Spending poor people
         V923817, V960565, 
         # Less Government 
         V925729, V961144, 
         # Preferential Hiring 
         V925935,	V937405, V941001, V961208, 
         # Favor Death Penalty 
         V925933, V941041, V961197, 
         # Immigrant number 
         V926235, V941016, V961325, 
         # Opose homo
         V925923, V937324, V961193, 
         # T: Feminists 
         V925317, V970053, 
         # T: Welfare
         V925318, V940309, V961036,	V970057, 
         # T: Environmental
         V925329,	V940310, V961041, 
         # T: Illegal Immigrants
         V925331, V940317, 
         # Equal Opportunity Success
         V926024,	V940914, V961229, 
         # Equal rights too far 
         V926025,	V940915, V961230, 
         # Not everyone given a chance 
         V926029,	V940916, V961231, 
         # Worry less equality 
         V926026, V940917, V961232, 
         # Unequal no problem
         V926027,	V940918, V961233, 
         # More equality less problem
         V926028, V940919, V961234, 
         # Moral relativity 
         V926115, V941030, V961248, 
         # Traditional Familiy values
         V926117, V941031, V961249, 
         # Hrd work lazy whites
         V926221, V961311, 
         # Hrd work lazy blacks 
         V926222, V961312, 
         # Slave difficulty 
         V926129, V941051, 
         # Black no favors 
         V926126, V941049, V970182, 
         # Matter hard work
         V926128,	V941050, V970181, 
         # Religious important
         V923820, V941043, V960571,
         # Complicated (2,4,7; 5-pt; 0,9 - NA, 8 - 3)
         V926104, V941039, V961246,
         # Dontcare (2,4,7; 0,9 - NA, 8 - 3)
         V926103, V941037, V961244,
         # Pay attention (2,7 - 0,8,9 - NA)
         V926125, V961256,
         # Elections matter (2,7 - 0,8,9 - NA)
         V926124, V961255,
         # Satisfied democracy(5,7; NA - 0,8,9)
         V952119, V961459
         ) %>%
  zap_labels() %>%
  mutate(weight_1 = as.character(V923008a), 
         weight_2 = as.character(V923008b), 
         weight_3 = weight_2,
         weight_4 = as.character(V940006a), 
         weight_5 = weight_4,
         weight_6 = as.character(V960005a), 
         weight_7 = as.character(V960005b),
         weight_8 = weight_7) %>%
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
                  V960563, V960565), 
                ~recode(.x, "1"=1, "2"=3, "3"=2, "7"=3,
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
         across(c(V923634, V937370, V940655, V952263a, V960420, V970106, 
                  V923101, V940124, V960201, 
                  V923817, V960565),
                ~ifelse(.x %in% c(7,8,9), NA, .x)),
         V960571 = ifelse(V960571 == 2, 5, V960571),
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
                  V926122, V941035, V961253, 
                  V925729, V961144, 
                  V925933, V941041, V961197, 
                  V926235, V941016, V961325, 
                  V925923, V937324, V961193, 
                  V926024, V940914, V961229, 
                  V926025, V940915,	V961230, 
                  V926029, V940916, V961231, 
                  V926026, V940917, V961232, 
                  V926027, V940918, V961233, 
                  V926028, V940919, V961234, 
                  V926115, V941030,	V961248, 
                  V926117, V941031,	V961249, 
                  V926221, V961311, 
                  V926222, V961312, 
                  V926129, V941051, 
                  V926126, V941049, V970182, 
                  V926128, V941050,	V970181, 
                  V923820, V941043, V960571,
                  V926125, V961256,
                  V926124, V961255,
                  V952119, V961459), 
                ~ifelse(.x %in% c(0,8,9, 96), NA, .x)),
         across(c(V926104, V941039, V961246,
                  V926103, V941037, V961244,
                  V926102, V941038, V960568, V961245, V970014),
                ~recode(.x, "1"=1, "2"=1, "3"=3, "4"=5, "5"=5, 
                        "0"=NA_real_, "9"=NA_real_, "8"=3)),
         across(c(V923820, V941043), ~ ifelse(.x == 2, 5, .x)),
         across(c(V925935, V937405, V941001, V961208),
                ~ifelse(.x %in% c(0,7,8,9), NA, .x)),
         across(c(V923732, V941014, V960503),
                ~ifelse(.x %in% c(6,7,8,9), NA, .x)),
         across(c(V923815, V940817, V960561,
                  V923727, V940821, V960498,
                  V923811, V940819, V960560,
                  V923725, V940822, V960496,
                  V923818, V940823, V960562,
                  V923730, V960501,
                  V923813, V940824, V960564, 
                  V923726, V940820, V960497, #welfare
                  V923814, V940825, V960563,
                  V923817),
                ~ifelse(.x == 7, 3, ifelse(.x > 7, NA, .x))),
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
           V925324, V940308, V961039, 
           V925317, V970053, 
           V925318, V940309, V961036,	V970057, 
           V925329, V940310, V961041, 
           V925331,	V940317),
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
         runfew_7 = as.character(V961253), 
         attentioncpg_1 = as.character(V923101), 
         attentioncpg_4 = as.character(V940124), 
         attentioncpg_6 = as.character(V960201), 
         ppllikeme_2 = as.character(V926102),
         ppllikeme_4 =	as.character(V941038), 
         ppllikeme_6 = as.character(V960568), 
         ppllikeme_7 = as.character(V961245), 
         ppllikeme_8 = as.character(V970014), 
         natpoor_1 = as.character(V923817), 
         natpoor_6 = as.character(V960565), 
         lessgvt_2 = as.character(V925729), 
         lessgvt_7 = as.character(V961144), 
         prefhiring_2 = as.character(V925935),
         prefhiring_3 = as.character(V937405), 
         prefhiring_4 = as.character(V941001), 
         prefhiring_7 = as.character(V961208), 
         cappun_2 = as.character(V925933), 
         cappun_4 = as.character(V941041), 
         cappun_7 = as.character(V961197), 
         letin1a_2 = as.character(V926235), 
         letin1a_4 = as.character(V941016), 
         letin1a_7 = as.character(V961325), 
         opposehomo_2 = as.character(V925923), 
         opposehomo_3 = as.character(V937324), 
         opposehomo_7 = as.character(V961193), 
         ftfeminists_1 = as.character(V925317), 
         ftfeminists_8 = as.character(V970053), 
         ftwelfare_2 = as.character(V925318), 
         ftwelfare_4 = as.character(V940309), 
         ftwelfare_7 = as.character(V961036),	
         ftwelfare_8 = as.character(V970057), 
         ftenvironmental_2 = as.character(V925329),
         ftenvironmental_4 = as.character(V940310), 
         ftenvironmental_7 = as.character(V961041), 
         ftillimmigrants_2 = as.character(V925331), 
         ftillimmigrants_4 = as.character(V940317), 
         eqoppsuccess_2 = as.character(V926024),
         eqoppsuccess_4 = as.character(V940914),
         eqoppsuccess_7 = as.character(V961229), 
         eqrightstoofar_2 = as.character(V926025),	
         eqrightstoofar_4 = as.character(V940915), 
         eqrightstoofar_7 = as.character(V961230), 
         noteveryonechance_2 = as.character(V926029),
         noteveryonechance_4 = as.character(V940916), 
         noteveryonechance_7 = as.character(V961231), 
         worrylesseq_2 = as.character(V926026), 
         worrylesseq_4 = as.character(V940917), 
         worrylesseq_7 = as.character(V961232), 
         unequalnoprob_2 = as.character(V926027), 
         unequalnoprob_4 = as.character(V940918), 
         unequalnoprob_7 = as.character(V961233), 
         equallessprob_2 = as.character(V926028),
         equallessprob_4 = as.character(V940919), 
         equallessprob_7 = as.character(V961234), 
         moralrel_2 = as.character(V926115), 
         moralrel_4 = as.character(V941030),
         moralrel_7 = as.character(V961248),
         tradfamily_2 = as.character(V926117), 
         tradfamily_4 = as.character(V941031), 
         tradfamily_7 = as.character(V961249), 
         hrdwrklazywhite_2 = as.character(V926221), 
         hrdwrklazywhite_7 = as.character(V961311), 
         hrdwrklazyblack_2 = as.character(V926222), 
         hrdwrklazyblack_7 = as.character(V961312), 
         slavediff_2 = as.character(V926129), 
         slavediff_4 = as.character(V941051),
         wrkwayup_2 = as.character(V926126), 
         wrkwayup_4 = as.character(V941049), 
         wrkwayup_8 = as.character(V970182), 
         matterhrdwrk_2 = as.character(V926128), 
         matterhrdwrk_4 = as.character(V941050), 
         matterhrdwrk_8 = as.character(V970181), 
         religimp_1 = as.character(V923820), 
         religimp_4 = as.character(V941043), 
         religimp_6 = as.character(V960571),
         # Complicated (2,4,7; 5-pt; 0,9 - NA, 8 - 3)
         complicated_2 = as.character(V926104), 
         complicated_4 = as.character(V941039), 
         complicated_7 = as.character(V961246),
         # Dontcare (2,4,7; 0,9 - NA, 8 - 3)
         dontcare_2 = as.character(V926103), 
         dontcare_4 = as.character(V941037), 
         dontcare_7 = as.character(V961244),
         # Pay attention (2,7 - 0,8,9 - NA)
         polattn_2 = as.character(V926125), 
         polattn_7 = as.character(V961256),
         # Elections matter (2,7 - 0,8,9 - NA)
         elexmatter_2 = as.character(V926124), 
         elexmatter_7 = as.character(V961255),
         satdemo_5 = as.character(V952119), 
         satdemo_7 = as.character(V961459)
  ) %>%
  select(c(id, weight_1:weight_8, age_1:age_8, date_1, date_2:date_7, date_8:satdemo_7)) %>%
  pivot_longer(weight_1:satdemo_7) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(abortion:crooked, defscale:wrkwayup),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, jobguar, govins, helpblk,
                  spendserv, defscale, eqrole, hrdwrklazywhite, 
                  hrdwrklazyblack),
                ~(.x - 1)/6*100),
         across(c(trust, fair),
                ~(.x-1)*100),
         across(c(stayhome, crooked, wastetax, trustgov, runfew, 
                  attentioncpg, ppllikeme, prefhiring, cappun, 
                  letin1a, opposehomo, eqoppsuccess, eqrightstoofar, 
                  noteveryonechance, worrylesseq, unequalnoprob, equallessprob, moralrel,
                  tradfamily, slavediff, wrkwayup, matterhrdwrk, religimp,
                  complicated, dontcare, polattn, elexmatter),
                ~(.x - 1)/4*100),
         across(c(abortion, satdemo),
                ~(.x - 1)/3*100),
         across(c(natenvir, nataids, natsoc, natfood, natschools,
                  nathome, natchld, natfare, natcrime, natpoor, 
                  lessgvt),
                ~(.x - 1)/2*100)) %>% 
  pivot_longer(c(abortion, attentioncpg:crooked, defscale:wastetax, worrylesseq:wrkwayup)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
  mutate(df = "1992-97 ANES")

# Clean Data 2000-04 ANES ====
anes0_long <- anes0 %>%
  mutate(id = 1:nrow(anes0)) %>%
  select(id, 
         WT00PRE, WT00PO, WT02PRE, WT02PO, WT04,
         M000908,M023126X,M045193,
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
         M001537, M025177, M045152, #crooked
         M001535, M025175, M045150, #wastetax
         M001534, M025174, M045149, #trustgov
         M001536, M025176, M045151,#runfew
         # Attention political
         M000301,	M001201, M023001,	M025001, 
         # People like me 
         M001528,	M025173, M045148, 
         # Spend poor ppl
         M000680, M023053A,	M025115A,	M045075a, 
         # Spend military
         M025105,	M045081, 
         # Spend aid to blacks
         M000687, M023057, M025119,	M045079,
         # Immigrant number
         M000510, M045109, 
         # Favor or oppose laws around homosexuals
         M001478, M045111, 
         # Pay more or less taxes 
         M023069, M025141, M045119, 
         # T: asian-americans
         M001327, M025072, M045040, 
         # T: feminists
         M001326, M025071, M045039, 
         # T: welfare 
         M001315, M025062, M045030, 
         # T: environmentalists
         M001320, M025066, M045034, 
         # Religion important 
         M000872, M023082, M045173,
         # Many vote (2,4,5: 5pt; NA:0,9, 8-3)
         M001520, M025171, M045146,
         # Pol dont care (2,4,5; 5 pt; NA: 0,9; 8-3)
         M001527, M025172, M045147,
         # elections matter (2,4,5; 5 pt, NA: 0,8,9)
         M001538, M025178, M045153,
         # satisfied with democracy
         M001651, M025179, M045154
  ) %>%
  zap_labels() %>%
  mutate(weight_1 = as.character(WT00PRE), 
         weight_2 = as.character(WT00PO), 
         weight_3 = as.character(WT02PRE), 
         weight_4 = as.character(WT02PO), 
         weight_5 = as.character(WT04)) %>%
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
                  M000683, M000685, M000676, M000684,
                  M000680, M000687), 
                ~recode(.x, "1"=1, "5"=2, "3"=3, "7"=3,
                        "8"=NA_real_, "9"=NA_real_, "0"=NA_real_)),
         across(c(M023052, M025113, M045068,
                  M023054, M025116, M045076,
                  M023045, M025106, M045069,
                  M023055, M025117, M045077,
                  M023047A, M025108A, M045071a,
                  M023049, M025109, M045073, 
                  M023046, M025107, M045070, 
                  M023048, M025109, M045072,
                  M023053A, M025115A, M045075a,
                  M023057, M025119,	M045079,
                  M025105, M045081),
                ~recode(.x, "1"=1, "2"=3, "3"=2,
                        "4"=3)),
         across(c(M000523, M023038X, M045058x), 
                ~ifelse(.x %in% c(7,8,9), NA, .x)),
         across(c(M001520, M025171, M045146,
                  M001527, M025172, M045147,
                  M001528,	M025173, M045148),
                ~recode(.x, "1"=1, "2"=1, "3"=3, "4"=5, "5"=5, 
                        "0"=NA_real_, "9"=NA_real_, "8"=3)),
         across(c(M000446, M001368, M023022,
                  M001538, M025178, M045153), 
                ~ifelse(.x %in% c(0,8,9,90), NA, .x)),
         across(c(M001475, M025101, M045158,
                  M000513a, M023033, M045143,
                  M000545, M001385, M045127,
                  M001477, M025103, M045160,
                  M001476, M025102, M045159,
                  M001537, M025177, M045152,
                  M001535, M025175, M045150,
                  M001534, M025174, M045149,
                  M001536, M025176, M045151, 
                  M000301,	M001201, M023001,	M025001, 
                  M000510, M045109, 
                  M001478, M045111, 
                  M000872, M023082, M045173,
                  M001651, M025179, M045154), 
                ~ifelse(.x %in% c(0,8,9), NA, .x)),
         across(c(M000694, M001403, M045110, 
                  M023069, M025141, M045119),
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
           M001324, M025069, M045037, 
           M001327, M025072, M045040, 
           M001326, M025071, M045039, 
           M001315, M025062, M045030, 
           M001320, M025066, M045034),
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
         runfew_5 = as.character(M045151), 
         attentioncpg_1 = as.character(M000301),	
         attentioncpg_2 = as.character(M001201), 
         attentioncpg_3 = as.character(M023001),	
         attentioncpg_4 = as.character(M025001), 
         ppllikeme_2 = as.character(M001528),	
         ppllikeme_4 = as.character(M025173), 
         ppllikeme_5 = as.character(M045148), 
         natpoor_1 = as.character(M000680), 
         natpoor_3 = as.character(M023053A),	
         natpoor_4 = as.character(M025115A),	
         natpoor_5 = as.character(M045075a), 
         natarms_4 = as.character(M025105),	
         natarms_5 = as.character(M045081), 
         natrace_1 = as.character(M000687),
         natrace_3 =  as.character(M023057), 
         natrace_4 = as.character(M025119),	
         natrace_5 = as.character(M045079), 
         letin1a_1 = as.character(M000510),
         letin1a_4 = as.character(M045109), 
         opposehomo_2 = as.character(M001478),
         opposehomo_5 = as.character(M045111), 
         tax_3 = as.character(M023069),
         tax_4 = as.character(M025141),
         tax_5 = as.character(M045119), 
         ftasianamericans_2 = as.character(M001327), 
         ftasianamericans_4 = as.character(M025072),
         ftasianamericans_5 = as.character(M045040), 
         ftfeminists_2 = as.character(M001326),
         ftfeminists_4 = as.character(M025071), 
         ftfeminists_5 = as.character(M045039), 
         ftwelfare_2 = as.character(M001315),
         ftwelfare_4 = as.character(M025062),
         ftwelfare_5 = as.character(M045030), 
         ftenvironmental_2 = as.character(M001320), 
         ftenvironmental_4 = as.character(M025066),
         ftenvironmental_5 = as.character(M045034), 
         religimp_1 = as.character(M000872),
         religimp_3 = as.character(M023082),
         religimp_5 = as.character(M045173),
         # Many vote (2,4,5: 5pt; NA:0,9, 8-3)
         manyvote_2 = as.character(M001520), 
         manyvote_4 = as.character(M025171), 
         manyvote_5 = as.character(M045146),
         # Pol dont care (2,4,5; 5 pt; NA: 0,9; 8-3)
         dontcare_2 = as.character(M001527), 
         dontcare_4 = as.character(M025172), 
         dontcare_5 = as.character(M045147),
         # elections matter (2,4,5; 5 pt, NA: 0,8,9)
         elexmatter_2 = as.character(M001538), 
         elexmatter_4 = as.character(M025178), 
         elexmatter_5 = as.character(M045153),
         satdemo_2 = as.character(M001651), 
         satdemo_4 = as.character(M025179), 
         satdemo_5 = as.character(M045154)
  ) %>% 
  select(c(id, weight_1:weight_5, age_1:age_5, date_1:satdemo_5)) %>%
  pivot_longer(weight_1:satdemo_5) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(abortion:crooked, dontcare:weight),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, spendserv),
                ~(.x-1)/6*100),
         across(c(trust, fair, helpful,
                  stayhome, crooked, wastetax,
                  runfew, attentioncpg, ppllikeme, 
                  letin1a, opposehomo, tax, 
                  religimp, manyvote, dontcare,
                  elexmatter),
                ~(.x - 1)/4*100),
         across(c(abortion, satdemo),
                ~(.x-1)/3*100),
         across(c(natenvir, nataid,
                  nataids, natsoc, natschools, natchld,
                  natfare, natcrime, natpoor, 
                  trustgov, natarms, natrace),
                ~(.x-1)/2*100)) %>%
  pivot_longer(c(abortion,attentioncpg,crooked, dontcare:wastetax)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
  mutate(df = "2000-04 ANES")

# Clean GSS 2006-10 ====
gss6_long <- gss6 %>%
  mutate(id = 1:nrow(gss6)) %>%
  select(id, 
         wtpannr12, wtpannr123,
         age_1, age_2, age_3,
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
         natcrime_1, natcrime_2, natcrime_3,
         nateduc_1, nateduc_2, nateduc_3, 
         natarms_1, natarms_2, natarms_3, 
         natrace_1, natrace_2, natrace_3, 
         cappun_1, cappun_2, cappun_3, 
         letin1a_1, letin1a_2, letin1a_3,
         tax_1, tax_2, tax_3,
         wrkwayup_1, wrkwayup_2, wrkwayup_3) %>%
  mutate(weight_2 = as.character(wtpannr12),
         weight_3 = as.character(wtpannr123),
         age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
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
         across(c(tax_1, tax_2, tax_3), ~ifelse(.x %in% c(4), NA, .x)),
         across(c(trust_1, trust_2, trust_3,
                  fair_1, fair_2, fair_3,
                  helpful_1, helpful_2, helpful_3), 
                ~ifelse(.x == 3, 1.5, .x)),
         across(partyid_1:wrkwayup_3,
                ~as.character(.x))) %>%
  select(-c(wtpannr12, wtpannr123, dateintv_1, dateintv_2, dateintv_3)) %>%
  pivot_longer(c(weight_2, weight_3, age_1:date_3)) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(weight, age, cappun, fair:wrkwayup),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         polviews = (polviews-1)/6*100,
         across(c(trust, fair,helpful),
                ~(.x-1)*100),
         across(c(natenvir, nataid, natsoc, natchld,
                  natfare, natcrime, nateduc, natarms, 
                  natrace, tax),
                ~(.x-1)/2*100), 
         across(c(cappun), 
                ~(.x)/2*100),
         across(c(letin1a, wrkwayup), 
                ~(.x - 1)/4*100)) %>%
  pivot_longer(c(cappun, fair:trust, wrkwayup)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
  mutate(df = "2006-10 GSS")

# Clean GSS 2008-12 ====
gss8_long <- gss8 %>%
  mutate(id = 1:nrow(gss8)) %>%
  select(id, 
         wtpannr12, wtpannr123,
         age_1, age_2, age_3, 
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
         natcrime_1, natcrime_2, natcrime_3, 
         nateduc_1, nateduc_2, nateduc_3, 
         natarms_1, natarms_2, natarms_3, 
         natrace_1, natrace_2, natrace_3, 
         cappun_1, cappun_2, cappun_3, 
         letin1a_1, letin1a_2, letin1a_3,
         tax_1, tax_2, tax_3,
         wrkwayup_1, wrkwayup_2, wrkwayup_3) %>%
  mutate(weight_2 = as.character(wtpannr12),
         weight_3 = as.character(wtpannr123),
         age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
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
         across(c(tax_1, tax_2, tax_3), ~ifelse(.x %in% c(4), NA, .x)),
         across(partyid_1:wrkwayup_3,
                ~as.character(.x))) %>%
  select(-c(wtpannr12, wtpannr123, dateintv_1, dateintv_2, dateintv_3)) %>%
  pivot_longer(c(weight_2, weight_3, age_1:date_3)) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(weight, age, cappun, fair:wrkwayup),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         polviews = (polviews-1)/6*100,
         across(c(trust, fair,helpful),
                ~(.x-1)*100),
         across(c(natenvir, nataid,
                  natsoc, natchld, natfare,
                  natcrime, nateduc, natarms,
                  natrace, tax),
                ~(.x-1)/2*100), 
         across(c(cappun), 
                ~(.x)/2*100), 
         across(c(letin1a, wrkwayup), 
                ~(.x - 1)/4*100)) %>%
  pivot_longer(c(cappun, fair:trust, wrkwayup)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
  mutate(df = "2008-12 GSS")

# Clean GSS 2010-14 ====

gss10_long <- gss10 %>%
  mutate(id = 1:nrow(gss10)) %>%
  select(id, 
         wtpannr12, WTPANNR123,
         age_1, age_2, age_3,
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
         natcrime_1, natcrime_2, natcrime_3, 
         nateduc_1, nateduc_2, nateduc_3, 
         natarms_1, natarms_2, natarms_3, 
         natrace_1, natrace_2, natrace_3, 
         cappun_1, cappun_2, cappun_3, 
         letin1a_1, letin1a_2, letin1a_3,
         tax_1, tax_2, tax_3,
         wrkwayup_1, wrkwayup_2, wrkwayup_3) %>%
  mutate(weight_2 = as.character(wtpannr12),
         weight_3 = as.character(WTPANNR123),
         age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
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
         across(c(tax_1, tax_2, tax_3), ~ifelse(.x %in% c(4), NA, .x)),
         across(partyid_1:wrkwayup_3,
                ~as.character(.x))) %>%
  select(-c(wtpannr12, WTPANNR123, dateintv_1, dateintv_2, dateintv_3)) %>%
  pivot_longer(c(weight_2, weight_3, age_1:date_3)) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(weight, age, cappun, fair:wrkwayup),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         polviews = (polviews-1)/6*100,
         across(c(trust, fair,helpful),
                ~(.x-1)*100),
         across(c(natenvir, nataid,
                  natsoc, natchld, natfare, natcrime, 
                  nateduc, natarms, natrace,tax),
                ~(.x-1)/2*100), 
         across(c(cappun), 
                ~(.x)/2*100),
         across(c(letin1a, wrkwayup), 
                ~(.x - 1)/4*100)) %>%
  pivot_longer(c(cappun, fair:trust, wrkwayup)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
  mutate(df = "2010-14 GSS")

# Clean GSS 2016-20 ====
gss20_long <- gss20 %>%
  mutate(id = 1:nrow(gss20)) %>%
  select(id, 
         wtssnr_2,
         age_1a, age_1b, age_2,
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
         natcrime_1a, natcrime_1b, natcrime_2, 
         nateduc_1a, nateduc_1b, nateduc_2, 
         natarms_1a, natarms_1b, natarms_2, 
         natrace_1a, natrace_1b, natrace_2, 
         cappun_1a, cappun_1b, cappun_2, 
         letin1a_1a, letin1a_1b, letin1a_2,
         tax_1a, tax_1b, tax_2,
         wrkwayup_1a, wrkwayup_1b, wrkwayup_2) %>%
  mutate(weight_2 = as.character(wtssnr_2)) %>%
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
         across(partyid_1a:wrkwayup_2,
                ~as.character(.x))) %>%
  select(id, weight_2, age_1a:age_2, date_1a, date_1b, date_2, partyid_1a:wrkwayup_2) %>% 
  pivot_longer(weight_2:wrkwayup_2) %>%
  separate(name, into = c("measure", "wave")) %>%
  mutate(wave = recode(wave, "1a"="1", "1b"="2", "2"="3")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(weight, age, cappun, fair:wrkwayup),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         polviews = (polviews-1)/6*100,
         across(c(trust, fair, helpful),
                ~(.x-1)*100),
         across(c(natenvir, nataid,
                  natsoc, natchld,
                  natfare, natcrime, nateduc, 
                  natarms, natrace,
                  tax),
                ~(.x-1)/2*100), 
         across(c(cappun), 
                ~(.x)/2*100), 
         across(c(letin1a, wrkwayup), 
                ~(.x - 1)/4*100)) %>%
  pivot_longer(c(cappun, fair:trust, wrkwayup)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
  mutate(df = "2016-20 GSS")

# Clean ANES 2016-20 ====
anes16_long <- anes16 %>%
  mutate(id = 1:nrow(anes16)) %>%
  zap_labels() %>%
  select(id, V161267, V201507x,
         V160102,#weight for pre- and post 2016 
         V200011a, #weight for pre-16 and pre-20 combined
         V200011b, #weight for post-16 and post-20 combined
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
         V162110, V202171, 
         # Attention Campaigns
         V161004, V201006, 
         # People like me Likert
         V162216, V202213, 
         # Spend Poor 
         V161211, V201318,
         # Less gvt 
         V162185, V202253, 
         # Preferential hiring
         V162238, V202249,
         # Death penalty
         V161233, V201343,
         # Number of migrants
         V162157, V202232,
         # Favor Oppose Homo
         V161229, V201412,
         # T: asian-americans
         V162310, V202477,
         # T: feminists
         V162096, V202160,
         # T: illegal immigrants
         V162313, V202481,
         # T: Equality of opportunity for success
         V162243, V202260,
         # Worry less equality
         V162244, V202261,
         # Unequal no problem
         V162245, V202262,
         # Equality less problem
         V162246, V202263,
         # Moral Relativity
         V162207, V202264,
         # Traditional family
         V162210, V202265,
         # Hard work whites
         V162345, V202515,
         # Hard work blk 
         V162346, V202516,
         # Slavery difficulty
         V162212, V202301,
         # Blacks no favors
         V162211, V202300,
         # Matter of Hard work
         V162214, V202303,
         # Religious importance
         V161241, V201433,
         # Dont care (2,4; 5pt; NA < -9,-7,-6; -8=3)
         V162215, V202212,
         # elexmatter (1,2,3; NA <0)
         V161220, V201238,
         # Satisfied with democracy (1,2,4,5, recode to 1234)
         V162290, V202440
  ) %>%
  mutate(age_1 = ifelse(V161267 < 0 & V201507x > 0, V201507x, ifelse(V161267 < 0, NA, V161267)),
         age_2 = age_1,
         age_3 = ifelse(!is.na(age_1) & V201507x < 0, age_1 + 4, V201507x),
         age_4 = age_3,
         across(age_1:age_4, ~as.character(.x))) %>%
  mutate(weight_2 = as.character(V160102),
         weight_3 = as.character(V200011a), 
         weight_4 = as.character(V200011b)) %>%
  mutate(date_1 = convert_to_date(V164002),
         date_2 = convert_to_date(V165002),
         date_3 = convert_to_date(V203053),
         date_4 = convert_to_date(V203078),
         across(c(V162215, V202212,
                  V162216, V202213),
                ~recode(.x, "1"=1, "2"=1, "3"=3,
                        "4"=5, "5"=5, "-8"=3,
                        "-9"=NA_real_, "-6"=NA_real_,
                        "-4"=NA_real_, "-5"=NA_real_)),
         across(c(V162290, V202440),
                ~recode(.x, "1"=1, "2"=2, "4"=3, "5"=4,
                        "-5"=NA_real_, "-6"=NA_real_,
                        "-7"=NA_real_, "-8"=NA_real_,
                        "-9"=NA_real_)),
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
                  V162110, V202171, 
                  V162310, V202477, 
                  V162096, V202160,
                  V162313, V202481),
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
                  V161181, V201249, 
                  V161004, V201006,
                  V162157, V202232,
                  V162243, V202260,
                  V162244, V202261,
                  V162245, V202262,
                  V162246, V202263,
                  V162207, V202264,
                  V162210, V202265,
                  V162345, V202515, 
                  V162346, V202516,
                  V162212, V202301,
                  V162211, V202300, 
                  V162214, V202303),
                ~ifelse(.x < 0 | .x > 7, NA, .x)),
         across(c(V161232, V201336, 
                  V161211, V201318, 
                  V162185, V202253,
                  V162238, V202249,
                  V161233, V201343,
                  V161229, V201412,
                  V161241, V201433,
                  V161220, V201238),
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
         ftcops_4 = as.character(V202171),
         attentioncpg_1 = as.character(V161004),
         attentioncpg_3 = as.character(V201006),
         ppllikeme_2 = as.character(V162216),
         ppllikeme_4 = as.character(V202213), 
         natpoor_1 = as.character(V161211),
         natpoor_3 = as.character(V201318),
         lessgvt_2 = as.character(V162185),
         lessgvt_4 = as.character(V202253),
         prefhiring_2 = as.character(V162238), 
         prefhiring_4 = as.character(V202249),
         cappun_1 = as.character(V161233), 
         cappun_3 = as.character(V201343),
         letin1a_2 = as.character(V162157), 
         letin1a_4 = as.character(V202232), 
         opposehomo_1 = as.character(V161229), 
         opposehomo_3 = as.character(V201412),
         ftasianamericans_2 = as.character(V162310),
         ftasianamericans_4 = as.character(V202477),
         ftfeminists_2 = as.character(V162096), 
         ftfeminists_4 = as.character(V202160),
         ftillimmigrants_2 = as.character(V162313),
         ftillimmigrants_4 = as.character(V202481),
         eqoppsuccess_2 = as.character(V162243),
         eqoppsuccess_4 = as.character(V202260),
         worrylesseq_2 = as.character(V162244),
         worrylesseq_4 = as.character(V202261),
         unequalnoprob_2 = as.character(V162245),
         unequalnoprob_4 = as.character(V202262),
         equallessprob_2 = as.character(V162246),
         equallessprob_4 = as.character(V202263),
         moralrel_2 = as.character(V162207),
         moralrel_4 = as.character(V202264),
         tradfamily_2 = as.character(V162210),
         tradfamily_4 = as.character(V202265),
         hrdwrklazywhite_2 = as.character(V162345),
         hrdwrklazywhite_4 = as.character(V202515),
         hrdwrklazyblack_2 = as.character(V162346),
         hrdwrklazyblack_4 = as.character(V202516),
         slavediff_2 = as.character(V162212),
         slavediff_4 = as.character(V202301),
         wrkwayup_2 = as.character(V162211),
         wrkwayup_4 = as.character(V202300),
         matterhrdwrk_2 = as.character(V162214),
         matterhrdwrk_4 = as.character(V202303),
         # Dont care (2,4; 5pt; NA < -9,-7,-6; -8=3)
         dontcare_2 = as.character(V162215), 
         dontcare_4 = as.character(V202212),
         # elexmatter (1,2,3; NA <0)
         elexmatter_2 = as.character(V161220), 
         elexmatter_4 = as.character(V201238),
         # Satisfied with democracy (1,2,4,5, recode to 1234)
         satdemo_2 = as.character(V162290), 
         satdemo_4 = as.character(V202440)
  ) %>% 
  select(id, weight_2:weight_4, age_1:age_4, date_1:satdemo_4) %>%
  pivot_longer(weight_2:satdemo_4) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(weight, abortion:cappun, defscale:wrkwayup),
                ~as.numeric(.x)),
         across(c(partyid, polviews, jobguar,
                  govins, helpblk, spendserv,
                  defscale, hrdwrklazywhite, hrdwrklazyblack),
                ~(.x-1)/6*100),
         across(c(stayhome, runfew,
                  natenvir, natsoc, natschools, natfare,
                  natcrime, natpoor, lessgvt, prefhiring,
                  cappun, opposehomo),
                ~(.x-1)*100),
         across(c(wastetax, attentioncpg,
                  elexmatter),
                ~(.x-1)/2*100),
         across(c(abortion, satdemo),
                ~(.x-1)/3*100), 
         across(c(ppllikeme, letin1a,
                  eqoppsuccess, worrylesseq,
                  unequalnoprob, equallessprob,
                  moralrel, tradfamily,
                  slavediff, wrkwayup,
                  matterhrdwrk, 
                  dontcare), 
                ~(.x-1)/4*100)) %>%
  pivot_longer(c(abortion, attentioncpg:cappun, defscale:wastetax, worrylesseq:wrkwayup)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
  mutate(df = "2016-20 ANES")


# Clean ANES 2020-22 ====
anes20_long <- anes20 %>%
  mutate(id = 1:nrow(anes20)) %>%
  select(id, profile_age,
         weight_pre, #weight for wave 1 only
         weight_post, #weight for pre- and post-election combined
         w3weight, #weight for data including wave 3
         start, w2start, w3startdt,
         pid7x, w2pid7x, w3pid7x,
         lcself, w2lcself, w3lcself,
         hp_you, w2hp_you,
         ftdem, w2ftdem, w3ftdem,
         ftrep, w2ftrep, w3ftrep,
         w2ftgay, w3ftgay, 
         w2ftpolice, w3ftpolice,
         w2ftfeminists, 
         w3ftfeminists,
         rr2, w2rr2,
         rr1,	w2rr1) %>%
  mutate(weight_1 = as.character(weight_pre),
         weight_2 = as.character(weight_post),
         weight_3 = as.character(w3weight)) %>%
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
                  w2ftpolice, w3ftpolice, 
                  w2ftfeminists,	w3ftfeminists,
                  rr2, w2rr2, 
                  rr1,	w2rr1),
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
         ftcops_3 = as.character(w3ftpolice),
         ftfeminists_2 = as.character(w2ftfeminists),
         ftfeminists_3 = as.character(w3ftfeminists),
         slavediff_1 = as.character(rr2),
         slavediff_2 = as.character(w2rr2),
         wrkwayup_1 = as.character(rr1),
         wrkwayup_2 = as.character(w2rr1)
  ) %>%
  select(id, weight_1:weight_3, age_1:age_3, date_1:wrkwayup_2) %>%
  pivot_longer(weight_1:wrkwayup_2) %>%
  separate(name, into = c("measure", "wave")) %>%
  spread(measure, value) %>%
  mutate(date = as.Date(date),
         across(c(weight, age, ftcops:slavediff, wrkwayup),
                ~as.numeric(.x)),
         partyid = (partyid/6)*100,
         across(c(polviews, govins),
                ~(.x)/6*100),
         across(c(slavediff, 
                  wrkwayup), 
                ~(.x-1)/4*100)) %>%
  pivot_longer(c(ftcops:slavediff, wrkwayup)) %>%
  filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
  mutate(df = "2020-22 ANES")

# Old way of combining data sets
#long_data <- bind_rows(anes5_long %>% mutate(df = "1956-60 ANES"),
#                       anes7_long %>% mutate(df = "1972-76 ANES"),
#                       anes8_long %>% mutate(df = "1980 ANES"),
#                       anes90_long %>% mutate(df = "1990-92 ANES"),
#                       anes9_long %>% mutate(df = "1992-97 ANES"),
#                       anes0_long %>% mutate(df = "2000-04 ANES"),
#                       gss6_long %>% mutate(df = "2006-10 GSS"),
#                       gss8_long %>% mutate(df = "2008-12 GSS"),
#                       gss10_long %>% mutate(df = "2010-14 GSS"),
#                       gss20_long %>% mutate(df = "2016-20 GSS"),
#                       anes16_long %>% mutate(df = "2016-20 ANES"),
#                       anes20_long %>% mutate(df = "2020-22 ANES")) %>%
#  ungroup() %>%
#  mutate(dec_diff = (as.numeric(difftime(date, min(date), units = "days")))/3652.5,
#         d = as.numeric(d)) %>%

#Combine the data sets to a long "raw score" 
#data frame first
long_scores <- bind_rows(anes5_long, anes7_long, anes8_long, 
                         anes90_long, anes9_long, anes0_long,
                         gss6_long, gss8_long, gss10_long,
                         gss20_long, anes16_long, anes20_long) %>%
  group_by(id, name, df) %>% 
  mutate(nobs = n()) %>% filter(nobs > 1) %>% ungroup()

# Need Cleaning
# 1990-92 ANES matterhrdwrk

#Then combine them to calculate difference scores
#This calculation takes a very long time, I'm
#sure there's some way to speed it up. But I think it's
#better having this code be parsimonious/not replicating
#with each panel.

long_difference <- long_scores %>%
  mutate(
    i1_value = lead(id),
    i2_value = lead(id,2),
    i3_value = lead(id,3),
    i4_value = lead(id,4),
    i5_value = lead(id,5),
    i6_value = lead(id,6),
    
    n1_value = lead(name),
    n2_value = lead(name,2),
    n3_value = lead(name,3),
    n4_value = lead(name,4),
    n5_value = lead(name,5),
    n6_value = lead(name,6),
    
    #Get weight from second observation
    w1_value = lead(weight),
    w2_value = lead(weight,2),
    w3_value = lead(weight,3),
    w4_value = lead(weight,4),
    w5_value = lead(weight,5),
    w6_value = lead(weight,6),
    #Note second wave number
    t1_value = lead(wave),
    t2_value = lead(wave,2),
    t3_value = lead(wave,3),
    t4_value = lead(wave,4),
    t5_value = lead(wave,5),
    t6_value = lead(wave,6),
    #Calculate duration between waves
    d1_value = lead(date) - date,
    d2_value = lead(date,2) - date,
    d3_value = lead(date,3) - date,
    d4_value = lead(date,4) - date,
    d5_value = lead(date,5) - date,
    d6_value = lead(date,6) - date,
    #Calculate absolute difference between waves
    a1_value = abs(value - lead(value)),
    a2_value = abs(value - lead(value,2)),
    a3_value = abs(value - lead(value,3)),
    a4_value = abs(value - lead(value,4)),
    a5_value = abs(value - lead(value,5)),
    a6_value = abs(value - lead(value,6))) %>% 
  pivot_longer(
    cols = c(i1_value, i2_value, i3_value, i4_value, i5_value, i6_value,
             n1_value, n2_value, n3_value, n4_value, n5_value, n6_value,
             t1_value, t2_value, t3_value, t4_value, t5_value, t6_value,
             w1_value, w2_value, w3_value, w4_value, w5_value, w6_value,
             d1_value, d2_value, d3_value, d4_value, d5_value, d6_value,
             a1_value, a2_value, a3_value, a4_value, a5_value, a6_value),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)_value") %>% 
  filter(!is.na(a)) %>%
  mutate(d = d/365.25) %>%
  filter(id == i, name == n) %>% 
  select(-c(i,n)) %>%
  mutate(t1 = wave, t2 = t, duration = d, abs_diff = a, weight = w) %>%
  select(df, id, weight, name, t1, t2, date, age, duration, abs_diff, value) %>%
  mutate(age_group = ifelse(age < 26, "18-25", "34-65"),
         age_group = ifelse(age > 25 & age < 34, "26-33", age_group),
         age_group = ifelse(age > 65, "65+", age_group))


# Save dataset ====
#save(long_data, file = "~/Dropbox/extended_formative_windows/clean_data/long_data.Rdata")
save(long_difference, file = "~/Dropbox/extended_formative_windows/clean_data/long_difference.Rdata")


# cappun has a midpoint in the 90-92 and 92-97 scales, but not in the GSS panels
# and then has it in the 16-20 ANEs
