###############################################
#### Bringing in the demographic variables ####
###############################################

# Packages ====
library(tidyverse)
library(haven)

# Nico Read in the data ====
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


# ANES 1950 demographics ====
anes5_demog <- anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(
    id,
    #Marital status
    V560177,
    V580474,
    V600690,
    #Education
    V560181,
    V580478,
    V600694,
    #Children in household
    V560178,
    V580475,
    V600691,
    #sex
    V560171,
    V580468,
    V600684,
    #race
    V560172,
    V580469,
    V600685,
    #student status 
    V560120
  ) %>%
  mutate(
    marital_1 = V560177,
    marital_2 = V580474,
    marital_3 = V600690,
    marital_1 = ifelse(V560177 == 9, NA, V560177),
    marital_1 = ifelse(V560177 %in% c(2), "single/nm", marital_1),
    marital_1 = ifelse(V560177 %in% c(1), "married", marital_1),
    marital_1 = ifelse(V560177 %in% c(3, 4, 5, 7), "other", marital_1),
    marital_2 = ifelse(V580474 == 9, NA, V580474),
    marital_2 = ifelse(V580474 %in% c(2), "single/nm", marital_2),
    marital_2 = ifelse(V580474 %in% c(1), "married", marital_2),
    marital_2 = ifelse(V580474 %in% c(3, 4, 5), "other", marital_2),
    marital_3 = ifelse(V600690 == 9, NA, V600690),
    marital_3 = ifelse(V600690 %in% c(2), "single/nm", marital_3),
    marital_3 = ifelse(V600690 %in% c(1), "married", marital_3),
    marital_3 = ifelse(V600690 %in% c(3, 4, 5), "other", marital_3),
    #IS there a child under 18 in the household.
    anychild_1 = as.character(ifelse(V560178 == 99, NA, V560178)),
    anychild_1 = as.character(ifelse(V560178 == 0, 0, 1)),
    anychild_2 = as.character(ifelse(V580475 == 99, NA, V580475)),
    anychild_2 = as.character(ifelse(V580475 == 0, 0, 1)),
    anychild_3 = as.character(ifelse(V600691 == 99, NA, V600691)),
    anychild_3 = as.character(ifelse(V600691 == 0, 0, 1)),
    #The third education variable is differently coded
    ed_1 = ifelse(V560181 == 9, NA, V560181),
    ed_1 = ifelse(V560181 %in% c(0, 1, 2, 3, 4), "less than", ed_1),
    ed_1 = ifelse(V560181 %in% c(5, 6, 7), "hs", ed_1),
    ed_1 = ifelse(V560181 %in% c(8), "ba", ed_1),
    ed_2 = ifelse(V580478 == 9, NA, V580478),
    ed_2 = ifelse(V580478 %in% c(0, 1, 2, 3, 4), "less than", ed_2),
    ed_2 = ifelse(V580478 %in% c(5, 6, 7), "hs", ed_2),
    ed_2 = ifelse(V580478 %in% c(8), "ba", ed_2),
    ed_3 = ifelse(V600694 %in% c(98, 99), NA, V600694),
    ed_3 = ifelse(
      V600694 %in% c(0, 11, 12, 13, 14, 15, 16, 17, 21, 31, 32, 33, 41, 42, 43),
      "less than",
      ed_3
    ),
    ed_3 = ifelse(V600694 %in% c(51, 61, 71), "hs", ed_3),
    ed_3 = ifelse(V600694 %in% c(81, 82), "ba", ed_3),
    #1 = male, 2 = female
    sex_1 = as.character(V560171),
    sex_2 = as.character(V580468),
    sex_3 = as.character(V600684),
    #1 = white, 2 = negro, 3 = other.
    race_1 = as.character(V560172),
    race_2 = as.character(V580469),
    race_3 = as.character(V600685),
    # Student status 
    student_1 = as.character(ifelse(V560120 %in% c(92), 1, 0))
  )  %>% 
  select(
    id, marital_1:student_1
  ) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(marital_1:student_1) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

# ANES 1970 demographics ====
anes7_demog <- anes7 %>% 
  mutate(id = 1:nrow(.), 
         across(c(V742410:V742417,V763373:V763378), ~ifelse(.x %in% c(0,98,99), NA, .x)),
         oldestkid_3 = pmax(V742410,V742411,V742412,
                            V742413,V742414,V742415,
                            V742416,V742417, na.rm = TRUE),
         oldestkid_4 = pmax(V763373,V763374,V763375,
                            V763376,V763377,V763378, na.rm = TRUE), 
         #resp's education
         ed_1 = ifelse(V720300 %in% c(98,99), NA, V720300),
         ed_1 = ifelse(V720300 %in% c(11,12,13,14,15,16,17,18,21,22,31,32,33,
                                      41,42,43), "less than", ed_1),
         ed_1 = ifelse(V720300 %in% c(51,61,71,50), "hs", ed_1),
         ed_1 = ifelse(V720300 %in% c(81,82,83,84,85,86), "ba", ed_1),
         ed_2 = ed_1,
         ed_3 = ifelse(V742423 %in% c(98,99), NA, V742423),
         ed_3 = ifelse(V742423 %in% c(0,1,2,3,4), "less than", ed_3),
         ed_3 = ifelse(V742423 %in% c(5,6,7,8), "hs", ed_3),
         ed_3 = ifelse(V742423 %in% c(9,10), "ba", ed_3),
         ed_4 = ifelse(V763389 %in% c(98, 99), NA, V763389),
         ed_4 = ifelse(V763389 %in% c(0,1,2,3,4), "less than", ed_4),
         ed_4 = ifelse(V763389 %in% c(5,6,7,8), "hs", ed_4),
         ed_4 = ifelse(V763389 %in% c(9,10), "ba", ed_4),
         ed_5 = ed_4,
         #marital status
         marital_1 = ifelse(V720295 == 9, NA, V720295),
         marital_1 = ifelse(V720295 %in% c(2), "single/nm", marital_1),
         marital_1 = ifelse(V720295 %in% c(1,7), "married", marital_1),
         marital_1 = ifelse(V720295 %in% c(3,4,5), "other", marital_1),
         marital_2 = marital_1,
         marital_3 = ifelse(V742407 == 9, NA, V742407),
         marital_3 = ifelse(V742407 %in% c(2), "single/nm", marital_3),
         marital_3 = ifelse(V742407 %in% c(1,7), "married", marital_3),
         marital_3 = ifelse(V742407 %in% c(3,4,5), "other", marital_3),
         marital_4 = ifelse(V763370 == 9, NA, V763370),
         marital_4 = ifelse(V763370 %in% c(2), "single/nm", marital_4),
         marital_4 = ifelse(V763370 %in% c(1,7), "married", marital_4),
         marital_4 = ifelse(V763370 %in% c(3,4,5), "other", marital_4),
         marital_5 = marital_4,
         #children
         #Indicator for whether a respondent has any children
         anychild_3 = ifelse(V742408 %in% c(0, 9), NA, V742408),
         anychild_3 = ifelse(V742408 == 1, 1, anychild_3),
         anychild_3 = ifelse(V742408 == 5, 0, anychild_3),
         anychild_4 = ifelse(V763371 %in% c(0, 9), NA, V763371),
         anychild_4 = ifelse(V763371 == 1, 1, anychild_4),
         anychild_4 = ifelse(V763371 == 5, 0, anychild_4),
         
         ##
         anychild_1 = ifelse(anychild_3 == 0 | oldestkid_3 < 2, 0, 1),
         anychild_1a = ifelse(anychild_4 == 0 | oldestkid_4 < 4, 0, 1),
         anychild_1 = ifelse(is.na(anychild_1), anychild_1a, anychild_1), 
         
         anychild_2 = anychild_1,
         anychild_5 = anychild_4,
         
         # Sex
         sex_1 = ifelse(V720424 == 9, NA, V720424), 
         sex_3 = ifelse(V742553 == 9, NA, V742553), 
         sex_4 = ifelse(V763512 == 9, NA, V763512),
         #Race 
         race_1 = case_when(
           V720425 == 1 ~ 1, 
           V720425 == 2 ~ 2, 
           V720425 %in% c(3,4,5,6,7) ~ 3, 
           V720425 == 9 ~ NA_real_), 
         race_3 = case_when(
           V742554 == 1 ~ 1, 
           V742554 == 2 ~ 2, 
           V742554 %in% c(3,4,5,6,7) ~ 3, 
           V742554 == 9 ~ NA_real_), 
         race_4 = case_when(
           V763513 == 1 ~ 1, 
           V763513 == 2 ~ 2, 
           V763513 %in% c(3,4,5,6,7) ~ 3, 
           V763513 == 9 ~ NA_real_), 
         student_1 = case_when(
           V720306 == 8 ~ 1,
           V720306 %in% c(1,2,3,4,5,6,7) ~ 0,
           V720306 == 0 ~ NA_real_),
         student_3 = case_when(
           V742443 == 8 ~ 1,
           V742443 %in% c(1,2,3,4,5,6,7) ~ 0,
           V742443 == 0 ~ NA_real_),
         student_4 = case_when(
           V763409 == 8 ~ 1,
           V763409 %in% c(1,2,3,4,5,6,7) ~ 0,
           V763409 == 0 ~ NA_real_),
         ) %>% 
  select(id, oldestkid_3:student_4) %>% 
  select(-c(anychild_1a)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(oldestkid_3:student_4) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  ) %>%
  arrange(id, wave)

# ANES 1980 demographics ====

anes8_demographic <- anes8 %>% 
  mutate(id = 1:nrow(.), 
         # Marital [1: married, 2: never married, 3: divorced, 4: separated, 5: widowed, 7: living together, 9: NA]
         marital_1 = case_when(
           VMP0326 %in% c(1,7) ~ "married", 
           VMP0326 == 2 ~ "single/nm", 
           VMP0326 %in% c(3,4,5) ~ "other", 
           VMP0326 == 9 ~ NA_character_
         ), 
         marital_2 = case_when(
           VMP2377 %in% c(1,7) ~ "married", 
           VMP2377 == 2 ~ "single/nm", 
           VMP2377 %in% c(3,4,5) ~ "other", 
           VMP2377 == 9 ~ NA_character_
         ), 
         marital_3 = case_when(
           VMP3387 %in% c(1,7) ~ "married", 
           VMP3387 == 2 ~ "single/nm", 
           VMP3387 %in% c(3,4,5) ~ "other", 
           VMP3387 == 9 ~ NA_character_
         ), 
         # Education Summary 
         educ_1 = case_when(
           VMP0334 %in% c(1,2,3,4) ~ "less than",
           VMP0334 %in% c(5,6,7,8) ~ "hs", 
           VMP0334 %in% c(9,10) ~ "ba", 
           VMP0334 %in% c(98,99) ~ NA_character_
         ), 
         # Children -- sum total amount of children 
         across(c(VMP0589,VMP0590,VMP0591,VMP0592,
                  VMP2435,VMP2436,VMP2437,VMP2438,
                  VMP3581,VMP3582,VMP3583,VMP3583), 
                ~ifelse(.x == 9, NA_real_, .x)),
         
         nkids_1 = VMP0589 + VMP0590 + VMP0591 + VMP0592, 
         nkids_2 = VMP2435 + VMP2436 + VMP2437 + VMP2438, 
         nkids_3 = VMP3581 + VMP3582 + VMP3583 + VMP3583, 
         anychild_1 = case_when(
           VMP3388 == 1 ~ 1, 
           VMP3388 == 5 ~ 0, 
           VMP3388 %in% c(0,9) ~ NA_real_
         ),
         anychild_2 = ifelse(nkids2 > 0 | anychild_1==1,1,0),
         # Sex 
         sex_1 = ifelse(VMP3552 == 9, NA, VMP3552), 
         sex_3 = ifelse(VMP4097 == 9, NA, VMP4097),
         # Race 
         race_1 = case_when(
           VMP0558 == 1 ~ 1, 
           VMP0558 == 2 ~ 2, 
           VMP0558 %in% c(3,4,7) ~ 3, 
           VMP0558 == 9 ~ NA_real_
         ), 
         race_2 = case_when(
           VMP2407 == 1 ~ 1, 
           VMP2407 == 2 ~ 2, 
           VMP2407 %in% c(3,4,7) ~ 3, 
           VMP2407 == 9 ~ NA_real_
         ), 
         race_3 = case_when(
           VMP3553 == 1 ~ 1, 
           VMP3553 == 2 ~ 2, 
           VMP3553 %in% c(3,4,7) ~ 3, 
           VMP3553 == 9 ~ NA_real_
         ), 
         student_1 = ifelse(VMP0352 == 8, 1, 0)
  ) %>% 
  select(id, marital_1:student_1) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(marital_1:student_1) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

# ANES 1990-92 demographics ====

anes90_demog <- anes90 %>% 
  mutate(
    id = 1:nrow(.), 
    marital_1 = case_when(
      V900553 %in% c(1) ~ "married", 
      V900553 == 2 ~ "single/nm", 
      V900553 %in% c(3,4,5,7) ~ "other", 
      V900553 %in% c(8,9) ~ NA_character_
    ),
    marital_2 = marital_1,
    marital_3 = case_when(
      V923904 %in% c(1) ~ "married", 
      V923904 == 2 ~ "single/nm", 
      V923904 %in% c(3,4,5,7) ~ "other", 
      V923904 %in% c(0,8,9) ~ NA_character_
    ), 
    marital_4 = marital_3,
    # Education 
    educ_1 = case_when(
      V900557 %in% c(1,2) ~ "less than",
      V900557 %in% c(3,4,5) ~ "hs", 
      V900557 %in% c(6,7) ~ "ba", 
      V900557 %in% c(98,99) ~ NA_character_
    ), 
    educ_2 = educ_1,
    educ_3 = case_when(
      V923908 %in% c(1,2) ~ "less than",
      V923908 %in% c(3,4,5) ~ "hs", 
      V923908 %in% c(6,7) ~ "ba", 
      V923908 %in% c(98,99) ~ NA_character_
    ), 
    educ_4 = educ_3,
    # Children 
    across(c(V900030,V900031,V900032,V900033), 
           ~ifelse(.x == 9, NA_real_, .x)), 
    nkids_1 = V900030 + V900031 + V900032 +V900033, 
    anychild_1 = ifelse(nkids_1 > 0, 1, 0),
    anychild_2 = anychild_1,
    anychild_3 = case_when(
      V924136 %in% c(1,2) ~ 1, 
      V924136 == 5 ~ 0, 
      V924136 %in% c(0,9) ~ NA_real_
    ), 
    anychild_4 = anychild_3,
    across(c(V924137,V924138,V924139,V924140), 
           ~ifelse(.x %in% c(8,9), NA_real_, .x)), 
    childlt_3 = V924137+V924138+V924139+V924140, 
    # Sex
    sex_1 = V900547,
    sex_2 = sex_1,
    sex_3 = ifelse(V924201 %in% c(0,9), NA, V924201),
    sex_4 = sex_3,
    # Race 
    race_1 = case_when(
      V900549 == 1 ~ 1, 
      V900549 == 2 ~ 2, 
      V900549 %in% c(3,4) ~ 3, 
      V900549 == 9 ~ NA_real_), 
    race_2 = race_1,
    race_3 = case_when(
      V924202 == 1 ~ 1, 
      V924202 == 2 ~ 2, 
      V924202 %in% c(3,4) ~ 3, 
      V924202 %in% c(0,9) ~ NA_real_), 
    race_4 = race_3,
    student_1 = case_when(
      V900565 %in% c(80, 81) ~ 1,
      V900565 %in% c(10,15,16,17,18,20,
                     40,50,51,60,61,70,
                     71,75) ~ 0,
      V900565 == 99 ~ NA_real_),
    student_2 = student_1,
    student_3 = case_when(
      V923914 == 0 ~ NA_real_,
      V923914 %in% c(10,15,16,17,18,20,
                     40,50,51,60,61,70,
                     71,75) ~ 0,
      V923914 %in% c(80, 81) ~ 1,
      V923914 == 99 ~ NA_real_),
    student_4 = student_3
    ) %>% 
  select(id, marital_1:student_4, -c(nkids_1, childlt_3)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(marital_1:student_4) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

# ANES 1992-1997 demographics ====

anes9_demog <- anes9 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, 
         #education
         V923908,V941209,V960610,
         #marital status
         V923904,V941204,V960606,
         #children
         V924136,
         V923079:V923082,
         V960048:V960051,
         V941428, 
         # Sex 
         V924201, 
         V941434,
         V960066,
         # Race
         V924202, 
         V941435,
         V960067, 
         # Student status 
         V923914,
         V941215,
         V960615
  ) %>% 
  mutate(
    marital_1 = ifelse(V923904 %in% c(9,0), NA, V923904),
    marital_1 = ifelse(V923904 %in% c(1,7), "married", marital_1),
    marital_1 = ifelse(V923904 %in% c(2), "single/nm", marital_1),
    marital_1 = ifelse(V923904 %in% c(3,4,5,8), "other", marital_1),
    marital_2 = marital_1,
    marital_3 = marital_1,
    marital_4 = ifelse(V941204 %in% c(9,0,8), NA, V941204),
    marital_4 = ifelse(V941204 %in% c(1,7), "married", marital_4),
    marital_4 = ifelse(V941204 %in% c(2), "single/nm", marital_4),
    marital_4 = ifelse(V941204 %in% c(3,4,5), "other", marital_4),
    marital_5 = marital_4,
    marital_6 = ifelse(V960606 %in% c(9,0,8), NA, V960606),
    marital_6 = ifelse(V960606 %in% c(1,6), "married", marital_6),
    marital_6 = ifelse(V960606 %in% c(5), "single/nm", marital_6),
    marital_6 = ifelse(V960606 %in% c(2,3,4), "other", marital_6),
    marital_7 = marital_6,
    marital_8 = marital_7,
    #children
    anychild_1 = case_when(
      V924136 %in% c(1,2) ~ 1,
      V924136 == 5 ~ 0,
      V924136 %in% c(0,9) ~ NA_real_
    ),
    anychild_2 = anychild_1,
    anychild_3 = anychild_2,
    #any children?
    anychild_4 = case_when(
      V941428 %in% c(8,9) ~ NA,
      V941428 %in% c(1,2) ~ 1,
      V941428 == 5 ~ 0
    ),
    anychild_5 = anychild_4,
    across(c(V960048, V960049, V960050, V960051),
           ~ifelse(.x > 5, 0, .x)),
    nkids_6 = V960048 + V960049 + V960050 + V960051,
    anychild_6 = ifelse(nkids_6 > 0, 1, 0),
    anychild_7 = anychild_6,
    anychild_8 = anychild_7,
    #education,
    ed_1 = ifelse(V923908 %in% c(98,99,0), NA, V923908),
    ed_1 = ifelse(V923908 %in% c(1,2), "less than", ed_1),
    ed_1 = ifelse(V923908 %in% c(3,4,5), "hs", ed_1),
    ed_1 = ifelse(V923908 %in% c(6,7), "ba", ed_1),
    ed_2 = ed_1,
    ed_3 = ed_2,
    ed_4 = ifelse(V941209 %in% c(98,99,0), NA, V941209),
    ed_4 = ifelse(V941209 %in% c(1,2), "less than", ed_4),
    ed_4 = ifelse(V941209 %in% c(3,4,5), "hs", ed_4),
    ed_4 = ifelse(V941209 %in% c(6,7), "ba", ed_4) ,
    ed_5 = ed_4,
    ed_6 = ifelse(V960610 %in% c(8,9,0), NA, V960610),
    ed_6 = ifelse(V960610 %in% c(1,2), "less than", ed_6),
    ed_6 = ifelse(V960610 %in% c(3,4,5), "hs", ed_6),
    ed_6 = ifelse(V960610 %in% c(6,7), "ba", ed_6), 
    ed_7 = ed_6,
    ed_8 = ed_7,
    #Sex 
    sex_1 = ifelse(V924201 == 9, NA, V924201),
    sex_2 = sex_1,
    sex_3 = sex_2,
    sex_4 = ifelse(V941434 == 9, NA, V941434),
    sex_5 = sex_4,
    sex_6 = V960066,
    sex_7 = sex_6,
    sex_8 = sex_7,
    #Race 
    race_1 = case_when(
      V924202 == 1 ~ 1, 
      V924202 == 2 ~ 2, 
      V924202 %in% c(3,4,7) ~ 3, 
      V924202 == 9 ~ NA_real_
    ), 
    race_2 = race_1,
    race_3 = race_2,
    race_4 = case_when(
      V941435 == 1 ~ 1, 
      V941435 == 2 ~ 2, 
      V941435 %in% c(3,4,7) ~ 3, 
      V941435 == 9 ~ NA_real_
    ), 
    race_5 = race_4,
    race_6 = case_when(
      V960067 == 1 ~ 1, 
      V960067 == 2 ~ 2, 
      V960067 %in% c(3,4,7) ~ 3, 
      V960067 == 9 ~ NA_real_
    ), 
    race_7 = race_6,
    race_8 = race_7,
    # Student status 
    student_1 = case_when(
      V923914 %in% c(18,75,80,91) ~ 1,
      V923914 == 99 ~ NA_real_,
      V923914 %in% c(10,15,16,17,20,
                     40,50,51,60,61,70,
                     71,75,80,81) ~ 0),
    student_2 = student_1,
    student_3 = student_2,
    student_4 = case_when(
      V941215 %in% c(18,75,80,91) ~ 1,
      V923914 == 99 ~ NA_real_,
      V923914 %in% c(10,15,16,17,20,
                     40,50,51,60,61,70,
                     71,75,80,81) ~ 0),
    student_5 = student_4,
    student_6 = case_when(
      V960615 %in% c(18,75,80,91) ~ 1,
      V960615 == 99 ~ NA_real_,
      V960615 %in% c(10,15,16,17,20,
                     40,50,51,60,61,70,
                     71,75,80,81) ~ 0),
    student_7 = student_6,
    student_8 = student_7
  ) %>%
  select(id, marital_1:student_8, -c(nkids_6)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(marital_1:student_8) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

# ANES 2000 demographics ====

anes0_demog <- anes0 %>% 
  mutate(
    id = 1:nrow(.), 
    # Marriage
    marital_1 = case_when(
      M000909 == 1 ~ "married", 
      M000909 == 5 ~ "single/nm", 
      M000909 %in% c(2,3,4,6) ~ "other", 
      M000909 %in% c(8,9) ~ NA_character_
    ), 
    marital_3 = case_when(
      M023127A == 1 ~ "married", 
      M023127A == 5 ~ "single/nm", 
      M023127A %in% c(2,3,4,6) ~ "other", 
      M023127A %in% c(8,9) ~ NA_character_
    ), 
    marital_5 = case_when(
      M045176 == 1 ~ "married", 
      M045176 == 5 ~ "single/nm", 
      M045176 %in% c(2,3,4,6) ~ "other", 
      M045176 %in% c(8,9) ~ NA_character_
    ), 
    # Children 
    anychild_1 = case_when(
      M001023 %in% c(1,3) ~ 1, 
      M001023 == 5 ~ 0, 
      M001023 %in% c(0,8,9) ~ NA_real_
    ), 
    nkids_1 = ifelse(
      M001024 > 15, NA_real_, 
      M001024
    ), 
    # Education 
    educ_2 = case_when(
      M023131 %in% c(1,2) ~ "less than", 
      M023131 %in% c(3,4,5) ~ "hs", 
      M023131 %in% c(6,7) ~ "ba", 
      M023131 %in% c(0,9) ~ NA_character_
    ), 
    # Sex 
    sex_2 = case_when(
      M023153 == 1 ~ 1, 
      M023153 == 2 ~ 2, 
      M023153 %in% c(0,9) ~ NA_real_
    ), 
    # Race 
    race_3 = case_when(
      M045185x == 50 ~ 1, 
      M045185x == 10 ~ 2, 
      M045185x %in% c(12,13,14,15,20,23,
                      24,25,30,34,35,40,
                      45,70) ~ 3, 
      M045185x %in% c(80, 88, 89, 0) ~ NA_real_
    ), 
    # student status 
    student_1 = ifelse(M000919 %in% c(18, 75, 80, 81), 1, 0)
  ) %>% 
  select(id, marital_1:student_1) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(marital_1:student_1) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )


# ANES 2016 demographics ====
anes16_demog <- anes16 %>%
  mutate(
    id = 1:nrow(.),
    # Marriage
    marital_1 = case_when(
      V165739 < 1 ~ NA_character_,
      V165739 == 1 ~ "married",
      V165739 == 6 ~ "single/nm",
      V165739 %in% c(3, 4, 5) ~ "other"
    ),
    marital_2 = case_when(
      V201508 < 1 ~ NA_character_,
      V201508 %in% c(1, 2) ~ "married",
      V201508 == 6 ~ "single/nm",
      V201508 %in% c(3, 4, 5) ~ "other"
    ),
    # Children
    childlt_1 = ifelse(V161324 < 0, NA_real_, V161324),
    childlt_2 = ifelse(V201567 < 0, NA_real_, V201567),
    anychild_1 = case_when(V165506 > 1 ~ NA_real_, V165506 == 1 ~ 1, V165506 == 2 ~ 0), 
    # Education 
    educ_2 = case_when(
      V201511x < 1 ~ NA_character_,
      V201511x == 1 ~ "less than", 
      V201511x %in% c(2,3) ~ "hs", 
      V201511x %in% c(4,5) ~ "ba"
    ), 
    # Sex
    sex_2 = case_when(
      V201600 < 1 ~ NA_real_, 
      V201600 == 1 ~ 1, 
      V201600 == 2 ~ 2
    ), 
    # Race
    race_1 = case_when(
      V161310x < 1 ~ NA_real_,
      V161310x == 1 ~ 1, 
      V161310x == 2 ~ 2, 
      V161310x %in% c(3,4,5,6) ~ 3
    ), 
    race_2 = case_when(
      V201549x < 1 ~ NA_real_,
      V201549x == 1 ~ 1, 
      V201549x == 2 ~ 2, 
      V201549x %in% c(3,4,5,6) ~ 3
    ), 
    student_1 = ifelse(
      V161275x %in% c(18, 80, 81), 
      1, 
      0
    ), 
    student_2 = ifelse(
      V201533x %in% c(18, 80, 81), 
      1, 
      0
    )
  ) %>% 
  select(id, marital_1:student_2) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(marital_1:student_2) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

# ANES 2020 demographics ====

# Here, we only have info for one time 
# for all demographic variables
# We also don't have information about number of children
# We don't have information here about current student (employment) status

anes20_demog <- anes20 %>% 
  mutate(
    id = 1:nrow(.), 
    # Marriage
    marital = case_when(
      profile_marital == 1 ~ "married", 
      profile_marital == 5 ~ "single/nm", 
      profile_marital %in% c(2,3,4,6) ~ "other"
    ), 
    # It seems we don't have information about children
    # Education
    educ = case_when(
      profile_educ5 == 1 ~ "less than", 
      profile_educ5 %in% c(2,3) ~ "hs", 
      profile_educ5 %in% c(4,5) ~ "ba"
    ), 
    # Sex 
    sex = profile_gender, 
    # Race
    race = case_when(
      profile_racethnicity == 1 ~ 1, 
      profile_racethnicity == 2 ~ 2, 
      profile_racethnicity %in% c(3,4) ~ 3
    )
  ) %>% 
  select(id, marital:race) %>% 
  mutate_all(as.character)

# GSS 2006-2010 demographics ====

gss6_demog <- gss6 %>%
  zap_labels() %>%
  mutate(id = 1:nrow(.)) %>% 
  select(id,
         age_1, age_2, age_3, 
         marital_1, marital_2, marital_3,
         childs_1, childs_2, childs_3,
         degree_1, degree_2, degree_3, 
         sex_1, sex_2, sex_3, 
         race_1, race_2, race_3, 
         wrkstat_1
  ) %>%
  mutate(age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
         age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3 - 4, age_1)) %>%
  mutate(marital_1 = recode(marital_1, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         marital_2 = recode(marital_2, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         marital_3 = recode(marital_3, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         anychild_1 = ifelse(childs_1 > 0, 1, 0),
         anychild_2 = ifelse(childs_2 > 0, 1, 0),
         anychild_3 = ifelse(childs_3 > 0, 1, 0),
         ed_1 = recode(degree_1, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"),
         ed_2 = recode(degree_2, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"),
         ed_3 = recode(degree_3, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"), 
         student_1 = ifelse(wrkstat_1 %in% c(6), 1, 0)
  ) %>% 
  select(id, sex_1:race_3, marital_1:student_1) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(sex_1:student_1) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )
# I am worried that some of the education statuses seem to be changing quite wildly 

# GSS 2008-2012 demographics ====
gss8_demog <- gss8 %>%
  zap_labels() %>%
  mutate(id = 1:nrow(.)) %>% 
  select(id, 
         age_1, age_2, age_3, 
         marital_1, marital_2, marital_3,
         childs_1, childs_2, childs_3,
         degree_1, degree_2, degree_3,
         sex_1, sex_2, sex_3, 
         race_1, race_2, race_3, 
         wrkstat_1) %>%
  mutate(age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
         age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3 - 4, age_1)) %>%
  mutate(marital_1 = recode(marital_1, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         marital_2 = recode(marital_2, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         marital_3 = recode(marital_3, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         anychild_1 = ifelse(childs_1 > 0, 1, 0),
         anychild_2 = ifelse(childs_2 > 0, 1, 0),
         anychild_3 = ifelse(childs_3 > 0, 1, 0),
         ed_1 = recode(degree_1, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"),
         ed_2 = recode(degree_2, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"),
         ed_3 = recode(degree_3, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"), 
         student_1 = ifelse(wrkstat_1 %in% c(6), 1, 0)) %>% 
  select(id, sex_1:race_3, marital_1:student_1) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(sex_1:student_1) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

# Again some inconsistency with the education values here 

# GSS 2010-2014 demographics ====

gss10_demog <- gss10 %>%
  zap_labels() %>%
  mutate(id = 1:nrow(.)) %>% 
  select(id, age_1, age_2, age_3, 
         marital_1, marital_2, marital_3,
         childs_1, childs_2, childs_3,
         degree_1, degree_2, degree_3, 
         sex_1, sex_2, sex_3, 
         race_1, race_2, race_3, 
         wrkstat_1) %>%
  mutate(age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
         age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3 - 4, age_1)) %>%
  mutate(marital_1 = recode(marital_1, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         marital_2 = recode(marital_2, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         marital_3 = recode(marital_3, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         childs_1 = ifelse(childs_1 > 0, 1, 0),
         childs_2 = ifelse(childs_2 > 0, 1, 0),
         childs_3 = ifelse(childs_3 > 0, 1, 0),
         ed_1 = recode(degree_1, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"),
         ed_2 = recode(degree_2, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"),
         ed_3 = recode(degree_3, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"), 
         student_1 = ifelse(wrkstat_1 %in% c(6), 1, 0)) %>% 
  select(id, sex_1:race_3, marital_1:student_1) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(sex_1:student_1) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

# GSS 2016-2020 demographics ====

gss20_demog <- gss20 %>%
  zap_labels() %>%
  mutate(id = 1:nrow(.)) %>% 
  select(id, age_1a, age_1b, age_2, 
         marital_1a, marital_1b, marital_2,
         childs_1a, childs_1b, childs_2,
         degree_1a, degree_1b, degree_2, 
         sex_1a, sex_1b, sex_2, 
         race_1a, race_1b, race_2, 
         wrkstat_1a, wrkstat_1b, wrkstat_2) %>% 
  rename(age_3 = age_2, 
         age_2 = age_1b, 
         age_1 = age_1a, 
         
         marital_3 = marital_2, 
         marital_2 = marital_1b, 
         marital_1 = marital_1a,
         
         childs_3 = childs_2, 
         childs_2 = childs_1b, 
         childs_1 = childs_1a, 
         
         degree_3 = degree_2, 
         degree_2 = degree_1b, 
         degree_1 = degree_1a,
         
         sex_3 = sex_2, 
         sex_2 = sex_1b, 
         sex_1 = sex_1a, 
         
         race_3 = race_2, 
         race_2 = race_1b, 
         race_1 = race_1a, 
         
         student_3 = wrkstat_2, 
         student_2 = wrkstat_1b, 
         student_1 = wrkstat_1a) %>% 
  mutate(age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
         age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3 - 4, age_1)) %>%
  mutate(marital_1 = recode(marital_1, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         marital_2 = recode(marital_2, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         marital_3 = recode(marital_3, "1"="married", "2"="other", "3"="other", "4"="other",
                            "5"="single/nm"),
         childs_1 = ifelse(childs_1 > 0, 1, 0),
         childs_2 = ifelse(childs_2 > 0, 1, 0),
         childs_3 = ifelse(childs_3 > 0, 1, 0),
         ed_1 = recode(degree_1, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"),
         ed_2 = recode(degree_2, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"),
         ed_3 = recode(degree_3, "0"="less than", "1"="hs", "2"="hs",
                       "3"="ba", "4"="ba"), 
         student_1 = ifelse(student_1 %in% c(6), 1, 0), 
         student_2 = ifelse(student_2 %in% c(6), 1, 0), 
         student_3 = ifelse(student_3 %in% c(6), 1, 0)) %>% 
  select(id, sex_1:race_3, marital_1:student_3) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(sex_1:student_3) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )
