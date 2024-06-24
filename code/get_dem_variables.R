###############################################
#### Bringing in the demographic variables ####
###############################################

# Packages ====
library(tidyverse)
library(haven)

# Read in the data ====
anes5 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_1956to1960.dta")
anes7 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_1972to1976.dta")
anes8 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes1980.dta")
anes90 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_1990to1992.dta")
anes9 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_1992to1997.dta")
anes0 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_2000to2004.dta")
anes16 <- read_sav("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_mergedfile_2016to2020.sav")
anes20 <- read_csv("~/Library/CloudStorage/Box-Box/panel_surveys/anes/anes_2022.csv")

gss6 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/gss/gsspanel06.dta")
gss8 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/gss/gsspanel08.dta")
gss10 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/gss/gsspanel10.dta")
gss20 <- read_dta("~/Library/CloudStorage/Box-Box/panel_surveys/gss/gsspanel20.dta")

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
    V600685
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
    child_1 = as.character(ifelse(V560178 == 99, NA, V560178)),
    child_1 = as.character(ifelse(V560178 == 0, 0, 1)),
    child_2 = as.character(ifelse(V580475 == 99, NA, V580475)),
    child_2 = as.character(ifelse(V580475 == 0, 0, 1)),
    child_3 = as.character(ifelse(V600691 == 99, NA, V600691)),
    child_3 = as.character(ifelse(V600691 == 0, 0, 1)),
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
    
  )  %>% 
  select(
    id, marital_1:race_3
  ) %>% 
  pivot_longer(marital_1:race_3) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

# ANES 1970 demographics ====
anes7_demog <- anes7 %>% 
  mutate(id = 1:nrow(.), 
         across(c(V742410:V742417,V763373:V763378), ~ifelse(.x %in% c(0,98,99), NA, .x)),
         oldestkid_2 = pmax(V742410,V742411,V742412,
                            V742413,V742414,V742415,
                            V742416,V742417, na.rm = TRUE),
         oldestkid_3 = pmax(V763373,V763374,V763375,
                            V763376,V763377,V763378, na.rm = TRUE), 
         #resp's education
         ed_1 = ifelse(V720300 %in% c(98,99), NA, V720300),
         ed_1 = ifelse(V720300 %in% c(11,12,13,14,15,16,17,18,21,22,31,32,33,
                                      41,42,43), "less than", ed_1),
         ed_1 = ifelse(V720300 %in% c(51,61,71,50), "hs", ed_1),
         ed_1 = ifelse(V720300 %in% c(81,82,83,84,85,86), "ba", ed_1),
         ed_2 = ifelse(V742423 %in% c(98,99), NA, V742423),
         ed_2 = ifelse(V742423 %in% c(0,1,2,3,4), "less than", ed_2),
         ed_2 = ifelse(V742423 %in% c(5,6,7,8), "hs", ed_2),
         ed_2 = ifelse(V742423 %in% c(9,10), "ba", ed_2),
         ed_3 = ifelse(V763389 %in% c(98, 99), NA, V763389),
         ed_3 = ifelse(V763389 %in% c(0,1,2,3,4), "less than", ed_3),
         ed_3 = ifelse(V763389 %in% c(5,6,7,8), "hs", ed_3),
         ed_3 = ifelse(V763389 %in% c(9,10), "ba", ed_3),
         #marital status
         marital_1 = ifelse(V720295 == 9, NA, V720295),
         marital_1 = ifelse(V720295 %in% c(2), "single/nm", marital_1),
         marital_1 = ifelse(V720295 %in% c(1,7), "married", marital_1),
         marital_1 = ifelse(V720295 %in% c(3,4,5), "other", marital_1),
         marital_2 = ifelse(V742407 == 9, NA, V742407),
         marital_2 = ifelse(V742407 %in% c(2), "single/nm", marital_2),
         marital_2 = ifelse(V742407 %in% c(1,7), "married", marital_2),
         marital_2 = ifelse(V742407 %in% c(3,4,5), "other", marital_2),
         marital_3 = ifelse(V763370 == 9, NA, V763370),
         marital_3 = ifelse(V763370 %in% c(2), "single/nm", marital_3),
         marital_3 = ifelse(V763370 %in% c(1,7), "married", marital_3),
         marital_3 = ifelse(V763370 %in% c(3,4,5), "other", marital_3),
         #children
         child_2 = ifelse(V742408 %in% c(0, 9), NA, V742408),
         child_2 = ifelse(V742408 == 1, 1, child_2),
         child_2 = ifelse(V742408 == 5, 0, child_2),
         child_3 = ifelse(V763371 %in% c(0, 9), NA, V763371),
         child_3 = ifelse(V763371 == 1, 1, child_3),
         child_3 = ifelse(V763371 == 5, 0, child_3),
         child_1 = ifelse(child_2 == 0 | oldestkid_2 < 2, 0, 1),
         child_1a = ifelse(child_3 == 0 | oldestkid_3 < 4, 0, 1),
         child_1 = ifelse(is.na(child_1), child_1a, child_1), 
         # Sex
         sex_1 = ifelse(V720424 == 9, NA, V720424), 
         sex_2 = ifelse(V742553 == 9, NA, V742553), 
         sex_3 = ifelse(V763512 == 9, NA, V763512),
         #Race 
         race_1 = case_when(
           V720425 == 1 ~ 1, 
           V720425 == 2 ~ 2, 
           V720425 %in% c(3,4,5,6,7) ~ 3, 
           V720425 == 9 ~ NA_real_), 
         race_2 = case_when(
           V742554 == 1 ~ 1, 
           V742554 == 2 ~ 2, 
           V742554 %in% c(3,4,5,6,7) ~ 3, 
           V742554 == 9 ~ NA_real_), 
         race_3 = case_when(
           V763513 == 1 ~ 1, 
           V763513 == 2 ~ 2, 
           V763513 %in% c(3,4,5,6,7) ~ 3, 
           V763513 == 9 ~ NA_real_)) %>% 
  select(id, oldestkid_2:race_3) %>% 
  select(-c(child_1a)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(oldestkid_2:race_3) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

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
           VMP0334 %in% c(5,6,7) ~ "hs", 
           VMP0334 %in% c(8,9,10) ~ "ba", 
           VMP0334 %in% c(98,99) ~ NA_character_
         ), 
         # Children -- sum total amount of children 
         across(c(VMP0589,VMP0590,VMP0591,VMP0592,
                  VMP2435,VMP2436,VMP2437,VMP2438,
                  VMP3581,VMP3582,VMP3583,VMP3583), 
                ~ifelse(.x == 9, NA_real_, .x)),
         
         child_1 = VMP0589 + VMP0590 + VMP0591 + VMP0592, 
         child_2 = VMP2435 + VMP2436 + VMP2437 + VMP2438, 
         child_3 = VMP3581 + VMP3582 + VMP3583 + VMP3583, 
         anychild_1 = case_when(
           VMP3388 == 1 ~ 1, 
           VMP3388 == 5 ~ 0, 
           VMP3388 %in% c(0,9) ~ NA_real_
         ),
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
         )
         ) %>% 
  select(id, marital_1:race_3) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(marital_1:race_3) %>%
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
    marital_2 = case_when(
      V923904 %in% c(1) ~ "married", 
      V923904 == 2 ~ "single/nm", 
      V923904 %in% c(3,4,5,7) ~ "other", 
      V923904 %in% c(0,8,9) ~ NA_character_
    ), 
    # Education 
    educ_1 = case_when(
      V900557 %in% c(1,2) ~ "less than",
      V900557 %in% c(3,4) ~ "hs", 
      V900557 %in% c(5,6,7) ~ "ba", 
      V900557 %in% c(98,99) ~ NA_character_
    ), 
    educ_3 = case_when(
      V923908 %in% c(1,2) ~ "less than",
      V923908 %in% c(3,4) ~ "hs", 
      V923908 %in% c(5,6,7) ~ "ba", 
      V923908 %in% c(98,99) ~ NA_character_
    ), 
    # Children 
    across(c(V900030,V900031,V900032,V900033), 
           ~ifelse(.x == 9, NA_real_, .x)), 
    child_1 = V900030 + V900031 + V900032 +V900033, 
    anychild_3 = case_when(
      V924136 %in% c(1,2) ~ 1, 
      V924136 == 5 ~ 0, 
      V924136 %in% c(0,9) ~ NA_real_
    ), 
    across(c(V924137,V924138,V924139,V924140), 
           ~ifelse(.x %in% c(8,9), NA_real_, .x)), 
    childlt_3 = V924137+V924138+V924139+V924140, 
    # Sex
    sex_1 = ifelse(V924201 %in% c(0,9), NA, V924201),
    # Race 
    race_1 = case_when(
      V900549 == 1 ~ 1, 
      V900549 == 2 ~ 2, 
      V900549 %in% c(3,4) ~ 3, 
      V900549 == 9 ~ NA_real_
    ), 
    race_3 = case_when(
      V924202 == 1 ~ 1, 
      V924202 == 2 ~ 2, 
      V924202 %in% c(3,4) ~ 3, 
      V924202 %in% c(0,9) ~ NA_real_
    )
  ) %>% 
  select(id, marital_1:race_3) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(marital_1:race_3) %>%
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
         V923079:V923082,
         V960048:V960051,
         V941428, 
         # Sex 
         V924201, 
         # Race
         V924202, 
         V941435,
         V960067
  ) %>% 
  mutate(
    marital_1 = ifelse(V923904 %in% c(9,0), NA, V923904),
    marital_1 = ifelse(V923904 %in% c(1,7), "married", marital_1),
    marital_1 = ifelse(V923904 %in% c(2), "single/nm", marital_1),
    marital_1 = ifelse(V923904 %in% c(3,4,5,8), "other", marital_1),
    marital_2 = ifelse(V941204 %in% c(9,0,8), NA, V941204),
    marital_2 = ifelse(V941204 %in% c(1,7), "married", marital_2),
    marital_2 = ifelse(V941204 %in% c(2), "single/nm", marital_2),
    marital_2 = ifelse(V941204 %in% c(3,4,5), "other", marital_2),
    marital_3 = ifelse(V960606 %in% c(9,0,8), NA, V960606),
    marital_3 = ifelse(V960606 %in% c(1,6), "married", marital_3),
    marital_3 = ifelse(V960606 %in% c(5), "single/nm", marital_3),
    marital_3 = ifelse(V960606 %in% c(2,3,4), "other", marital_3),
    #children
    nkids_1 = ifelse(V923079 == 9, 0, V923079) + ifelse(V923080 == 9, 0, V923080) + 
      ifelse(V923081 == 9, 0, V923081) + ifelse(V923081 == 9, 0, V923081),
    childs_1 = ifelse(nkids_1 > 0, 1, 0),
    #any children?
    childs_2 = ifelse(V941428 %in% c(8,9), NA, V941428),
    childs_2 = ifelse(V941428 %in% c(1,2), 1, childs_2),
    childs_2 = ifelse(V941428 %in% c(5), 0, childs_2),
    #education
    ed_1 = ifelse(V923908 %in% c(98,99,0), NA, V923908),
    ed_1 = ifelse(V923908 %in% c(1,2), "less than", ed_1),
    ed_1 = ifelse(V923908 %in% c(3,4,5), "hs", ed_1),
    ed_1 = ifelse(V923908 %in% c(6,7), "ba", ed_1),
    ed_2 = ifelse(V941209 %in% c(98,99,0), NA, V941209),
    ed_2 = ifelse(V941209 %in% c(1,2), "less than", ed_2),
    ed_2 = ifelse(V941209 %in% c(3,4,5), "hs", ed_2),
    ed_2 = ifelse(V941209 %in% c(6,7), "ba", ed_2) ,
    ed_3 = ifelse(V960610 %in% c(8,9,0), NA, V960610),
    ed_3 = ifelse(V960610 %in% c(1,2), "less than", ed_3),
    ed_3 = ifelse(V960610 %in% c(3,4,5), "hs", ed_3),
    ed_3 = ifelse(V960610 %in% c(6,7), "ba", ed_3), 
    #Sex 
    sex_1 = ifelse(V924201 == 9, NA, V924201), 
    #Race 
    race_1 = case_when(
      V924202 == 1 ~ 1, 
      V924202 == 2 ~ 2, 
      V924202 %in% c(3,4,7) ~ 3, 
      V924202 == 9 ~ NA_real_
    ), 
    race_2 = case_when(
      V941435 == 1 ~ 1, 
      V941435 == 2 ~ 2, 
      V941435 %in% c(3,4,7) ~ 3, 
      V941435 == 9 ~ NA_real_
    ), 
    race_3 = case_when(
      V960067 == 1 ~ 1, 
      V960067 == 2 ~ 2, 
      V960067 %in% c(3,4,7) ~ 3, 
      V960067 == 9 ~ NA_real_
    )
  ) %>%
  select(id, marital_1:race_3) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(marital_1:race_3) %>%
  separate(name, into = c("measure", "wave")) %>% 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )
