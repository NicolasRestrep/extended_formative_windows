# Packages ----
library(tidyverse)
library(patchwork)

# Load Data ----
load("~/Documents/extended_formative_windows/Data/clean_anes5.Rdata")
load("~/Documents/extended_formative_windows/Data/clean_anes7.Rdata")
load("~/Documents/extended_formative_windows/Data/clean_anes9.Rdata")
load("~/Documents/extended_formative_windows/Data/clean_g6.Rdata")

# PARTYID - ANES56 ----
# Strict
a5_pid_s <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("party")) %>%
  filter(
    partyid_1 != 7,
    partyid_1 != 8,
    partyid_1 != 9,
    partyid_2 != 7,
    partyid_2 != 8,
    partyid_2 != 9,
    partyid_3 != 7,
    partyid_3 != 8,
    partyid_3 != 9
  ) %>%
  mutate(
    change_w2 = abs(partyid_2 - partyid_1),
    change_w3 = abs(partyid_3 - partyid_2),
    total_change_s = change_w2 + change_w3, 
    any_change_s = if_else(total_change_s >= 1, 1, 0), 
    crossed_midpoint_s = case_when(
      partyid_1 < 3 & partyid_2 > 3 ~ 1,
      partyid_1 > 3 & partyid_2 < 3 ~ 1,
      partyid_2 < 3 & partyid_3 > 3 ~ 1,
      partyid_2 > 3 & partyid_3 < 3 ~ 1,
      partyid_1 < 3 & partyid_3 > 3 ~ 1,
      partyid_1 > 3 & partyid_3 < 3 ~ 1,
      TRUE ~ 0
    ), 
    two_points_s = if_else(
      abs(partyid_3-partyid_1) >= 2, 1,0
    )
  ) %>% 
  select(id, age_1, total_change_s, any_change_s, crossed_midpoint_s, two_points_s) %>% 
  mutate(variable = "partyid")

# Recodes 7s and 8s into 3s to be less strict 
a5_pid_l <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("party")) %>%
  filter(
    partyid_1 != 9,
    partyid_2 != 9,
    partyid_3 != 9
  ) %>%
  mutate(
    across(contains("party"), 
            ~ case_when(
             . == 0 ~ 0,
             . == 1 ~ 1, 
             . == 2 ~ 2, 
             . == 3 ~ 3, 
             . == 4 ~ 4, 
             . == 5 ~ 5, 
             . == 6 ~ 6, 
             . == 7 ~ 3,
             . == 8 ~ 3, 
           )), 
    change_w2 = abs(partyid_2 - partyid_1),
    change_w3 = abs(partyid_3 - partyid_2),
    total_change_l = change_w2 + change_w3, 
    any_change_l = if_else(total_change_l >= 1, 1, 0), 
    crossed_midpoint_l = case_when(
      partyid_1 < 3 & partyid_2 > 3 ~ 1,
      partyid_1 > 3 & partyid_2 < 3 ~ 1,
      partyid_2 < 3 & partyid_3 > 3 ~ 1,
      partyid_2 > 3 & partyid_3 < 3 ~ 1,
      partyid_1 < 3 & partyid_3 > 3 ~ 1,
      partyid_1 > 3 & partyid_3 < 3 ~ 1,
      TRUE ~ 0
    ), 
    two_points_l = if_else(
      abs(partyid_3-partyid_1) >= 2, 1,0
    )
  ) %>% 
  select(id, age_1, total_change_l, any_change_l, crossed_midpoint_l, two_points_l) %>% 
  mutate(variable = "partyid")

# HELP BLACK - ANES56 ----

# Code for waves 1 & 3 
# 0                     0. NO OPINION
# 1                 1. AGREE STRONGLY
# 2    2. AGREE BUT NOT VERY STRONGLY
# 3           3. NOT SURE, IT DEPENDS
# 4 4. DISAGREE BUT NOT VERY STRONGLY
# 5              5. DISAGREE STRONGLY
# 8                             8. DK
# 9                             9. NA

# code for wave 2 

# 1                 1. AGREE STRONGLY
# 2   2. AGREE, BUT NOT VERY STRONGLY
# 3                       3. NOT SURE
# 4 4. DISAGREE BUT NOT VERY STRONGLY
# 5              5. DISAGREE STRONGLY
# 7                     7. NO OPINION
# 8                             8. DK
# 9                             9. NA

# Again, strict gets rid of 0, 8 and 9. 

a5_hlpblk_s <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("helpblk")) %>%
  # Recode the inconsistent coding here
  mutate(helpblk5_2 = ifelse(helpblk5_2==7, 0, helpblk5_2)) %>% 
  filter(
    # Strict filtering gets rid of more than half the sample
    helpblk5_1 != 0,
    helpblk5_1 != 8,
    helpblk5_1 != 9,
    helpblk5_2 != 0,
    helpblk5_2 != 8,
    helpblk5_2 != 9,
    helpblk5_3 != 0,
    helpblk5_3 != 8,
    helpblk5_3 != 9
  ) %>%
  mutate(
change_w2 = abs(helpblk5_2 - helpblk5_1),
change_w3 = abs(helpblk5_3 - helpblk5_2),
total_change_s = change_w2 + change_w3,
any_change_s = if_else(total_change_s >= 1, 1, 0),
crossed_midpoint_s = case_when(
  helpblk5_1 < 3 & helpblk5_2 > 3 ~ 1,
  helpblk5_1 > 3 & helpblk5_2 < 3 ~ 1,
  helpblk5_2 < 3 & helpblk5_3 > 3 ~ 1,
  helpblk5_2 > 3 & helpblk5_3 < 3 ~ 1,
  helpblk5_1 < 3 & helpblk5_3 > 3 ~ 1,
  helpblk5_1 > 3 & helpblk5_3 < 3 ~ 1,
  TRUE ~ 0
),
two_points_s = if_else(abs(helpblk5_3 - helpblk5_1) >= 2, 1, 0)
) %>%
  select(id,
         age_1,
         total_change_s,
         any_change_s,
         crossed_midpoint_s,
         two_points_s) %>%
  mutate(variable = "helpblk")
  
# Now do the loose version 
# We keep a much more substantial amount of the sample here

a5_hlpblk_l <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("helpblk")) %>%
  # Recode the inconsistent coding here
  mutate(helpblk5_2 = ifelse(helpblk5_2==7, 0, helpblk5_2)) %>% 
  filter(
    helpblk5_1 != 9,
    helpblk5_2 != 9,
    helpblk5_3 != 9
  ) %>% 
  mutate( 
    across(
      contains("help"), 
      ~ case_when(
        . == 0 ~ 3, 
        . == 1 ~ 1, 
        . == 2 ~ 2, 
        . == 3 ~ 3, 
        . == 4 ~ 4, 
        . == 5 ~ 5, 
        . == 8 ~ 3
      )
    ), 
    change_w2 = abs(helpblk5_2 - helpblk5_1),
    change_w3 = abs(helpblk5_3 - helpblk5_2),
    total_change_l = change_w2 + change_w3,
    any_change_l = if_else(total_change_l >= 1, 1, 0),
    crossed_midpoint_l = case_when(
      helpblk5_1 < 3 & helpblk5_2 > 3 ~ 1,
      helpblk5_1 > 3 & helpblk5_2 < 3 ~ 1,
      helpblk5_2 < 3 & helpblk5_3 > 3 ~ 1,
      helpblk5_2 > 3 & helpblk5_3 < 3 ~ 1,
      helpblk5_1 < 3 & helpblk5_3 > 3 ~ 1,
      helpblk5_1 > 3 & helpblk5_3 < 3 ~ 1,
      TRUE ~ 0
    ),
    two_points_l = if_else(abs(helpblk5_3 - helpblk5_1) >= 2, 1, 0)
    ) %>% 
  select(id, 
         age_1, 
         total_change_l, 
         any_change_l, 
         crossed_midpoint_l, 
         two_points_l) %>% 
  mutate(variable = "helpblk")

# INTEGRATE - ANES56 ----

# Code wave 1 & 3 

# 0                     0. NO OPINION
# 1                 1. AGREE STRONGLY
# 2    2. AGREE BUT NOT VERY STRONGLY
# 3           3. NOT SURE, IT DEPENDS
# 4 4. DISAGREE BUT NOT VERY STRONGLY
# 5              5. DISAGREE STRONGLY
# 8                             8. DK
# 9                             9. NA

# Same quirk as before where 7 refers to no opinion on the 2nd wave 

# Let's do the strict version 

a5_integrate_s <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("integrate")) %>%
  # Recode the inconsistent coding here
  mutate(integrate5_2 = ifelse(integrate5_2 == 7, 0, integrate5_2)) %>%
  filter(
    # Strict filtering gets rid of more than half the sample
    integrate5_1 != 0,
    integrate5_1 != 8,
    integrate5_1 != 9,
    integrate5_2 != 0,
    integrate5_2 != 8,
    integrate5_2 != 9,
    integrate5_3 != 0,
    integrate5_3 != 8,
    integrate5_3 != 9
  ) %>%
  mutate(
    change_w2 = abs(integrate5_2 - integrate5_1),
    change_w3 = abs(integrate5_3 - integrate5_2),
    total_change_s = change_w2 + change_w3,
    any_change_s = if_else(total_change_s >= 1, 1, 0),
    crossed_midpoint_s = case_when(
      integrate5_1 < 3 & integrate5_2 > 3 ~ 1,
      integrate5_1 > 3 & integrate5_2 < 3 ~ 1,
      integrate5_2 < 3 & integrate5_3 > 3 ~ 1,
      integrate5_2 > 3 & integrate5_3 < 3 ~ 1,
      integrate5_1 < 3 & integrate5_3 > 3 ~ 1,
      integrate5_1 > 3 & integrate5_3 < 3 ~ 1,
      TRUE ~ 0
    ),
    two_points_s = if_else(abs(integrate5_3 - integrate5_1) >= 2, 1, 0)
  ) %>%
  select(id,
         age_1,
         total_change_s,
         any_change_s,
         crossed_midpoint_s,
         two_points_s) %>%
  mutate(variable = "integrate")


a5_integrate_l <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("integrate")) %>%
  # Recode the inconsistent coding here
  mutate(integrate5_2 = ifelse(integrate5_2 == 7, 0, integrate5_2)) %>%
  filter(
    integrate5_1 != 9,
    integrate5_2 != 9,
    integrate5_3 != 9
  ) %>% 
  mutate( 
    across(
      contains("integrate"), 
      ~ case_when(
        . == 0 ~ 3, 
        . == 1 ~ 1, 
        . == 2 ~ 2, 
        . == 3 ~ 3, 
        . == 4 ~ 4, 
        . == 5 ~ 5, 
        . == 8 ~ 3
      )
    ),
    change_w2 = abs(integrate5_2 - integrate5_1),
    change_w3 = abs(integrate5_3 - integrate5_2),
    total_change_l = change_w2 + change_w3,
    any_change_l = if_else(total_change_l >= 1, 1, 0),
    crossed_midpoint_l = case_when(
      integrate5_1 < 3 & integrate5_2 > 3 ~ 1,
      integrate5_1 > 3 & integrate5_2 < 3 ~ 1,
      integrate5_2 < 3 & integrate5_3 > 3 ~ 1,
      integrate5_2 > 3 & integrate5_3 < 3 ~ 1,
      integrate5_1 < 3 & integrate5_3 > 3 ~ 1,
      integrate5_1 > 3 & integrate5_3 < 3 ~ 1,
      TRUE ~ 0
    ),
    two_points_l = if_else(abs(integrate5_3 - integrate5_1) >= 2, 1, 0)
  ) %>%
  select(id,
         age_1,
         total_change_l,
         any_change_l,
         crossed_midpoint_l,
         two_points_l) %>%
  mutate(variable = "integrate")

# GOVJOBS - ANES56 -----

# Thankfully same structure as before 

# Let's do the strict version 

a5_govjobs_s <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("govjob")) %>%
  # Recode the inconsistent coding here
  mutate(govjob5_2 = ifelse(govjob5_2 == 7, 0, govjob5_2)) %>%
  filter(
    # Strict filtering gets rid of more than half the sample
    govjob5_1 != 0,
    govjob5_1 != 8,
    govjob5_1 != 9,
    govjob5_2 != 0,
    govjob5_2 != 8,
    govjob5_2 != 9,
    govjob5_3 != 0,
    govjob5_3 != 8,
    govjob5_3 != 9
  ) %>%
  mutate(
    change_w2 = abs(govjob5_2 - govjob5_1),
    change_w3 = abs(govjob5_3 - govjob5_2),
    total_change_s = change_w2 + change_w3,
    any_change_s = if_else(total_change_s >= 1, 1, 0),
    crossed_midpoint_s = case_when(
      govjob5_1 < 3 & govjob5_2 > 3 ~ 1,
      govjob5_1 > 3 & govjob5_2 < 3 ~ 1,
      govjob5_2 < 3 & govjob5_3 > 3 ~ 1,
      govjob5_2 > 3 & govjob5_3 < 3 ~ 1,
      govjob5_1 < 3 & govjob5_3 > 3 ~ 1,
      govjob5_1 > 3 & govjob5_3 < 3 ~ 1,
      TRUE ~ 0
    ),
    two_points_s = if_else(abs(govjob5_3 - govjob5_1) >= 2, 1, 0)
  ) %>%
  select(id,
         age_1,
         total_change_s,
         any_change_s,
         crossed_midpoint_s,
         two_points_s) %>%
  mutate(variable = "govjobs")


a5_govjobs_l <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("govjob")) %>%
  # Recode the inconsistent coding here
  mutate(govjob5_2 = ifelse(govjob5_2 == 7, 0, govjob5_2)) %>%
  filter(
    govjob5_1 != 9,
    govjob5_2 != 9,
    govjob5_3 != 9
  ) %>% 
  mutate( 
    across(
      contains("govjob"), 
      ~ case_when(
        . == 0 ~ 3, 
        . == 1 ~ 1, 
        . == 2 ~ 2, 
        . == 3 ~ 3, 
        . == 4 ~ 4, 
        . == 5 ~ 5, 
        . == 8 ~ 3
      )
    ),
    change_w2 = abs(govjob5_2 - govjob5_1),
    change_w3 = abs(govjob5_3 - govjob5_2),
    total_change_l = change_w2 + change_w3,
    any_change_l = if_else(total_change_l >= 1, 1, 0),
    crossed_midpoint_l = case_when(
      govjob5_1 < 3 & govjob5_2 > 3 ~ 1,
      govjob5_1 > 3 & govjob5_2 < 3 ~ 1,
      govjob5_2 < 3 & govjob5_3 > 3 ~ 1,
      govjob5_2 > 3 & govjob5_3 < 3 ~ 1,
      govjob5_1 < 3 & govjob5_3 > 3 ~ 1,
      govjob5_1 > 3 & govjob5_3 < 3 ~ 1,
      TRUE ~ 0
    ),
    two_points_l = if_else(abs(govjob5_3 - govjob5_1) >= 2, 1, 0)
  ) %>%
  select(id,
         age_1,
         total_change_l,
         any_change_l,
         crossed_midpoint_l,
         two_points_l) %>%
  mutate(variable = "govjobs")

# BUILDSCHOOLS - ANES56 ----
# Thankfully same structure as before 

# Let's do the strict version 

a5_bldschool_s <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("bldschls")) %>%
  # Recode the inconsistent coding here
  mutate(bldschls5_2 = ifelse(bldschls5_2 == 7, 0, bldschls5_2)) %>%
  filter(
    # Strict filtering gets rid of more than half the sample
    bldschls5_1 != 0,
    bldschls5_1 != 8,
    bldschls5_1 != 9,
    bldschls5_2 != 0,
    bldschls5_2 != 8,
    bldschls5_2 != 9,
    bldschls5_3 != 0,
    bldschls5_3 != 8,
    bldschls5_3 != 9
  ) %>%
  mutate(
    change_w2 = abs(bldschls5_2 - bldschls5_1),
    change_w3 = abs(bldschls5_3 - bldschls5_2),
    total_change_s = change_w2 + change_w3,
    any_change_s = if_else(total_change_s >= 1, 1, 0),
    crossed_midpoint_s = case_when(
      bldschls5_1 < 3 & bldschls5_2 > 3 ~ 1,
      bldschls5_1 > 3 & bldschls5_2 < 3 ~ 1,
      bldschls5_2 < 3 & bldschls5_3 > 3 ~ 1,
      bldschls5_2 > 3 & bldschls5_3 < 3 ~ 1,
      bldschls5_1 < 3 & bldschls5_3 > 3 ~ 1,
      bldschls5_1 > 3 & bldschls5_3 < 3 ~ 1,
      TRUE ~ 0
    ),
    two_points_s = if_else(abs(bldschls5_3 - bldschls5_1) >= 2, 1, 0)
  ) %>%
  select(id,
         age_1,
         total_change_s,
         any_change_s,
         crossed_midpoint_s,
         two_points_s) %>%
  mutate(variable = "bldschool")


a5_bldschool_l <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("bldschls")) %>%
  # Recode the inconsistent coding here
  mutate(bldschls5_2 = ifelse(bldschls5_2 == 7, 0, bldschls5_2)) %>%
  filter(
    bldschls5_1 != 9,
    bldschls5_2 != 9,
    bldschls5_3 != 9
  ) %>% 
  mutate( 
    across(
      contains("bldschls"), 
      ~ case_when(
        . == 0 ~ 3, 
        . == 1 ~ 1, 
        . == 2 ~ 2, 
        . == 3 ~ 3, 
        . == 4 ~ 4, 
        . == 5 ~ 5, 
        . == 8 ~ 3
      )
    ),
    change_w2 = abs(bldschls5_2 - bldschls5_1),
    change_w3 = abs(bldschls5_3 - bldschls5_2),
    total_change_l = change_w2 + change_w3,
    any_change_l = if_else(total_change_l >= 1, 1, 0),
    crossed_midpoint_l = case_when(
      bldschls5_1 < 3 & bldschls5_2 > 3 ~ 1,
      bldschls5_1 > 3 & bldschls5_2 < 3 ~ 1,
      bldschls5_2 < 3 & bldschls5_3 > 3 ~ 1,
      bldschls5_2 > 3 & bldschls5_3 < 3 ~ 1,
      bldschls5_1 < 3 & bldschls5_3 > 3 ~ 1,
      bldschls5_1 > 3 & bldschls5_3 < 3 ~ 1,
      TRUE ~ 0
    ),
    two_points_l = if_else(abs(bldschls5_3 - bldschls5_1) >= 2, 1, 0)
  ) %>%
  select(id,
         age_1,
         total_change_l,
         any_change_l,
         crossed_midpoint_l,
         two_points_l) %>%
  mutate(variable = "bldschool")

# PRIVATE POWER - ANES56 ----
# Thankfully same structure as before 

# Let's do the strict version 

a5_private_s <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("prvtpwr")) %>%
  # Recode the inconsistent coding here
  mutate(prvtpwr5_2 = ifelse(prvtpwr5_2 == 7, 0, prvtpwr5_2)) %>%
  filter(
    # Strict filtering gets rid of more than half the sample
    prvtpwr5_1 != 0,
    prvtpwr5_1 != 8,
    prvtpwr5_1 != 9,
    prvtpwr5_2 != 0,
    prvtpwr5_2 != 8,
    prvtpwr5_2 != 9,
    prvtpwr5_3 != 0,
    prvtpwr5_3 != 8,
    prvtpwr5_3 != 9
  ) %>%
  mutate(
    change_w2 = abs(prvtpwr5_2 - prvtpwr5_1),
    change_w3 = abs(prvtpwr5_3 - prvtpwr5_2),
    total_change_s = change_w2 + change_w3,
    any_change_s = if_else(total_change_s >= 1, 1, 0),
    crossed_midpoint_s = case_when(
      prvtpwr5_1 < 3 & prvtpwr5_2 > 3 ~ 1,
      prvtpwr5_1 > 3 & prvtpwr5_2 < 3 ~ 1,
      prvtpwr5_2 < 3 & prvtpwr5_3 > 3 ~ 1,
      prvtpwr5_2 > 3 & prvtpwr5_3 < 3 ~ 1,
      prvtpwr5_1 < 3 & prvtpwr5_3 > 3 ~ 1,
      prvtpwr5_1 > 3 & prvtpwr5_3 < 3 ~ 1,
      TRUE ~ 0
    ),
    two_points_s = if_else(abs(prvtpwr5_3 - prvtpwr5_1) >= 2, 1, 0)
  ) %>%
  select(id,
         age_1,
         total_change_s,
         any_change_s,
         crossed_midpoint_s,
         two_points_s) %>%
  mutate(variable = "private")


a5_private_l <- clean_anes5 %>%
  mutate(id = 1:nrow(.)) %>%
  select(id, age_1, contains("prvtpwr")) %>%
  # Recode the inconsistent coding here
  mutate(prvtpwr5_2 = ifelse(prvtpwr5_2 == 7, 0, prvtpwr5_2)) %>%
  filter(
    prvtpwr5_1 != 9,
    prvtpwr5_2 != 9,
    prvtpwr5_3 != 9
  ) %>% 
  mutate( 
    across(
      contains("prvtpwr"), 
      ~ case_when(
        . == 0 ~ 3, 
        . == 1 ~ 1, 
        . == 2 ~ 2, 
        . == 3 ~ 3, 
        . == 4 ~ 4, 
        . == 5 ~ 5, 
        . == 8 ~ 3
      )
    ),
    change_w2 = abs(prvtpwr5_2 - prvtpwr5_1),
    change_w3 = abs(prvtpwr5_3 - prvtpwr5_2),
    total_change_l = change_w2 + change_w3,
    any_change_l = if_else(total_change_l >= 1, 1, 0),
    crossed_midpoint_l = case_when(
      prvtpwr5_1 < 3 & prvtpwr5_2 > 3 ~ 1,
      prvtpwr5_1 > 3 & prvtpwr5_2 < 3 ~ 1,
      prvtpwr5_2 < 3 & prvtpwr5_3 > 3 ~ 1,
      prvtpwr5_2 > 3 & prvtpwr5_3 < 3 ~ 1,
      prvtpwr5_1 < 3 & prvtpwr5_3 > 3 ~ 1,
      prvtpwr5_1 > 3 & prvtpwr5_3 < 3 ~ 1,
      TRUE ~ 0
    ),
    two_points_l = if_else(abs(prvtpwr5_3 - prvtpwr5_1) >= 2, 1, 0)
  ) %>%
  select(id,
         age_1,
         total_change_l,
         any_change_l,
         crossed_midpoint_l,
         two_points_l) %>%
  mutate(variable = "private")

# PARTID - ANES74

