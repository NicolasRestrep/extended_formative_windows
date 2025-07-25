

# cleaning/run_all.R

# Load libraries and utils
library(tidyverse)
source("code/cleaning/utils.R")
source("code/cleaning/clean_anes_56_60.R")
source("code/cleaning/clean_anes_72_76.R")
source("code/cleaning/clean_anes_80.R")
source("code/cleaning/clean_anes_90_92.R")
source("code/cleaning/clean_anes_92_97.R")
source("code/cleaning/clean_anes_00_04.R")
source("code/cleaning/clean_gss_06_10.R")
source("code/cleaning/clean_gss_08_12.R")
source("code/cleaning/clean_gss_10_14.R")
source("code/cleaning/clean_gss_16_20.R")
source("code/cleaning/clean_anes_16_20.R")
source("code/cleaning/clean_anes_20_22.R")
# Add more as needed

# Set raw file paths
path_1956 <- "~/Dropbox/data/anes/anes5660/anes_mergedfile_1956to1960.dta"
path_1972 <- "~/Dropbox/data/anes/anes7276/anes_mergedfile_1972to1976.dta"
path_1980 <- "~/Dropbox/data/anes/anes1980/anes1980.dta"
path_1990 <- "~/Dropbox/data/anes/anes9092/anes_mergedfile_1990to1992.dta"
path_1992 <- "~/Dropbox/data/anes/anes9092/anes_mergedfile_1990to1992.dta"
path_1992 <- "~/Dropbox/data/anes/anes9297/anes_mergedfile_1992to1997.dta"
path_2000 <- "~/Dropbox/data/anes/anes0004/anes_mergedfile_2000to2004.dta"
path_2006 <- "~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta"
path_2008 <- "~/Dropbox/data/gss_data/gsspanels/gsspanel08.dta"
path_2010 <- "~/Dropbox/data/gss_data/gsspanels/gsspanel10.dta"
path_2016g <- "~/Dropbox/data/gss2020panel/gss2020panel.dta"
path_2016a <- "~/Dropbox/data/anes/anes1620.sav"
path_2020 <- "~/Dropbox/data/anes/anes2022/anes2022"

# Run cleaning
anes_1956 <- clean_anes_1956_60(path_1956)
anes_1972 <- clean_anes_1972_76(path_1972)
anes_1980 <- clean_anes_1980(path_1980)
anes_1990 <- clean_anes_90_92(path_1990)
anes_1992 <- clean_anes_92_97(path_1992)
anes_2000 <- clean_anes_00_04(path_2000)
anes_2016 <- clean_anes_16_20(path_2016a)
anes_2020 <- clean_anes_20_22(path_2020)
gss_2006 <- clean_gss_06_10(path_2006)
gss_2008 <- clean_gss_08_12(path_2008)
gss_2010 <- clean_gss_10_14(path_2010)
gss_2016 <- clean_gss_16_20(path_2016g)

# Combine and save
all_panels <- bind_rows(anes_1956, anes_1972, anes_1980, anes_1990,
                        anes_1992, anes_2000, gss_2006, gss_2008, 
                        gss_2010, gss_2016, anes_2016, anes_2020) %>%
  group_by(id, name, df) %>% 
  filter(n() > 1) %>% ungroup() 


#Calculates wave-pair variables, including differences, 
#durations, and organizes weights, ages, times, etc.
#Removes some questions that should not be included
#Gives weight = 1 to questions without weight
#Removes cases with only one observation

long_difference <- all_panels %>%
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
  mutate(t = (as.numeric(difftime(date, max(date), units = "days")))/3652.5,
         a = abs_diff,
         d = duration,
         ag = age) %>%
  mutate(weight = ifelse(weight == 0, 1, weight)) %>%
  mutate(name = ifelse(name == "nateduc", "natschools", name)) %>%
  select(-c(abs_diff, duration, age)) %>%
  filter(ag <= 80) %>%
  mutate(birth_year = year(date) - ag,
         a30 = ifelse(ag < 30, 1, 0),
         ag = (ag - 44)/10,
         co = (birth_year - 1950)/10,
         dfid = paste(df, id, sep = "-")) %>%
  filter(name != "eqrightstoofar",
         name != "ftcath",
         name != "ftillimmigrants",
         name != "nathome",
         name != "lessgvt",
         name != "noteveryonechance",
         name != "polattn",
         name != "slavediff",
         name != "natfood") %>%
  select(dfid, df:co) %>%
  group_by(dfid) %>%
  mutate(N = n()) %>%
  filter (N != 1) %>%
  ungroup() 

#Save the file
save(long_difference, file = "data/long_difference.Rdata")
