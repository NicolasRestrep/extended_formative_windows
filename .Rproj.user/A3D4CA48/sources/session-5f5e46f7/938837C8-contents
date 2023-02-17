
## Make data sets for each data frame. 
## Variables for each wave, 
## control variables
## weights
## age
## education



## 1950 ANES
## 1970 ANES
## 1990 ANES
## 2006 GSS


### Coding decisions
#I collapsed educaiton into a three-category item: less than hs, high school degree, ba
#There's no real record of advanced degrees in the 1950s anes.

# Marital status variable
# Never married/single
# Married
# Was married? (widowed, divorced, separated, etc.)

#1956-60 ANES Panel
# Panel available: https://electionstudies.org/data-center/1956-1960-panel-study/

#The number of cases doesn't match what's in the codebook. 
#What's up with that?

anes5 <- read_dta("~/Dropbox/data/anes/anes5660/anes_mergedfile_1956to1960.dta") 

clean_anes5 <- anes5 %>% 
  select(#Negroes fair treatment
         V560044,V580329,V600628, 
         #Busing
         V560074,V580333,V600636,
         #Job guarantee
         V560032,V580321,V600620,
         #Help poor countries
         V560041,V580327,V600626,
         #Build schools
         V560053,V580325,V600624,
         #Fight communism
         V560056,V580331,V600630,
         #Leave utilities to prvt mkt
         V560059,V580319,V600618,
         #US better off stay home
         V560035,V580323,V600622,
         #Partyid
         V560088,V580360,V600835,
         #Age
         V560295, V580472, V600688, 
         #Marital status
         V560177, V580474, V600690,
         #Education
         V560181, V580478, V600694,
         #Children in household
         V560178, V580475, V600691,
         #sex
         V560171, V580468, V600684,
         #race
         V560172, V580469, V600685,
         #survey weight
         V600569) %>%
  mutate(weight = V600569,
         age_1 = V560295,
         age_2 = V580472,
         age_3 = V600688,
         marital_1 = V560177, marital_2 = V580474,
         marital_3 = V600690,
         marital_1 = ifelse(V560177 == 9, NA, V560177),
         marital_1 = ifelse(V560177 %in% c(2), "single/nm", marital_1),
         marital_1 = ifelse(V560177 %in% c(1), "married", marital_1),
         marital_1 = ifelse(V560177 %in% c(3,4,5,7), "other", marital_1),
         marital_2 = ifelse(V580474 == 9, NA, V580474),
         marital_2 = ifelse(V580474 %in% c(2), "single/nm", marital_2),
         marital_2 = ifelse(V580474 %in% c(1), "married", marital_2),
         marital_2 = ifelse(V580474 %in% c(3,4,5), "other", marital_2),
         marital_3 = ifelse(V600690 == 9, NA, V600690),
         marital_3 = ifelse(V600690 %in% c(2), "single/nm", marital_3),
         marital_3 = ifelse(V600690 %in% c(1), "married", marital_3),
         marital_3 = ifelse(V600690 %in% c(3,4,5), "other", marital_3),
         #IS there a child under 18 in the household.
         child_1 = ifelse(V560178 == 99, NA, V560178),
         child_1 = ifelse(V560178 == 0, 0, 1),
         child_2 = ifelse(V580475 == 99, NA, V580475),
         child_2 = ifelse(V580475 == 0, 0, 1),
         child_3 = ifelse(V600691 == 99, NA, V600691),
         child_3 = ifelse(V600691 == 0, 0, 1),
         #The third education variable is differently coded
         ed_1 = ifelse(V560181 == 9, NA, V560181),
         ed_1 = ifelse(V560181 %in% c(0,1,2,3,4), "less than", ed_1),
         ed_1 = ifelse(V560181 %in% c(5,6,7), "hs", ed_1),
         ed_1 = ifelse(V560181 %in% c(8), "ba", ed_1),
         ed_2 = ifelse(V580478 == 9, NA, V580478),
         ed_2 = ifelse(V580478 %in% c(0,1,2,3,4), "less than", ed_2),
         ed_2 = ifelse(V580478 %in% c(5,6,7), "hs", ed_2),
         ed_2 = ifelse(V580478 %in% c(8), "ba", ed_2),
         ed_3 = ifelse(V600694 %in% c(98, 99), NA, V600694),
         ed_3 = ifelse(V600694 %in% c(0,11,12,13,14,15,16,17,21,31,32,33,41,42,43), "less than", ed_3),
         ed_3 = ifelse(V600694 %in% c(51,61,71), "hs", ed_3),
         ed_3 = ifelse(V600694 %in% c(81,82), "ba", ed_3),
         #1 = male, 2 = female
         sex_1 = V560171, sex_2 = V580468, sex_3 = V600684,
         #1 = white, 2 = negro, 3 = other.
         race_1 = V560172, race_2 = V580469, race_3 = V600685) %>%
  #Recoding age at time 1 if that is missing
  #using later ages. 
  mutate(age_1 = ifelse(age_1 %in% c(98,99) & age_2 < 98, age_2-2,
                         age_1), 
         age_1 = ifelse(age_1 %in% c(98,99) & age_3 < 98, age_3-4,
                         age_1)) %>%
  mutate(helpblk5_1 = V560044, helpblk5_2 = V580329,
         helpblk5_3 = V600628,
         integrate5_1 = V560074, integrate5_2 = V580333,
         integrate5_3 = V600636,
         govjob5_1 = V560032, govjob5_2 = V580321,
         govjob5_3 = V600620,
         fgnaid5_1 = V560041, fgnaid5_2 = V580327,
         fgnaid5_3 = V600626,
         bldschls5_1 = V560053, bldschls5_2 = V580325,
         bldschls5_3 = V600624,
         fghtcomm5_1 = V560056, fghtcomm5_2 = V580331,
         fghtcomm5_3 = V600630,
         stayhome5_1 = V560035, stayhome5_2 = V580323,
         stayhome5_3 = V600622,
         prvtpwr5_1 = V560059, prvtpwr5_2 = V580319,
         prvtpwr5_3 = V600618,
         partyid5_1 = V560088, partyid5_2 = V580360,
         partyid5_3 = V600835) %>%
  select(-c(V560044,V580329,V600628,
            V560074,V580333,V600636,
            V560032,V580321,V600620,
            V560041,V580327,V600626,
            V560053,V580325,V600624,
            V560056,V580331,V600630,
            V560059,V580319,V600618,
            V560088,V580360,V600835,
            V560035,V580323,V600622,
            V560295, V580472, V600688,
            V560177, V580474, V600690,
            V560181, V580478, V600694,
            V560178, V580475, V600691,
            V560171, V580468, V600684,
            V560172, V580469, V600685,
            V600569))

#opinion items, wave 1 and wave 3
# 0 = no opinion, 8 = don't know; 9 = NA
#opinion items, wave 2
# 7 = no opinion, 8 = don't know; 9 = NA

#partyid_1; partyid_2; partyid_3
# 7 = other party; 8 = apoliticall; 9 = NA






save(clean_anes5, file = "~/Dropbox/formative_period/clean_anes5.Rdata")


#### 1970s
anes7 <- read_dta("~/Dropbox/data/anes/anes7276/anes_mergedfile_1972to1976.dta")

#Anes 1972-1976 panel
# Available at: https://electionstudies.org/data-center/1972-1976-merged-file/


#Documentation for the 1702-76 ANES Panels say it is "unweighted"

#Education:
# V720300 (years, mostly)
# V742423 (different educational summary); 742588 - like 720300.
# V763389 (educational summary)

#Marital status (1: married; 2: never married; 3: div
# 4: separated; 5: widowed; 7: common law mar)
# V720295
# V742407
# V763370

#Have children?
#Question doesn't seem clear in the first wave, but
# could work backwards.
#721024 Any children of school age?
#720296 child between 5 and 18
#742408 any children
#742409 how many? (742410 - 742417 ages?)
#763371 any children?
#763372 how many? (763373 - 763380 ages?)

#create variable with oldest child age at wave 2
anes7$oldestkid_2 <- pmax(anes7$V742410,anes7$V742411,anes7$V742412,
                          anes7$V742413,anes7$V742414,anes7$V742415,
                          anes7$V742416,anes7$V742417)

#create variable with oldest child age at wave 3
anes7$oldestkid_3 <- pmax(anes7$V763373,anes7$V763374,anes7$V763375,
                          anes7$V763376,anes7$V763377,anes7$V763378)

#Sex
#720424
#742553
#763512 (1 - male, 2 = female)

#Race
#720425
#742554
#763513 (1 - white, 2 - negro, 3 - PR, 
#4 - mexian/chicano, 5 oriental, 6: american indian,
#7 - other)

clean_anes7 <- anes7 %>% 
  #Select members of the panel...
  filter(V764002 == 1) %>%
  select(#id number
    V720002,
    #age
    V720294, V742406, V763369,
    #help minorities scale
    V720629,V742296,V763264,
    #school busing
    V720202,V742288,V763257,
    #job guarantee
    V721067,V742265,V763758,
    #urban unrest scale
    V720670,V742273,V763767,
    #rights of accused
    V720621,V742281,V763248,
    #partyid
    V720140,V742204,V763174,
    #lib-con scale
    V720652,V742305,V763286,
    #equal role for women
    V720232,V742302,V763787,
    #resp's education
    V720300,V742423,V763389,
    #marital status
    V720295, V742407, V763370,
    #children
    V742408, V763371,
    #oldest kid stuff
    V742410:V742417,V763373:V763378) %>%
  mutate(age_1 = V720294,
         age_2 = V742406,
         age_3 = V763369) %>%
  mutate(across(c(V742410:V742417,V763373:V763378), ~ifelse(.x %in% c(0,98,99), NA, .x)),
         oldestkid_2 = pmax(V742410,V742411,V742412,
                            V742413,V742414,V742415,
                            V742416,V742417, na.rm = TRUE),
         oldestkid_3 = pmax(V763373,V763374,V763375,
                            V763376,V763377,V763378, na.rm = TRUE)) %>%
  mutate(ed_1 = ifelse(V720300 %in% c(98,99), NA, V720300),
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
         child_2 = ifelse(V742408 %in% c(0, 9), NA, V742408),
         child_2 = ifelse(V742408 == 1, 1, child_2),
         child_2 = ifelse(V742408 == 5, 0, child_2),
         child_3 = ifelse(V763371 %in% c(0, 9), NA, V763371),
         child_3 = ifelse(V763371 == 1, 1, child_3),
         child_3 = ifelse(V763371 == 5, 0, child_3),
         child_1 = ifelse(child_2 == 0 | oldestkid_2 < 2, 0, 1),
         child_1a = ifelse(child_3 == 0 | oldestkid_3 < 4, 0, 1),
         child_1 = ifelse(is.na(child_1), child_1a, child_1)) %>%
  #select(age_1, age_2, age_3) %>%
  mutate(age_1 = ifelse(age_1 == 0 & age_2 != 0, age_2-2, age_1),
         age_1 = ifelse(age_1 == 0 & age_3 != 0, age_3-4, age_1)) %>%
  mutate(age_1 = ifelse(age_1 == 0, NA, age_1))%>%
  mutate(helpblk7_1 = V720629, helpblk7_2 = V742296,
         helpblk7_3 = V763264,
         busing7_1 = V720202, busing7_2 = V742288,
         busing7_3 = V763257,
         jobguar7_1 = V721067, jobguar7_2 = V742265,
         jobguar7_3 = V763758,
         urbunrest7_1 = V720670, urbunrest7_2 = V742273,
         urbunrest7_3 = V763767,
         eqrole7_1 = V720232,eqrole7_2 = V742302,
         eqrole7_3 = V763787,
         accused7_1 = V720621, accused7_2 = V742281,
         accused7_3 = V763248,
         partyid7_1 = V720140, partyid7_2 = V742204,
         partyid7_3 = V763174,
         libcon7_1 = V720652, libcon7_2 = V742305,
         libcon7_3 = V763286) %>%
    select(-c(V720294, V742406, V763369,
              V720002, V720629,V742296,V763264,
              V720002, V720202,V742288,V763257,
              V721067,V742265,V763758,
              V720670,V742273,V763767,
              V720621,V742281,V763248,
              V720140,V742204,V763174,
              V720652,V742305,V763286,
              V720232,V742302,V763787,
              V720300,V742423,V763389,
              V720295, V742407, V763370,
              V742408, V763371,
              V742410:V742417,V763373:V763378,
              oldestkid_2, oldestkid_3, child_1a))

#opinions (helpblk, busing, jobguar, urban, eqrole, accused): wave 1
# 0 = haven't thought; 8 = don't know; 9 = NA
# wave 2
# 0 = INAP/haven't thought, 8 = don't know, 9 = NA (not clear if we can separate inap/havent...)
#wave 3
# 0 = INAP/haven't thought, 8 = don't know, 9 = NA (not clear if we can separate inap/havent...)

#partyid (all waves)
#0-6 strong D to strong R; 7 - other, 8 - apolitical; 9 - NA/dk

#libcon (w1/w2)
# 1-7 extremely l to extremely c; 0 haven't thought; 8 = dk, 9 = na/inap
# (w3)
#  1-7 extremely l to extremely c; 0/inap; 8 = dk, 9 = na

save(clean_anes7, file = "~/Dropbox/formative_period/clean_anes7.Rdata")


### 1990 ANES

anes9 <- read_dta("~/Dropbox/data/anes/anes9297/anes_mergedfile_1992to1997.dta") 

# 1992-1996 ANES panel
# Available here: https://electionstudies.org/data-center/1992-1997-merged-file/

#Education
#M923908: summary measure
#M941209 : summary
#M960610 : summary 

#marital
#M923904 (1: married, 2: never; 3: divorced; 4: separated
# 5: widowed; 7: partners not married; 8: not married, refused to specify)
#M941204
#M960606

#Children?
# number of children  ...
#M923079 under 6
#M923080 6-9
# M923081 10-13
# M923082 14-17

#Do you have any children:
# M941428

#number of children...
# M960048 under 6
# M960049 6-9
# M960050 10-13
# M960051 14-18

clean_anes9 <- anes9 %>%
  #select by panel weight... 1316 cases.
  #filter(!is.na(V960004), V960004 != 0) %>%
  mutate(weight = V960004) %>%
  select(weight,
    V923903, V941203, V960605,
         V923716,V940950,V960479,
         V923718,V940930,V960483,
         V923701,V940940,V960450,
         V923724,V940936,V960487,
         V923634,V940655,V960420,
         V923509,V940839,V961269,
    #equal opportunity
    V926024,V940914,V961229,
    #equal role
    V923801, V940928,V960543,
         #education
         V923908,V941209,V960610,
    #marital status
    V923904,V941204,V960606,
    #children
    V923079:V923082,
    V960048:V960051,
    V941428
         ) %>%
  mutate(age_1 = V923903,
         age_2 = V941203,
         age_3 = V960605) %>%
  mutate(age_1 = ifelse(is.na(age_1) & !is.na(age_2),
                         age_2 - 2, age_1),
         age_1 = ifelse(is.na(age_1) & !is.na(age_3),
                         age_3 - 4, age_1)) %>%
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
    ed_3 = ifelse(V960610 %in% c(6,7), "ba", ed_3) 
    
  ) %>%
  mutate(govins9_1 = V923716, govins9_2 = V940950,
         govins9_3 = V960479,
         jobguar9_1 = V923718, jobguar9_2 = V940930,
         jobguar9_3 = V960483,
         spendserv9_1 = V923701, spendserv9_2 = V940940,
         spendserv9_3 = V960450,
         govblks9_1 = V923724, govblks9_2 = V940936,
         govblks9_3 = V960487,
         #women should have equal role (7-pt)
         eqrole9_1 = V923801, eqrole9_2 = V940928,
         eqrole9_3 = V960543,
         #five-point scale
         eqop9_1 = V926024, eqop9_2 = V940914,
         eqop9_3 = V961229,
         partyid9_1 = V923634, partyid9_2 = V940655,
         partyid9_3 = V960420,
         libcon9_1 = V923509, libcon9_2 = V940839,
         libcon9_3 = V961269) %>%
  select(-c(V923903, V941203, V960605,
            V923716,V940950,V960479,
            V923718,V940930,V960483,
            V923701,V940940,V960450,
            V923724,V940936,V960487,
            V923634,V940655,V960420,
            V923509,V940839,V961269,
            V923079:V923082,
            V960048:V960051,
            V926024,V940914,V961229,
            V923801, V940928,V960543,
            V923908,V941209,V960610,
            V923904,V941204,V960606))

#marital status
# Thsi is a mess... 
#719 panel cases missing information on this in w1... 
# "not asked of panel respondents interviewed
# using the "short form" questionnaire, and their marital
# status from 1990 was copied here."
# Idon't think they did that... I think that's why so many are missing

# some people in the "panel" were not interviewed in 1992... 
# maybe that's why they're missing... 
# That's it... jesus.

# A bunch of these cases are missing in wave 1
# There are 719 cases that are weighted for the panel that
# are not in wave 1.

#non-weighted cases DO have w1 data

# I have to go find the cases in the 1990 post-election study
# V923006 is 1990 study id
#This doesn't work. These are all "0000". 

# There's a lot more churn in this panel. 
# If we only look at cases that are interviews in all three waves
# then we only have like, 600 cases?
#If we include people interviews in 2/3 (92-94 and 94-96)
# then we have > 1300
# The "panel" is 1316 cases, but some of them are not in 92
# A mess

save(clean_anes9, file = "~/Dropbox/formative_period/clean_anes9.Rdata")

### GSS Data

g6 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta")

#GSS panels available at: https://gss.norc.org/get-the-data/stata


#married:
#marital_1, martial_2, marital_3

#sex
#sex_1, sex_2, sex_3

#education
#years: educ_1, educ_2, educ_3
#degree: degree_1, degree_2, degree_3

#childs_1, childs_2, childs_3 (how many ever had?)

#This messes up the missing data codes (don't knows getting coded as missing)
clean_g6 <- g6 %>%
  zap_labels() %>%
  select(wtpannr123,
         age_1, age_2, age_3, 
         marital_1, marital_2, marital_3,
         childs_1, childs_2, childs_3,
         degree_1, degree_2, degree_3,
         helppoor_1, helppoor_2, helppoor_3,
         helpnot_1, helpnot_2, helpnot_3,
         helpsick_1, helpsick_2, helpsick_3,
         helpblk_1, helpblk_2, helpblk_3,
         eqwlth_1, eqwlth_2, eqwlth_3,
         polviews_1, polviews_2, polviews_3,
         partyid_1, partyid_2, partyid_3) %>%
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
                       "3"="ba", "4"="ba"))
  

save(clean_g6, file = "~/Dropbox/formative_period/clean_g6.Rdata")


  




