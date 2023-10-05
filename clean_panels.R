#Cleaning for Reliability Models
library(tidyverse)
library(haven)
library(broom)


anes5 <- read_dta("~/Dropbox/data/anes/anes5660/anes_mergedfile_1956to1960.dta") 

clean_anes5 <- anes5 %>% 
  select(#Negroes fair treatment
    V560044,V580329,V600628, 
    #deseg
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
    #closerjob
    V560034, V580322, V600621,
    #closeraid
    V560043, V580328, V600627,
    #closertreat
    V560046, V580330, V600629,
    #closerbuild
    V560055, V580326, V600625,
    #closerfghtcomm
    V560058, V580332, V600631,
    #closerprvtpwr
    V560061, V580320, V600619,
    #closerdeseg
    V560076, V580336, V600639,
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
  mutate(helpblk_1 = V560044, helpblk_2 = V580329,
         helpblk_3 = V600628,
         integrate_1 = V560074, integrate_2 = V580333,
         integrate_3 = V600636,
         govjob_1 = V560032, govjob_2 = V580321,
         govjob_3 = V600620,
         fgnaid_1 = V560041, fgnaid_2 = V580327,
         fgnaid_3 = V600626,
         bldschls_1 = V560053, bldschls_2 = V580325,
         bldschls_3 = V600624,
         fghtcomm_1 = V560056, fghtcomm_2 = V580331,
         fghtcomm_3 = V600630,
         stayhome_1 = V560035, stayhome_2 = V580323,
         stayhome_3 = V600622,
         prvtpwr_1 = V560059, prvtpwr_2 = V580319,
         prvtpwr_3 = V600618,
         partyid_1 = V560088, partyid_2 = V580360,
         partyid_3 = V600835,
         #closerjob
         clsrjob_1 = V560034, clsrjob_2 = V580322, 
         clsrjob_3 = V600621,
         #closeraid
         clsraid_1 = V560043, clsraid_2 = V580328, 
         clsraid_3 = V600627,
         #closertreat
         clsrtreat_1 = V560046, clsrtreat_2 = V580330, 
         clsrtreat_3 = V600629,
         #closerbuild
         clsrbuild_1 = V560055, clsrbuild_2 = V580326, 
         clsrbuild_3 = V600625,
         #closerfghtcomm
         clsroverseas_1 = V560058, clsroverseas_2 = V580332, 
         clsroverseas_3 = V600631,
         #closerprvtpwr
         clsrprvtpwr_1 = V560061, clsrprvtpwr_2 = V580320, 
         clsrprvtpwr_3 = V600619,
         #closerdeseg
         clsrdeseg_1 = V560076, clsrdeseg_2 = V580336, 
         clsrdeseg_3 = V600639) %>%
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
            V600569,
            V560034, V580322, V600621,
            V560043, V580328, V600627,
            V560046, V580330, V600629,
            V560055, V580326, V600625,
            V560058, V580332, V600631,
            V560061, V580320, V600619,
            V560076, V580336, V600639))

anes7 <- read_dta("~/Dropbox/data/anes/anes7276/anes_mergedfile_1972to1976.dta")

clean_anes7 <- anes7 %>% 
  #Select members of the panel...
  filter(V764002 == 1) %>%
  mutate(#id number
    id=V720002,
    across(c(V742410:V742417,V763373:V763378), ~ifelse(.x %in% c(0,98,99), NA, .x)),
    oldestkid_2 = pmax(V742410,V742411,V742412,
                       V742413,V742414,V742415,
                       V742416,V742417, na.rm = TRUE),
    oldestkid_3 = pmax(V763373,V763374,V763375,
                       V763376,V763377,V763378, na.rm = TRUE),
    #age
    age_1 = V720294, age_2 = V742406, 
    age_3 = V763369,
    #toofast
    toofast_1 = V720112, toofast_2 = V742264, 
    toofast_3 = V763213,
    #thermscoop
    thermscoop_1 = V720259, thermscoop_2 = V742348,
    thermscoop_3 = V763303,
    #thermwallace
    thermwallace_1 = V720253,thermwallace_2 = V742338,
    thermwallace_3 = V763297,
    #thermnixon
    thermnixon_1 = V720255,thermnixon_2 = V742354,thermnixon_3 = V763307,
    #thermted
    thermted_1 = V720258,thermted_2 = V742347,thermted_3 = V763302,
    #thermhump
    thermhump_1 = V720264,thermhump_2 = V742352,thermhump_3 = V763300,
    #nosay
    nosay_1 = V720269,nosay_2 = V742222,nosay_3 = V763815,
    #votesay
    votesay_1=V720270,votesay_2=V742223,votesay_3=V763816,
    #toocomplicated
    toocomplicated_1 = V720271,toocomplicated_2=V742224,
    toocomplicated_3=V763817,
    #dontcare
    dontcare_1 = V720272,dontcare_2=V742225,dontcare_3=V763818,
    #losetouch
    losetouch_1 =V720273,losetouch_2=V742226,losetouch_3=V763819,
    #onlyvotes
    onlyvotes_1=V720274,onlyvotes_2=V742227,onlyvotes_3=V763820,
    #wastemoney
    wastetax_1=V720570,wastetax_2=V742229,wastetax_3=V763162,
    #govdoright
    govdoright_1=V720571,govdoright_2=V742230,govdoright_3=V763163,
    #runforfew
    runforfew_1=V720572,runforfew_2=V742231,runforfew_3=V763164,
    #govcapable
    govcapable_1=V720573,govcapable_2=V742232,govcapable_3=V763165,
    #govcrooked
    govcrooked_1=V720574,govcrooked_2=V742233,govcrooked_3=V763166,
    #partyattend
    partyattend_1=V720576,partyattend_2=V742235,partyattend_3=V763742,
    #repattend
    repattend_1=V720578,repattend_2=V742237,repattend_3=V763744,
    #thermbiz
    thermbiz_1=V720707,thermbiz_2=V742356,thermbiz_3=V763821,
    #thermlibs
    thermlibs_1=V720709,thermlibs_2=V742358,thermlibs_3=V763823,
    #thermcops
    thermcops_1=V720714,thermcops_2=V742360,thermcops_3=V763828,
    #thermmil
    thermmil_1=V720717,thermmil_2=V742362,thermmil_3=V763831,
    #thermwhite
    thermwhite_1=V720718,thermwhite_2=V742363,thermwhite_3=V763846,
    #thermdems
    thermdems_1=V720719,thermdems_2=V742364,thermdems_3=V763833,
    #thermblacks
    thermblacks_1=V720720,thermblacks_2=V742365,thermblacks_3=V763832,
    #thermreps
    thermreps_1=V720721,thermreps_2=V742366,thermreps_3=V763835,
    #thermunion
    thermunion_1=V720722,thermunion_2=V742367,thermunion_3=V763836,
    #thermcons
    thermcons_1=V720724,thermcons_2=V742369,thermcons_3=V763838,
    #influnion
    influnion_1=V720737,influnion_2=V742379,influnion_3=V763566,
    #inflbiz
    inflbiz_1=V720744,inflbiz_2=V742383,inflbiz_3=V763573,
    #inflblacks
    inflblacks_1=V720745,inflblacks_2=V742384,inflblacks_3=V763570,
    #inflyoung
    inflyoung_1=V720749,inflyoung_2=V742388,inflyoung_3=V763578,
    #inflreps
    inflreps_1=V720752,inflreps_2=V742390,inflreps_3=V763581,
    #inflwelf
    inflwelf_1=V720753,inflwelf_2=V742391,inflwelf_3=V763582,
    #inflold
    inflold_1=V720755,inflold_2=V742393,inflold_3=V763584,
    #infldems
    infldems_1=V720756,infldems_2=V742394,infldems_3=V763585,
    #help minorities scale
    helpblks_1=V720629,helpblks_2=V742296,helpblks_3=V763264,
    #school busing
    busing_1=V720202,busing_2=V742288,busing_3=V763257,
    #job guarantee
    jobguar_1=V721067,jobguar_2=V742265,jobguar_3=V763758,
    #urban unrest scale
    unrest_1=V720670,unrest_2=V742273,unrest_3=V763767,
    #rights of accused
    accused_1=V720621,accused_2=V742281,accused_3=V763248,
    #partyid
    partyid_1=V720140,partyid_2=V742204,partyid_3=V763174,
    #lib-con scale
    polviews_1=V720652,polviews_2=V742305,polviews_3=V763286,
    #equal role for women
    eqrole_1=V720232,eqrole_2=V742302,eqrole_3=V763787,
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
    child_1 = ifelse(is.na(child_1), child_1a, child_1)) %>%
  select(id:child_1,   
         V742410:V742417,V763373:V763378) %>%
  mutate(age_1 = ifelse(age_1 == 0 & age_2 != 0, age_2-2, age_1),
         age_1 = ifelse(age_1 == 0 & age_3 != 0, age_3-4, age_1)) %>%
  mutate(age_1 = ifelse(age_1 == 0, NA, age_1))%>%
  select(-c(oldestkid_2, oldestkid_3,V742410:V763378))

anes80 <- read_dta("~/Dropbox/data/anes/anes1980/anes1980.dta")
clean_anes8 <- anes80 %>%
  mutate(id=VMP0004, age_1=VMP0325, age_2=VMP2404, age_3=VMP3386,
         partyid_1=VMP0191,partyid_2=VMP2212,partyid_3=VMP3212,
         polviews_1=VMP0121,polviews_2=VMP2125,polviews_3=VMP3213,
         thermcarter_1=VMP0039, thermcarter_2=VMP2038,thermcarter_3=VMP3095,
         thermregan_1=VMP0040,thermregan_2=VMP2039,thermregan_3=VMP3096,
         thermted_1=VMP0041,thermted_2=VMP2040,thermted_3=VMP3097,
         thermconnaly_1=VMP0042,thermconnaly_2=VMP2041,thermconnaly_3=VMP3098,
         thermford_1=VMP0043,thermford_2=VMP2042,thermford_3=VMP3099,
         thermbrown_1=VMP0044,thermbrown_2=VMP2043,thermbrown_3=VMP3100,
         thermbaker_1=VMP0045,thermbaker_2=VMP2044,thermbaker_3=VMP3101,
         thermmondale_1=VMP0046,thermmondale_2=VMP2045,thermmondale_3=VMP3102,
         thermbush_1=VMP0047,thermbush_2=VMP2046,thermbush_3=VMP3103,
         thermmcgovern_1=VMP0050,thermmcgovern_2=VMP2050,thermmcgovern_3=VMP3106,
         thermdems_1=VMP0052,thermdems_2=VMP2052,thermdems_3=VMP3109,
         thermreps_1=VMP0053,thermreps_2=VMP2053,thermreps_3=VMP3110,
         therminds_1=VMP0054,therminds_2=VMP2054,therminds_3=VMP3111,
         thermparties_1=VMP0055,thermparties_2=VMP2055,thermparties_3=VMP3112,
         defspend_1=VMP0133,defspend_2=VMP2137,defspend_3=VMP3234,
         spendserv_1=VMP0144,spendserv_2=VMP2159,spendserv_3=VMP3254,
         friendussr_1=VMP0155,friendussr_2=VMP2170,friendussr_3=VMP3264,
         inflate_1=VMP0166,inflate_2=VMP2181,inflate_3=VMP3274,
         energypol_1=VMP0177,energypol_2=VMP2196,energypol_3=VMP3314,
         rationgas_1=VMP0179,rationgas_2=VMP2198,rationgas_3=VMP3316,
         nukeplant_1=VMP0184,nukeplant_2=VMP2203,nukeplant_3=VMP3321,
         marital_1=recode(VMP0326, "1"="married", "2"="single/nm",
                          "3"="other", "4"="other", "5"="other",
                          "7"="married", "9"=NA_character_),
         marital_2=recode(VMP2377, "1"="married", "2"="single/nm",
                          "3"="other", "4"="other", "5"="other",
                          "7"="married", "9"=NA_character_),
         marital_3=recode(VMP3387, "1"="married", "2"="single/nm",
                          "3"="other", "4"="other", "5"="other",
                          "7"="married", "9"=NA_character_),
         ed_1=ifelse(VMP0327<12, "lessthan",
                     ifelse(VMP0327>=16, "ba", "hs"))) %>%
  select(id, marital_1:marital_3, ed_1,age_1:nukeplant_3)


anes90 <- read_dta("~/Dropbox/data/anes/anes9092/anes_mergedfile_1990to1992.dta")
clean_anes90 <- anes90 %>%
  mutate(id = V900004,
         age_1 = V900548,
         thermbush_1 = V900134, thermbush_2 = V912205,
         thermbush_3 = V923305,
         thermquayle_1 = V900137, thermquayle_2 = V912212,
         thermquayle_3 = V923308,
         thermjessie_1 = V900139,
         thermjessie_2 = V912211,
         thermjessie_3 = V923316,
         thermdems_1 = V900151, thermdems_2 = V912222,
         thermdems_3 = V923317,
         thermreps_1 = V900152, thermreps_2 = V912228,
         thermreps_3 = V923318,
         thermblacks_1 = V900155, thermblacks_2 = V912232,
         thermblacks_3 = V925323,
         thermcons_1 = V900156, thermcons_2 = V912220,
         thermcons_3 = V925319,
         thermfemvmt_1 = V900158, thermfemvmt_2 = V912239,
         thermfemvmt_3 = V925324,
         thermenviro_1 = V900160, thermenviro_2 = V912230,
         thermenviro_3 = V925329,
         thermlibs_1 = V900161, thermlibs_2 = V912226,
         thermlibs_3 = V925326,
         partyid_1 = V900320, partyid_2 = V912333,
         partyid_3 = V923634,
         betterecon_1 = V900390, betterecon_2 = V912415, 
         betterecon_3 = V923545,
         polviews_1 = V900406, polviews_2 = V912450,
         polviews_3 = V923509,
         toofar_1 = V900427, toofar_2 = V912705, 
         toofar_3 = V926025,
         treateq_1 = V900431, treateq_2 = V912703,
         treateq_3 = V926028,
         defspend_1 = V900439, defspend_2 = V912475,
         defspend_3 = V923707,
         affrmact_1 = V900464, affrmact_2 = V912561,
         affrmact_3 = V925936,
         newstyles_1 = V900500, newstyles_2 = V912704,
         newstyles_3 = V926118,
         adjmoral_1 = V900501, adjmoral_2 = V912702,
         adjmoral_3 = V926115,
         trustgov_1 = V900504, trustgov_2 = V912487,
         trustgov_3 = V926120,
         runbiz_1 = V900506, runbiz_2 = V912488,
         runbiz_3 = V926122,
         nosay_1 = V900509, nosay_2 = V912489,
         nosay_3 = V926102) %>%
  select(id:nosay_3)

canes90 <- clean_anes90 %>%
  zap_labels() %>%
  mutate(across(c(nosay_1, nosay_2, nosay_3,
                  adjmoral_1, adjmoral_2, adjmoral_3,
                  newstyles_1, newstyles_2, newstyles_3,
                  treateq_1, treateq_2, treateq_3,
                  toofar_1, toofar_2, toofar_3),
                ~recode(.x, "1"=1, "2"=2, "3"=3, 
                        "4"=4, "5"=5, "8"=3, "9"=NA_real_,
                        "0"=NA_real_)),
         across(c(runbiz_1, runbiz_2, runbiz_3),
                ~recode(.x, "1"=1, "8"=2, "5"=3, "9"=NA_real_,
                        "0"=NA_real_)),
         across(c(trustgov_1, trustgov_2, trustgov_3),
                ~recode(.x, "1"=1, "3"=2, "5"=3, "7"=3,
                        "8"=NA_real_, "9"=NA_real_,
                        "0"=NA_real_)),
         across(c(affrmact_1, affrmact_2, affrmact_3),
                ~recode(.x, "1"=1, "2"=2, "3"=3, "8"=3,
                        "4"=4, "5"=5, "9"=NA_real_,
                        "0"=NA_real_)),
         across(c(defspend_1, defspend_2, defspend_3,
                  polviews_1, polviews_2, polviews_3),
                ~recode(.x, "9"=NA_real_, "8"=4, "0"=4)),
         across(c(betterecon_1, betterecon_2),
                ~recode(.x, "1"=1, "3"=2, "5"=3, "6"=2, "8"=2,
                        "9"=NA_real_)),
         across(c(betterecon_3),
                ~recode(.x, "1"=1, "2"=3, "3"=2, "4"=2, "7"=2,
                        "8"=2, "9"=NA_real_)),
         across(c(partyid_1, partyid_2, partyid_3),
                ~recode(.x, "7"=NA_real_, "8"=3, "9"=NA_real_)),
         across(c(thermbush_1, thermbush_2, thermbush_3,
                  thermquayle_1, thermquayle_2, thermquayle_3,
                  thermjessie_1, thermjessie_2, thermjessie_3,
                  thermdems_1, thermdems_2, thermdems_3,
                  thermreps_1, thermreps_2, thermreps_3,
                  thermblacks_1, thermblacks_2, thermblacks_3,
                  thermcons_1, thermcons_2, thermcons_3,
                  thermfemvmt_1, thermfemvmt_2, thermfemvmt_3,
                  thermenviro_1, thermenviro_2, thermenviro_3,
                  thermlibs_1, thermlibs_2, thermlibs_3),
                ~ifelse(.x %in% c(997, 998), 50, ifelse(.x %in% c(888,999), NA, .x)))) %>%
  pivot_longer(thermbush_1:nosay_3) %>%
  separate(name, into = c("var", "wave")) %>%
  mutate(wave = paste0("y", wave)) %>%
  spread(wave, value) 



anes9 <- read_dta("~/Dropbox/data/anes/anes9297/anes_mergedfile_1992to1997.dta") 
clean_anes9 <- anes9 %>%
  #select by panel weight... 1316 cases.
  #filter(!is.na(V960004), V960004 != 0) %>%
  mutate(weight = V960004,
         govins_1 = V923716, govins_2 = V940950,
         govins_3 = V960479,
         jobguar_1 = V923718, jobguar_2 = V940930,
         jobguar_3 = V960483,
         spendserv_1 = V923701, spendserv_2 = V940940,
         spendserv_3 = V960450,
         govblks_1 = V923724, govblks_2 = V940936,
         govblks_3 = V960487,
         #women should have equal role (7-pt)
         eqrole_1 = V923801, eqrole_2 = V940928,
         eqrole_3 = V960543,
         #five-point scale
         eqop_1 = V926024, eqop_2 = V940914,
         eqop_3 = V961229,
         partyid_1 = V923634, partyid_2 = V940655,
         partyid_3 = V960420,
         polviews_1 = V923509, polviews_2 = V940839,
         polviews_3 = V961269,
         betterecon_1 = V923545, betterecon_2 = V940829,betterecon_3 = V960397,
         betterforeign_1 = V923546,betterforeign_2 = V940832,betterforeign_3 = V960398,
         betterhealth_1 = V923548,betterhealth_2 = V940833,betterhealth_3 = V960399,
         raisetax_1=V923550,raisetax_2=V940828,raisetax_3=V960407,
         affrmact_1=V925936,affrmact_2=V941002,affrmact_3=V961209,
         toofar_1=V926025,toofar_2=V940915,toofar_3=V961230,
         nochance_1=V926029,nochance_2=V940916,nochance_3=V961231,
         morechance_1=V926027,morechance_2=V940918,morechance_3=V961233,
         worryless_1=V926026,worryless_2=V940917,worryless_3=V961232,
         treateq_1=V926028,treateq_2=V940919,treateq_3=V961234,
         newstyles_1=V926118,newstyles_2=V941029,newstyles_3=V961247,
         moretol_1=V926116,moretol_2=V941032,moretol_3=V961250,
         adjmoral_1=V926115,adjmoral_2=V941030,adjmoral_3=V961248,
         tradties_1=V926117,tradties_2=V941031,tradties_3=V961249,
         dontcare_1=V926103,dontcare_2=V941037,dontcare_3=V961244,
         toocomplicated_1=V926104,toocomplicated_2=V941039,toocomplicated_3=V961246,
         govbiz_1=V926122,govbiz_2=V941035,govbiz_3=V961253,
         termlimits_1=V923747,termlimits_2=V940651,termlimits_3=V960412,
         stayhome_1=V923604,stayhome_2=V941019,stayhome_3=V960410,
         thermclinton_1=V925302,thermclinton_2=V940223,thermclinton_3=V961019,
         thermperot_1=V925303,thermperot_2=V940224,thermperot_3=V961021,
         thermgore_1=V923309,thermgore_2=V940227,thermgore_3=V960275,
         thermhrc_1=V923313,thermhrc_2=V940229,thermhrc_3=V960281,
         thermjessie_1=V923316,thermjessie_2=V940228,thermjessie_3=V960283,
         thermblacks_1=V925323,thermblacks_2=V940305,thermblacks_3=V961029,
         thermwhites_1=V925333,thermwhites_2=V940313,thermwhites_3=V961030,
         thermlibs_1=V925326,thermlibs_2=V940311,thermlibs_3=V961032,
         thermcons_1=V925319,thermcons_2=V940306,thermcons_3=V961031,
         thermunion_1=V925316,thermunion_2=V940307,thermunion_3=V961033,
         thermbiz_1=V925322,thermbiz_2=V940314,thermbiz_3=V961034,
         thermpoor_1=V925320,thermpoor_2=V940312,thermpoor_3=V961035,
         thermwelf_1=V925318,thermwelf_2=V940309,thermwelf_3=V961036,
         thermhisp_1=V925327,thermhisp_2=V940304,thermhisp_3=V961037,
         thermenviro_1= V925329,thermenviro_2=V940310,thermenviro_3=V961041,
         thermfelib_1=V925324,thermfelib_2=V940308,thermfelib_3=V961039,
         fschild_1=V923813,fschild_2=V940824,fschild_3=V960564,
         fscrime_1=V923814,fscrime_2=V940825,fscrime_3=V960563,
         fsaids_1=V923727,fsaids_2=V940821,fsaids_3=V960498,
         fsschools_1=V923818,fsschools_2=V940823,fsschools_3=V960562,
         fswelf_1=V923726,fswelf_2=V940820,fswelf_3=V960497,
         fsfood_1=V923725,fsfood_2=V940822,fsfood_3=V960496,
         fsenviro_1=V923815,fsenviro_2=V940817,fsenviro_3=V960561,
         fssocsec_1=V923811,fssocsec_2=V940819,fssocsec_3=V960560,
         incimm_1=V926235,incimm_2=V941016,incimm_3=V961325,
         homomil_1=V925926,homomil_2=V937331,homomil_3=V961196,
         homojob_1=V925924,homojob_2=V937327,homojob_3=V961194,
         abort_1=V923732,abort_2=V941014,abort_3=V960503,
         prayer_1=V925945,prayer_2=V941020,prayer_3=V961214,
         govcrooked_1=V926123,govcrooked_2=V941036,govcrooked_3=V961254,
         trustgov_1=V926120,trustgov_2=V941033,trustgov_3=V961251,
         wastetax_1=V926121,wastetax_2=V941034,wastetax_3=V961252,
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
         #marital status
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
         age_1 = V923903,
         age_2 = V941203,
         age_3 = V960605,
         nkids_1 = ifelse(V923079 == 9, 0, V923079) + ifelse(V923080 == 9, 0, V923080) + 
           ifelse(V923081 == 9, 0, V923081) + ifelse(V923081 == 9, 0, V923081),
         childs_1 = ifelse(nkids_1 > 0, 1, 0),
         #any children?
         childs_2 = ifelse(V941428 %in% c(8,9), NA, V941428),
         childs_2 = ifelse(V941428 %in% c(1,2), 1, childs_2),
         childs_2 = ifelse(V941428 %in% c(5), 0, childs_2)) %>%
  select(weight,
         govins_1:childs_2, 
         #children
         V923079:V923082,
         V960048:V960051,
         V941428, V923903) %>%
  mutate(age_1 = ifelse(is.na(age_1) & !is.na(age_2),
                        age_2 - 2, age_1),
         age_1 = ifelse(is.na(age_1) & !is.na(age_3),
                        age_3 - 4, age_1)) %>%
  select(-c(V923079:V923903))

anes2k <- read_dta("~/Dropbox/data/anes/anes0004/anes_mergedfile_2000to2004.dta")

clean_anes2k <- anes2k %>%
  mutate(id=ID,
         stayhome_1=M000514,stayhome_2=M023033,stayhome_3=M045143,
         dontcare_1=M001527,dontcare_2=M025172,dontcare_3=M045147,
         nosay_1=M001528,nosay_2=M025173,nosay_3=M045148,
         govbiz_1=M001536,govbiz_2=M025176,govbiz_3=M045151,
         electfair_1=M001291,electfair_2=M025042,electfair_3=M045056,
         thermbush_1=M001294,thermbush_2=M025043,thermbush_3=M045007,
         thermdick_1=M000367,thermdick_2=M023011,thermdick_3=M045009,
         thermgore_1=M001293,thermgore_2=M023012,thermgore_3=M045010,
         thermnader_1=M001295,thermnader_2=M023014,thermnader_3=M045012,
         thermjessie_1=M001296,thermjessie_2=M023018,thermjessie_3=M045016,
         thermhrc_1=M000368,thermhrc_2=M023020,thermhrc_3=M045018,
         thermcourt_1=M001304,thermcourt_2=M025051,thermcourt_3=M045019,
         thermcong_1=M001305,thermcong_2=M025052,thermcong_3=M045020,
         thermmil_1=M001306,thermmil_2=M025053,thermmil_3=M045021,
         thermfed_1=M001307,thermfed_2=M025054,thermfed_3=M045022,
         thermblacks_1=M001308,thermblacks_2=M025055,thermblacks_3=M045023,
         thermwhites_1=M001309,thermwhites_2=M025056,thermwhites_3=M045024,
         thermcons_1=M001310,thermcons_2=M025057,thermcons_3=M045025,
         thermlibs_1=M001311,thermlibs_2=M025058,thermlibs_3=M045026,
         thermunion_1=M001312,thermunion_2=M025059,thermunion_3=M045027,
         thermbiz_1=M001313,thermbiz_2=M025060,thermbiz_3=M045028,
         thermpoor_1=M001314,thermpoor_2=M025061,thermpoor_3=M045029,
         thermwelf_1=M001315,thermwelf_2=M025062,thermwelf_3=M045030,
         thermhisp_1=M001316,thermhisp_2=M025063,thermhisp_3=M045031,
         thermfund_1=M001317,thermfund_2=M025064,thermfund_3=M045032,
         thermold_1=M001319,thermold_2=M025065,thermold_3=M045033,
         thermenviro_1=M001320,thermenviro_2=M025066,thermenviro_3=M045034,
         thermlgbt_1=M001321,thermlgbt_2=M025067,thermlgbt_3=M045035,
         thermfems_1=M001326,thermfems_2=M025071,thermfems_3=M045039,
         partyid_1=M000523,partyid_2=M023038X,partyid_3=M045058x,
         fsenviro_1=M000682,fsenviro_2=M025113X,fsenviro_3=M045068,
         fsaids_1=M000677,fsaids_2=M025106X,fsaids_3=M045069,
         fswelf_1=M000676,fswelf_2=M025107X,fswelf_3=M045070,
         fsschools_1=M000683,fsschools_2=ifelse(is.na(M025108X), M025108Y, M025108X),
         fsschools_3=M045071x,
         fscrime_1=M000684,fscrime_2=M025109X,fscrime_3=M045072,
         fschild_1=M000685,fschild_2=M025110X,fschild_3=M045073,
         fspoor_1=M000680,fspoor_2=ifelse(is.na(M025115X), M025115Y, M025115X),
         fspoor_3=M045075x,
         fsforeignaid_1=M000678,fsforeignaid_2=M025116X,fsforeignaid_3=M045076,
         fssocsec_1=M000681,fssocsec_2=M025117X,fssocsec_3=M045077,
         fsblacks_1=M000687,fsblacks_2=M025119X,fsblacks_3=M045079,
         finref_1=M001490,finref_2=M025146,finref_3=M045092,
         trustgov_1=M001534,trustgov_2=M025174,trustgov_3=M045149,
         wastetax_1=M001535,wastetax_2=M025175,wastetax_3=M045150,
         govcrooked_1=M001537,govcrooked_2=M025177,govcrooked_3=M045152,
         electattn_1=M001538,electattn_2=M025178,electattn_3=M045153,
         age_1=M000908,age_2=M023126X,age_3=M045193,
         marital_1=M000909,marital_2=M023127A,
         marital_3=M045176,
         ed_1=M000913,
         ed_2=M023131) %>%
  select(id:ed_2)


## GSS Panels
g6 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel06.dta")

clean_g6 <- g6 %>%
  zap_labels() %>%
  mutate(natspac_1 = ifelse(is.na(natspac_1), natspacy_1, natspac_1), 
         natenvir_1 = ifelse(is.na(natenvir_1), natenviy_1, natenvir_1), 
         natheal_1 = ifelse(is.na(natheal_1), nathealy_1, natheal_1), 
         natcity_1 = ifelse(is.na(natcity_1), natcityy_1, natcity_1), 
         natcrime_1 = ifelse(is.na(natcrime_1), natcrimy_1, natcrime_1), 
         natdrug_1 = ifelse(is.na(natdrug_1), natdrugy_1, natdrug_1), 
         nateduc_1 = ifelse(is.na(nateduc_1), nateducy_1, nateduc_1), 
         natrace_1 = ifelse(is.na(natrace_1), natracey_1, natrace_1), 
         natarms_1 = ifelse(is.na(natarms_1), natarmsy_1, natarms_1), 
         nataid_1 = ifelse(is.na(nataid_1), nataidy_1, nataid_1), 
         natfare_1 = ifelse(is.na(natfare_1), natfarey_1, natfare_1),
         natspac_2 = ifelse(is.na(natspac_2), natspacy_2, natspac_2), 
         natenvir_2 = ifelse(is.na(natenvir_2), natenviy_2, natenvir_2), 
         natheal_2 = ifelse(is.na(natheal_2), nathealy_2, natheal_2), 
         natcity_2 = ifelse(is.na(natcity_2), natcityy_2, natcity_2), 
         natcrime_2 = ifelse(is.na(natcrime_2), natcrimy_2, natcrime_2), 
         natdrug_2 = ifelse(is.na(natdrug_2), natdrugy_2, natdrug_2), 
         nateduc_2 = ifelse(is.na(nateduc_2), nateducy_2, nateduc_2), 
         natrace_2 = ifelse(is.na(natrace_2), natracey_2, natrace_2), 
         natarms_2 = ifelse(is.na(natarms_2), natarmsy_2, natarms_2), 
         nataid_2 = ifelse(is.na(nataid_2), nataidy_2, nataid_2), 
         natfare_2 = ifelse(is.na(natfare_2), natfarey_2, natfare_2),
         natspac_3 = ifelse(is.na(natspac_3), natspacy_3, natspac_3), 
         natenvir_3 = ifelse(is.na(natenvir_3), natenviy_3, natenvir_3), 
         natheal_3 = ifelse(is.na(natheal_3), nathealy_3, natheal_3), 
         natcity_3 = ifelse(is.na(natcity_3), natcityy_3, natcity_3), 
         natcrime_3 = ifelse(is.na(natcrime_3), natcrimy_3, natcrime_3), 
         natdrug_3 = ifelse(is.na(natdrug_3), natdrugy_3, natdrug_3), 
         nateduc_3 = ifelse(is.na(nateduc_3), nateducy_3, nateduc_3), 
         natrace_3 = ifelse(is.na(natrace_3), natracey_3, natrace_3), 
         natarms_3 = ifelse(is.na(natarms_3), natarmsy_3, natarms_3), 
         nataid_3 = ifelse(is.na(nataid_3), nataidy_3, nataid_3), 
         natfare_3 = ifelse(is.na(natfare_3), natfarey_3, natfare_3)) %>%
  select(wtpannr123,
         age_1, age_2, age_3, 
         marital_1, marital_2, marital_3,
         childs_1, childs_2, childs_3,
         degree_1, degree_2, degree_3,
         natspac_1, natspac_2, natspac_3, natenvir_1, natenvir_2,
         natenvir_3, natheal_1, natheal_2, natheal_3, natcity_1, 
         natcity_2, natcity_3, natcrime_1, natcrime_2, natcrime_3,
         natdrug_1, natdrug_2, natdrug_3, nateduc_1, nateduc_2,
         nateduc_3, natrace_1, natrace_2, natrace_3, natarms_1,
         natarms_2, natarms_3, nataid_1, nataid_2, nataid_3, 
         natfare_1, natfare_2, natfare_3, natroad_1, natroad_2,
         natroad_3, natsoc_1, natsoc_2, natsoc_3, natmass_1,
         natmass_2, natmass_3, natpark_1, natpark_2, natpark_3,
         natchld_1, natchld_2, natchld_3, natsci_1, natsci_2, natsci_3,
         cappun_1, cappun_2, cappun_3, grass_1, grass_2,
         grass_3, prayer_1, prayer_2, prayer_3, courts_1,
         courts_2, courts_3, gunlaw_1, gunlaw_2, gunlaw_3,
         helppoor_1, helppoor_2, helppoor_3,
         helpnot_1, helpnot_2, helpnot_3,
         helpsick_1, helpsick_2, helpsick_3,
         helpblk_1, helpblk_2, helpblk_3,
         eqwlth_1, eqwlth_2, eqwlth_3,
         tax_1, tax_2, tax_3,
         marhomo_1, marhomo_2, marhomo_3,
         sexeduc_1, sexeduc_2, sexeduc_3,
         pillok_1, pillok_2, pillok_3,
         affrmact_1, affrmact_2, affrmact_3,
         fejobaff_1, fejobaff_2, fejobaff_3,
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

g8 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel08.dta")

clean_g8 <- g8 %>%
  zap_labels() %>%
  mutate(natspac_1 = ifelse(is.na(natspac_1), natspacy_1, natspac_1), 
         natenvir_1 = ifelse(is.na(natenvir_1), natenviy_1, natenvir_1), 
         natheal_1 = ifelse(is.na(natheal_1), nathealy_1, natheal_1), 
         natcity_1 = ifelse(is.na(natcity_1), natcityy_1, natcity_1), 
         natcrime_1 = ifelse(is.na(natcrime_1), natcrimy_1, natcrime_1), 
         natdrug_1 = ifelse(is.na(natdrug_1), natdrugy_1, natdrug_1), 
         nateduc_1 = ifelse(is.na(nateduc_1), nateducy_1, nateduc_1), 
         natrace_1 = ifelse(is.na(natrace_1), natracey_1, natrace_1), 
         natarms_1 = ifelse(is.na(natarms_1), natarmsy_1, natarms_1), 
         nataid_1 = ifelse(is.na(nataid_1), nataidy_1, nataid_1), 
         natfare_1 = ifelse(is.na(natfare_1), natfarey_1, natfare_1),
         natspac_2 = ifelse(is.na(natspac_2), natspacy_2, natspac_2), 
         natenvir_2 = ifelse(is.na(natenvir_2), natenviy_2, natenvir_2), 
         natheal_2 = ifelse(is.na(natheal_2), nathealy_2, natheal_2), 
         natcity_2 = ifelse(is.na(natcity_2), natcityy_2, natcity_2), 
         natcrime_2 = ifelse(is.na(natcrime_2), natcrimy_2, natcrime_2), 
         natdrug_2 = ifelse(is.na(natdrug_2), natdrugy_2, natdrug_2), 
         nateduc_2 = ifelse(is.na(nateduc_2), nateducy_2, nateduc_2), 
         natrace_2 = ifelse(is.na(natrace_2), natracey_2, natrace_2), 
         natarms_2 = ifelse(is.na(natarms_2), natarmsy_2, natarms_2), 
         nataid_2 = ifelse(is.na(nataid_2), nataidy_2, nataid_2), 
         natfare_2 = ifelse(is.na(natfare_2), natfarey_2, natfare_2),
         natspac_3 = ifelse(is.na(natspac_3), natspacy_3, natspac_3), 
         natenvir_3 = ifelse(is.na(natenvir_3), natenviy_3, natenvir_3), 
         natheal_3 = ifelse(is.na(natheal_3), nathealy_3, natheal_3), 
         natcity_3 = ifelse(is.na(natcity_3), natcityy_3, natcity_3), 
         natcrime_3 = ifelse(is.na(natcrime_3), natcrimy_3, natcrime_3), 
         natdrug_3 = ifelse(is.na(natdrug_3), natdrugy_3, natdrug_3), 
         nateduc_3 = ifelse(is.na(nateduc_3), nateducy_3, nateduc_3), 
         natrace_3 = ifelse(is.na(natrace_3), natracey_3, natrace_3), 
         natarms_3 = ifelse(is.na(natarms_3), natarmsy_3, natarms_3), 
         nataid_3 = ifelse(is.na(nataid_3), nataidy_3, nataid_3), 
         natfare_3 = ifelse(is.na(natfare_3), natfarey_3, natfare_3)) %>%
  select(wtpannr123,
         age_1, age_2, age_3, 
         marital_1, marital_2, marital_3,
         childs_1, childs_2, childs_3,
         degree_1, degree_2, degree_3,
         natspac_1, natspac_2, natspac_3, natenvir_1, natenvir_2,
         natenvir_3, natheal_1, natheal_2, natheal_3, natcity_1, 
         natcity_2, natcity_3, natcrime_1, natcrime_2, natcrime_3,
         natdrug_1, natdrug_2, natdrug_3, nateduc_1, nateduc_2,
         nateduc_3, natrace_1, natrace_2, natrace_3, natarms_1,
         natarms_2, natarms_3, nataid_1, nataid_2, nataid_3, 
         natfare_1, natfare_2, natfare_3, natroad_1, natroad_2,
         natroad_3, natsoc_1, natsoc_2, natsoc_3, natmass_1,
         natmass_2, natmass_3, natpark_1, natpark_2, natpark_3,
         natchld_1, natchld_2, natchld_3, natsci_1, natsci_2, natsci_3,
         cappun_1, cappun_2, cappun_3, grass_1, grass_2,
         grass_3, prayer_1, prayer_2, prayer_3, courts_1,
         courts_2, courts_3, gunlaw_1, gunlaw_2, gunlaw_3,
         helppoor_1, helppoor_2, helppoor_3,
         helpnot_1, helpnot_2, helpnot_3,
         helpsick_1, helpsick_2, helpsick_3,
         helpblk_1, helpblk_2, helpblk_3,
         eqwlth_1, eqwlth_2, eqwlth_3,
         tax_1, tax_2, tax_3,
         marhomo_1, marhomo_2, marhomo_3,
         sexeduc_1, sexeduc_2, sexeduc_3,
         pillok_1, pillok_2, pillok_3,
         affrmact_1, affrmact_2, affrmact_3,
         fejobaff_1, fejobaff_2, fejobaff_3,
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
g10 <- read_dta("~/Dropbox/data/gss_data/gsspanels/gsspanel10.dta")

clean_g10 <- g10 %>%
  zap_labels() %>%
  mutate(natspac_1 = ifelse(is.na(natspac_1), natspacy_1, natspac_1), 
         natenvir_1 = ifelse(is.na(natenvir_1), natenviy_1, natenvir_1), 
         natheal_1 = ifelse(is.na(natheal_1), nathealy_1, natheal_1), 
         natcity_1 = ifelse(is.na(natcity_1), natcityy_1, natcity_1), 
         natcrime_1 = ifelse(is.na(natcrime_1), natcrimy_1, natcrime_1), 
         natdrug_1 = ifelse(is.na(natdrug_1), natdrugy_1, natdrug_1), 
         nateduc_1 = ifelse(is.na(nateduc_1), nateducy_1, nateduc_1), 
         natrace_1 = ifelse(is.na(natrace_1), natracey_1, natrace_1), 
         natarms_1 = ifelse(is.na(natarms_1), natarmsy_1, natarms_1), 
         nataid_1 = ifelse(is.na(nataid_1), nataidy_1, nataid_1), 
         natfare_1 = ifelse(is.na(natfare_1), natfarey_1, natfare_1),
         natspac_2 = ifelse(is.na(natspac_2), natspacy_2, natspac_2), 
         natenvir_2 = ifelse(is.na(natenvir_2), natenviy_2, natenvir_2), 
         natheal_2 = ifelse(is.na(natheal_2), nathealy_2, natheal_2), 
         natcity_2 = ifelse(is.na(natcity_2), natcityy_2, natcity_2), 
         natcrime_2 = ifelse(is.na(natcrime_2), natcrimy_2, natcrime_2), 
         natdrug_2 = ifelse(is.na(natdrug_2), natdrugy_2, natdrug_2), 
         nateduc_2 = ifelse(is.na(nateduc_2), nateducy_2, nateduc_2), 
         natrace_2 = ifelse(is.na(natrace_2), natracey_2, natrace_2), 
         natarms_2 = ifelse(is.na(natarms_2), natarmsy_2, natarms_2), 
         nataid_2 = ifelse(is.na(nataid_2), nataidy_2, nataid_2), 
         natfare_2 = ifelse(is.na(natfare_2), natfarey_2, natfare_2),
         natspac_3 = ifelse(is.na(natspac_3), natspacy_3, natspac_3), 
         natenvir_3 = ifelse(is.na(natenvir_3), natenviy_3, natenvir_3), 
         natheal_3 = ifelse(is.na(natheal_3), nathealy_3, natheal_3), 
         natcity_3 = ifelse(is.na(natcity_3), natcityy_3, natcity_3), 
         natcrime_3 = ifelse(is.na(natcrime_3), natcrimy_3, natcrime_3), 
         natdrug_3 = ifelse(is.na(natdrug_3), natdrugy_3, natdrug_3), 
         nateduc_3 = ifelse(is.na(nateduc_3), nateducy_3, nateduc_3), 
         natrace_3 = ifelse(is.na(natrace_3), natracey_3, natrace_3), 
         natarms_3 = ifelse(is.na(natarms_3), natarmsy_3, natarms_3), 
         nataid_3 = ifelse(is.na(nataid_3), nataidy_3, nataid_3), 
         natfare_3 = ifelse(is.na(natfare_3), natfarey_3, natfare_3)) %>%
  select(WTPANNR123,
         age_1, age_2, age_3, 
         marital_1, marital_2, marital_3,
         childs_1, childs_2, childs_3,
         degree_1, degree_2, degree_3,
         natspac_1, natspac_2, natspac_3, natenvir_1, natenvir_2,
         natenvir_3, natheal_1, natheal_2, natheal_3, natcity_1, 
         natcity_2, natcity_3, natcrime_1, natcrime_2, natcrime_3,
         natdrug_1, natdrug_2, natdrug_3, nateduc_1, nateduc_2,
         nateduc_3, natrace_1, natrace_2, natrace_3, natarms_1,
         natarms_2, natarms_3, nataid_1, nataid_2, nataid_3, 
         natfare_1, natfare_2, natfare_3, natroad_1, natroad_2,
         natroad_3, natsoc_1, natsoc_2, natsoc_3, natmass_1,
         natmass_2, natmass_3, natpark_1, natpark_2, natpark_3,
         natchld_1, natchld_2, natchld_3, natsci_1, natsci_2, natsci_3,
         cappun_1, cappun_2, cappun_3, grass_1, grass_2,
         grass_3, prayer_1, prayer_2, prayer_3, courts_1,
         courts_2, courts_3, gunlaw_1, gunlaw_2, gunlaw_3,
         helppoor_1, helppoor_2, helppoor_3,
         helpnot_1, helpnot_2, helpnot_3,
         helpsick_1, helpsick_2, helpsick_3,
         helpblk_1, helpblk_2, helpblk_3,
         eqwlth_1, eqwlth_2, eqwlth_3,
         tax_1, tax_2, tax_3,
         marhomo_1, marhomo_2, marhomo_3,
         sexeduc_1, sexeduc_2, sexeduc_3,
         pillok_1, pillok_2, pillok_3,
         affrmact_1, affrmact_2, affrmact_3,
         fejobaff_1, fejobaff_2, fejobaff_3,
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


canes5 <- clean_anes5 %>%
  mutate(id = 1:nrow(clean_anes5)) %>%
  select(-c(ed_2, ed_3, age_2, age_3, marital_2, marital_3, child_2, child_3,
            sex_1:race_3)) %>%
  zap_labels() %>%
  mutate(ed_1 = factor(ed_1),
         ed_1 = relevel(ed_1, ref = "hs"),
         marital_1 = factor(marital_1),
         marital_1 = relevel(marital_1, ref = "married")) %>%
  mutate(across(c(helpblk_1, helpblk_3, integrate_1, integrate_3,
                  govjob_1, govjob_3, fgnaid_1, fgnaid_3, 
                  bldschls_1, bldschls_3, fghtcomm_1, fghtcomm_3,
                  stayhome_1, stayhome_3, prvtpwr_1, prvtpwr_3), 
                ~recode(.x, "0"=3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3,
                        "9"=NA_real_)),
         across(c(helpblk_2, integrate_2, govjob_2, fgnaid_2,
                  bldschls_2, fghtcomm_2, stayhome_2, prvtpwr_2), 
                ~recode(.x, "7"=3, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "8"=3,
                        "9"=NA_real_)),
         across(c(clsrjob_1, clsrjob_2, clsrjob_3,
                  clsraid_1, clsraid_2, clsraid_3,
                  clsrtreat_1, clsrtreat_2, clsrtreat_3,
                  clsrbuild_1, clsrbuild_2, clsrbuild_3,
                  clsroverseas_1, clsroverseas_2, clsroverseas_3,
                  clsrprvtpwr_1, clsrprvtpwr_2, clsrprvtpwr_3,
                  clsrdeseg_1, clsrdeseg_2, clsrdeseg_3),
                ~recode(.x, "0"=NA_real_, "1"=1, "3"=2, "5"=3, "8"=2,
                         "9"=NA_real_)),
         across(c(partyid_1, partyid_2, partyid_3), 
                ~recode(.x, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, 
                        "6"=7, "7"=NA_real_, "8"=NA_real_, "9"=NA_real_))) %>%
  pivot_longer(helpblk_1:clsrdeseg_3) %>%
  separate(name, into = c("var", "wave")) %>%
  mutate(wave = paste0("y", wave)) %>%
  spread(wave, value) 

canes7 <- clean_anes7 %>%
  mutate(id = 1:nrow(clean_anes7)) %>%
  select(-c(ed_2, ed_3, age_2, age_3, marital_2, marital_3, child_2, child_3)) %>%
  zap_labels() %>%
  mutate(ed_1 = factor(ed_1),
         ed_1 = relevel(ed_1, ref = "hs"),
         marital_1 = factor(marital_1),
         marital_1 = relevel(marital_1, ref = "married")) %>%
  mutate(across(c(helpblks_1, helpblks_2, helpblks_3,
                  busing_1, busing_2, busing_3,
                  jobguar_1, jobguar_2, jobguar_3,
                  unrest_1, unrest_2, unrest_3,
                  accused_1, accused_2, accused_3,
                  eqrole_1, eqrole_2, eqrole_3,
                  polviews_1, polviews_2, polviews_3), 
                ~recode(.x, "0"=4, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                        "7"=7, "8"= 4,"9"=NA_real_)),
         across(c(toofast_1, toofast_2, toofast_3,
                  nosay_1,nosay_2,nosay_3,
                  votesay_1,votesay_2,votesay_3,
                  toocomplicated_1,toocomplicated_2,
                  toocomplicated_3,
                  dontcare_1,dontcare_2,dontcare_3,
                  losetouch_1,losetouch_2,losetouch_3,
                  onlyvotes_1,onlyvotes_2,onlyvotes_3,
                  wastetax_1,wastetax_2,wastetax_3),
                ~recode(.x, "1"=1, "3"=2, "5"=3, "8"=3,
                        "9"=NA_real_, "0"=NA_real_)),
         across(c(govdoright_1,govdoright_2,govdoright_3,
                  govcrooked_1,govcrooked_2,govcrooked_3,
                  partyattend_1,partyattend_2,partyattend_3,
                  repattend_1,repattend_2,repattend_3),
                ~recode(.x, "1"=1, "3"=2, "5"=3, "7"=3, "8"=NA_real_,
                        "9"=NA_real_, "0"=NA_real_)),
         across(c(runforfew_1,runforfew_2,runforfew_3,
                  govcapable_1,govcapable_2,govcapable_3),
                ~recode(.x, "1"=1, "5"=3, "7"=2, "8"=2,
                        "9"=NA_real_)),
         across(c(thermscoop_1, thermscoop_2,thermscoop_3,
                  thermwallace_1,thermwallace_2,thermwallace_3,
                  thermnixon_1,thermnixon_2,thermnixon_3,
                  thermted_1,thermted_2,thermted_3,
                  thermhump_1,thermhump_2,thermhump_3,
                  thermbiz_1,thermbiz_2,thermbiz_3,
                  thermlibs_1,thermlibs_2,thermlibs_3,
                  thermcops_1,thermcops_2,thermcops_3,
                  thermmil_1,thermmil_2,thermmil_3,
                  thermwhite_1,thermwhite_2,thermwhite_3,
                  thermdems_1,thermdems_2,thermdems_3,
                  thermblacks_1,thermblacks_2,thermblacks_3,
                  thermreps_1,thermreps_2,thermreps_3,
                  thermunion_1,thermunion_2,thermunion_3,
                  thermcons_1,thermcons_2,thermcons_3),
                ~ifelse(.x == 98, 50, ifelse(.x > 98, NA_real_, .x))),
         across(c(influnion_1,influnion_2,influnion_3,
                  inflbiz_1,inflbiz_2,inflbiz_3,
                  inflblacks_1,inflblacks_2,inflblacks_3,
                  inflyoung_1,inflyoung_2,inflyoung_3,
                  inflreps_1,inflreps_2,inflreps_3,
                  inflwelf_1,inflwelf_2,inflwelf_3,
                  inflold_1,inflold_2,inflold_3,
                  infldems_1,infldems_2,infldems_3),
                ~recode(.x, "1"=1, "2"=2, "3"=3, "0"=NA_real_, 
                        "8"=2, "9"=NA_real_)),
         across(c(partyid_1, partyid_2, partyid_3), 
                ~recode(.x, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, 
                        "6"=7, "7"=NA_real_, "8"=NA_real_, "9"=NA_real_))) %>%
  pivot_longer(toofast_1:eqrole_3) %>%
  separate(name, into = c("var", "wave")) %>%
  mutate(wave = paste0("y", wave)) %>%
  spread(wave, value) 


canes8 <- clean_anes8 %>%
  zap_labels() %>%
  mutate(
    across(c(partyid_1, partyid_2, partyid_3),
           ~ifelse(.x > 6, NA, .x)),
    across(c(polviews_1, polviews_2, polviews_3),
           ~ifelse(.x < 1 |.x > 7, NA, .x)),
    across(c(thermcarter_1, thermcarter_2,thermcarter_3,
             thermregan_1,thermregan_2,thermregan_3,
             thermted_1,thermted_2,thermted_3,
             thermconnaly_1,thermconnaly_2,thermconnaly_3,
             thermford_1,thermford_2,thermford_3,
             thermbrown_1,thermbrown_2,thermbrown_3,
             thermbaker_1,thermbaker_2,thermbaker_3,
             thermmondale_1,thermmondale_2,thermmondale_3,
             thermbush_1,thermbush_2,thermbush_3,
             thermmcgovern_1,thermmcgovern_2,thermmcgovern_3,
             thermdems_1,thermdems_2,thermdems_3,
             thermreps_1,thermreps_2,thermreps_3,
             therminds_1,therminds_2,therminds_3,
             thermparties_1,thermparties_2,thermparties_3),
           ~ifelse(.x == 998, 50, ifelse(.x %in% c( 999, 990), NA, .x))),
    across(c(defspend_1, defspend_2, defspend_3,
             spendserv_1,spendserv_2,spendserv_3,
             friendussr_1,friendussr_2,friendussr_3,
             inflate_1,inflate_2,inflate_3),
           ~ifelse(.x == 0|.x == 8, 4, ifelse(.x < 1 | .x > 7, NA, .x))),
    across(c(defspend_1, defspend_2, defspend_3),
           ~ifelse(.x == 1, 2, .x)),
    across(c(energypol_1, energypol_2, energypol_3,
             rationgas_1,rationgas_2,rationgas_3),
           ~recode(.x, "1"=1, "5"=3, "6"=3, 
                   "8"=2, "9"=NA_real_)),
    across(c(nukeplant_1,nukeplant_2,nukeplant_3),
           ~ifelse(.x > 3, NA, .x))
  ) %>%
  pivot_longer(partyid_1:nukeplant_3) %>%
  separate(name, into = c("var", "wave")) %>%
  mutate(wave = paste0("y", wave)) %>%
  spread(wave, value) 



canes9 <- clean_anes9 %>%
  mutate(id = 1:nrow(clean_anes9)) %>%
  select(-c(ed_2, ed_3, age_2, age_3, marital_2, marital_3)) %>%
  zap_labels() %>%
  mutate(ed_1 = factor(ed_1),
         ed_1 = relevel(ed_1, ref = "hs"),
         marital_1 = factor(marital_1),
         marital_1 = relevel(marital_1, ref = "married")) %>%
  mutate(across(c(govins_1, govins_2, govins_3,
                  jobguar_1, jobguar_2, jobguar_3,
                  spendserv_1, spendserv_2, spendserv_3,
                  govblks_1, govblks_2, govblks_3,
                  eqrole_1, eqrole_2, eqrole_3,
                  polviews_1, polviews_2, polviews_3), 
                ~recode(.x, "0"=4, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6,
                        "7"=7, "8"=4,"9"=NA_real_)),
         across(c(partyid_1, partyid_2, partyid_3), 
                ~recode(.x, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, 
                        "6"=7, "7"=NA_real_, "8"=NA_real_, "9"=NA_real_)),
         across(c(eqop_1, eqop_2, eqop_3),
                ~recode(.x, "0"=NA_real_, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, 
                        "8"= 3,"9"=NA_real_)),
         across(c(betterecon_1, betterecon_3,
                  betterforeign_1, betterforeign_3,
                  betterhealth_1, betterhealth_3,
                  raisetax_3),
                ~recode(.x, "1"=1, "2"=3, "3"=2, "0"=NA_real_, "7"=2, "8"=NA_real_,
                        "9"=NA_real_, "0"=NA_real_)),
         across(c(betterecon_2, betterforeign_2, betterhealth_2,
                  raisetax_1, raisetax_2),
                ~recode(.x, "1"=1, "3"=2, "5"=3, "6"=2, "8"=NA_real_,
                        "9"=NA_real_)),
         across(c(affrmact_1, affrmact_2, affrmact_3),
                ~recode(.x, "0"=NA_real_, "1"=1, "2"=2, "4"=3, "5"=4, 
                        "8"=NA_real_, "9"=NA_real_)),
         across(c(toofar_1, toofar_2, toofar_3,
                  nochance_1,nochance_2,nochance_3,
                  morechance_1,morechance_2,morechance_3,
                  worryless_1,worryless_2,worryless_3,
                  treateq_1,treateq_2,treateq_3,
                  newstyles_1,newstyles_2,newstyles_3,
                  moretol_1,moretol_2,moretol_3,
                  adjmoral_1,adjmoral_2,adjmoral_3,
                  tradties_1,tradties_2,tradties_3,
                  dontcare_1,dontcare_2,dontcare_3,
                  toocomplicated_1,toocomplicated_2,toocomplicated_3,
                  incimm_1, incimm_2, incimm_3,
                  homomil_1,homomil_2,homomil_3,
                  homojob_1,homojob_2,homojob_3),
                ~recode(.x, "0"=NA_real_, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, 
                        "8"=3, "9"=NA_real_)),
         across(c(govbiz_1,govbiz_2,
                  termlimits_1,termlimits_2,termlimits_3,
                  stayhome_1,stayhome_2,stayhome_3),
                ~recode(.x, "0"=NA_real_, "1"=1, "5"=3,
                        "8"=2, "9"=NA_real_, "7"=NA_real_)),
         across(c(govbiz_3),
                ~recode(.x, "0"=NA_real_, "1"=3, "5"=1,
                        "8"=2, "9"=NA_real_)),
         across(c(thermclinton_1,thermclinton_2,thermclinton_3,
                  thermperot_1,thermperot_2,thermperot_3,
                  thermgore_1,thermgore_2,thermgore_3,
                  thermhrc_1,thermhrc_2,thermhrc_3,
                  thermjessie_1,thermjessie_2,thermjessie_3,
                  thermblacks_1,thermblacks_2,thermblacks_3,
                  thermwhites_1,thermwhites_2,thermwhites_3,
                  thermlibs_1,thermlibs_2,thermlibs_3,
                  thermcons_1,thermcons_2,thermcons_3,
                  thermunion_1,thermunion_2,thermunion_3,
                  thermbiz_1,thermbiz_2,thermbiz_3,
                  thermpoor_1,thermpoor_2,thermpoor_3,
                  thermwelf_1,thermwelf_2,thermwelf_3,
                  thermhisp_1,thermhisp_2,thermhisp_3,
                  thermenviro_1,thermenviro_2,thermenviro_3,
                  thermfelib_1,thermfelib_2,thermfelib_3),
                ~ifelse(.x > 100, NA, .x)),
         across(c(fschild_1,fschild_2,
                  fscrime_1,fscrime_2,
                  fsaids_1,fsaids_2,
                  fsschools_1,fsschools_2,
                  fswelf_1,fswelf_2,
                  fsfood_1,fsfood_2,
                  fsenviro_1,fsenviro_2,
                  fssocsec_1,fssocsec_2),
                ~recode(.x, "1"=1, "2"=2, "3"=3, "7"=3, "8"=NA_real_,
                        "9"=NA_real_)),
         across(c(fschild_3,fscrime_3,fsaids_3,fsschools_3,
                  fswelf_3,fsfood_3,fsenviro_3,fssocsec_3),
                ~recode(.x, "1"=1,"2"=3,"3"=2,"7"=3,
                        "8"=NA_real_, "9"=NA_real_)),
         across(c(abort_1, abort_2, abort_3,
                  prayer_1, prayer_2, prayer_3),
                ~recode(.x, "1"=1, "2"=2, "3"=3, "4"=4,
                        "6"=NA_real_, "7"=NA_real_,
                        "8"=NA_real_, "9"=NA_real_,
                        "0"=NA_real_)),
         across(c(govcrooked_1, govcrooked_2, 
                  wastetax_1, wastetax_2),
                ~recode(.x, "0"=NA_real_, "1"=1, "3"=2, "5"=3,
                        "7"=3,
                        "8"=NA_real_, "9"=NA_real_)),
         across(c(wastetax_3), 
                ~recode(.x, "0"=NA_real_, "1"=3, "2"=2, "3"=1,
                        "8"=NA_real_, "9"=NA_real_)),
         across(c(trustgov_1, trustgov_2),
                ~recode(.x, "1"=1, "3"=2, "5"=3, "7"=3,
                        "8"=NA_real_, "9"=NA_real_)),
         across(c(trustgov_3),
                ~recode(.x, "0"=NA_real_, "1"=1, "2"=2, "3"=3,
                        "4"=3, "8"=NA_real_, "9"=NA_real_)),
         across(c(govcrooked_3),
                ~recode(.x, "1"=3, "3"=2, "5"=1,
                        "0"=NA_real_, "8"=NA_real_, "9"=NA_real_))
  ) %>%
  pivot_longer(govins_1:wastetax_3) %>%
  separate(name, into = c("var", "wave")) %>%
  mutate(wave = paste0("y", wave)) %>%
  spread(wave, value) 

canes0 <- clean_anes2k %>%
  zap_labels() %>%
  mutate(marital_1 = recode(marital_1, "0"=NA_character_, "1"="married",
                            "2"="other", "3"="other", "4"="other", 
                            "5"="single/nm", "6"="married", 
                            "8"=NA_character_, "9"=NA_character_),
         across(c(stayhome_1, stayhome_2, stayhome_3,
                  govbiz_1,govbiz_2,govbiz_3),
                ~recode(.x, "1"=1, "5"=3, "8"=2,
                        "9"=NA_real_)),
         across(c(dontcare_1,dontcare_2,dontcare_3,
                  nosay_1,nosay_2,nosay_3,
                  electfair_1,electfair_2,electfair_3),
                ~ifelse(.x < 1 | .x > 5, NA, .x)),
         across(c(thermbush_1,thermbush_2,thermbush_3,
                  thermdick_1,thermdick_2,thermdick_3,
                  thermgore_1,thermgore_2,thermgore_3,
                  thermnader_1,thermnader_2,thermnader_3,
                  thermjessie_1,thermjessie_2,thermjessie_3,
                  thermhrc_1,thermhrc_2,thermhrc_3,
                  thermcourt_1,thermcourt_2,thermcourt_3,
                  thermcong_1,thermcong_2,thermcong_3,
                  thermmil_1,thermmil_2,thermmil_3,
                  thermfed_1,thermfed_2,thermfed_3,
                  thermblacks_1,thermblacks_2,thermblacks_3,
                  thermwhites_1,thermwhites_2,thermwhites_3,
                  thermcons_1,thermcons_2,thermcons_3,
                  thermlibs_1,thermlibs_2,thermlibs_3,
                  thermunion_1,thermunion_2,thermunion_3,
                  thermbiz_1,thermbiz_2,thermbiz_3,
                  thermpoor_1,thermpoor_2,thermpoor_3,
                  thermwelf_1,thermwelf_2,thermwelf_3,
                  thermhisp_1,thermhisp_2,thermhisp_3,
                  thermfund_1,thermfund_2,thermfund_3,
                  thermold_1,thermold_2,thermold_3,
                  thermenviro_1,thermenviro_2,thermenviro_3,
                  thermlgbt_1,thermlgbt_2,thermlgbt_3,
                  thermfems_1,thermfems_2,thermfems_3),
                ~ifelse(.x > 100, NA, .x)),
         across(c(partyid_1,partyid_2,partyid_3),
                ~ifelse(.x > 7, NA, .x)),
         across(c(fsenviro_1,fsaids_1,fswelf_1,
                  fsschools_1,fscrime_1,fschild_1,
                  fspoor_1,fsforeignaid_1,fssocsec_1,
                  fsblacks_1),
                ~recode(.x, "1"=1, "3"=3, "5"=2, "7"=3, 
                        "8"=NA_real_, "9"=NA_real_)),
         across(c(fsenviro_2,fsenviro_3,
                  fsaids_2,fsaids_3,
                  fswelf_2,fswelf_3,
                  fsschools_2,fsschools_3,
                  fscrime_2,fscrime_3,
                  fschild_2,fschild_3,
                  fspoor_2,fspoor_3,
                  fsforeignaid_2,fsforeignaid_3,
                  fssocsec_2,fssocsec_3,
                  fsblacks_2,fsblacks_3),
                ~recode(.x, "1"=1, "2"=3, "3"=2, "4"=3, 
                        "8"=NA_real_, "9"=NA_real_)),
         across(c(finref_1, finref_2, finref_3),
                ~recode(.x, "1"=1, "3"=2, "5"=3, "7"=4,
                        "8"=NA_real_, "9"=NA_real_, "0"=NA_real_)),
         across(c(trustgov_1, trustgov_2, trustgov_3),
                ~recode(.x, "0"=NA_real_, "1"=1, "2"=2,
                        "3"=3, "4"=3, "8"=NA_real_, "9"=NA_real_)),
         across(c(wastetax_1, wastetax_2, wastetax_3,
                  govcrooked_1,govcrooked_2,govcrooked_3,
                  electattn_1,electattn_2,electattn_3),
                ~recode(.x, "0"=NA_real_, "1"=1, "3"=2,
                        "5"=3, "8"=NA_real_, "9"=NA_real_)),
         across(c(marital_1, marital_2, marital_3),
                ~recode(.x, "1"="married", "0"=NA_character_, "2"="other",
                        "3"="other", "4"="other", "5"="single/nm",
                        "6"="married", "8"=NA_character_, "9"=NA_character_)),
         ed_1 = ifelse(ed_1 > 7 | ed_1 < 1, NA, ifelse(ed_1 < 3, "less than", 
                                                       ifelse(ed_1 >= 6, "ba", "high school"))),
         ed_2 = ifelse(ed_2 > 7 | ed_2 < 1, NA, ifelse(ed_2 < 3, "less than", 
                                                       ifelse(ed_2 >= 6, "ba", "high school")))
  ) %>%
  pivot_longer(stayhome_1:electattn_3) %>%
  select(-c(age_2, age_3, marital_2, marital_3, ed_2)) %>%
  separate(name, into = c("var", "wave")) %>%
  mutate(wave = paste0("y", wave)) %>%
  spread(wave, value) 





cg6 <- clean_g6 %>%
  mutate(id = 1:nrow(clean_g6)) %>%
  select(-c(ed_2, ed_3, age_2, age_3, marital_2, marital_3)) %>%
  zap_labels() %>%
  mutate(ed_1 = factor(ed_1),
         ed_1 = relevel(ed_1, ref = "hs"),
         marital_1 = factor(marital_1),
         marital_1 = relevel(marital_1, ref = "married")) %>%
  mutate(across(c(partyid_1, partyid_2, partyid_3), 
                ~recode(.x, "7"=NA_real_, "8"=NA_real_, "9"=NA_real_)),
         across(c(courts_1, courts_2, courts_3),
                ~recode(.x, "1"=1, "2"=3, "3"=2))) %>%
  pivot_longer(natspac_1:partyid_3) %>%
  separate(name, into = c("var", "wave")) %>%
  mutate(wave = paste0("y", wave)) %>%
  spread(wave, value) 

cg8 <- clean_g8 %>%
  mutate(id = 1:nrow(clean_g8)) %>%
  select(-c(ed_2, ed_3, age_2, age_3, marital_2, marital_3)) %>%
  zap_labels() %>%
  mutate(ed_1 = factor(ed_1),
         ed_1 = relevel(ed_1, ref = "hs"),
         marital_1 = factor(marital_1),
         marital_1 = relevel(marital_1, ref = "married")) %>%
  mutate(across(c(partyid_1, partyid_2, partyid_3), 
                ~recode(.x, "7"=NA_real_, "8"=NA_real_, "9"=NA_real_)),
         across(c(courts_1, courts_2, courts_3),
                ~recode(.x, "1"=1, "2"=3, "3"=2))) %>%
  pivot_longer(natspac_1:partyid_3) %>%
  separate(name, into = c("var", "wave")) %>%
  mutate(wave = paste0("y", wave)) %>%
  spread(wave, value) %>%
  filter(var != "sexeduc") #No variance for one group


cg10 <- clean_g10 %>%
  mutate(id = 1:nrow(clean_g10)) %>%
  select(-c(ed_2, ed_3, age_2, age_3, marital_2, marital_3)) %>%
  zap_labels() %>%
  mutate(ed_1 = factor(ed_1),
         ed_1 = relevel(ed_1, ref = "hs"),
         marital_1 = factor(marital_1),
         marital_1 = relevel(marital_1, ref = "married")) %>%
  mutate(across(c(partyid_1, partyid_2, partyid_3), 
                ~recode(.x, "7"=NA_real_, "8"=NA_real_, "9"=NA_real_)),
         across(c(courts_1, courts_2, courts_3),
                ~recode(.x, "1"=1, "2"=3, "3"=2))) %>%
  pivot_longer(natspac_1:partyid_3) %>%
  separate(name, into = c("var", "wave")) %>%
  mutate(wave = paste0("y", wave)) %>%
  spread(wave, value) 

save(cg10, file = "/Dropbox/extended_formative_windows/clean_data/cg10.Rdata")
save(cg8, file = "/Dropbox/extended_formative_windows/clean_data/cg8.Rdata")
save(cg6, file = "/Dropbox/extended_formative_windows/clean_data/cg6.Rdata")
save(canes5, file = "/Dropbox/extended_formative_windows/clean_data/canes5.Rdata")
save(canes7, file = "/Dropbox/extended_formative_windows/clean_data/canes7.Rdata")
save(canes8, file = "/Dropbox/extended_formative_windows/clean_data/canes8.Rdata")
save(canes90, file = "/Dropbox/extended_formative_windows/clean_data/canes90.Rdata")
save(canes9, file = "/Dropbox/extended_formative_windows/clean_data/canes9.Rdata")
save(canes0, file = "/Dropbox/extended_formative_windows/clean_data/canes0.Rdata")



