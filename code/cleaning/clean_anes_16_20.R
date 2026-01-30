
# cleaning/clean_anes_16_20.R

source("code/cleaning/utils.R")


clean_anes_16_20 <- function(path) {
  
  anes16 <- read_csv(path) %>%
    zap_labels()
  
  cleaned <- anes16 %>%
    mutate(id = row_number()) %>%
    select(id, V161267, V201507x, V241458x,
           V160102,#weight for pre- and post 2016 
           V200011a, #weight for pre-16 and pre-20 combined
           V200011b, #weight for post-16 and post-20 combined
           #dates
           V164002, V165002, V203053, V203078,V243050, V243060,
           #partyid
           V161158x, V201231x, V241227x,
           #ideology
           V161126, V162171, V201200,V241177,
           #stayhome
           V161153, V201347, V241310,
           #wastetax
           V161217, V201235, V241232,
           #runfew
           V161216, V201234, V241231,
           #jobguar
           V161189, V201255, V241252,
           #govins
           V161184, V201252, V241245,
           #helpblk
           V161198, V201258, V241255,
           #abortion
           V161232, V201336, V241302,
           #spendserv
           V161178, V201246, V241239,
           #defspend
           V161181, V201249, V241242,
           #natenvir
           V161212, V201321, V241282,
           #natsoc
           V161205, V201300, V241261,
           #natschools
           V161206, V201303, V241264,
           #natwelf
           V161209, V201312, V241273,
           #natcrime
           V161208, V201309, V241270,
           #natpoor
           V161211, V201318, V241279,
           #ftdems
           V161095, V201156, V241166,
           #ftreps
           V161096, V201157, V241167,
           #ftblk
           V162312, V202480, V242516,
           #wtwhite
           V162314, V202482, V242518,
           #ftcons
           V162101, V202164, V242142,
           #ftlibs
           V162097, V202161, V242139,
           #ftbiz
           V162100, V202163, V242141,
           #ftlabor 
           V162098, V202162, V242140,
           #fthisp
           V162311, V202479, V242515,
           #fthomo
           V162103, V202166, V242144,
           #ftjew
           V162108, V202170, V242149,
           #ftpolice
           V162110, V202171, V242150,
           # Attention Campaigns
           V161004, V201006, V241005,
           # People like me Likert
           V162216, V202213, V242201,
           # Less gvt 
           V162185, V202253, V242246,
           # Preferential hiring
           V162238, V202249, V242237,
           # Death penalty
           V161233, V201343, V241306,
           # Number of migrants
           V162157, V202232, V242227,
           # Favor Oppose Homo
           V161229, V201412, V241376,
           # T: asian-americans
           V162310, V202477, V242514,
           # T: feminists
           V162096, V202160, V242138,
           # T: illegal immigrants
           V162313, V202481, V242517,
           # T: Equality of opportunity for success
           V162243, V202260, V242254,
           # Worry less equality
           V162244, V202261, V242255,
           # Unequal no problem
           V162245, V202262, V242256,
           # Equality less problem
           V162246, V202263, V242257,
           # Moral Relativity
           V162207, V202264, V242258,
           # Traditional family
           V162210, V202265, V242259,
           # Hard work whites
           V162345, V202515, V242541,
           # Hard work blk 
           V162346, V202516, V242542,
           # Slavery difficulty
           V162212, V202301, V242301,
           # Blacks no favors
           V162211, V202300, V242300,
           # Matter of Hard work
           V162214, V202303, V242303,
           # Religious importance
           V161241, V201433, V241420,
           # Dont care (2,4; 5pt; NA < -9,-7,-6; -8=3)
           V162215, V202212, V242200,
           # elexmatter (1,2,3; NA <0)
           V161220, V201238, V241235,
           # Satisfied with democracy (1,2,4,5, recode to 1234)
           V162290, V202440, V242439
    ) %>%
    mutate(age_1 = ifelse(V161267 < 0 & V201507x > 0, V201507x, ifelse(V161267 < 0, NA, V161267)),
           age_2 = age_1,
           age_3 = ifelse(!is.na(age_1) & V201507x < 0, age_1 + 4, V201507x),
           age_4 = age_3,
           age_5 = V241458x,
           age_6 = age_5,
           across(age_1:age_6, ~as.character(.x))) %>%
    mutate(weight_2 = as.character(V160102),
           weight_3 = as.character(V200011a), 
           weight_4 = as.character(V200011b)) %>%
    mutate(date_1 = convert_to_date(V164002),
           date_2 = convert_to_date(V165002),
           date_3 = convert_to_date(V203053),
           date_4 = convert_to_date(V203078),
           date_5 = convert_to_date(V243050),
           date_6 = convert_to_date(V243060),
           across(c(V162215, V202212, V242200,
                    V162216, V202213, V242201),
                  ~recode(.x, "1"=1, "2"=1, "3"=3,
                          "4"=5, "5"=5, "-8"=3,
                          "-9"=NA_real_, "-6"=NA_real_,
                          "-4"=NA_real_, "-5"=NA_real_)),
           across(c(V162290, V202440, V242439),
                  ~recode(.x, "1"=1, "2"=2, "4"=3, "5"=4,
                          "-5"=NA_real_, "-6"=NA_real_,
                          "-7"=NA_real_, "-8"=NA_real_,
                          "-9"=NA_real_)),
           across(c(V161095, V201156, V241166,
                    V161096, V201157, V241167,
                    V162312, V202480, V242516,
                    V162314, V202482, V242518,
                    V162101, V202164, V242142,
                    V162097, V202161, V242139,
                    V162100, V202163, V242141,
                    V162098, V202162, V242140,
                    V162311, V202479, V242515,
                    V162103, V202166, V242144,
                    V162108, V202170, V242149,
                    V162110, V202171, V242150,
                    V162310, V202477, V242514,
                    V162096, V202160, V242138,
                    V162313, V202481, V242517),
                  ~ifelse(.x < 0 | .x > 100, NA, .x)),
           across(c(V161158x, V201231x, V241227x,
                    V161126, V162171, V201200, V241177,
                    V161153, V201347, V241310,
                    V161217, V201235, V241232,
                    V161216, V201234, V241231,
                    V161189, V201255, V241252,
                    V161184, V201252, V241245, 
                    V161198, V201258, V241255, 
                    V161178, V201246, V241239,
                    V161181, V201249, V241242,
                    V161004, V201006, V241005,
                    V162157, V202232, V242227,
                    V162243, V202260, V242254,
                    V162244, V202261, V242255,
                    V162245, V202262, V242256,
                    V162246, V202263, V242257,
                    V162207, V202264, V242258,
                    V162210, V202265, V242259,
                    V162345, V202515, V242541, 
                    V162346, V202516, V242542,
                    V162212, V202301, V242301,
                    V162211, V202300, V242300,
                    V162214, V202303, V202303),
                  ~ifelse(.x < 0 | .x > 7, NA, .x)),
           across(c(V161232, V201336, V241302,
                    V161211, V201318, V241279,
                    V162185, V202253, V242246,
                    V162238, V202249, V242237,
                    V161233, V201343, V241306,
                    V161229, V201412, V241376,
                    V161241, V201433, V241420,
                    V161220, V201238, V241235),
                  ~ifelse(.x < 0 | .x > 4, NA, .x)),
           across(c(V161212, V201321, V241282,
                    V161205, V201300, V241261,
                    V161206, V201303, V241264,
                    V161209, V201312, V241273,
                    V161208, V201309, V241270,
                    V161211, V201318, V241279),
                  ~ifelse(.x == 3, 1.5, ifelse(.x < 0, NA, .x))),
           partyid_1 = as.character(V161158x),
           partyid_3 = as.character(V201231x),
           partyid_5 = as.character(V241227x),
           polviews_1 = as.character(V161126),
           polviews_2 = as.character(V162171),
           polviews_3 = as.character(V201200),
           polviews_5 = as.character(V241177),
           stayhome_1 = as.character(V161153), 
           stayhome_3 = as.character(V201347),
           stayhome_5 = as.character(V241310),
           wastetax_1 = as.character(V161217), 
           wastetax_3 = as.character(V201235),
           wastetax_5 = as.character(V241232),
           runfew_1 = as.character(V161216), 
           runfew_3 = as.character(V201234),
           runfew_5 = as.character(V241231),
           jobguar_1 = as.character(V161189), 
           jobguar_3 = as.character(V201255),
           jobguar_5 = as.character(V241252),
           govins_1 = as.character(V161184), 
           govins_3 = as.character(V201252),
           govins_5 = as.character(V241245),
           helpblk_1 = as.character(V161198), 
           helpblk_3 = as.character(V201258),
           helpblk_5 = as.character(V241255),
           abortion_1 = as.character(V161232), 
           abortion_3 = as.character(V201336),
           abortion_5 = as.character(V241302),
           spendserv_1 = as.character(V161178), 
           spendserv_3 = as.character(V201246),
           spendserv_5 = as.character(V241239),
           defscale_1 = as.character(V161181), 
           defscale_3 = as.character(V201249),
           defscale_5 = as.character(V241242),
           natenvir_1 = as.character(V161212), 
           natenvir_3 = as.character(V201321),
           natenvir_5 = as.character(V241282),
           natsoc_1 = as.character(V161205), 
           natsoc_3 = as.character(V201300),
           natsoc_5 = as.character(V241261),
           natschools_1 = as.character(V161206), 
           natschools_3 = as.character(V201303),
           natschools_5 = as.character(V241264),
           natfare_1 = as.character(V161209), 
           natfare_3 = as.character(V201312),
           natfare_5 = as.character(V241273),
           natcrime_1 = as.character(V161208),
           natcrime_3 = as.character(V201309),
           natcrime_5 = as.character(V241270),
           natpoor_1 = as.character(V161211), 
           natpoor_3 = as.character(V201318),
           natpoor_5 = as.character(V241279),
           ftdems_1 = as.character(V161095), 
           ftdems_3 = as.character(V201156),
           ftdems_5 = as.character(V241166),
           ftreps_1 = as.character(V161096), 
           ftreps_3 = as.character(V201157),
           ftreps_5 = as.character(V241167),
           ftblacks_2 = as.character(V162312), 
           ftblacks_4 = as.character(V202480),
           ftblacks_6 = as.character(V242516),
           ftwhites_2 = as.character(V162314), 
           ftwhites_4 = as.character(V202482),
           ftwhites_6 = as.character(V242518),
           ftcons_2 = as.character(V162101), 
           ftcons_4 = as.character(V202164),
           ftcons_6 = as.character(V242142),
           ftlibs_2 = as.character(V162097), 
           ftlibs_4 = as.character(V202161),
           ftlibs_6 = as.character(V242139),
           ftbiz_2 = as.character(V162100), 
           ftbiz_4 = as.character(V202163),
           ftbiz_6 = as.character(V242141),
           ftlabor_2 = as.character(V162098), 
           ftlabor_4 = as.character(V202162),
           ftlabor_6 = as.character(V242140),
           fthisp_2 = as.character(V162311), 
           fthisp_4 = as.character(V202479),
           fthisp_6 = as.character(V242515),
           fthomo_2 = as.character(V162103), 
           fthomo_4 = as.character(V202166),
           fthomo_6 = as.character(V242144),
           ftjews_2 = as.character(V162108), 
           ftjews_4 = as.character(V202170),
           ftjews_6 = as.character(V242149),
           ftcops_2 = as.character(V162110), 
           ftcops_4 = as.character(V202171),
           ftcops_6 = as.character(V242150),
           ftasianamericans_2 = as.character(V162310),
           ftasianamericans_4 = as.character(V202477),
           ftasianamericans_6 = as.character(V242514),
           ftfeminists_2 = as.character(V162096), 
           ftfeminists_4 = as.character(V202160),
           ftfeminists_6 = as.character(V242138),
           ftillimmigrants_2 = as.character(V162313),
           ftillimmigrants_4 = as.character(V202481),
           ftillimmigrants_6 = as.character(V242517),
           attentioncpg_1 = as.character(V161004),
           attentioncpg_3 = as.character(V201006),
           attentioncpg_5 = as.character(V241005),
           ppllikeme_2 = as.character(V162216),
           ppllikeme_4 = as.character(V202213), 
           ppllikeme_6 = as.character(V242201),
           lessgvt_2 = as.character(V162185),
           lessgvt_4 = as.character(V202253),
           lessgvt_6 = as.character(V242246),
           prefhiring_2 = as.character(V162238), 
           prefhiring_4 = as.character(V202249),
           prefhiring_6 = as.character(V242237),
           cappun_1 = as.character(V161233), 
           cappun_3 = as.character(V201343),
           cappun_5 = as.character(V241306),
           letin1a_2 = as.character(V162157), 
           letin1a_4 = as.character(V202232), 
           letin1a_6 = as.character(V242227),
           opposehomo_1 = as.character(V161229), 
           opposehomo_3 = as.character(V201412),
           opposehomo_5 = as.character(V241376),
           eqoppsuccess_2 = as.character(V162243),
           eqoppsuccess_4 = as.character(V202260),
           eqoppsuccess_6 = as.character(V242254),
           worrylesseq_2 = as.character(V162244),
           worrylesseq_4 = as.character(V202261),
           unequalnoprob_2 = as.character(V162245),
           unequalnoprob_4 = as.character(V202262),
           unequalnoprob_6 = as.character(V242256),
           equallessprob_2 = as.character(V162246),
           equallessprob_4 = as.character(V202263),
           equallessprob_6 = as.character(V242257),
           moralrel_2 = as.character(V162207),
           moralrel_4 = as.character(V202264),
           moralrel_6 = as.character(V242258),
           tradfamily_2 = as.character(V162210),
           tradfamily_4 = as.character(V202265),
           tradfamily_6 = as.character(V242259),
           workwhts_2 = as.character(V162345),
           workwhts_4 = as.character(V202515),
           workwhts_6 = as.character(V242541),
           workblks_2 = as.character(V162346),
           workblks_4 = as.character(V202516),
           workblks_6 = as.character(V242542),
           slavediff_2 = as.character(V162212),
           slavediff_4 = as.character(V202301),
           slavediff_6 = as.character(V242301),
           wrkwayup_2 = as.character(V162211),
           wrkwayup_4 = as.character(V202300),
           wrkwayup_6 = as.character(V242300),
           matterhrdwrk_2 = as.character(V162214),
           matterhrdwrk_4 = as.character(V202303),
           matterhrdwrk_6 = as.character(V202303),
           # Dont care (2,4; 5pt; NA < -9,-7,-6; -8=3)
           dontcare_2 = as.character(V162215), 
           dontcare_4 = as.character(V202212),
           dontcare_6 = as.character(V242200),
           # elexmatter (1,2,3; NA <0)
           elexmatter_2 = as.character(V161220), 
           elexmatter_4 = as.character(V201238),
           elexmatter_6 = as.character(V241235),
           # Satisfied with democracy (1,2,4,5, recode to 1234)
           satdemo_2 = as.character(V162290), 
           satdemo_4 = as.character(V202440),
           satdemo_6 = as.character(V242439)
    ) %>% 
    select(id, weight_2:weight_4, age_1:age_6, date_1:satdemo_6) %>%
    pivot_longer(weight_2:satdemo_6) %>%
    separate(name, into = c("measure", "wave")) %>%
    spread(measure, value) %>%
    mutate(date = as.Date(date),
           across(c(weight, abortion:cappun, defscale:wrkwayup),
                  ~as.numeric(.x)),
           across(c(partyid, polviews, jobguar,
                    govins, helpblk, spendserv,
                    defscale, workwhts, workblks),
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
    pivot_longer(c(abortion, attentioncpg:cappun, defscale:wastetax, workblks:wrkwayup)) %>%
    filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
    mutate(df = "2016-24 ANES")
  
  return(cleaned)
}