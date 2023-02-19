


#Higher values indicate more conservative

scale5 <- clean_anes5 %>%
  zap_labels() %>%
  mutate(across(c(integrate5_1, integrate5_3, prvtpwr5_1, prvtpwr5_3), ~recode(.x, "1"=5, "2"=4, "3"=3, "4"=2, "5"=1,
                                                       "0"=3, "8"=3, "9"=NA_real_)),
         across(c(integrate5_2, prvtpwr5_2), ~recode(.x, "1"=5, "2"=4, "3"=3, "4"=2, "5"=1,
                                         "7"=3, "8"=3, "9"=NA_real_)),
         across(c(helpblk5_1, helpblk5_3, govjob5_1, govjob5_3,
                  bldschls5_1, bldschls5_3), ~recode(.x, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                                                       "0"=3, "8"=3, "9"=NA_real_)),
         across(c(helpblk5_2, govjob5_2, bldschls5_2), ~recode(.x, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5,
                                         "7"=3, "8"=3, "9"=NA_real_))) %>%
  mutate(scale_1 = (integrate5_1 + prvtpwr5_1 + helpblk5_1 + govjob5_1 + bldschls5_1)/5,
         scale_2 = (integrate5_2 + prvtpwr5_2 + helpblk5_2 + govjob5_2 + bldschls5_2)/5,
         scale_3 = (integrate5_3 + prvtpwr5_3 + helpblk5_3 + govjob5_3 + bldschls5_3)/5) %>%
  mutate(dist = abs(scale_2 - scale_1) + abs(scale_3 - scale_2)) %>%
  mutate(young = ifelse(age_1 <= 30, 1, 0))

#helpblk, #busing, #jobguar, #urbanunrest, eqrole, accused, libcon
scale7 <- clean_anes7 %>%
  zap_labels() %>%
  mutate(across(c(helpblk7_1, helpblk7_2, helpblk7_3, 
                  busing7_1, busing7_2, busing7_3,
                  jobguar7_1, jobguar7_2, jobguar7_3,
                  urbunrest7_1, urbunrest7_2, urbunrest7_3,
                  eqrole7_1, eqrole7_2, eqrole7_3,
                  accused7_1, accused7_2,accused7_3,
                  libcon7_1, libcon7_2, libcon7_3), 
                ~recode(.x, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                                       "0"=NA_real_, "8"=4, "9"=NA_real_)),
         across(c(helpblk7_1, helpblk7_2, helpblk7_3, 
                  busing7_1, busing7_2, busing7_3,
                  jobguar7_1, jobguar7_2, jobguar7_3,
                  urbunrest7_1, urbunrest7_2, urbunrest7_3,
                  eqrole7_1, eqrole7_2, eqrole7_3,
                  accused7_1, accused7_2,accused7_3,
                  libcon7_1, libcon7_2, libcon7_3), ~(.x- 1)*(4/6) + 1)) %>%
  mutate(scale_1 = (helpblk7_1 + busing7_1 + jobguar7_1 + urbunrest7_1 + eqrole7_1 + accused7_1 + libcon7_1)/7,
         scale_2 = (helpblk7_2 + busing7_2 + jobguar7_2 + urbunrest7_2 + eqrole7_2 + accused7_2 + libcon7_2)/7,
         scale_3 = (helpblk7_3 + busing7_3 + jobguar7_3 + urbunrest7_3 + eqrole7_3 + accused7_3 + libcon7_3)/7) %>%
  mutate(dist = abs(scale_2 - scale_1) + abs(scale_3 - scale_2),
         change = abs(scale_3 - scale_1)) %>%
  mutate(young = ifelse(age_1 <= 30, 1, 0)) 


scale9 <- clean_anes9 %>%
  zap_labels() %>%
  mutate(across(c(govins9_1, govins9_2, govins9_3, jobguar9_1, jobguar9_2, jobguar9_3,
                  govblks9_1, govblks9_2, govblks9_3,
                  eqrole9_1, eqrole9_2, eqrole9_3, 
                  libcon9_1, libcon9_2, libcon9_3), 
         ~recode(.x, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7,
                 "8"=4, "9"=NA_real_, "0"=4)),
         across(c(spendserv9_1, spendserv9_2, spendserv9_3), 
                ~recode(.x, "1"=7, "2"=6, "3"=5, "4"=4, "5"=4, "6"=3, "7"=2,
                        "8"=4, "9"=NA_real_, "0"=4))) %>%
  mutate(across(c(govins9_1, govins9_2, govins9_3, 
                jobguar9_1, jobguar9_2, jobguar9_3,
                govblks9_1, govblks9_2, govblks9_3,
                eqrole9_1, eqrole9_2, eqrole9_3, 
                libcon9_1, libcon9_2, libcon9_3,
                spendserv9_1, spendserv9_2, spendserv9_3), ~(.x- 1)*(4/6) + 1)) %>%
  mutate(scale_1 = (govins9_1 + jobguar9_1 + govblks9_1 + eqrole9_1 + libcon9_1 + spendserv9_1)/6,
         scale_2 = (govins9_2 + jobguar9_2 + govblks9_2 + eqrole9_2 + libcon9_2 + spendserv9_2)/6,,
         scale_3 = (govins9_3 + jobguar9_3 + govblks9_3 + eqrole9_3 + libcon9_3 + spendserv9_3)/6) %>%
  mutate(dist = abs(scale_2 - scale_1) + abs(scale_3 - scale_2),
         change = abs(scale_3 - scale_1)) %>%
  mutate(young = ifelse(age_1 <= 30, 1, 0))
  

scale06 <- clean_g6 %>%
  mutate(across(c(eqwlth_1, eqwlth_2, eqwlth_3, polviews_1, polviews_2, polviews_3), ~(.x- 1)*(4/6) + 1 )) %>%
  mutate(scale_1 = (helppoor_1 + helpnot_1 + helpsick_1 + helpblk_1 + eqwlth_1 + polviews_1)/6,
         scale_2 = (helppoor_2 + helpnot_2 + helpsick_2 + helpblk_2 + eqwlth_2 + polviews_2)/6,
         scale_3 = (helppoor_3 + helpnot_3 + helpsick_3 + helpblk_3 + eqwlth_3 + polviews_3)/6) %>%
  mutate(dist = abs(scale_2 - scale_1) + abs(scale_3 - scale_2),
         change = abs(scale_3 - scale_1)) %>%
  mutate(young = ifelse(age_1 <= 30, 1, 0)))




bind_rows(tidy(lm(dist ~ young + ed_1 + marital_1, data = scale5)) %>% mutate(panel = "anes5"),
          tidy(lm(dist ~ young + ed_1 + marital_1, data = scale7)) %>% mutate(panel = "anes7"),
          tidy(lm(dist ~ young + ed_1 + marital_1, data = scale9)) %>% mutate(panel = "anes9"),
          tidy(lm(dist ~ young + ed_1 + marital_1, data = scale06)) %>% mutate(panel = "gss")) %>%
  ggplot(aes(x = estimate, y = panel)) + 
  facet_grid(.~term, scales = "free") + 
  geom_vline(xintercept = 0, linetype = 2, color = "gray") + 
  geom_linerange(aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error)) +
  geom_point(shape = 21, fill = "gray") + 
  theme_bw()
  
  
  
