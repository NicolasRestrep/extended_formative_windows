


long_demog <- bind_rows(anes5_demog,
          anes7_demog,
          anes8_demog,
          anes90_demog,
          anes9_demog,
          anes0_demog,
          anes16_demog,
          anes20_demog,
          gss6_demog,
          gss8_demog,
          gss10_demog,
          gss20_demog) %>%
  mutate(evermarried = recode(marital, "married"=1, "other"=1, "single/nm"=0),
         female = recode(sex, "1"=0, "2"=1),
         ba = recode(ed, "hs"=0, "less than"=0, "ba"=1),
         lt = recode(ed, "hs"=0, "less than"=1, "ba"=0),
         everkid = as.numeric(everkid),
         kidinhouse = as.numeric(kidinhouse),
         student = as.numeric(student),
         id = as.numeric(id),
         rac_black = ifelse(race == "2", 1, 0),
         rac_other = ifelse(race == "3", 1, 0)) %>%
  select(df, id, wave, evermarried, student, everkid, kidinhouse, ba, lt, rac_black,
         rac_other, female) 



test <- left_join(long_scores, long_demog,
          by = c("df"="df", "id"="id", "wave"="wave"))

one_obs <- test %>%
  group_by(df, id, wave) %>%
  slice(1) %>%
  mutate(age_group = ifelse(age < 26, "18-25", ifelse(age > 25 & age < 34, "26-33",
                                                      ifelse(age > 33 & age < 65, "33-65", 
                                                             "65+")))) %>%
  ungroup() %>%
  mutate(dec_diff = (as.numeric(difftime(date, min(date), units = "days")))/3652.5) %>%
  mutate(female_time = female*dec_diff)
  
one_obs %>%
  select(-c(name, value)) %>%
  pivot_longer(c(evermarried:lt)) %>% 
  group_by(df, wave, name, age_group) %>% 
  summarise(mean_value = weighted.mean(value,na.rm=TRUE),
            date = mean(date),
            n = n(),
            nmiss = sum(is.na(value))) %>% 
  ggplot(aes(x = date, y = mean_value, color = age_group)) +
  facet_wrap(~name) + 
  geom_point() + 
  geom_line() + 
  theme_bw()

one_obs %>%
    select(-c(name, value)) %>%
    pivot_longer(c(evermarried:lt, impute_kid:impute_kih)) %>% 
  filter(name %in% c("everkid", "impute_kid", "kidinhouse", "impute_kih")) %>%
    mutate(group = ifelse(name %in% c("everkid", "kidinhouse"), "1. Observed", "2. Imputed"),
           variable = ifelse(name %in% c("everkid", "impute_kid"), "everkid", "kidinhouse")) %>%
    group_by(df, wave, name, age_group, group, variable) %>%
    summarise(mean_value = mean(value,na.rm=TRUE),
              date = mean(date),
              n = n(),
              nmiss = sum(is.na(value)))%>% #View()
    ggplot(aes(x = date, y = mean_value, color = age_group, linetype = group)) +
    facet_grid(variable~group) + 
    geom_point() + 
    geom_line() + 
    theme_bw() 
  


m1 <- glm(everkid ~ age + date + age*date + kidinhouse + kidinhouse*age + kidinhouse*date + kidinhouse*age*date,
    data = one_obs, family = binomial(link = "logit"))

library(mice)

predMat <- matrix(c())

mi.test <- one_obs %>% ungroup() %>% select(-c(id, wave, name, value, df, nobs, age_group))

mi1 <- mice(mi.test)

mi.comp <- complete(mi1, 1)

one_obs$impute_kid <- mi.comp$everkid
one_obs$impute_kih <- mi.comp$kidinhouse
ik <- 
  one_obs %>% select(df, id, wave, impute_kid, impute_kih)



