


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
         everkid = as.numeric(everkid),
         kidinhouse = as.numeric(kidinhouse),
         student = as.numeric(student),
         id = as.numeric(id)) %>%
  select(df, id, wave, evermarried, student, everkid, kidinhouse, ba)



test <- left_join(long_scores, long_demog,
          by = c("df"="df", "id"="id", "wave"="wave"))

one_obs <- test %>%
  group_by(df, id, wave) %>%
  slice(1) %>%
  mutate(age_group = ifelse(age < 26, "18-25", ifelse(age > 25 & age < 34, "26-33",
                                                      ifelse(age > 33 & age < 65, "33-65", 
                                                             "65+"))))

one_obs %>%
  select(-c(name, value)) %>%
  pivot_longer(evermarried:ba) %>%
  group_by(df, wave, name, age_group) %>% 
  summarise(mean_value = weighted.mean(value,na.rm=TRUE),
            date = mean(date),
            n = n(),
            nmiss = sum(is.na(value)))%>% #View()
  ggplot(aes(x = date, y = mean_value, color = age_group)) +
  facet_wrap(~name) + 
  geom_point() + 
  geom_line() + 
  theme_bw()
