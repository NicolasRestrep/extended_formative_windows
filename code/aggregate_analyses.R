


#########################################
####      Analyses at Aggregate      ####
#########################################

#Panel summaries
psummaries <- long_difference %>%
  mutate(wave_pair = paste(t1, t2, df, sep = "-")) %>%
  group_by(name, wave_pair) %>%
  summarise(d = weighted.mean(duration), a = weighted.mean(abs_diff),
            date = weighted.mean(date), n = n(), sd = sd(abs_diff)) %>%
  #Filtering out bad questions
  filter(name != "natpoor", 
         name != "nathome") %>% ungroup() %>%
  mutate(dec_diff = (as.numeric(difftime(date, max(date), units = "days")))/3652.5) %>%
  mutate(se = sd/sqrt(n))

# Summarize absolute difference at the wave-pair level for each age group
# for each question. 
summaries <- long_diff_demo %>%
  left_join(ik, by = c("df"="df", "id"="id", "t1"="wave")) %>%
  filter(age <= 80, !is.na(ba), !is.na(evermarried), !is.na(student)) %>%
  mutate(wave_pair = paste(t1, t2, df, sep = "-")) %>%
  group_by(wave_pair, name, age_group) %>%
  summarise(d = weighted.mean(duration, weight = weight), a = weighted.mean(abs_diff, weight = weight),
            pct_ba = weighted.mean(ba, weight = weight),
            pct_lt = weighted.mean(lt, weight = weight),
            pct_married = weighted.mean(evermarried, weight = weight),
            pct_student = weighted.mean(student, weight = weight),
            pct_kid = weighted.mean(impute_kid, weight = weight),
            pct_kih = weighted.mean(impute_kih, weight = weight),
            date = weighted.mean(date, weight = weight), n = n(), sd = sd(abs_diff),
            weighted_n = sum(weight)) %>%
  #Filtering out bad questions
  filter(name != "natpoor", 
         name != "nathome") %>% ungroup() %>%
  mutate(dec_diff = (as.numeric(difftime(date, max(date), units = "days")))/3652.5) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(name = ifelse(name == "nateduc", "natschools", name))




# Multilevel model
m0 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + 
             (1 + d + dec_diff + age_group|name),
           data = summaries %>% filter(sd > 0), weights = 1/se)

m1 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + 
             (1 + d + dec_diff + age_group + dec_diff*age_group|name),
           data = summaries %>% filter(sd > 0), weights = 1/se)

m2 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + d*age_group + 
             (1 + d + dec_diff + age_group + dec_diff*age_group + d*age_group|name),
           data = summaries %>% filter(sd > 0), weights = 1/se)

#Fully elaborated model
m4 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + d*age_group + 
             d*dec_diff + d*age_group*dec_diff +
             (1 + d + dec_diff + age_group + dec_diff*age_group + d*age_group + 
                d*dec_diff + d*age_group*dec_diff|name),
           data = summaries %>% filter(sd > 0), weights = 1/se)

#Remove third-order interaction and random effects around third-order interaction term
m4.1 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + d*age_group +
               d*dec_diff +
               (1 + d + dec_diff + age_group + dec_diff*age_group + d*age_group + 
                  d*dec_diff|name),
             data = summaries %>% filter(sd > 0), weights = 1/se)
#Significant improvement in model fit (>400 decrease in BIC)

#Remove random effects for d*dec_diff
m4.2 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + d*age_group +
               d*dec_diff +
               (1 + d + dec_diff + age_group + dec_diff*age_group + d*age_group|name),
             data = summaries %>% filter(sd > 0), weights = 1/se)
# 70-pt improvement

#Remove random effects for d*age_group
m4.3 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + d*age_group +
               d*dec_diff +
               (1 + d + dec_diff + age_group + dec_diff*age_group|name),
             data = summaries %>% filter(sd > 0), weights = 1/se)
#300-pt improvement

#Removing dec_diff*age_group random effect
m4.4 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + d*age_group + d*dec_diff +
               (1 + d + dec_diff + age_group|name),
             data = summaries %>% filter(sd > 0), weights = 1/se)

#Seems to be the best fitting model
m4.5 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + d*dec_diff +
               (1 + d + dec_diff + age_group|name),
             data = summaries %>% filter(sd > 0), weights = 1/se)

m4.5b <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + d*dec_diff +
                pct_ba + pct_lt + pct_married + pct_student + pct_kid + pct_kih +
                (1 + d + dec_diff + age_group|name),
              data = summaries %>% filter(sd > 0), weights = 1/se)


m4.6 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + d*dec_diff +
               (1 + d + dec_diff + age_group|name),
             data = summaries %>% filter(sd > 0), weights = 1/se)


# m3 <- lmer(a ~ d + dec_diff + 
#              (1 + d + dec_diff |name),
#            data = psummaries %>% filter(sd > 0), weights = 1/se)


# Data to predict
new.data <- expand_grid(age_group = c(unique(summaries$age_group)),
                        dec_diff = seq(-6.4, 0, by = .1),
                        d = c(0,1,2), name = unique(summaries$name))

#Predict data
new.data$yhat <- predict(m4.5, newdata = new.data)

#Graph predictions
new.data %>%
  filter(d == 2) %>%
  mutate(group = paste(age_group, name, sep = "-")) %>%
  mutate(year = as.Date("2020-11-12") + dec_diff*3652.5) %>%
  ggplot(aes(x = year, y = yhat, color = age_group)) + 
  geom_hline(yintercept = 0, color = "black") + 
  geom_line(alpha = .2, aes(group = group)) + 
  geom_smooth(linewidth = 2) + 
  theme_bw() + 
  facet_wrap(~age_group) + 
  labs(x = "Year", y = "Predicted wave-to-wave change",
       color = "Age Group",
       title = "Predicted wave-to-wave change by age group",
       subtitle = "Individual question trajectories and overall trajectory") + 
  scale_color_brewer(type = "qual", palette = 2) +
  theme(legend.position = "none")

#Marginal effect of "year" duration
new.data %>%
  spread(d, yhat) %>%
  mutate(year_diff = `1`-`0`) %>%
  mutate(group = paste(age_group, name, sep = "-")) %>%
  mutate(year = as.Date("2020-11-12") + t*3652.5) %>%
  ggplot(aes(x = year, y = year_diff, color = age_group)) + 
  geom_hline(yintercept = 0, color = "black") + 
  geom_line(alpha = .2, aes(group = group)) + 
  geom_smooth(linewidth = 2) + 
  theme_bw() + 
  facet_wrap(~age_group) + 
  labs(x = "Year", y = "Predicted change for one-year duration change",
       color = "Age Group",
       title = "Predicted marginal effect of additional year of observation by age group",
       subtitle = "Individual question trajectories and overall trajectory") + 
  scale_color_brewer(type = "qual", palette = 2) 


slopes <- as.data.frame(ranef(m4.5b)$name) %>%
  rownames_to_column(var = "name") %>%
  mutate(slope = dec_diff) %>%
  select(name, slope) 

#Look at questions instead
new.data %>%
  filter(d == 1, age_group %in% c("18-25", "34-65")) %>%
  group_by(name) %>%
  left_join(slopes) %>% 
  #left_join(test, by = c("name"="name")) %>%
  mutate(group = paste(age_group, name, sep = "-")) %>%
  ggplot(aes(x = dec_diff, y = yhat, color = age_group)) + 
  #geom_point(data = summaries, aes(y = a)) + 
  geom_line(alpha = .7, aes(group = group)) + 
  theme_bw() + 
  facet_wrap(~reorder(name, slope), nrow = 11, ncol = 7) + 
  labs(x = "Year", y = "Predicted wave-to-wave change",
       color = "Age Group") + 
  theme()




time_1 <- summaries %>% filter(wave_pair == "1-2-1956-60 ANES") %>%
  group_by(age_group) %>%
  summarise(across(c(pct_ba, pct_lt, pct_married, pct_student, pct_kid, pct_kih),
                   ~mean(.x))) %>%
  mutate(time = "1")
time_t <- summaries %>% filter(wave_pair == "2-3-2020-22 ANES") %>%
  group_by(age_group) %>% 
  summarise(across(c(pct_ba, pct_lt, pct_married, pct_student, pct_kid, pct_kih),
                   ~mean(.x))) %>% 
  mutate(time = "t")
times <- bind_rows(time_1, time_t)

new.data <- expand_grid(age_group = c(unique(summaries$age_group)),
                        dec_diff = c(-6.4,0),
                        d = c(0,1,2), name = unique(summaries$name),
                        time = c("1", "t")) %>%
  left_join(times, by = c("age_group" = "age_group", "time"="time")) %>%
  filter(dec_diff == 0 | time == 1)

#Predict data
new.data$yhat <- predict(m4.5b, newdata = new.data)

#Graph predictions 
new.data %>%
  filter(d == 2) %>%
  mutate(time = recode(time, "1"="Counterfactual", "t"="Observed")) %>%
  mutate(time = ifelse(dec_diff == -6.4, "Start", time)) %>%
  group_by(age_group, dec_diff, time) %>%
  summarise(yhat = mean(yhat)) %>%
  ungroup() %>%
  select(-dec_diff) %>%
  spread(time, yhat) %>%
  pivot_longer(Counterfactual:Observed) %>%
  mutate(Start.Time = -6.4, Finish.Time = 0) %>%
  ggplot(aes(x = Start.Time, xend = Finish.Time, y = Start, yend = value,
             linetype = name, color = age_group)) +
  geom_segment() + 
  geom_line(aes(color = age_group)) + 
  #geom_point(size = 3) +
  theme_bw() + 
  scale_shape_manual(values = c(21,22)) + 
  scale_fill_brewer(type = "qual", palette = 2,
                    guide = "none") + 
  scale_color_brewer(type = "qual", palette = 2,
                     guide = "none") + 
  labs(x = "Age Group", y = "Expected change for 2-year duration",
       shape = "Estimate",
       title = "Counterfactual expectation of change",
       subtitle = "Holding demographic variables constant at 1956 levels") + 
  facet_wrap(~age_group)

#Graph predictions
new.data %>%
  filter(d == 2) %>%
  mutate(group = paste(age_group, name, sep = "-")) %>%
  mutate(year = as.Date("2020-11-12") + dec_diff*3652.5) %>%
  ggplot(aes(x = year, y = yhat, color = age_group)) + 
  geom_hline(yintercept = 0, color = "black") + 
  #geom_line(alpha = .2, aes(group = group)) + 
  geom_smooth(linewidth = 2) + 
  theme_bw() + 
  facet_wrap(~age_group) + 
  labs(x = "Year", y = "Predicted wave-to-wave change",
       color = "Age Group",
       title = "Predicted wave-to-wave change by age group",
       subtitle = "Individual question trajectories and overall trajectory") + 
  scale_color_brewer(type = "qual", palette = 2) +
  theme(legend.position = "none")




