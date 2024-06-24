

#########################################
####      Analysis of Stability      ####
#########################################


#Load in long data frame from clean_data.R file

load("./clean_data/long_difference.Rdata")

#Panel summaries
psummaries <- long_difference %>%
  mutate(wave_pair = paste(t1, t2, df, sep = "-")) %>%
  group_by(name, df) %>%
  summarise(d = weighted.mean(duration), a = weighted.mean(abs_diff),
            date = weighted.mean(date), n = n(), sd = sd(abs_diff)) %>%
  #Filtering out bad questions
  filter(name != "natpoor", 
         name != "nathome") %>% ungroup() %>%
  mutate(dec_diff = (as.numeric(difftime(date, max(date), units = "days")))/3652.5) %>%
  mutate(se = sd/sqrt(n))


# Summarize absolute difference at the wave-pair level for each age group
# for each question. 
summaries <- long_difference %>%
  mutate(wave_pair = paste(t1, t2, df, sep = "-")) %>%
  group_by(wave_pair, name, age_group) %>%
  summarise(d = weighted.mean(duration), a = weighted.mean(abs_diff),
            date = weighted.mean(date), n = n(), sd = sd(abs_diff)) %>%
  #Filtering out bad questions
  filter(name != "natpoor", 
         name != "nathome") %>% ungroup() %>%
  mutate(dec_diff = (as.numeric(difftime(date, max(date), units = "days")))/3652.5) %>%
  mutate(se = sd/sqrt(n)) %>%
  mutate(name = ifelse(name == "nateduc", "natschools", name))

# Multilevel model
m1 <- lmer(a ~ d + dec_diff + age_group + dec_diff*age_group + 
             (1 + d + dec_diff + age_group + dec_diff*age_group|name),
           data = summaries %>% filter(sd > 0), weights = 1/se)

# Data to predict
new.data <- expand_grid(age_group = c(unique(summaries$age_group)),
                        dec_diff = seq(-6.4, 0, by = .1),
                        d = 0, name = unique(summaries$name))

#Predict data
new.data$yhat <- predict(m1, newdata = new.data)

#Graph predictions
new.data %>%
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

#Look at questions instead
new.data %>%
  mutate(group = paste(age_group, name, sep = "-")) %>%
  ggplot(aes(x = dec_diff, y = yhat, color = age_group)) + 
  geom_point(data = summaries, aes(y = a)) + 
  geom_line(alpha = .7, aes(group = group)) + 
  theme_bw() + 
  facet_wrap(~name) + 
  labs(x = "Year", y = "Predicted wave-to-wave change",
       color = "Age Group")



m1 <- lm(a ~ d + df + dec_diff + I(d*dec_diff), data = long_data)

# Every year since someone was last observed increases their absolute change
# 1.1354 on average.

long_data %>%
  ggplot(aes(x = d, y = a, color = df)) +
  #geom_point(alpha = .1) + 
  geom_smooth(method = "lm", se = FALSE) + 
  #facet_wrap(~df) + 
  theme_bw() + 
  facet_wrap(~name) +
  labs(x = "Duration between observations",
       y = "Expected absolute difference",
       color = "Panel") + 
  scale_color_viridis_d()

long_data %>%
  group_by(name, df, set, age_group) %>%
  summarise(a = mean(a), n = n()) %>% ungroup() %>% 
  mutate(year = recode(df, "1956-60 ANES"=1956, "1972-76 ANES"=1972,
                       "1980 ANES"=1980, "1990-92 ANES"=1990, 
                       "1992-97 ANES"=1992, "2000-04 ANES"=2000,
                       "2006-10 GSS"=2006, "2008-12 GSS"=2008, 
                       "2010-14 GSS"=2010, "2016-20 GSS"=2016,
                       "2016-20 ANES"=2016, "2020-22 ANES"=2020)) %>%
  ggplot(aes(x = year, y = a, color = age_group)) + 
  geom_point(shape = 21, aes(fill = age_group), color = "black") + 
  facet_wrap(~name) + 
  geom_smooth(method = "lm", se = FALSE) + 
  theme_bw()


long_data %>% arrange(desc(a))

m1 <- lm(a ~ d + dec_diff + I(d*dec_diff) + name + age_group + age_group*dec_diff, 
         data = long_data)

m2 <- lmer(a ~ d + dec_diff + (1 + d + dec_diff | name),
           data = long_data, weights = weight)

test <- tidy(m1) %>%
  filter(effect == "fixed") %>%
  mutate(fe = estimate) %>%
  select(term, fe)


augment(ranef(m1, condVar = TRUE)) %>%
  left_join(test, by = c("variable"="term")) %>%
  mutate(estimate = estimate + fe, lb = lb + fe, ub = ub + fe) %>%
  ggplot(aes(x = estimate, y = level, xmin = lb, xmax = ub)) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_vline(data = tidy(fixef(m1)) %>% mutate(variable = names, estimate = x),
             aes(xintercept = estimate), color = "firebrick", linetype = 2) +
  geom_linerange() + 
  geom_point() + 
  facet_grid(.~variable, scales = "free_x") + 
  theme_bw()



diff_function <- function(x) {
  m1 <- lm(a ~ d, data = x)
  return(tidy(m1))
}

long_data %>%
  group_by(df, name) %>%
  nest() %>%
  mutate(lm = map(data, diff_function)) %>%
  unnest(lm) %>% 
  #filter(name %in% c("partyid", "polviews")) %>%
  mutate(year = recode(df, "1956-60 ANES"=1956, "1972-76 ANES"=1972,
                       "1980 ANES"=1980, "1990-92 ANES"=1990, 
                       "1992-97 ANES"=1992, "2000-04 ANES"=2000,
                       "2006-10 GSS"=2006, "2008-12 GSS"=2008, 
                       "2010-12 GSS"=2010, "2016-20 GSS"=2016,
                       "2020-22 ANES"=2020)) %>%
  filter(name == "natenvir") %>%
  ggplot(aes(x = year, 
             # ymin = estimate - 1.96*std.error,
             # ymax = estimate + 1.96*std.error,
             y = estimate)) + 
  geom_point() + 
  #geom_linerange() + 
  theme_bw() + 
  geom_smooth(method = "lm") +
  labs(x = "Year", y = "Slope Coefficient",
       title = "Expected abs. difference for 1-year duration: Party ID & Polviews",
       subtitle = "Variable rescaled to 0-100") + 
  facet_grid(term~name, scales = "free_y")


anes90_long %>%
  filter(name == "trust") %>%
  group_by(wave) %>%
  summarise(mean = mean(value),
            mean_d = mean(d),
            sd_d = sd(d))

long_data %>%
  ggplot(aes(x = date, y = a)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(~name) + 
  theme_bw()


lm_func <- function(x) {
  tidy(lm(a ~ date, data = x))
}

long_data %>%
  group_by(name) %>%
  nest() %>%
  mutate(t = map(data, lm_func)) %>%
  unnest(t) %>%
  filter(term == "date") %>%
  unnest(data) %>% 
  mutate(direction = ifelse(estimate < 0, "negative", "positive")) %>%
  ggplot(aes(x = date, y = a, color = direction)) + 
  facet_wrap(~reorder(name, estimate)) +
  geom_smooth(method = "lm") + 
  theme_bw() + 
  labs(x = "Year", y = "Absolute difference",
       title = "Expected wave-to-wave absolute difference by observation year") + 
  scale_color_brewer(type = "qual") + 
  theme(legend.position = "none")


m2 <- lmer(a ~ d + dec_diff + (1 + d + dec_diff | name),
           data = long_data)

