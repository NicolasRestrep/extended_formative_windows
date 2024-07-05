

#########################################
####      Analysis of Stability      ####
#########################################


#Load in long data frame from clean_data.R file

load("./clean_data/long_difference.Rdata")
load("./clean_data/long_demog.Rdata")

source("./code/get_dem_variables.R")

long_diff_demo <- left_join(long_difference, long_demog, by = c("df"="df", "id"="id", "t1"="wave"))

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
  mutate(year = as.Date("2020-11-12") + dec_diff*3652.5) %>%
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






#########################################
####      Analyses at Individual    ####
#########################################

#Multiple imputation of missing demographic information
mi2 <- mice(long_diff_demo)

#Testing models with just one of the imputations
test_data <- complete(mi2, 1) %>%
  mutate(t = (as.numeric(difftime(date, max(date), units = "days")))/3652.5) %>%
  mutate(d = duration, a = abs_diff) %>%
  filter(weight > 0) %>%
  mutate(across(evermarried:lt, ~(.x-mean(.x))))

# Basic model
m0 <- lmer(a ~ d + t + age_group + t*age_group + 
             (1 + t|name),
           data = test_data, weights = weight)


#Fully elaborated model
m4 <- lmer(a ~ d + t + age_group + t*age_group + d*age_group + 
             d*t + d*age_group*t +
             (1 + d + t + age_group + t*age_group + d*age_group + 
                d*t + d*age_group*t|name),
           data = data = test_data, weights = weight)

# Summary preferred model.. Still the case that this is preferred?
m4.5t <- lmer(a ~ d + t + age_group + t*age_group + d*t +
                (1 + d + t + age_group|name),
              data = test_data, weights = weight)
#Same general coefficient estimates
# Intercept around 18 (higher here, 18.6) 
# Duration effect of 1.1ish
# Slightly positive, non-significant year effect
# Older groups negative
# No more negative slope for time*26-33
# Older gorups negative slopes for time
# Positive duration*time effect

# Summary preferred model with demographic covariates
m4.5tb <- lmer(a ~ d + t + age_group + t*age_group + d*t +
                 ba + lt + evermarried + student + everkid + kidinhouse +
                 (1 + d + t + age_group|name),
               data = test_data, weights = weight)
# Same general coefficient estimates
# Ba negative effect
# LT HS positive effect (significant here)
# Married negative effect
# Student small negative effect
# Kid ever positive
# Kid house positive


new.data <- expand_grid(age_group = c(unique(test_data$age_group)),
                        t = seq(-6.4, 0, by = .1),
                        d = c(0,1,2), name = unique(test_data$name))

new.data$yhat <- predict(m4.5t, newdata = new.data)

#Graph predictions
new.data %>%
  filter(d == 2) %>%
  mutate(group = paste(age_group, name, sep = "-")) %>%
  mutate(year = as.Date("2020-11-12") + t*3652.5) %>%
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
ranef(m4.5t)







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

test <- tidy(m2) %>%
  filter(effect == "fixed") %>%
  mutate(fe = estimate) %>%
  select(term, fe)


augment(ranef(m2, condVar = TRUE)) %>%
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


## Overall trajectories for questions

new.data2 <- expand_grid(dec_diff = seq(-6.4, 0, by = .1),
                        d = 0, name = unique(psummaries$name))
#Predict data
new.data2$yhat <- predict(m3, newdata = new.data2)
new.data2 %>%
  left_join(test, by = c("name"="name")) %>%
  mutate(direction = ifelse(dec_diff.y > .5, "1. Positive", 
                            ifelse(dec_diff.y < -.5, "3. Negative", 
                                   "2. Stable"))) %>%
  ggplot(aes(x = dec_diff.x, y = yhat)) + 
  geom_line(alpha = .7, aes(color = direction)) + 
  theme_bw() + 
  facet_wrap(~reorder(name, dec_diff.y)) + 
  labs(x = "Year", y = "Predicted wave-to-wave change")


test <- ranef(m3)$name %>%
  rownames_to_column(var = "name") %>%
  select(name, dec_diff)



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

