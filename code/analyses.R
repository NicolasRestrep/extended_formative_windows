

#########################################
####      Analysis of Stability      ####
#########################################


#Load in long data frame from clean_data.R file

load("./clean_data/long_difference.Rdata")
load("./clean_data/long_demog.Rdata")

source("./code/get_dem_variables.R")

long_diff_demo <- left_join(long_difference, long_demog, by = c("df"="df", "id"="id", "t1"="wave")) %>%
  mutate(t = (as.numeric(difftime(date, max(date), units = "days")))/3652.5,
         a = abs_diff,
         d = duration) %>%
  mutate(weight = ifelse(weight == 0, 1, weight)) %>%
  filter(name != "natfood") %>%
  mutate(name = ifelse(name == "nateduc", "natschools", name)) %>%
  select(-c(abs_diff, duration)) %>%
  filter(age <= 80)

#########################################
####      Analyses at Individual    ####
#########################################

library(optimx)

#Don't need to impute missing data for theoretical model
# Multilevel model
m0 <- lmer(a ~ d + t + age_group + t*age_group + 
             (1|name),
           data = long_diff_demo, weights = weight,
           REML = FALSE,
           control = lmerControl(optimizer ="nloptwrap"))
#BIC: 8800268

m1 <- lmer(a ~ d + t + age_group + t*age_group + 
             (1 + d|name),
           data = long_diff_demo, weights = weight,
           REML = FALSE,
           control = lmerControl(optimizer ="nloptwrap"))
# BIC: 8799775

m2 <- lmer(a ~ d + t + age_group + t*age_group + 
             (1 + d + t|name),
           data = long_diff_demo, weights = weight,
           REML = FALSE,
           control = lmerControl(optimizer ="nloptwrap"))
#BIC: 8798411

m3a <- lmer(a ~ d + t + age_group + t*age_group + 
              (1 + d + t + age_group|name),
            data = long_diff_demo, weights = weight,
            REML = FALSE,
            control = lmerControl(optimizer ="nloptwrap"))
#BIC: 8797630

m3b <- lmer(a ~ d + t + age_group + t*age_group + d*age_group + 
              (1 + d + t + age_group + t*age_group + d*age_group|name),
            data = long_diff_demo, weights = weight,
            REML = FALSE,
            control = lmerControl(optimizer ="nloptwrap"))
#BIC: 8798035

#The best fitting model at the aggregate level:
m4.5 <- lmer(a ~ d + t + age_group + t*age_group + d*t +
               (1 + d + t + age_group|name),
             data = long_diff_demo, weights = weight,
             REML = FALSE,
             control = lmerControl(optimizer ="nloptwrap"))
# BIC: 8797591
# Previously the best model, now ~2 points BIC off m4.4 (the best fitting)

#Now the best-fitting model
m4.4 <- lmer(a ~ d + t + age_group + t*age_group + d*age_group + d*t +
               (1 + d + t + age_group|name),
             data = long_diff_demo, weights = weight,
             REML = FALSE,
             control = lmerControl(optimizer ="nloptwrap"))
#BIC: 8797589
# This is the model reported as M1 in Table 2 and discussed
# in the results section

#This doesn't fit as well
m4.3 <- lmer(a ~ d + t + age_group + t*age_group + d*age_group +
               d*t +
               (1 + d + t + age_group + t*age_group|name),
             data = long_diff_demo, weights = weight,
             REML = FALSE,
             control = lmerControl(optimizer ="nloptwrap"))
# BIC: 8797618

#Havent' been able to estimate more complicated models yet. 
m4.2 <- lmer(a ~ d + t + age_group + t*age_group + d*age_group +
               d*t +
               (1 + d + t + age_group + t*age_group + d*age_group|name),
             data = long_diff_demo, weights = weight,
             REML = FALSE,
             control = lmerControl(optimizer ="nloptwrap"))

m4.1 <- lmer(a ~ d + t + age_group + t*age_group + d*age_group +
               d*t +
               (1 + d + t + age_group + t*age_group + d*age_group + 
                  d*t|name),
             data = long_diff_demo, weights = weight,
             REML = FALSE,
             control = lmerControl(optimizer ="nloptwrap"))

#Including the fully elaborated model
m4 <- lmer(a ~ d + t + age_group + t*age_group + d*age_group + 
             d*t + d*age_group*t +
             (1 + d + t + age_group + t*age_group + d*age_group + 
                d*t + d*age_group*t|name),
           data = long_diff_demo, weights = weight,
           REML = FALSE,
           control = lmerControl(optimizer ="nloptwrap"))

############################################################
####      Predictions used in Figure 1 and Figure 4     ####
############################################################

## Prediction data
new.data <- expand_grid(age_group = c(unique(long_diff_demo$age_group)),
                        t = seq(-6.4, 0, by = .1),
                        d = c(0,1,2), name = unique(long_diff_demo$name))

# Predict using model
new.data$yhat <- predict(m4.4, newdata = new.data)

# Plotting predicted values (Figure 2)
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
       color = "Age Group") + 
  scale_color_brewer(type = "qual", palette = 2) +
  theme(legend.position = "none")

# Estimating marginal effect of additional year of observation 
# over time for age groups (not included in document)
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

# Generating figure 4
slopes <- ranef(m4.4)$name %>% as.data.frame() %>% rownames_to_column(var = "name") %>%
  mutate(slope = t) %>%
  select(name, slope)
new.data %>%
  filter(d == 2, age_group %in% c("18-25", "34-65")) %>%
  group_by(name) %>%
  left_join(slopes, by = c("name"="name")) %>% 
  mutate(year = as.Date("2020-11-12") + t*3652.5) %>%
  #left_join(test, by = c("name"="name")) %>%
  mutate(group = paste(age_group, name, sep = "-")) %>%
  ggplot(aes(x = year, y = yhat, color = age_group)) + 
  #geom_point(data = summaries, aes(y = a)) + 
  geom_line(alpha = .7, aes(group = group)) + 
  theme_bw() + 
  facet_wrap(~reorder(name, slope), ncol = 8) + 
  labs(x = "Year", y = "Predicted wave-to-wave change",
       color = "Age Group") + 
  theme_classic() + 
  theme(legend.position = "none")



#####################################################
####      Models with Demographic Predictors     ####
#####################################################


#Imputation of missing data
#Grand mean centering
ldd_grandm <- long_diff_demo
mi_grandm <- mice(ldd_grandm)

long <- complete(mi_grandm, action='long', include=TRUE) %>%
  group_by(.imp) %>%
  mutate(across(evermarried:lt, ~(.x - mean(.x)))) 

#Imputed data sets are very similar
mi_grandm <- as.mids(long)

#Testing analysis with just one data set. 
mi_test <- complete(mi_grandm, action = 1)

# m_test_baseline <- lmer(a ~ d + t + age_group + t*age_group + d*t + d*age_group + 
#                           evermarried + student + everkid + kidinhouse + 
#                           ba + lt + 
#                           (1 + d + t + age_group|name), weights = weight,
#                         data = mi_test,
#                         REML = FALSE,
#                         control = lmerControl(optimizer ="nloptwrap"))

# This is Model 2 in Table 2
# m4.4 with demographic predictors
# All demographics are allowed to vary randomly by questions
# HAd hard time estimating Multiple Imputation data set, and 
# they're so similar that I just used one imputation.

# Need to fix this later.
ldd_fill <- complete(ldd, 1)

m4.5_demogs <- lmer(a ~ d + t + age_group + t*age_group + d*age_group + d*t +
                      evermarried + student + everkid + kidinhouse + 
                      ba + lt + 
                      (1 + d + t + age_group + evermarried + student + 
                         everkid + kidinhouse + ba + lt|name),
                    data = ldd_fill, weights = weight,
                    REML = FALSE,
                    control = lmerControl(optimizer ="nloptwrap"))

#Storing all the results on my computer. 
# Some of them are too large to story in github
# save(m0, file = "~/Dropbox/efw_results/m0")
# save(m1, file = "~/Dropbox/efw_results/m1")
# save(m2, file = "~/Dropbox/efw_results/m2")
# save(m3a, file = "~/Dropbox/efw_results/m3a")
# save(m3b, file = "~/Dropbox/efw_results/m3b")
# save(m4.3, file = "~/Dropbox/efw_results/m43")
# save(m4.4, file = "~/Dropbox/efw_results/m44")
# save(m4.5, file = "~/Dropbox/efw_results/m45")
# save(m4.5_grandm, file = "~/Dropbox/efw_results/mi_results")
# save(m4.4, file = "~/Dropbox/efw_results/main_model")
# save(m4.5_demogs, file = "~/Dropbox/efw_results/demographic_model")

# Generating data to make Figure 3
new.data <- expand_grid(age_group = c(unique(ldd_fill$age_group)),
                        t = c(-6.4,0),
                        d = c(0,1,2), name = unique(ldd_fill$name),
                        time = c("1", "t")) %>%
  left_join(times, by = c("age_group" = "age_group", "time"="time")) %>%
  filter(t == 0 | time == 1)

new.data$yhat <- predict(m4.5_demogs, newdata=new.data)

new.data %>%
  filter(d == 2) %>%
  mutate(time = recode(time, "1"="2. Counterfactual", "t"="1. Observed")) %>%
  mutate(time = ifelse(t == -6.4, "Start", time)) %>%
  group_by(age_group, t, time) %>%
  summarise(yhat = mean(yhat)) %>%
  ungroup() %>%
  select(-t) %>%
  spread(time, yhat) %>%
  pivot_longer(`2. Counterfactual`:`1. Observed`) %>%
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
       shape = "Estimate")+
  facet_wrap(~age_group) + 
  theme(legend.position = "none")


# Using imputed data to Make Figure 1
complete(ldd, 1) %>%
  group_by(df, id, t1) %>%
  slice(1) %>%
  group_by(df, t1, age_group) %>%
  summarise(across(c(date,evermarried:lt), ~weighted.mean(.x, w = weight)),
            n = n()) %>%
  filter(n > 10) %>%
  pivot_longer(evermarried:lt) %>%
  mutate(name = recode(name, "ba"="6. Ed: BA", "lt"="5. Ed: <HS", "student"="4. Student",
                       "kidinhouse"="3. Kid <18 in house", "everkid"="2. Ever had kid", 
                       "evermarried"="1. Ever married")) %>%
  
  ggplot(aes(x = date, y = value, color = age_group)) + 
  facet_wrap(~name) +
  geom_point() + 
  geom_smooth() + 
  theme_bw() + 
  labs(x = "Year", y = "Proportion", color = "Age Group")


# Plotting coefficients from demographic variables
# to see variation in random effects
test <- tidy(m4.5_demogs) %>%
  filter(effect == "fixed") %>%
  mutate(fe = estimate) %>%
  select(term, fe) %>%
  filter(term %in% c("evermarried", "student", "lt", "ba"))

augment(ranef(m4.5_demogs, condVar = TRUE)) %>%
  left_join(test, by = c("variable"="term")) %>%
  mutate(estimate = estimate + fe, lb = lb + fe, ub = ub + fe) %>%
  filter(variable %in% c("evermarried", "student", "lt", "ba")) %>%
  ggplot(aes(x = estimate, y = level, xmin = lb, xmax = ub)) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_vline(data = tidy(fixef(m4.5_demogs)) %>% mutate(variable = names, estimate = x) %>%
               filter(variable %in% c("evermarried", "student", "lt", "ba")),
             aes(xintercept = estimate), color = "firebrick", linetype = 2) +
  geom_linerange() + 
  geom_point() + 
  facet_grid(.~variable, scales = "free_x") + 
  theme_bw()

# m_test_demogs <- lmer(a ~ d + t + age_group + t*age_group +  d*t + d*age_group + 
#                           evermarried + student + everkid + kidinhouse + 
#                           ba + lt + 
#                           (1 + d + t + age_group + ba + lt + evermarried + 
#                              student + everkid + kidinhouse|name), weights = weight,
#                         data = mi_test,
#                         REML = FALSE,
#                         control = lmerControl(optimizer ="nloptwrap"))

# Model results using Multiple imputation
# Didn't allow demographic variables to vary by question
# So these resutls aren't great. Not using them for
# the time being

# m4.5_grandm <- with(mi_grandm, lmer(a ~ d + t + age_group + t*age_group + d*t +
#                                       evermarried + student + everkid + kidinhouse + 
#                                       ba + lt + 
#                                       (1 + d + t + age_group|name), weights = weight,
#                                     REML = FALSE,
#                                     control = lmerControl(optimizer ="nloptwrap")))


summary(pool(m4.5_grandm)) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = estimate, xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error,
             y = term)) + 
  geom_linerange() + 
  geom_point() + 
  theme_bw()

# #Generating Figure 3
# time_1 <- complete(mi_grandm, action = 1) %>% filter(df == "1956-60 ANES") %>%
#   group_by(age_group) %>%
#   summarise(across(c(ba, lt, evermarried, student, everkid, kidinhouse),
#                    ~mean(.x))) %>%
#   mutate(time = "1")
# time_t <- complete(mi_grandm, action = 1) %>% filter(df == "2020-22 ANES") %>%
#   group_by(age_group) %>% 
#   summarise(across(c(ba, lt, evermarried, student, everkid, kidinhouse),
#                    ~mean(.x))) %>% 
#   mutate(time = "t")
# times <- bind_rows(time_1, time_t)

# new.data <- expand_grid(age_group = c(unique(long_diff_demo$age_group)),
#                         t = c(-6.4,0),
#                         d = c(0,1,2), name = unique(long_diff_demo$name),
#                         time = c("1", "t")) %>%
#   left_join(times, by = c("age_group" = "age_group", "time"="time")) %>%
#   filter(t == 0 | time == 1)
# 
# predlist <- lapply(m4.5_grandm$analyses, predict, newdata=new.data)
# 
# new.data$yhat <- predlist[[1]]
# 
# new.data %>%
#   filter(d == 2) %>%
#   mutate(time = recode(time, "1"="2. Counterfactual", "t"="1. Observed")) %>%
#   mutate(time = ifelse(t == -6.4, "Start", time)) %>%
#   group_by(age_group, t, time) %>%
#   summarise(yhat = mean(yhat)) %>%
#   ungroup() %>%
#   select(-t) %>%
#   spread(time, yhat) %>%
#   pivot_longer(`2. Counterfactual`:`1. Observed`) %>%
#   mutate(Start.Time = -6.4, Finish.Time = 0) %>%
#   ggplot(aes(x = Start.Time, xend = Finish.Time, y = Start, yend = value,
#              linetype = name, color = age_group)) +
#   geom_segment() + 
#   geom_line(aes(color = age_group)) + 
#   #geom_point(size = 3) +
#   theme_bw() + 
#   scale_shape_manual(values = c(21,22)) + 
#   scale_fill_brewer(type = "qual", palette = 2,
#                     guide = "none") + 
#   scale_color_brewer(type = "qual", palette = 2,
#                      guide = "none") + 
#   labs(x = "Age Group", y = "Expected change for 2-year duration",
#        shape = "Estimate")+
#   facet_wrap(~age_group) + 
#   theme(legend.position = "none")


#Group mean centering
# ldd_groupm <- long_diff_demo %>%
#   group_by(name) %>%
#   mutate(across(evermarried:lt, ~(.x-mean(.x, na.rm=TRUE))))
# mi_grandm <- mice(ldd_grandm)
# 
# m4.5_groupm <- with(mi_groupm, lmer(a ~ d + t + age_group + t*age_group + d*t +
#                       evermarried + student + everkid + kidinhouse + 
#                       ba + lt + 
#                       (1 + d + t + age_group|name), weights = weight,
#                     REML = FALSE,
#                     control = lmerControl(optimizer ="nloptwrap")))


# #No centering
# ldd_nocent <- long_diff_demo
# mi_nocent <- mice(nocent)
# 
# m4.5_nocent <- with(mi_nocent, lmer(a ~ d + t + age_group + t*age_group + d*t +
#                                       evermarried + student + everkid + kidinhouse + 
#                                       ba + lt + 
#                                       (1 + d + t + age_group|name), weights = weight,
#                                     REML = FALSE,
#                                     control = lmerControl(optimizer ="nloptwrap")))








