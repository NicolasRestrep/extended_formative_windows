library(tidyverse)
library(lavaan)
library(haven)

source("~/Dropbox/formative_period/clean_panels.R")





heise_rel_con <- '

y1 ~~ v1*y1
y2 ~~ v2*y2
y3 ~~ v3*y3

y1 ~~ c12*y2
y1 ~~ c13*y3
y2 ~~ c23*y3

r12 := c12/(sqrt(v1)*sqrt(v2))
r13 := c13/(sqrt(v1)*sqrt(v3))
r23 := c23/(sqrt(v2)*sqrt(v3))

stab12 := r13/r23
stab23 := r13/r12

rel := (c12*c23)/(c13*v2)
'


heise_rel_fun <- function(x, n_vals) {
  n_vals <- x$n_vals[1]
  if (n_vals == 2) {
    m1 <- sem(heise_rel_2, x,
              ordered = c("y1", "y2", "y3"))
    t1 <- tidy(m1) %>% mutate(model = "heise_rel_2")
  } else if (n_vals == 3) {
    m1 <- sem(heise_rel_3, x,
              ordered = c("y1", "y2", "y3"))
    t1 <- tidy(m1) %>% mutate(model = "heise_rel_3")
  } else if (n_vals == 4) {
    m1 <- sem(heise_rel_4, x,
              ordered = c("y1", "y2", "y3"))
    t1 <- tidy(m1) %>% mutate(model = "heise_rel_4")
  } else if (n_vals == 5) {
    m1 <- sem(heise_rel_5, x,
              ordered = c("y1", "y2", "y3"))
    t1 <- tidy(m1) %>% mutate(model = "heise_rel_5")
  } else if (n_vals == 6) {
    m1 <- sem(heise_rel_6, x,
              ordered = c("y1", "y2", "y3"))
    t1 <- tidy(m1) %>% mutate(model = "heise_rel_6")
  } else if (n_vals == 7) {
    m1 <- sem(heise_rel_7, x,
              ordered = c("y1", "y2", "y3"))
    t1 <- tidy(m1) %>% mutate(model = "heise_rel_7")
  } else {
    m1 <- sem(heise_rel_con, x)
    t1 <- tidy(m1) %>% mutate(model = "heise_rel_con")
  }
  return(t1)
}


heise_rel_fun <- function(x, n_vals) {
  n_vals <- x$n_vals[1]
  if (n_vals < 16) {
    m1 <- sem(heise_rel_con, x,
              ordered = c("y1", "y2", "y3"))
    t1 <- tidy(m1) %>% mutate(model = "heise_rel_cat")
  } else {
    m1 <- sem(heise_rel_con, x)
    t1 <- tidy(m1) %>% mutate(model = "heise_rel_con")
  }
  return(t1)
}

simplex_reliabilities <- bind_rows(canes5 %>% mutate(df = "anes5"),
                                   canes7 %>% mutate(df = "anes7"),
                                   canes9 %>% mutate(df = "anes9"),
                                   canes90 %>% mutate(df = "anes90"),
                                   cg6 %>% mutate(df = "gss6"),
                                   canes8 %>% mutate(df = "anes8"),
                                   canes0 %>% mutate(df = "anes0"),
                                   cg8 %>% mutate(df = "gss8"),
                                   cg10 %>% mutate(df = "gss10")) %>% 
  filter(age_1 < 83) %>%
  group_by(var, df) %>%
  mutate(n_vals = (max(y1, na.rm = TRUE)-min(y1, na.rm = TRUE))+1) %>%
  mutate(group = ifelse(age_1 < 26, "18-25",
                        ifelse(age_1 >= 26 & age_1 < 34, "26-33",
                               ifelse(age_1 >= 34 & age_1 < 42, "34-41",
                                      ifelse(age_1 >= 42 & age_1 < 50, "42-49",
                                             ifelse(age_1 >= 50 & age_1 < 58, "50-57",
                                                    ifelse(age_1 >= 58 & age_1 < 66, "58-65",
                                                           "66-83"))))))) %>% 
  filter(!is.na(y1) , !is.na(y2) , !is.na(y3)) %>%
  group_by(var, group, df) %>% 
  nest() %>%
  mutate(measure = map(data, heise_rel_fun),
         n = map(data, nrow)) %>%
  unnest(measure, n)


simplex_full <- bind_rows(canes5 %>% mutate(df = "anes5"),
                                   canes7 %>% mutate(df = "anes7"),
                                   canes9 %>% mutate(df = "anes9"),
                                   cg6 %>% mutate(df = "gss6"),
                                   canes8 %>% mutate(df = "anes8"),
                                   cg8 %>% mutate(df = "gss8"),
                                   cg10 %>% mutate(df = "gss10")) %>% 
  filter(age_1 < 83) %>%
  filter(!is.na(y1), !is.na(y2), !is.na(y3)) %>%
  group_by(var, df) %>%
  nest() %>%
  mutate(measure = map(data, heise_rel_fun)) %>%
  unnest(measure)


nix <- canes9 %>% 
  filter(var == "abort") %>% 
  filter(!is.na(y1), !is.na(y2), !is.na(y3)) %>%
  mutate(group = ifelse(age_1 < 26, "18-25",
                        ifelse(age_1 >= 26 & age_1 < 34, "26-33",
                               ifelse(age_1 >= 34 & age_1 < 42, "34-41",
                                      ifelse(age_1 >= 42 & age_1 < 50, "42-49",
                                             ifelse(age_1 >= 50 & age_1 < 58, "50-57",
                                                    ifelse(age_1 >= 58 & age_1 < 66, "58-65",
                                                           "66-83")))))))

simplex_reliabilities %>% filter(label == "rel") %>%
  group_by(df, var) %>%
  filter(max(estimate) < 1.1,
         min(estimate) > .1) %>%
  group_by(df, group) %>% summarise(mean_rel = mean(estimate)) %>%
  mutate(year = recode(df, "anes5"=1956, "anes7"=1972, "anes9"=1992,
                       "anes8"=1980, "anes90"=1990,
                       "anes0"=2000,
                       "gss6"=2006, "gss8"=2008, "gss10"=2010)) %>%
  #filter(group != "66-83") %>%
  ggplot(aes(x = year, y = mean_rel, fill = group)) + 
  #geom_smooth(aes(color = group), method = 'lm', se = FALSE, alpha = .6) + 
  geom_point(shape = 21) + 
  theme_bw() + 
  labs(x = "Year", y = "Average Reliability", shape = "Age Group",
       color = "Age Group",
       title = "Average reliability estimate by age group",
       subtitle = "1980 reliabilities adjusted to be comparable with other panels")

simplex_reliabilities %>% filter(label %in% c("stab12", "stab23")) %>% 
  #group_by(df, var) %>%
  filter(max(estimate) < 1.4,
         min(estimate) > 0) %>%
  group_by(df, group) %>% summarise(mean = mean(estimate)) %>%
  mutate(year = recode(df, "anes5"=1956, "anes7"=1972, "anes9"=1992,
                       "anes8"=1980, "anes90"=1990,
                       "anes0"=2000,
                       "gss6"=2006, "gss8"=2008, "gss10"=2010)) %>%
  #filter(group != "66-83") %>%
  ggplot(aes(x = year, y = mean, fill = group)) + 
  geom_smooth(aes(color = group), method = 'lm', se = FALSE, alpha = .6) + 
  geom_point(shape = 21) + 
  theme_bw() + 
  labs(x = "Year", y="Average Stability",
       title = "Average stability estimate by age group",
       fill = "Group",
       subtitle = "1980 stability adjusted to be comparable with other panels")


stabilities <- simplex_reliabilities %>% filter(label %in% c("stab12", "stab23")) %>% 
  group_by(df, var) %>%
  filter(max(estimate) < 1.4,
         min(estimate) > 0) %>%
  mutate(year = recode(df, "anes5"=1956, "anes7"=1972, "anes9"=1992,
                       "anes8"=1980, "anes90"=1990,
                       "anes0"=2000,
                       "gss6"=2006, "gss8"=2008, "gss10"=2010))

summary(lm(estimate ~ year + df + group,
           data = stabilities))












# Question-specific fixed-threshold estimators
# don't work because much of the data is too sparse
heise_rel_2 <- '

y1 ~~ v1*y1
y2 ~~ v2*y2
y3 ~~ v3*y3

y1 ~~ c12*y2
y1 ~~ c13*y3
y2 ~~ c23*y3

r12 := c12/(sqrt(v1)*sqrt(v2))
r13 := c13/(sqrt(v1)*sqrt(v3))
r23 := c23/(sqrt(v2)*sqrt(v3))

stab12 := r13/r23
stab23 := r13/r12

rel := (c12*c23)/(c13*v2)

y1 | a1*t1
y2 | a1*t1
y3 | a1*t1
'

heise_rel_3 <- '

y1 ~~ v1*y1
y2 ~~ v2*y2
y3 ~~ v3*y3

y1 ~~ c12*y2
y1 ~~ c13*y3
y2 ~~ c23*y3

r12 := c12/(sqrt(v1)*sqrt(v2))
r13 := c13/(sqrt(v1)*sqrt(v3))
r23 := c23/(sqrt(v2)*sqrt(v3))

stab12 := r13/r23
stab23 := r13/r12

rel := (c12*c23)/(c13*v2)

y1 | a1*t1 + a2*t2
y2 | a1*t1 + a2*t2
y3 | a1*t1 + a2*t2
'

heise_rel_4 <- '

y1 ~~ v1*y1
y2 ~~ v2*y2
y3 ~~ v3*y3

y1 ~~ c12*y2
y1 ~~ c13*y3
y2 ~~ c23*y3

r12 := c12/(sqrt(v1)*sqrt(v2))
r13 := c13/(sqrt(v1)*sqrt(v3))
r23 := c23/(sqrt(v2)*sqrt(v3))

stab12 := r13/r23
stab23 := r13/r12

rel := (c12*c23)/(c13*v2)

y1 | a1*t1 + a2*t2 + a3*t3
y2 | a1*t1 + a2*t2 + a3*t3
y3 | a1*t1 + a2*t2 + a3*t3
'


heise_rel_5 <- '

y1 ~~ v1*y1
y2 ~~ v2*y2
y3 ~~ v3*y3

y1 ~~ c12*y2
y1 ~~ c13*y3
y2 ~~ c23*y3

r12 := c12/(sqrt(v1)*sqrt(v2))
r13 := c13/(sqrt(v1)*sqrt(v3))
r23 := c23/(sqrt(v2)*sqrt(v3))

stab12 := r13/r23
stab23 := r13/r12

rel := (c12*c23)/(c13*v2)

y1 | a1*t1 + a2*t2 + a3*t3 + a4*t4
y2 | a1*t1 + a2*t2 + a3*t3 + a4*t4
y3 | a1*t1 + a2*t2 + a3*t3 + a4*t4
'

heise_rel_6 <- '

y1 ~~ v1*y1
y2 ~~ v2*y2
y3 ~~ v3*y3

y1 ~~ c12*y2
y1 ~~ c13*y3
y2 ~~ c23*y3

r12 := c12/(sqrt(v1)*sqrt(v2))
r13 := c13/(sqrt(v1)*sqrt(v3))
r23 := c23/(sqrt(v2)*sqrt(v3))

stab12 := r13/r23
stab23 := r13/r12

rel := (c12*c23)/(c13*v2)

y1 | a1*t1 + a2*t2 + a3*t3 + a4*t4 + a5*t5
y2 | a1*t1 + a2*t2 + a3*t3 + a4*t4 + a5*t5
y3 | a1*t1 + a2*t2 + a3*t3 + a4*t4 + a5*t5
'


heise_rel_7 <- '

y1 ~~ v1*y1
y2 ~~ v2*y2
y3 ~~ v3*y3

y1 ~~ c12*y2
y1 ~~ c13*y3
y2 ~~ c23*y3

r12 := c12/(sqrt(v1)*sqrt(v2))
r13 := c13/(sqrt(v1)*sqrt(v3))
r23 := c23/(sqrt(v2)*sqrt(v3))

stab12 := r13/r23
stab23 := r13/r12

rel := (c12*c23)/(c13*v2)

y1 | a1*t1 + a2*t2 + a3*t3 + a4*t4 + a5*t5 + a6*t6
y2 | a1*t1 + a2*t2 + a3*t3 + a4*t4 + a5*t5 + a6*t6
y3 | a1*t1 + a2*t2 + a3*t3 + a4*t4 + a5*t5 + a6*t6
'











