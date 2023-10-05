
library(lme4)
bind_rows(canes5 %>% mutate(df = "anes5"),
          canes7 %>% mutate(df = "anes7"),
          canes9 %>% mutate(df = "anes9"),
          canes90 %>% mutate(df = "anes90"),
          cg6 %>% mutate(df = "gss6"),
          canes8 %>% mutate(df = "anes8"),
          canes0 %>% mutate(df = "anes0"),
          cg8 %>% mutate(df = "gss8"),
          cg10 %>% mutate(df = "gss10")) %>% 
  filter(age_1 < 83) %>%
  filter(!is.na(y1), !is.na(y2), !is.na(y3)) 
  

new.data <- data.frame(group = c("66-83", "58-65", "50-57", 
                                 "42-49", "34-41", "26-33", 
                                 "18-25")) 
glm.fun <- function(df) {
  m1 <- glmer(stable ~ 1 + (1|var) + (1|id) + group,
              data = df, 
              family = binomial(link = "logit"))
  return((m1))
}

pred.fun <- function(model) {
  preds <- predict(model, new.data, re.form=NA, type = "response")
  return(cbind(new.data, preds))
}


test <- bind_rows(canes5 %>% mutate(df = "anes5"),
                  canes7 %>% mutate(df = "anes7"),
                  canes9 %>% mutate(df = "anes9"),
                  canes90 %>% mutate(df = "anes90"),
                  cg6 %>% mutate(df = "gss6"),
                  canes8 %>% mutate(df = "anes8"),
                  canes0 %>% mutate(df = "anes0"),
                  cg8 %>% mutate(df = "gss8"),
                  cg10 %>% mutate(df = "gss10")) %>%
  filter(age_1 < 83) %>%
  mutate(group = ifelse(age_1 < 26, "18-25",
                        ifelse(age_1 >= 26 & age_1 < 34, "26-33",
                               ifelse(age_1 >= 34 & age_1 < 42, "34-41",
                                      ifelse(age_1 >= 42 & age_1 < 50, "42-49",
                                             ifelse(age_1 >= 50 & age_1 < 58, "50-57",
                                                    ifelse(age_1 >= 58 & age_1 < 66, "58-65",
                                                           "66-83"))))))) %>%
  filter(!is.na(y1), !is.na(y2), !is.na(y3)) %>%
  group_by(df, var) %>%
  mutate(median = median(unique(y1))) %>%
  mutate(stable = ifelse((y1 <= median & y2 <= median & y3 <= median)|
                           (y1 >= median & y2 >= median & y3 >= median), 1, 0)) %>%
  group_by(df) %>%
  nest() %>%
  mutate(t = map(data, glm.fun)) %>%
  mutate(p = map(t, pred.fun))



test %>% unnest(p) %>%
  mutate(year = recode(df, "anes5"=1956, "anes7"=1972, "anes9"=1992,
                       "anes8"=1980, "anes90"=1990,
                       "anes0"=2000,
                       "gss6"=2006, "gss8"=2008, "gss10"=2010)) %>%
  ggplot(aes(x = year, y = preds)) + 
  #geom_linerange(aes(color = group)) + 
  geom_point(shape = 21, aes(fill = group), size = 3) + 
  theme_bw() + 
  #facet_grid(.~year, scale = "free") + 
  labs(x = "Year", y = "Predicted stability",
       fill = "Age Group",
       title = "Predicted stability across all questions",
       subtitle = "Logstic regression with random effects for individual and question")


m1 <- glmer(stable ~ 1 + (1|var) + (1|id) + group,
          data = test, 
          family = binomial(link = "logit"))

predict(m1, new.data, re.form=NA, type="response")




glm.fun.2 <- function(df) {
  m1 <- glmer(stable ~ 1 + kid + ever_mar + ba + (1|var) + (1|id) ,
              data = df, 
              family = binomial(link = "logit"))
  return((m1))
}

test.2 <- bind_rows(canes5 %>% mutate(df = "anes5"),
                  canes7 %>% mutate(df = "anes7"),
                  canes9 %>% mutate(df = "anes9"),
                  canes90 %>% mutate(df = "anes90"),
                  cg6 %>% mutate(df = "gss6"),
                  canes8 %>% mutate(df = "anes8"),
                  canes0 %>% mutate(df = "anes0"),
                  cg8 %>% mutate(df = "gss8"),
                  cg10 %>% mutate(df = "gss10")) %>%
  filter(age_1 < 42) %>%
  mutate(group = ifelse(age_1 < 26, "18-25",
                        ifelse(age_1 >= 26 & age_1 < 34, "26-33",
                               ifelse(age_1 >= 34 & age_1 < 42, "34-41",
                                      ifelse(age_1 >= 42 & age_1 < 50, "42-49",
                                             ifelse(age_1 >= 50 & age_1 < 58, "50-57",
                                                    ifelse(age_1 >= 58 & age_1 < 66, "58-65",
                                                           "66-83"))))))) %>%
  filter(!is.na(y1), !is.na(y2), !is.na(y3)) %>%
  mutate(ever_mar = ifelse(marital_1 == "single/nm", 0, 1),
         ba = ifelse(ed_1 == "ba", 1, 0),
         kid = ifelse(childs_1 == 1, 1, 0)) %>%
  group_by(df, var) %>%
  mutate(median = median(unique(y1))) %>%
  mutate(stable = ifelse((y1 <= median & y2 <= median & y3 <= median)|
                           (y1 >= median & y2 >= median & y3 >= median), 1, 0)) %>%
  group_by(df) %>%
  nest() %>%
  mutate(t = map(data, glm.fun.2)) 




test.2 %>%
  mutate(tidy = map(t, tidy)) %>%
  unnest(tidy) %>%
  filter(term != "sd__(Intercept)",
         term != "(Intercept)") %>%
  ggplot(aes(x = estimate, xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error,
             y = term)) +
  geom_vline(xintercept = 0, color = "gray", linetype = 2) + 
  geom_linerange() + 
  geom_point() + 
  facet_wrap(~df) + 
  theme_bw() + 
  labs(x = "Coefficient Estimate", y = "", 
       title = )


data <- test.2$data[[1]]
data.frame(kid = c(0,
                   1, 
                   mean(data$kid,na.rm=TRUE),
                   mean(data$kid,na.rm=TRUE),
                   mean(data$kid,na.rm=TRUE),
                   mean(data$kid,na.rm=TRUE)),
           ever_mar = c(mean(data$ever_mar,na.rm=TRUE),
                        mean(data$ever_mar,na.rm=TRUE),
                        0,
                        1,
                        mean(data$ever_mar,na.rm=TRUE),
                        mean(data$ever_mar,na.rm=TRUE)),
           ba = c(mean(data$ba,na.rm=TRUE),
                  mean(data$ba,na.rm=TRUE),
                  mean(data$ba,na.rm=TRUE),
                  mean(data$ba,na.rm=TRUE),
                  0,
                  1))

predictions <- vector(mode = "list", length = nrow(test.2))
for (i in 1:nrow(test.2)) {
  data <- test.2$data[[i]]
  m <- test.2$t[[i]]
  df <- data.frame(kid = c(0,
                           1, 
                           mean(data$kid,na.rm=TRUE),
                           mean(data$kid,na.rm=TRUE),
                           mean(data$kid,na.rm=TRUE),
                           mean(data$kid,na.rm=TRUE)),
                   ever_mar = c(mean(data$ever_mar,na.rm=TRUE),
                                mean(data$ever_mar,na.rm=TRUE),
                                0,
                                1,
                                mean(data$ever_mar,na.rm=TRUE),
                                mean(data$ever_mar,na.rm=TRUE)),
                   ba = c(mean(data$ba,na.rm=TRUE),
                          mean(data$ba,na.rm=TRUE),
                          mean(data$ba,na.rm=TRUE),
                          mean(data$ba,na.rm=TRUE),
                          0,
                          1))
  p <- predict(m, df, re.form=NA, type="response")
  predictions[[i]] <- cbind(df, p) %>% mutate(df = test.2$df[[i]])
}


bind_rows(predictions) %>%
  mutate(var = rep(c("kid", "kid", "mar", "mar", "ba", "ba"), 9)) %>%
  mutate(var = recode(var, "kid"="Has Child",
                      "mar"="Ever Married",
                      "ba"="College Degree")) %>% 
  group_by(df, var) %>%
  mutate(diff = p - dplyr::lag(p)) %>%
  mutate(year = recode(df, "anes5"=1956, "anes7"=1972, "anes9"=1992,
                       "anes8"=1980, "anes90"=1990,
                       "anes0"=2000,
                       "gss6"=2006, "gss8"=2008, "gss10"=2010)) %>%
  ggplot(aes(x = year, y = diff)) + 
  geom_hline(yintercept = 0) + 
  geom_point() + 
  facet_wrap(~var) + 
  theme_bw() + 
  labs(title = "Average Marginal Effect of Life Differences on Stability*",
       subtitle = "Sample confined to ages 18-41.",
       x = "Year", 
       y = "Average marginal difference")


ca5 <- test.2$data[[1]]

boots <- vector(mode = "list", length = 100)
summary <- vector(mode = "list", length = nrow(test.2))
for (i in 1:nrow(test.2)) {
  data <- test.2$data[[i]]
  df <- test.2$df[[i]]
  for (j in 1:100) {
    d <- data[sample(nrow(data), replace = TRUE),]
    m <- glm.fun.2(d)
    newdf <- data.frame(kid = c(0,
                                1, 
                                mean(d$kid,na.rm=TRUE),
                                mean(d$kid,na.rm=TRUE),
                                mean(d$kid,na.rm=TRUE),
                                mean(d$kid,na.rm=TRUE)),
                        ever_mar = c(mean(d$ever_mar,na.rm=TRUE),
                                     mean(d$ever_mar,na.rm=TRUE),
                                     0,
                                     1,
                                     mean(d$ever_mar,na.rm=TRUE),
                                     mean(d$ever_mar,na.rm=TRUE)),
                        ba = c(mean(d$ba,na.rm=TRUE),
                               mean(d$ba,na.rm=TRUE),
                               mean(data$ba,na.rm=TRUE),
                               mean(d$ba,na.rm=TRUE),
                               0,
                               1))
    p <- predict(m, newdf, re.form=NA, type="response")
    boots[[j]] <- cbind(newdf, p) %>% mutate(iter = j) %>%
      mutate(dim = c("kid", "kid", "mar", "mar", "ba", "ba"))
    print(j)
  }
  summary[[i]] <- bind_rows(boots) %>% mutate(df = df) %>%
    mutate(dim = rep(c("kid", "kid", "mar", "mar", "ba", "ba"), 100)) %>%
    mutate(dim = recode(dim, "kid"="Has Child",
                        "mar"="Ever Married",
                        "ba"="College Degree")) %>% 
    group_by(iter, dim) %>%
    mutate(diff = p - dplyr::lag(p)) %>% filter(!is.na(diff)) %>%
    group_by(df, dim) %>%
    summarise(min = quantile(diff, .05), max = quantile(diff, .95), median = quantile(diff, .5)) 
}

bind_rows(summary) %>%
  mutate(year = recode(df, "anes5"=1956, "anes7"=1972, "anes9"=1992,
                       "anes8"=1980, "anes90"=1990,
                       "anes0"=2000,
                       "gss6"=2006, "gss8"=2008, "gss10"=2010)) %>%
  ggplot(aes(x = year, y = median)) + 
  geom_linerange(aes(ymin = min, ymax = max)) +
  geom_hline(yintercept = 0) + 
  geom_point() + 
  facet_wrap(~dim) + 
  theme_bw() + 
  labs(title = "Average Marginal Effect of Life Differences on Stability*",
       subtitle = "Sample confined to ages 18-41.",
       x = "Year", 
       y = "Average marginal difference")
