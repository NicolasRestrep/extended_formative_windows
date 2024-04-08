
library(lme4)

source("./code/clean_panels.R")

canes5 <- canes5 %>% filter(var != "clsrprvtpwr")
vars <- unique(canes5$var)
group_meansc5 <- vector(mode = "list", length = length(vars))
variancec5 <- vector(mode = "list", length = length(vars))
for (i in 1:length(vars)) {
  v <- vars[i]
  df <- canes5 %>%
    filter(var == v) %>%
    pivot_longer(y1:y3) %>%
    filter(!is.na(value), !is.na(age_1)) %>%
    group_by(id) %>% filter(n() > 1) %>%
    ungroup() %>%
    mutate(year = as.numeric(gsub("y", "", name))) %>%
    select(-name) %>%
    mutate(value = (value - mean(value,na.rm=TRUE))/(sd(value,na.rm=TRUE))) %>%
    mutate(year = (year - 1)*2) %>%
    mutate(group = ifelse(age_1 <= 25, "18-25",
                          ifelse(age_1 >= 26 & age_1 <= 33, "26-33",
                                 "34+"))) 
  #Estimate the effect of one year on standardized response.
  m1 <- lmer(value ~ year + (1 + year|id), data = df)
  #Extract individual year effect
  cm1 <- coef(m1)
  df$yhat <- predict(m1)
  #Put them in data frame
  effects <- data.frame(cm1$id) %>% 
    select(-X.Intercept.) %>%
    rownames_to_column(var = "id") %>%
    mutate(id = as.numeric(id)) %>%
    mutate(slope = year) %>% select(-year)
  #Link to ids/groups
  df <- df %>%
    left_join(effects, by = c("id"="id"))
  group_meansc5[[i]] <- df %>% 
    group_by(id) %>% 
    slice(1) %>%
    group_by(group) %>%
    summarise(n = n(), abs_mean_slope = mean(abs(slope),na.rm=TRUE)) %>%
    mutate(var = v)
  
  variancec5[[i]] <- df %>%
    mutate(resid = (value - yhat)^2) %>%
    group_by(group) %>%
    summarise(ssr = sum(resid), 
              n_obs = n(),
              n_id = length(unique(id))) %>%
    mutate(mse = ssr/n_obs) %>%
    mutate(var = v)
  
  
}

mean_val5 <- bind_rows(group_meansc5) %>%
  group_by(group) %>% summarise(mean = mean(abs_mean_slope)) %>%
  mutate(df = "anes5")
bind_rows(group_meansc5) %>%
  ggplot(aes(x = abs_mean_slope, y = group, group = var)) + 
  geom_point(data = mean_val5, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_point(color = "black", shape = 21, aes(fill = group)) + 
  geom_path(alpha = .1) + 
  theme_bw() + 
  theme(legend.position = "none") 

mean_var5 <- bind_rows(variancec5) %>%
  group_by(group) %>% summarise(mean = mean(mse)) %>%
  mutate(df = "anes5")
bind_rows(variancec5) %>%
  ggplot(aes(x = mse, y = group, group = var)) + 
  geom_point(data = mean_var5, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_path(alpha = .1) + 
  geom_point() + 
  theme_bw()


canes7 <- canes7
vars <- unique(canes7$var)
group_meansc7 <- vector(mode = "list", length = length(vars))
variancec7 <- vector(mode = "list", length = length(vars))
for (i in 1:length(vars)) {
  v <- vars[i]
  df <- canes7 %>%
    filter(var == v) %>%
    pivot_longer(y1:y3) %>%
    filter(!is.na(value), !is.na(age_1)) %>%
    group_by(id) %>% filter(n() > 1) %>%
    ungroup() %>%
    mutate(year = as.numeric(gsub("y", "", name))) %>%
    select(-name) %>%
    mutate(value = (value - mean(value,na.rm=TRUE))/(sd(value,na.rm=TRUE))) %>%
    mutate(year = (year - 1)*2) %>%
    mutate(group = ifelse(age_1 <= 25, "18-25",
                          ifelse(age_1 >= 26 & age_1 <= 33, "26-33",
                                 "34+"))) 
  #Estimate the effect of one year on standardized response.
  m1 <- lmer(value ~ year + (1 + year|id), data = df)
  #Extract individual year effect
  cm1 <- coef(m1)
  df$yhat <- predict(m1)
  #Put them in data frame
  effects <- data.frame(cm1$id) %>% 
    select(-X.Intercept.) %>%
    rownames_to_column(var = "id") %>%
    mutate(id = as.numeric(id)) %>%
    mutate(slope = year) %>% select(-year)
  #Link to ids/groups
  df <- df %>%
    left_join(effects, by = c("id"="id"))
  group_meansc7[[i]] <- df %>% 
    group_by(id) %>% 
    slice(1) %>%
    group_by(group) %>%
    summarise(n = n(), abs_mean_slope = mean(abs(slope),na.rm=TRUE)) %>%
    mutate(var = v)
  
  variancec7[[i]] <- df %>%
    mutate(resid = (value - yhat)^2) %>%
    group_by(group) %>%
    summarise(ssr = sum(resid), 
              n_obs = n(),
              n_id = length(unique(id))) %>%
    mutate(mse = ssr/n_obs) %>%
    mutate(var = v)
}
mean_val7 <- bind_rows(group_meansc7) %>%
  group_by(group) %>% summarise(mean = mean(abs_mean_slope)) %>%
  mutate(df = "anes7")
bind_rows(group_meansc7) %>%
  ggplot(aes(x = abs_mean_slope, y = group, group = var)) + 
  geom_point(data = mean_val7, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_point(color = "black", shape = 21, aes(fill = group)) + 
  geom_path(alpha = .1) + 
  theme_bw() + 
  theme(legend.position = "none") 

mean_var7 <- bind_rows(variancec7) %>%
  group_by(group) %>% summarise(mean = mean(mse)) %>%
  mutate(df = "anes7")
bind_rows(variancec7) %>%
  ggplot(aes(x = mse, y = group, group = var)) + 
  geom_point(data = mean_var7, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_path(alpha = .1) + 
  geom_point() + 
  theme_bw()


canes9 <- canes9
vars <- unique(canes9$var)
group_meansc9 <- vector(mode = "list", length = length(vars))
variancec9 <- vector(mode = "list", length = length(vars))
for (i in 1:length(vars)) {
  v <- vars[i]
  df <- canes9 %>%
    filter(var == v) %>%
    pivot_longer(y1:y3) %>%
    filter(!is.na(value),
           !is.na(age_1)) %>%
    group_by(id) %>% filter(n() > 1) %>%
    ungroup() %>%
    mutate(year = as.numeric(gsub("y", "", name))) %>%
    select(-name) %>%
    mutate(value = (value - mean(value,na.rm=TRUE))/(sd(value,na.rm=TRUE))) %>%
    mutate(year = (year - 1)*2) %>%
    mutate(group = ifelse(age_1 <= 25, "18-25",
                          ifelse(age_1 >= 26 & age_1 <= 33, "26-33",
                                 "34+"))) 
  #Estimate the effect of one year on standardized response.
  m1 <- lmer(value ~ year + (1 + year|id), data = df)
  #Extract individual year effect
  cm1 <- coef(m1)
  df$yhat <- predict(m1)
  #Put them in data frame
  effects <- data.frame(cm1$id) %>% 
    select(-X.Intercept.) %>%
    rownames_to_column(var = "id") %>%
    mutate(id = as.numeric(id)) %>%
    mutate(slope = year) %>% select(-year)
  #Link to ids/groups
  df <- df %>%
    left_join(effects, by = c("id"="id"))
  group_meansc9[[i]] <- df %>% 
    group_by(id) %>% 
    slice(1) %>%
    group_by(group) %>%
    summarise(n = n(), abs_mean_slope = mean(abs(slope),na.rm=TRUE)) %>%
    mutate(var = v)
  
  variancec9[[i]] <- df %>%
    mutate(resid = (value - yhat)^2) %>%
    group_by(group) %>%
    summarise(ssr = sum(resid), 
              n_obs = n(),
              n_id = length(unique(id))) %>%
    mutate(mse = ssr/n_obs) %>%
    mutate(var = v)
}

mean_val9 <- bind_rows(group_meansc9) %>%
  group_by(group) %>% summarise(mean = mean(abs_mean_slope)) %>%
  mutate(df = "anes9")
bind_rows(group_meansc9) %>%
  ggplot(aes(x = abs_mean_slope, y = group, group = var)) + 
  geom_point(data = mean_val9, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_point(color = "black", shape = 21, aes(fill = group)) + 
  geom_path(alpha = .1) + 
  theme_bw() + 
  theme(legend.position = "none") 

mean_var9 <- bind_rows(variancec9) %>%
  group_by(group) %>% summarise(mean = mean(mse)) %>%
  mutate(df = "anes9")
bind_rows(variancec9) %>%
  ggplot(aes(x = mse, y = group, group = var)) + 
  geom_point(data = mean_var9, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_path(alpha = .1) + 
  geom_point() + 
  theme_bw()



canes8 <- canes8 %>% filter(var != "thermbaker",
                            var != "thermbush")
vars <- unique(canes8$var)
group_meansc8 <- vector(mode = "list", length = length(vars))
variancec8 <- vector(mode = "list", length = length(vars))
for (i in 1:length(vars)) {
  v <- vars[i]
  df <- canes8 %>%
    filter(var == v) %>%
    pivot_longer(y1:y3) %>%
    filter(!is.na(value),
           !is.na(age_1)) %>%
    group_by(id) %>% filter(n() > 1) %>%
    ungroup() %>%
    mutate(year = as.numeric(gsub("y", "", name))) %>%
    select(-name) %>%
    mutate(value = (value - mean(value,na.rm=TRUE))/(sd(value,na.rm=TRUE))) %>%
    mutate(year = (year - 1)*.5) %>%
    mutate(group = ifelse(age_1 <= 25, "18-25",
                          ifelse(age_1 >= 26 & age_1 <= 33, "26-33",
                                 "34+"))) 
  #Estimate the effect of one year on standardized response.
  m1 <- lmer(value ~ year + (1 + year|id), data = df)
  #Extract individual year effect
  cm1 <- coef(m1)
  df$yhat <- predict(m1)
  #Put them in data frame
  effects <- data.frame(cm1$id) %>% 
    select(-X.Intercept.) %>%
    rownames_to_column(var = "id") %>%
    mutate(id = as.numeric(id)) %>%
    mutate(slope = year) %>% select(-year)
  #Link to ids/groups
  df <- df %>%
    left_join(effects, by = c("id"="id"))
  group_meansc8[[i]] <- df %>% 
    group_by(id) %>% 
    slice(1) %>%
    group_by(group) %>%
    summarise(n = n(), abs_mean_slope = mean(abs(slope),na.rm=TRUE)) %>%
    mutate(var = v)
  
  variancec8[[i]] <- df %>%
    mutate(resid = (value - yhat)^2) %>%
    group_by(group) %>%
    summarise(ssr = sum(resid), 
              n_obs = n(),
              n_id = length(unique(id))) %>%
    mutate(mse = ssr/n_obs) %>%
    mutate(var = v)
}

mean_val8 <- bind_rows(group_meansc8) %>%
  group_by(group) %>% summarise(mean = mean(abs_mean_slope)) %>%
  mutate(df = "anes8")
bind_rows(group_meansc8) %>%
  ggplot(aes(x = abs_mean_slope, y = group, group = var)) + 
  geom_point(data = mean_val8, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_point(color = "black", shape = 21, aes(fill = group)) + 
  geom_path(alpha = .1) + 
  theme_bw() + 
  theme(legend.position = "none") 

mean_var8 <- bind_rows(variancec8) %>%
  group_by(group) %>% summarise(mean = mean(mse)) %>%
  mutate(df = "anes8")
bind_rows(variancec8) %>%
  ggplot(aes(x = mse, y = group, group = var)) + 
  geom_point(data = mean_var8, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_path(alpha = .1) + 
  geom_point() + 
  theme_bw()


canes2k <- canes0
vars <- unique(canes2k$var)
group_meansc2k <- vector(mode = "list", length = length(vars))
variancec2k <- vector(mode = "list", length = length(vars))
for (i in 1:length(vars)) {
  v <- vars[i]
  df <- canes2k %>%
    filter(var == v) %>%
    pivot_longer(y1:y3) %>%
    filter(!is.na(value),
           !is.na(age_1)) %>%
    group_by(id) %>% filter(n() > 1) %>%
    ungroup() %>%
    mutate(year = as.numeric(gsub("y", "", name))) %>%
    select(-name) %>%
    mutate(value = (value - mean(value,na.rm=TRUE))/(sd(value,na.rm=TRUE))) %>%
    mutate(year = (year - 1)*2) %>%
    mutate(group = ifelse(age_1 <= 25, "18-25",
                          ifelse(age_1 >= 26 & age_1 <= 33, "26-33",
                                 "34+"))) 
  #Estimate the effect of one year on standardized response.
  m1 <- lmer(value ~ year + (1 + year|id), data = df)
  #Extract individual year effect
  cm1 <- coef(m1)
  df$yhat <- predict(m1)
  #Put them in data frame
  effects <- data.frame(cm1$id) %>% 
    select(-X.Intercept.) %>%
    rownames_to_column(var = "id") %>%
    mutate(id = as.numeric(id)) %>%
    mutate(slope = year) %>% select(-year)
  #Link to ids/groups
  df <- df %>%
    left_join(effects, by = c("id"="id"))
  group_meansc2k[[i]] <- df %>% 
    group_by(id) %>% 
    slice(1) %>%
    group_by(group) %>%
    summarise(n = n(), abs_mean_slope = mean(abs(slope),na.rm=TRUE)) %>%
    mutate(var = v)
  
  variancec2k[[i]] <- df %>%
    mutate(resid = (value - yhat)^2) %>%
    group_by(group) %>%
    summarise(ssr = sum(resid), 
              n_obs = n(),
              n_id = length(unique(id))) %>%
    mutate(mse = ssr/n_obs) %>%
    mutate(var = v)
}

mean_val2k <- bind_rows(group_meansc2k) %>%
  group_by(group) %>% summarise(mean = mean(abs_mean_slope)) %>%
  mutate(df = "anes2k")
bind_rows(group_meansc2k) %>%
  ggplot(aes(x = abs_mean_slope, y = group, group = var)) + 
  geom_point(data = mean_val2k, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_point(color = "black", shape = 21, aes(fill = group)) + 
  geom_path(alpha = .1) + 
  theme_bw() + 
  theme(legend.position = "none") 

mean_var2k <- bind_rows(variancec2k) %>%
  group_by(group) %>% summarise(mean = mean(mse)) %>%
  mutate(df = "anes2k")
bind_rows(variancec2k) %>%
  ggplot(aes(x = mse, y = group, group = var)) + 
  geom_point(data = mean_var2k, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_path(alpha = .1) + 
  geom_point() + 
  theme_bw()


bind_rows(mean_val5, mean_val8, mean_val9, mean_val7,
          mean_val2k) %>%
  mutate(year = recode(df, "anes2k"=2000, "anes5"=1956,
                       "anes7"=1972, "anes9"=1992,
                       "anes8"=1980)) %>%
  ggplot(aes(x = year, y = mean, color = group)) + 
  geom_point() + 
  theme_bw() 

summary_c5 <- bind_rows(omegas_c5) %>%
  select(var, omega_25, omega_33, omega_34) %>%
  pivot_longer(omega_25:omega_34) %>%
  group_by(name) %>% summarise(mean = mean(value)) %>%
  mutate(year = 1956)
bind_rows(omegas_c5) %>%
  select(var, omega_25, omega_33, omega_34) %>%
  pivot_longer(omega_25:omega_34) %>%
  ggplot(aes(x = value, y = name, fill = name)) + 
  geom_point(data=summary_c5, aes(x =mean, fill = name),
             size = 8, shape = 21) + 
  geom_point(shape = 21) + 
  theme_bw() + 
  labs(x = "Omega", y = "", color = "Age Group", fill = "Age Group")

summary_c7 <- bind_rows(omegas_c7) %>%
  select(var, omega_25, omega_33, omega_34) %>%
  pivot_longer(omega_25:omega_34) %>%
  group_by(name) %>% summarise(mean = mean(value)) %>%
  mutate(year = 1972)
bind_rows(omegas_c7) %>%
  select(var, omega_25, omega_33, omega_34) %>%
  pivot_longer(omega_25:omega_34) %>%
  ggplot(aes(x = value, y = name, fill = name)) + 
  geom_point(data=summary_c7, aes(x =mean, fill = name),
             size = 8, shape = 21) + 
  geom_point(shape = 21) + 
  theme_bw() + 
  labs(x = "Omega", y = "", color = "Age Group", fill = "Age Group")

summary_c8 <- bind_rows(omegas_c8) %>%
  select(var, omega_25, omega_33, omega_34) %>%
  pivot_longer(omega_25:omega_34) %>%
  group_by(name) %>% summarise(mean = mean(value)) %>%
  mutate(year = 1980)
bind_rows(omegas_c8) %>%
  select(var, omega_25, omega_33, omega_34) %>%
  pivot_longer(omega_25:omega_34) %>%
  ggplot(aes(x = value, y = name, fill = name)) + 
  geom_point(data=summary_c8, aes(x =mean, fill = name),
             size = 8, shape = 21) + 
  geom_point(shape = 21) + 
  theme_bw() + 
  labs(x = "Omega", y = "", color = "Age Group", fill = "Age Group")


summary_c9 <- bind_rows(omegas_c9) %>%
  select(var, omega_25, omega_33, omega_34) %>%
  pivot_longer(omega_25:omega_34) %>%
  group_by(name) %>% summarise(mean = mean(value)) %>%
  mutate(year = 1992)
bind_rows(omegas_c9) %>%
  select(var, omega_25, omega_33, omega_34) %>%
  pivot_longer(omega_25:omega_34) %>%
  ggplot(aes(x = value, y = name, fill = name)) + 
  geom_point(data=summary_c9, aes(x =mean, fill = name),
             size = 8, shape = 21) + 
  geom_point(shape = 21) + 
  theme_bw() + 
  labs(x = "Omega", y = "", color = "Age Group", fill = "Age Group")

bind_rows(summary_c5, summary_c7, summary_c8, summary_c9) %>%
  ggplot(aes(x = mean, y = year, fill = name)) + 
  geom_point(shape = 21)


bind_rows(results) %>%
  filter(term %in% c("wave", "sd__wave")) %>%
  mutate(term = recode(term, "wave"="Wave", "sd__wave"="S.D. RE Wave")) %>%
  mutate(ci.low = estimate - 1.96*std.error,
         ci.high = estimate + 1.96*std.error,
         ci.low = ifelse(estimate < 0, -1*ci.low, ci.low),
         ci.high = ifelse(estimate < 0, -1*ci.high, ci.high),
         estimate = ifelse(estimate < 0, -1*estimate, estimate)) %>%
  ggplot(aes(x = estimate, y = group, xmin = ci.low,
             xmax = ci.high,
             color = as.factor(group))) + 
  geom_vline(xintercept = 0, linetype = 2, color = "black") + 
  geom_linerange() + geom_point() + 
  theme_bw() + 
  facet_grid(question~term) + 
  labs(x = "Estimate", y = "", color = "Age Group")

t <- canes5 %>%
  filter(var == "helpblk") %>%
  pivot_longer(y1:y3) %>%
  mutate(value = (value - mean(value, na.rm=TRUE))/sd(value,na.rm=TRUE)) %>%
  mutate(wave = as.numeric(gsub("y", "", name))) %>%
  mutate(group = ifelse(age_1 <= 25, "18-25",
                        ifelse(age_1 >= 26 & age_1 <= 33, "26-33",
                               "34+")))

m1 <- lmer(value ~ wave + (1+wave|id),
           data = t %>% filter(group == "18-25"))
m2 <- lmer(value ~ wave + (1+wave|id),
           data = t %>% filter(group == "26-33"))
m3 <- lmer(value ~ wave + (1+wave|id),
           data = t %>% filter(group == "34+"))
t1.56 <- tidy(m1) %>% mutate(year = 1956, group = "18-25")
t2.56 <- tidy(m2) %>% mutate(year = 1956, group = "26-33")
t3.56 <- tidy(m3) %>% mutate(year = 1956, group = "34+")


t <- cg10 %>%
  filter(var == "helpblk") %>%
  pivot_longer(y1:y3) %>%
  mutate(value = (value - mean(value, na.rm=TRUE))/sd(value,na.rm=TRUE)) %>%
  mutate(wave = as.numeric(gsub("y", "", name))) %>%
  mutate(group = ifelse(age_1 <= 25, "18-25",
                        ifelse(age_1 >= 26 & age_1 <= 33, "26-33",
                               "34+")))

m1 <- lmer(value ~ wave + (1+wave|id),
           data = t %>% filter(group == "18-25"))
m2 <- lmer(value ~ wave + (1+wave|id),
           data = t %>% filter(group == "26-33"))
m3 <- lmer(value ~ wave + (1+wave|id),
           data = t %>% filter(group == "34+"))




