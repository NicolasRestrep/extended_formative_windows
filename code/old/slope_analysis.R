
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



canes8 <- canes8
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
    mutate(year = (year - 1)*.375) %>%
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


canes90 <- canes90 %>%
  filter(var != "adjmoral",
         var != "newstyles")
vars <- unique(canes90$var)
group_meansc90 <- vector(mode = "list", length = length(vars))
variancec90 <- vector(mode = "list", length = length(vars))
for (i in 1:length(vars)) {
  v <- vars[i]
  df <- canes90 %>%
    filter(var == v) %>%
    pivot_longer(y1:y3) %>%
    filter(!is.na(value),
           !is.na(age_1)) %>%
    group_by(id) %>% filter(n() > 1) %>%
    ungroup() %>%
    mutate(year = as.numeric(gsub("y", "", name))) %>%
    select(-name) %>%
    mutate(value = (value - mean(value,na.rm=TRUE))/(sd(value,na.rm=TRUE))) %>%
    mutate(year = (year - 1)) %>%
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
  group_meansc90[[i]] <- df %>% 
    group_by(id) %>% 
    slice(1) %>%
    group_by(group) %>%
    summarise(n = n(), abs_mean_slope = mean(abs(slope),na.rm=TRUE)) %>%
    mutate(var = v)
  
  variancec90[[i]] <- df %>%
    mutate(resid = (value - yhat)^2) %>%
    group_by(group) %>%
    summarise(ssr = sum(resid), 
              n_obs = n(),
              n_id = length(unique(id))) %>%
    mutate(mse = ssr/n_obs) %>%
    mutate(var = v)
}

mean_val90 <- bind_rows(group_meansc90) %>%
  group_by(group) %>% summarise(mean = mean(abs_mean_slope)) %>%
  mutate(df = "anes90")
bind_rows(group_meansc90) %>%
  ggplot(aes(x = abs_mean_slope, y = group, group = var)) + 
  geom_point(data = mean_val90, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_point(color = "black", shape = 21, aes(fill = group)) + 
  geom_path(alpha = .1) + 
  theme_bw() + 
  theme(legend.position = "none") 

mean_var90 <- bind_rows(variancec90) %>%
  group_by(group) %>% summarise(mean = mean(mse)) %>%
  mutate(df = "anes90")
bind_rows(variancec90) %>%
  ggplot(aes(x = mse, y = group, group = var)) + 
  geom_point(data = mean_var90, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_path(alpha = .1) + 
  geom_point() + 
  theme_bw()



cg6 <- cg6 
vars <- unique(cg6$var)
group_meansc06 <- vector(mode = "list", length = length(vars))
variancec06 <- vector(mode = "list", length = length(vars))
for (i in 1:length(vars)) {
  v <- vars[i]
  df <- cg6 %>%
    filter(var == v) %>%
    pivot_longer(y1:y3) %>%
    filter(!is.na(value),
           !is.na(age_1)) %>%
    group_by(id) %>% filter(n() > 1) %>%
    ungroup() %>%
    mutate(year = as.numeric(gsub("y", "", name))) %>%
    select(-name) %>%
    mutate(value = (value - mean(value,na.rm=TRUE))/(sd(value,na.rm=TRUE))) %>%
    mutate(year = (year - 1)*1) %>%
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
  group_meansc06[[i]] <- df %>% 
    group_by(id) %>% 
    slice(1) %>%
    group_by(group) %>%
    summarise(n = n(), abs_mean_slope = mean(abs(slope),na.rm=TRUE)) %>%
    mutate(var = v)
  
  variancec06[[i]] <- df %>%
    mutate(resid = (value - yhat)^2) %>%
    group_by(group) %>%
    summarise(ssr = sum(resid), 
              n_obs = n(),
              n_id = length(unique(id))) %>%
    mutate(mse = ssr/n_obs) %>%
    mutate(var = v)
}

mean_val06 <- bind_rows(group_meansc06) %>%
  group_by(group) %>% summarise(mean = mean(abs_mean_slope)) %>%
  mutate(df = "gss06")
bind_rows(group_meansc06) %>%
  ggplot(aes(x = abs_mean_slope, y = group, group = var)) + 
  geom_point(data = mean_val06, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_point(color = "black", shape = 21, aes(fill = group)) + 
  geom_path(alpha = .1) + 
  theme_bw() + 
  theme(legend.position = "none") 

mean_var06 <- bind_rows(variancec06) %>%
  group_by(group) %>% summarise(mean = mean(mse)) %>%
  mutate(df = "gss06")
bind_rows(variancec06) %>%
  ggplot(aes(x = mse, y = group, group = var)) + 
  geom_point(data = mean_var06, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_path(alpha = .1) + 
  geom_point() + 
  theme_bw()


cg8 <- cg8 
vars <- unique(cg8$var)
group_meansc08 <- vector(mode = "list", length = length(vars))
variancec08 <- vector(mode = "list", length = length(vars))
for (i in 1:length(vars)) {
  v <- vars[i]
  df <- cg8 %>%
    filter(var == v) %>%
    pivot_longer(y1:y3) %>%
    filter(!is.na(value),
           !is.na(age_1)) %>%
    group_by(id) %>% filter(n() > 1) %>%
    ungroup() %>%
    mutate(year = as.numeric(gsub("y", "", name))) %>%
    select(-name) %>%
    mutate(value = (value - mean(value,na.rm=TRUE))/(sd(value,na.rm=TRUE))) %>%
    mutate(year = (year - 1)*1) %>%
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
  group_meansc08[[i]] <- df %>% 
    group_by(id) %>% 
    slice(1) %>%
    group_by(group) %>%
    summarise(n = n(), abs_mean_slope = mean(abs(slope),na.rm=TRUE)) %>%
    mutate(var = v)
  
  variancec08[[i]] <- df %>%
    mutate(resid = (value - yhat)^2) %>%
    group_by(group) %>%
    summarise(ssr = sum(resid), 
              n_obs = n(),
              n_id = length(unique(id))) %>%
    mutate(mse = ssr/n_obs) %>%
    mutate(var = v)
}

mean_val08 <- bind_rows(group_meansc08) %>%
  group_by(group) %>% summarise(mean = mean(abs_mean_slope)) %>%
  mutate(df = "gss08")
bind_rows(group_meansc08) %>%
  ggplot(aes(x = abs_mean_slope, y = group, group = var)) + 
  geom_point(data = mean_val08, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_point(color = "black", shape = 21, aes(fill = group)) + 
  geom_path(alpha = .1) + 
  theme_bw() + 
  theme(legend.position = "none") 

mean_var08 <- bind_rows(variancec08) %>%
  group_by(group) %>% summarise(mean = mean(mse)) %>%
  mutate(df = "gss08")
bind_rows(variancec08) %>%
  ggplot(aes(x = mse, y = group, group = var)) + 
  geom_point(data = mean_var08, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_path(alpha = .1) + 
  geom_point() + 
  theme_bw()


cg10 <- cg10
vars <- unique(cg10$var)
group_meansc10 <- vector(mode = "list", length = length(vars))
variancec10 <- vector(mode = "list", length = length(vars))
for (i in 1:length(vars)) {
  v <- vars[i]
  df <- cg10 %>%
    filter(var == v) %>%
    pivot_longer(y1:y3) %>%
    filter(!is.na(value),
           !is.na(age_1)) %>%
    group_by(id) %>% filter(n() > 1) %>%
    ungroup() %>%
    mutate(year = as.numeric(gsub("y", "", name))) %>%
    select(-name) %>%
    mutate(value = (value - mean(value,na.rm=TRUE))/(sd(value,na.rm=TRUE))) %>%
    mutate(year = (year - 1)*1) %>%
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
  group_meansc10[[i]] <- df %>% 
    group_by(id) %>% 
    slice(1) %>%
    group_by(group) %>%
    summarise(n = n(), abs_mean_slope = mean(abs(slope),na.rm=TRUE)) %>%
    mutate(var = v)
  
  variancec10[[i]] <- df %>%
    mutate(resid = (value - yhat)^2) %>%
    group_by(group) %>%
    summarise(ssr = sum(resid), 
              n_obs = n(),
              n_id = length(unique(id))) %>%
    mutate(mse = ssr/n_obs) %>%
    mutate(var = v)
}

mean_val10 <- bind_rows(group_meansc10) %>%
  group_by(group) %>% summarise(mean = mean(abs_mean_slope)) %>%
  mutate(df = "gss10")
bind_rows(group_meansc10) %>%
  ggplot(aes(x = abs_mean_slope, y = group, group = var)) + 
  geom_point(data = mean_val10, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_point(color = "black", shape = 21, aes(fill = group)) + 
  geom_path(alpha = .1) + 
  theme_bw() + 
  theme(legend.position = "none") 

mean_var10 <- bind_rows(variancec10) %>%
  group_by(group) %>% summarise(mean = mean(mse)) %>%
  mutate(df = "gss10")
bind_rows(variancec10) %>%
  ggplot(aes(x = mse, y = group, group = var)) + 
  geom_point(data = mean_var10, aes(x = mean, fill = group), size = 5, shape = 21) + 
  geom_path(alpha = .1) + 
  geom_point() + 
  theme_bw()




bind_rows(mean_val5, mean_val8, mean_val9, mean_val7,
          mean_val2k, mean_val90, mean_val06,
          mean_val08, mean_val10) %>%
  mutate(year = recode(df, "anes2k"=2000, "anes5"=1956,
                       "anes7"=1972, "anes9"=1992,
                       "anes8"=1980, "anes90"=1990,
                       "gss06"=2006, "gss08"=2008,
                       "gss10"=2010)) %>%
  filter(year != 1980, year != 1990) %>%
  ggplot(aes(x = year, y = mean, fill = group)) + 
  geom_point(size = 2, shape = 21, color = "black") + 
  theme_bw() 



bind_rows(bind_rows(group_meansc5) %>% mutate(year = 1956), 
          bind_rows(group_meansc7) %>% mutate(year = 1972),
          bind_rows(group_meansc9) %>% mutate(year = 1992), 
          bind_rows(group_meansc2k) %>% mutate(year = 2000),
          bind_rows(group_meansc06) %>% mutate(year = 2006),
          bind_rows(group_meansc08) %>% mutate(year = 2008),
          bind_rows(group_meansc10) %>% mutate(year = 2010)) %>%
  filter(var %in% c("polviews", "partyid")) %>%
  ggplot(aes(x = year, y = abs_mean_slope, fill = group)) +
  geom_point(shape = 21) + 
  facet_wrap(~var)
