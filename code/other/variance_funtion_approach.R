df <- all_panels |> 
  filter(name %in% c("partyid", "polviews")) |> 
  mutate(dfid = paste(df, id, sep = "-")) |> 
  group_by(dfid, name) |> 
  mutate(val_dev = value - mean(value),
         age_dev = age - mean(age)) |> 
  filter(age > 17, age < 89)  |> 
  mutate(birth_year = year(date) - age) |> 
  filter(birth_year > 1883) |> 
  ungroup()

df$cohort <- cut(
  df$birth_year,
  breaks = seq(min(df$birth_year, na.rm = TRUE),
               max(df$birth_year, na.rm = TRUE) + 3,
               by = 3),
  right = FALSE
)

df$age_grp <- cut(
  df$age,
  breaks = seq(min(df$age, na.rm = TRUE),
               max(df$age, na.rm = TRUE) + 3,
               by = 3),
  right = FALSE
)

df <- df |> 
  mutate(ac = paste(age, birth_year, sep = "_"))

df |> 
  group_by(age, birth_year, ac) |> 
  summarise(mean_var = mean(val_dev^2),
            n = n()) |> 
  #filter(n > 11) |> 
  ggplot(aes(x = age, y = birth_year, fill = mean_var, label = n)) + 
  geom_tile() + 
  geom_text(size = 2) + 
  theme_classic() + 
  scale_fill_viridis_c()

m1 <- lm(val_dev ~ name, data = df)
df$R2 <- resid(m1)^2
m2 <- glm(log(R2) ~ name + ac, data = df)

ndf <- expand_grid(age = unique(df$age), 
            cohort = unique(df$cohort))


p1 <- predict(m2, newdata = ndf, se.fit = TRUE)
ndf$fit <- p1$fit
ndf$se <- p1$se.fit

ndf |> 
  ggplot(aes(x = age, y = cohort, fill = fit)) + 
  geom_tile(color = "black") + 
  scale_fill_viridis_c() + 
  theme_classic()
  

long_difference |> 
  group_by(name, df) |> 
  summarise(pct_chg = mean(a))



