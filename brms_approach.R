



all_panels |> 
  mutate(df_id = paste(df, id, sep = "-")) |> 
  group_by(df, name, df_id) |> 
  mutate(deviation = (value - mean(value, na.rm=TRUE))^2) |> 
  group_by(df, name) |> 
  summarise(mean_deviation = sqrt(mean(deviation, na.rm=TRUE))) |> 
  ggplot(aes(y = name, x = df, fill = mean_deviation)) + 
  geom_tile() + 
  theme_classic() + 
  scale_fill_viridis_c()


t <- all_panels |> 
  mutate(df_id = paste(df, id, sep = "-")) |> 
  mutate(birth_year = year(date) - age) |> 
  group_by(df, name, df_id) |> 
  mutate(deviation = (value - mean(value, na.rm=TRUE))^2) |> 
  group_by(df, name, df_id) |> 
  summarise(age = min(age,na.rm=TRUE),
            birth_year = min(birth_year,na.rm=TRUE),
            deviation = mean(deviation)) |> 
  ungroup() |> 
  mutate(deviation = deviation + 1e-6) |> 
  mutate(ldev = log(deviation)) |> 
  filter(age > 17, age < 80)

library(lme4)
g1 <- lmer(ldev ~ (1|name) + df + as_factor(age), data = t)

library(broom.mixed)
tidy(g1) |> 
  filter(term != "(Intercept)",
         grepl("df", term)==FALSE,
         effect != "ran_pars") |> 
  mutate(term = as.numeric(gsub("as_factor\\(age\\)", "", term))) |> 
  filter(term < 90) |> 
  ggplot(aes(x = term, y = estimate, ymin = estimate - 1.96*std.error,
             ymax = estimate + 1.96*std.error)) + 
  geom_hline(yintercept = 0, linetype = 2, color = "gray") + 
  geom_linerange() + 
  geom_point() + 
  theme_classic() + 
  labs(x = "Age", y = "Coefficient",
       title = "")




t |> 
  group_by(df, name) |> 
  summarise(m = mean(ldev),
            n = n(),
            v = sd(ldev)) |> 
  mutate(se = v/sqrt(n),
         ci.low = m - 1.96*se, ci.high = m + 1.96*se,
         year = as.numeric(str_extract(df, "^\\d{4}"))) |> 
  group_by(name) |> filter(n() > 5) |> 
  ggplot(aes(x=year, y = m, color = df, ymin = ci.low,
             ymax = ci.high)) + 
  geom_linerange() + 
  geom_point() + 
  facet_wrap(~name, scales = "free_y") + 
  theme_bw() + 
  theme(legend.position = "none")


t2 <- t |> 
  mutate(ag = cut(age,
                  breaks = seq(17, 82, by = 5),
                  right = FALSE),
         cg = cut(birth_year,
                  breaks = seq(1875, 2004, by = 5),
                  right = FALSE)) |> 
  mutate(agcg = paste(ag, cg, sep = "-")) |> 
  group_by(agcg) |>  filter(n() > 15) |>  ungroup()
  

t2 |> 
  group_by(ag, cg) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = ag, y = cg, fill = n)) + 
  geom_tile()

l2 <- lmer(ldev ~ agcg + df + (1|name), data = t2)

newdata <- expand_grid(agcg = unique(t2$agcg), df = "2000-04 ANES") |> 
  separate(agcg, into = c("ag", "cg"), sep = "-", remove = FALSE)

newdata$fit <- predict(l2, newdata = newdata, re.form = NA)

newdata |> 
  ggplot(aes(x = ag, y = cg, fill = fit)) + 
  geom_tile() + 
  theme_classic() + 
  scale_fill_viridis_c()


install.packages("brms")
library(brms)




bf_var <- bf(
  ldev ~ 1 +
    s(age, by = cg, k = 3) +
    (1 | df) +
    (1 | name)
)

fit_var <- brm(
  bf_var,
  data = t2,
  family = gaussian(),
  cores = 4, chains = 4,
  prior = c(
    prior(normal(0, 1), class = "Intercept"),
    prior(exponential(1), class = "sd"),
    prior(exponential(1), class = "sds"),
    prior(exponential(1), class = "sigma")
  )
)


m1 <- glm(a ~ d + df, data = long_difference,
          family = binomial(link = "logit"))


long_difference |> 
  group_by(df, t1, t2, co, ag) |> 
  summarise(duration = mean(d),
            n = n(), 
            change = sum(a))






