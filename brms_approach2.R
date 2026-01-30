

all_panels |> 
  group_by(name) |> 
  summarise(n_pan = length(unique(df))) |> 
  arrange(n_pan)


change_summary <- all_panels |> 
  filter(age < 80, age > 17) |> 
  mutate(birth_year = year(date) - age) |> 
  filter(birth_year >= 1910) |> 
  mutate(cohort = cut(birth_year, 
                      breaks = seq(min(birth_year,na.rm=TRUE),
                                   max(birth_year, na.rm=TRUE), by = 10),
                      right = FALSE)) |> 
  group_by(df, id, name) |> 
  mutate(change = ifelse((value > 50 & dplyr::lag(value) <= 50) | (value < 50 & dplyr::lag(value) >= 50), 1, 0)) |> 
  filter(wave != 1) |> 
  filter(!is.na(change)) |> 
  group_by(name, df, cohort, age) |> 
  summarise(n = n(), change = sum(change),
            unique_waves = length(unique(wave))) |> 
  mutate(pct_change = change / n) |> 
  mutate(df = factor(df),
         name = factor(name),
         cohort = factor(cohort)) |> 
  na.omit() |> 
  ungroup()



change_summary |> 
  ggplot(aes(x = age, y = pct_change)) + 
  geom_point() + 
  facet_wrap(~cohort)

change_summary |> 
  group_by(cohort, age) |> 
  summarise(n = n()) |> 
  ggplot(aes(x = age, y = cohort, fill = n)) + 
  geom_tile()


library(splines)
library(lme4)
m1 <- lm(pct_change ~ df + name + ns(age, df = 6), data = change_summary)

new_data <- expand_grid(name = c("polviews", "partyid", "trust"),
           age = 18:89,
           df = "2006-10 GSS")

p1 <- predict(m1, newdata = new_data, se.fit = TRUE)
new_data$fit <- p1$fit
new_data$se <- p1$se.fit

new_data |> 
  ggplot(aes(x = age, y = fit, color = name,
             ymin = fit - 1.96*se, 
             ymax = fit + 1.96*se)) +
  geom_linerange() + 
  geom_point() + 
  geom_line() + 
  theme_classic()


change_summary |> 
  ggplot(aes(y = reorder(name, pct_change), x = pct_change, color = df)) + 
  geom_point() +
  theme_classic() + 
  scale_color_viridis_d()


summary(lm(pct_change ~ unique_waves + df, data = change_summary))

#### Bayesian Models ###########

library(brms)

fit_toy <- brm(
  change | trials(n) ~ 1 + age + cohort,
  data   = change_summary,
  family = binomial(link = "logit"),
  chains = 2, iter = 1000, cores = 2
)

fit_age <- brm(
  change | trials(n) ~ 1 + s(age, k = 5),
  data = change_summary,
  family = binomial(link = "logit"),
  chains = 2, iter = 1000, cores = 2
)

fit_age_cohort <- brm(
  change | trials(n) ~ 1 + s(age, cohort, bs = "fs", k = 5),
  data = change_summary,
  family = binomial(link = "logit"),
  chains = 2, iter = 1000, cores = 2
)

fit_age_cohort_panel <- brm(
  change | trials(n) ~ 1 + s(age, cohort, bs = "fs", k = 5) + 
    (1|df),
  data = change_summary,
  family = binomial(link = "logit"),
  chains = 2, iter = 1000, cores = 2
)

fit_age_cohort_panel_item <- brm(
  change | trials(n) ~ 1 + s(age, cohort, bs = "fs", k = 5) + 
    (1|df) + (1|name),
  data = change_summary,
  family = binomial(link = "logit"),
  chains = 2, iter = 1000, cores = 2
)

fit_age_cohort_panel_item_2 <- brm(
  change | trials(n) ~ 1 + s(age, cohort, bs = "fs", k = 5) + 
    (1|df) + (1|name),
  data = change_summary,
  family = binomial(link = "logit"),
  chains = 2, iter = 1000, cores = 2
)




bf_switch <- bf(
  change | trials(n) ~ 1 +
    s(age, by = cohort, k = 3) +  # cohort-specific age curves
    (1 | df) +                      # panel RE
    (1 | name)                         # item RE
)

priors <- c(
  prior(normal(0, 2), class = "Intercept"),
  prior(exponential(1), class = "sd"),    # panel, item SDs
  prior(exponential(1), class = "sds")    # smooth term SDs
)

fit_switch <- brm(
  bf_switch,
  data   = change_summary,
  family = binomial(link = "logit"),
  prior  = priors,
  chains = 4, cores = 4
)









