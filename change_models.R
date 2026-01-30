

load("./data/long_difference.Rdata")

wave_summary <- long_difference |> 
  mutate(cohort = cut(birth_year, 
                      breaks = seq(min(birth_year,na.rm=TRUE),
                                   max(birth_year, na.rm=TRUE), by = 10),
                      right = FALSE)) |> 
  group_by(name, t1, t2, df, cohort, ag) |> 
  summarise(d = mean(d), n = n(), change = sum(a)) |> 
  mutate(pct_change = change / n) |> 
  mutate(df = factor(df),
         name = factor(name),
         cohort = factor(cohort))


summary(lm(pct_change ~ d + name + df, data = wave_summary))


fit_toy <- brm(
  change | trials(n) ~ 1 + d + name,
  data   = wave_summary,
  family = binomial(link = "logit"),
  chains = 2, iter = 1000, cores = 2
)

fit_toy <- brm(
  change | trials(n) ~ 1 + d + (1 + d|name),
  data   = wave_summary,
  family = binomial(link = "logit"),
  chains = 2, iter = 1000, cores = 2
)


fit_age <- brm(
  change | trials(n) ~ 1 + s(ag, k = 5) + d,
  data = wave_summary,
  family = binomial(link = "logit"),
  chains = 2, iter = 1000, cores = 2
)


fit_age_cohort_panel_item_2 <- brm(
  change | trials(n) ~ 1 + d+ s(ag, cohort, bs = "fs", k = 5) + 
    (1|df) + (1 + d|name),
  data = wave_summary,
  family = binomial(link = "logit"),
  chains = 4, iter = 2000, cores = 4
)

save(fit_age_cohort_panel_item_2, file = "data/fit_acp.Rdata")

# fit_age_cohort <- brm(
#   change | trials(n) ~ 1 + s(ag, k = 5) + d,
#   data   = wave_summary,
#   family = binomial(link = "logit"),
#   chains = 2, iter = 1000, cores = 2
# )



wave_summary |> 
  filter(name %in% c("partyid", "polviews", "trust",
                     "attentioncpg", "ppllikeme")) |> 
  group_by(name, df, t1, t2) |> 
  summarise(d = mean(d),
            n = sum(n),
            c = sum(change)) |> 
  mutate(pct_change = c/n) |> 
  ggplot(aes(x = d, y = pct_change, color = name)) + 
  geom_point() + 
  geom_smooth(method = "lm") + theme_classic()
