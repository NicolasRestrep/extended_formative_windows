# Analyses ----
# Packages ----
library(tidyverse)
library(lme4)
library(broom.mixed)
library(gt)

# Load data ----
load('data/long_difference.Rdata')

# Get rid of questions less than 3 waves
long_difference <- long_difference |> 
  filter(name != "eqrightstoofar",
         name != "ftillimmigrants",
         name != "ftcath",
         name != "lessgvt",
         name != "nathome",
         name != "natfood",
         name != "noteveryonechance",
         name != "polattn", 
         name != "slavediff") 

# Get rid of people with only one observation
long_difference <- long_difference |> 
  group_by(dfid) |> 
  mutate(N = n()) |> 
  filter (N != 1) |> 
  ungroup()


# Standardize columns ----
long_difference <- mutate(long_difference, 
              dz = scale(d)[,1],
              agz = scale(ag)[,1],
              coz = scale(co)[,1])


# Fit the model (takes a while) ----
start <- Sys.time()
mod <- lmer(a ~ dz + agz + coz + agz*coz + 
              (1 + dz + agz + coz + agz*coz|name) + 
              (1|dfid),
            data = long_difference,
            weights = weight,
            control = lmerControl(optimizer="bobyqa"))
end <- Sys.time()

# Save model ----

saveRDS(mod,"smaller_model.rds")

# Table 1 ----
mod <- readRDS('smaller_model.rds')
# Get model output 
coefs <- tidy(mod)

# Get fixed effects 
fixed_effects <- coefs |>
  filter(effect == "fixed") |>
  mutate(
    term = recode(term,
                  `(Intercept)` = "Intercept",
                  `dz` = "Duration",
                  `agz` = "Age",
                  `coz` = "Cohort",
                  `agz:coz` = "Age * Cohort"),
    fixed_col = sprintf("%.3f\n(%.3f)", estimate, std.error)
  ) |>
  select(term, fixed_col)

# Get tandard deviations of random effects
sd_effects <- coefs |>
  filter(term %in% c("sd__(Intercept)", "sd__dz", "sd__agz", "sd__coz", "sd__agz:coz", "sd__Observation")) |>
  mutate(
    group = case_when(
      group == "name" ~ "Question Random Effects S.D.",
      group == "dfid" ~ "Individual Random Effects S.D.",
      TRUE ~ NA_character_
    ),
    term = recode(term,
                  `sd__(Intercept)` = "Intercept",
                  `sd__dz` = "Duration",
                  `sd__agz` = "Age",
                  `sd__coz` = "Cohort",
                  `sd__agz:coz` = "Age * Cohort")
  ) |>
  filter(!is.na(group)) |>
  select(term, group, estimate) |>
  pivot_wider(names_from = group, values_from = estimate)

# Combine FE and SDs for RE
table_df <- full_join(fixed_effects, sd_effects, by = "term") |>
  arrange(factor(term, levels = c("Intercept", "Duration", "Age", "Cohort", "Age * Cohort")))

# Add metadata from model 
meta <- tibble(
  term = c("N Questions", "N Individuals", "N"),
  fixed_col = as.character(c(length(unique(mod@frame$name)),
                             length(unique(mod@frame$dfid)),
                             nrow(mod@frame)))
)

# Bind rows and format
final_table <- bind_rows(table_df, meta) |>
  select(term, fixed_col, `Question Random Effects S.D.`, `Individual Random Effects S.D.`) |> 
  gt() |>
  cols_label(
    term = "",
    fixed_col = "Fixed Effects",
    `Question Random Effects S.D.` = "Question\nRandom Effects S.D.",
    `Individual Random Effects S.D.` = "Individual\nRandom Effects S.D."
  ) |>
  tab_options(
    table.font.size = 12,
    data_row.padding = px(5)
  ) |> 
  tab_caption("Model with Full Data")

# View the table
final_table

# Figure 2 ----
# Figure for age-cohort grid
# age (in decades, centered on 44) and 
# cohort (also in decades, centered on 1950)
new_data <- expand_grid(agz = seq(-1.77, 2.1, by = .1),
                         coz = seq(-3.5, 2.4, by = .1),
                         dz = 0,
                         name = c("polviews", "partyid")) # This will be ignored

new_data$fitted <- predict(mod, 
                            new_data,
                            re.form = NA) #re.form removes random effects from the prediction model

figure_2 <- new_data |>
  # Get them back into interpretable scale
  mutate(co = 10 * (coz * sd(long_difference$co) + mean(long_difference$co)) 
         + 1950,
         ag = 10 * (agz * sd(long_difference$ag) + mean(long_difference$ag)) 
         + 44) |>
  #construct a "period" term to remove cases we don't have data for
  mutate(period = co + ag) |>
  filter(period > 1940, period < 2030) |>
  ggplot(aes(x = ag, y = co, fill = fitted)) + 
  geom_tile(color = "black") +
  theme_classic(base_size = 20) + 
  scale_fill_viridis_c(option = "C") + 
  labs(x = "Age", y = "Cohort", 
       fill = "Predicted\nChange")

figure_2

# Figure 3 -----


# Get random effects from model object (takes a while)
model_refs <- as.data.frame(ranef(mod))

# Get intercepts and duration effects by question
re_question <- model_refs |>
  filter(grpvar == "name") |>
  filter(term %in% c("(Intercept)", "dz")) |>
  mutate(
    term = recode(term,
                  "(Intercept)" = " Intercept (Transitory Change)",
                  "dz" = "Duration (Durable Change)")
  )

# Get standard deviation for duration
duration_sd <- sd(long_difference$d)

# Rescale the slope and its standard error
re_question <- re_question |>
  mutate(
    expected = case_when(
      term == " Intercept (Transitory Change)" ~ condval + 17.8,
      term == "Duration (Durable Change)"     ~ condval + 1.011
    )
  )

#Main effect lines
mes <- data.frame(term = c(" Intercept (Transitory Change)",
                      "Duration (Durable Change)"), values = c(17.80, 1.01))

# Plot
re_plot <- re_question |> 
  mutate(grp = recode(grp, "unequalnoprob"="unequal", "worrylesseq"="worryless",
                      "ftasianamericans"="ftasian", "ftenvironmental"="ftenv",
                      "eqoppsuccess"="eqopp", "equallessprob"="eqlprob",
                      "matterhrdwrk"="matterwrk", "opposehomo"="gaylaw",
                      "ftfeminists"="ftfem")) %>%
  ggplot(aes(y = reorder(grp, condval), x = expected,
             xmin = expected - 1.96*condsd,
             xmax = expected + 1.96*condsd)) +
  geom_vline(data = mes, aes(xintercept = values), linetype = 2) +
  geom_point() +
  geom_errorbarh(height = 0.2) +
  facet_wrap(~ term, scales = "free_x") +
  theme_bw(base_size = 10) +
  labs(
    y = "Question",
    x = "Expected Absolute Change"
  )

re_plot

# Figure 4 ----

# Define cohort values (standardized)
raw_cohs <- long_difference |> 
  mutate(
    cohort = round(10 * (coz * sd(co) + mean(co)) + 1950)
  ) |> 
  filter(cohort %in% c(1930,1955,1980)) |> 
  select(cohort,coz) |> 
  group_by(cohort) |> 
  summarise(co = mean(coz)) |> 
  pull(co)


# Build grid for predictions
pred_grid <- expand_grid(
  agz = seq(min(long_difference$agz), max(long_difference$agz), length.out = 100),
  coz = raw_cohs,
  dz = 0,
  name = c("partyid", "polviews", "trust", "ppllikeme",
           "ftdems", "ftreps", "attentioncpg", "natenvir"),
  dfid = NA
)

# Transform cohort values 
pred_grid <- pred_grid |> 
  mutate(
    cohort = case_when(
      coz == raw_cohs[1] ~ 1930, 
      coz == raw_cohs[2] ~ 1955,
      coz == raw_cohs[3] ~ 1980
    ), 
    age = 10 * (agz * sd(long_difference$ag) + mean(long_difference$ag)) 
    + 44
  )

# Predict values
pred_grid$predicted_a <- predict(mod, newdata = pred_grid, re.form = ~(1 + dz + agz + coz + agz:coz | name))

# Plot
pred_grid |> 
  mutate(name = recode(name, "attentioncpg"="Interest in politics",
                       "ftdems"="Thermometer: Democrats", 
                       "ftreps"="Thermometer: Republicans",
                       "natenvir"="Spending on environment", 
                       "partyid"="Partisan identification (7-point)",
                       "polviews"="Ideological identification (7-point)",
                       "ppllikeme"="People have no say in gov",
                       "trust"="People can be trusted")) |>
ggplot(aes(x = age, y = predicted_a, color = factor(cohort),
           linetype = factor(cohort))) +
  geom_line(linewidth = 1) +
  facet_wrap(~ name, ncol = 2) +
  theme_bw(base_size = 14) +
  scale_color_viridis_d(name = "Cohort") +
  labs(
    x = "Age",
    y = "Predicted Change",
    linetype = "Cohort"
  )

# Only Partisan Identification

pid_df <- long_difference %>% filter(name == "partyid") %>%
  mutate(agz2 = agz^2)

mod_pid1 <- lm(a ~ dz + agz + coz + agz*coz,
              data = pid_df,
              weights = weight)

mod_pid2 <- lm(a ~ dz + agz + agz2+ coz + agz*coz + agz2*coz,
              data = pid_df,
              weights = weight)

mod_pid3 <- lm(a ~ dz + agz + coz + agz*coz + 
                dz*agz + dz*coz + dz*agz*coz,
              data = pid_df,
              weights = weight)

mod_pid4 <- lm(a ~ dz + agz + coz + agz*coz + agz2*coz + 
                dz*agz + dz*coz + dz*agz*coz + dz*agz2*coz,
              data = pid_df,
              weights = weight)

mod_pid5 <- lm(a ~ dz + agz + coz + agz*coz + 
                 dz*coz,
               data = pid_df,
               weights = weight)

BIC(mod_pid1, mod_pid2, mod_pid3, mod_pid4, mod_pid5)

#Constructing Figure 5


new_data <- expand_grid(agz = seq(-1.77, 2.1, by = .1),
                        coz = seq(-3.5, 2.4, by = .1),
                        dz = c(-2, 0)) # This will be ignored

new_data$fitted <- predict(mod_pid5, new_data)



t_change <- new_data %>%
  filter(dz == -2) %>%
  mutate(co = 10 * (coz * sd(long_difference$co) + mean(long_difference$co)) 
         + 1950,
         ag = 10 * (agz * sd(long_difference$ag) + mean(long_difference$ag)) 
         + 44) %>%
  mutate(period = co + ag) %>%
  filter(period > 1940, period < 2030) %>%
  ggplot(aes(x = ag, y = co, fill = fitted)) + 
  geom_tile(color = "black") + 
  scale_fill_viridis_c() + 
  theme_bw() + 
  labs(x = "Age", y = "Cohort",
       fill = "Transitory\nChange")

d_change <- new_data %>%
  spread(dz, fitted) %>%
  mutate(change = `0` - `-2`) %>%
  mutate(co = 10 * (coz * sd(long_difference$co) + mean(long_difference$co)) 
         + 1950,
         ag = 10 * (agz * sd(long_difference$ag) + mean(long_difference$ag)) 
         + 44) %>%
  mutate(period = co + ag) %>%
  filter(period > 1940, period < 2030) %>%
  ggplot(aes(x = ag, y = co, fill = change)) + 
  geom_tile(color = "black") + 
  scale_fill_viridis_c(option = "A") + 
  theme_bw() + 
  labs(x = "Age", y = "Cohort",
       fill = "Durable\nChange")

library(gridExtra)

p = grid.arrange(ncol = 2, nrow = 1, t_change, d_change)





######## Appendix A Tests ######

# Only Interest

interest_df <- long_difference %>% filter(name == "attentioncpg") %>%
  mutate(agz2 = agz^2)

mod_interest1 <- lm(a ~ dz + agz + coz + agz*coz,
               data = interest_df,
               weights = weight)

mod_interest2 <- lm(a ~ dz + agz + agz2+ coz + agz*coz + agz2*coz,
               data = interest_df,
               weights = weight)

mod_interest3 <- lm(a ~ dz + agz + coz + agz*coz + 
                 dz*agz + dz*coz + dz*agz*coz,
               data = interest_df,
               weights = weight)

mod_interest4 <- lm(a ~ dz + agz + coz + agz*coz + agz2*coz + 
                 dz*agz + dz*coz + dz*agz*coz + dz*agz2*coz,
               data = interest_df,
               weights = weight)

mod_interest5 <- lm(a ~ dz + agz + coz + agz*coz + 
                 dz*coz,
               data = interest_df,
               weights = weight)

BIC(mod_interest1, mod_interest2, mod_interest3, mod_interest4, mod_interest5)

#Only Fairness
fair_df <- long_difference %>% filter(name == "fair") %>%
  mutate(agz2 = agz^2)

mod_fair1 <- lm(a ~ dz + agz + coz + agz*coz,
                    data = fair_df,
                    weights = weight)

mod_fair2 <- lm(a ~ dz + agz + agz2+ coz + agz*coz + agz2*coz,
                    data = fair_df,
                    weights = weight)

mod_fair3 <- lm(a ~ dz + agz + coz + agz*coz + 
                      dz*agz + dz*coz + dz*agz*coz,
                    data = fair_df,
                    weights = weight)

mod_fair4 <- lm(a ~ dz + agz + coz + agz*coz + agz2*coz + 
                      dz*agz + dz*coz + dz*agz*coz + dz*agz2*coz,
                    data = fair_df,
                    weights = weight)

mod_fair5 <- lm(a ~ dz + agz + coz + agz*coz + 
                      dz*coz,
                    data = fair_df,
                    weights = weight)

BIC(mod_fair1, mod_fair2, mod_fair3, mod_fair4, mod_fair5)


#Only immmigrations
letin1a_df <- long_difference %>% filter(name == "letin1a") %>%
  mutate(agz2 = agz^2)

mod_letin1a1 <- lm(a ~ dz + agz + coz + agz*coz,
                data = letin1a_df,
                weights = weight)

mod_letin1a2 <- lm(a ~ dz + agz + agz2+ coz + agz*coz + agz2*coz,
                data = letin1a_df,
                weights = weight)

mod_letin1a3 <- lm(a ~ dz + agz + coz + agz*coz + 
                  dz*agz + dz*coz + dz*agz*coz,
                data = letin1a_df,
                weights = weight)

mod_letin1a4 <- lm(a ~ dz + agz + coz + agz*coz + agz2*coz + 
                  dz*agz + dz*coz + dz*agz*coz + dz*agz2*coz,
                data = letin1a_df,
                weights = weight)

mod_letin1a5 <- lm(a ~ dz + agz + coz + agz*coz + 
                  dz*coz,
                data = letin1a_df,
                weights = weight)

BIC(mod_letin1a1, mod_letin1a2, mod_letin1a3, mod_letin1a4, mod_letin1a5)

#Only ideology
polviews_df <- long_difference %>% filter(name == "polviews") %>%
  mutate(agz2 = agz^2)

mod_polviews1 <- lm(a ~ dz + agz + coz + agz*coz,
                   data = polviews_df,
                   weights = weight)

mod_polviews2 <- lm(a ~ dz + agz + agz2+ coz + agz*coz + agz2*coz,
                   data = polviews_df,
                   weights = weight)

mod_polviews3 <- lm(a ~ dz + agz + coz + agz*coz + 
                     dz*agz + dz*coz + dz*agz*coz,
                   data = polviews_df,
                   weights = weight)

mod_polviews4 <- lm(a ~ dz + agz + coz + agz*coz + agz2*coz + 
                     dz*agz + dz*coz + dz*agz*coz + dz*agz2*coz,
                   data = polviews_df,
                   weights = weight)

mod_polviews5 <- lm(a ~ dz + agz + coz + agz*coz + 
                     dz*coz,
                   data = polviews_df,
                   weights = weight)

BIC(mod_polviews1, mod_polviews2, mod_polviews3, mod_polviews4, mod_polviews5)


#Only trust
trust_df <- long_difference %>% filter(name == "trust") %>%
  mutate(agz2 = agz^2)

mod_trust1 <- lm(a ~ dz + agz + coz + agz*coz,
                    data = trust_df,
                    weights = weight)

mod_trust2 <- lm(a ~ dz + agz + agz2+ coz + agz*coz + agz2*coz,
                    data = trust_df,
                    weights = weight)

mod_trust3 <- lm(a ~ dz + agz + coz + agz*coz + 
                      dz*agz + dz*coz + dz*agz*coz,
                    data = trust_df,
                    weights = weight)

mod_trust4 <- lm(a ~ dz + agz + coz + agz*coz + agz2*coz + 
                      dz*agz + dz*coz + dz*agz*coz + dz*agz2*coz,
                    data = trust_df,
                    weights = weight)

mod_trust5 <- lm(a ~ dz + agz + coz + agz*coz + 
                      dz*coz,
                    data = trust_df,
                    weights = weight)

BIC(mod_trust1, mod_trust2, mod_trust3, mod_trust4, mod_trust5)



