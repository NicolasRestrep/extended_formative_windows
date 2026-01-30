
library(tidymodels)


basic_model <- function(x) {
  tidy(lm(mean_var ~ mean_dur, data = x), conf.int = TRUE)
}



test <- long_difference |> 
  filter(
         df != "2016-20 ANES",
         df != "2016-20 GSS") |>
  group_by(df, t1, t2) |> 
  summarise(mean_var = mean(a),
            mean_dur = mean(d),
            n = n()) |> filter(n > 100) |> 
  group_by(df) |> 
  nest() |> 
  mutate(t = map(data, basic_model)) |> 
  unnest(t)

test |> 
  ggplot(aes(x = estimate, y = df)) + 
  geom_linerange(aes(xmin = conf.low, xmax = conf.high)) + 
  geom_point() + 
  facet_wrap(~term, scales = "free")



pv <- long_difference |> 
  filter(name == "partyid") 
  

summary(lm(val_dev ~ ag_dev, data = pv))




m1 <- lm(value ~ df, data = pv)
pv$R <- resid(m1)
pv$R2 <- pv$R^2

lm(log(R2)~df, data = pv)


pv <- all_panels |> 
  filter(name == "partyid") |> 
  mutate(dfid = paste(df, id, sep = "-")) |> 
  group_by(dfid) |> 
  filter(n() > 2) |> 
  mutate(val_dev = value - mean(value),
         age_dev = age - mean(age)) |> 
  filter(abs(age_dev) < 8) |> 
  filter(age > 17, age < 89)  |> 
  mutate(birth_year = year(date) - age) |> 
  filter(birth_year > 1883) |> 
  mutate(age = age)

pv$cohort <- cut(
    pv$birth_year,
    breaks = seq(min(pv$birth_year, na.rm = TRUE),
                 max(pv$birth_year, na.rm = TRUE) + 3,
                 by = 3),
    right = FALSE
  )

pv$age <- cut(
  pv$age,
  breaks = seq(min(pv$age, na.rm = TRUE),
               max(pv$age, na.rm = TRUE) + 3,
               by = 3),
  right = FALSE
)


pv <- pv |> 
  mutate(cohort = as_factor(cohort),
         cohort = relevel(cohort, ref = "[1944,1947)")) |> 
  ungroup()
  
m1 <- lm(val_dev ~ age, data = pv)
tidy(m1, conf.int = TRUE) |> 
  filter(grepl("age", term)==TRUE) |> 
  mutate(term = as.numeric(gsub("age", "", term))) |> 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_linerange() + 
  geom_point() + 
  theme_classic()
pv$R2 <- resid(m1)^2



pv <- as.data.frame(pv)
# Turn age into a clean numeric, then re-factor it
pv$age_num  <- as.numeric(as.character(pv$age))
pv$age_fac  <- factor(pv$age_num)

# Drop any NAs just in case
pv <- subset(pv, !is.na(R2) & !is.na(age_fac))
# Force simple 0/1 treatment contrasts
contrasts(pv$age_fac) <- contr.treatment(nlevels(pv$age_fac))

m2 <- glm(
  R2 ~ age_fac,
  data    = pv,
  family  = Gamma(link = "log"),
  control = glm.control(maxit = 300)
)

m2 <- glm(R2 ~ cohort, data = pv,
          family = Gamma(link = "log"),
          control = glm.control(maxit = 600))



t2 <- tidy(m2, conf.int = TRUE) 

cohorts <- data.frame(cohort = unique(pv$cohort))

p2 <- predict(m2, newdata = cohorts, type = "link", se.fit = TRUE)

cohorts$fit <- p2$fit
cohorts$se <- p2$se.fit

cohorts |> 
  mutate(estimate = exp(fit),
         ci.low = exp(fit-1.96*se),
         ci.high = exp(fit+1.96*se)) |> 
  mutate(cohort = as.numeric(str_extract(cohort, "(?<=\\[)[0-9]+"))) |> 
  filter(cohort > 1900) |> 
  ggplot(aes(y = estimate, x = cohort, ymin = ci.low, ymax = ci.high)) + 
  geom_linerange() + 
  geom_point() + 
  labs(x = "Cohort", y = "Coef. Estimate")

t2 |> 
  filter(term != "(Intercept)", term != "age") |> 
  mutate(term = as.numeric(str_extract(term, "(?<=\\[)[0-9]+"))) |> 
  ggplot(aes(y = estimate, x = term, ymin = conf.low, ymax = conf.high)) + 
  geom_linerange() + 
  geom_point() + 
  labs(x = "Cohort", y = "Coef. Estimate")

