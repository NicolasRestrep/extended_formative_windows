## Code for Figures

library(tidyverse)
library(lme4)

# Figure for age-cohort grid
# here "m5" is the lmer model of change over time 
# as a function of duration (in years),
# age (in decades, centered on 44) and 
# cohort (also in decades, centered on 1950)

new.data2 <- expand_grid(ag = seq(-2.6, 3.6, by = .1),
                         co = seq(-7.4, 5.2, by = .1),
                         d = 2,
                         name = c("polviews", "partyid"))

new.data2$fitted <- predict(m5, new.data2,
                            re.form = ~(1 + d + ag + co + ag:co| name))
#re.form removes random effects from the prediction model

new.data2 %>%
  mutate(co = co * 10 + 1950,
         ag = ag * 10 + 44) %>%
  #construct a "period" term to remove cases we don't have data for
  mutate(period = co + ag) %>%
  filter(period > 1940, period < 2030) %>%
  ggplot(aes(x = ag, y = co, fill = fitted)) + 
  geom_tile(color = "black") +
  theme_classic(base_size = 20) + 
  scale_fill_viridis_c(option = "C") + 
  labs(x = "Age", y = "Cohort", 
       fill = "Predicted\nChange")


#Partyid/Polveiws Plot
new.data3 <- expand_grid(ag = seq(-2.6, 3.6, by = .1),
                         co = seq(-7.4, 5.2, by = .1),
                         d = 2,
                         name = c("polviews", "partyid"))

new.data3$fitted <- predict(m5, new.data2,
                            re.form = ~(1 + d + ag + co + ag:co| name))

new.data3 %>%
  mutate(co = co * 10 + 1950,
         ag = ag * 10 + 44) %>%
  #construct a "period" term to remove cases we don't have data for
  mutate(period = co + ag,
         name = recode(name, "partyid"="Party ID (Dem/Rep)",
                       "polviews"="Ideology (Lib/Con)")) %>%
  filter(period > 1940, period < 2030) %>%
  ggplot(aes(x = ag, y = co, fill = fitted)) + 
  geom_tile(color = "black") +
  theme_classic(base_size = 20) + 
  scale_fill_viridis_c(option = "C") + 
  facet_wrap(~name) +
  labs(x = "Age", y = "Cohort", 
       fill = "Predicted\nChange")


#Coefficient Plot
ranefs_sd <- tidy(m5) %>%
  filter(effect == "ran_pars", grepl("sd__", term),
         group != "Residual") %>%
  mutate(term = gsub("sd__", "", term)) %>%
  mutate(resd = estimate) %>%
  select(c(term, resd))

tidy(m5) %>%
  filter(effect == "fixed") %>%
  left_join(ranefs_sd) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term, "d"="Duration", "co"="Cohort\n(10 years)",
                       "ag"="Age\n(10 years)", "ag:co"="Interaction:\nAge * Cohort")) %>%
  ggplot(aes(x = estimate, xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error,
             y = term)) + 
  geom_vline(xintercept = 0, linetype = 2, color = "black") + 
  geom_linerange(aes(xmin = estimate - 1.96*resd, xmax = estimate + 1.96*resd),
                 color = "firebrick", alpha = .7, 
                 linewidth = 3) + 
  geom_linerange(linewidth = 2) + 
  geom_point(size = 2) + 
  theme_bw() +
  labs(x = "Coefficient Estimate", y = "",
       title = "Coefficient Estimates",
       subtitle = "Random effects distribution in Red")


#Random intercepts by question plot
#extract random effects from model object (takes a WHILE)
model_refs <- as.data.frame(ranef(m5)) 

intercepts = data.frame(term = c("1. Intercept (Transitive Change)", 
                                 "2. Duration (Durable Change)"),
                        val = c( 16.0760, 0.8450))
model_refs %>%
  filter(grpvar == "name", term %in% c("(Intercept)", "d")) %>%
  mutate(constant = ifelse(term == "d", 0.8450, 16.0760)) %>%
  mutate(term = recode(term, "(Intercept)"="1. Intercept (Transitive Change)",
                       "d"="2. Duration (Durable Change)")) %>%
  ggplot(aes(y = reorder(grp, condval), x = constant + condval,
             xmin = constant + condval - 1.96*condsd,
             xmax = constant + condval + 1.96*condsd)) + 
  geom_vline(data = intercepts, aes(xintercept = val),
             linetype = 2, color= "gray") + 
  geom_linerange() + 
  geom_point() + 
  theme_bw() +
  facet_wrap(~term, scales = "free_x") + 
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(y = "Question", x = "Expected Absolute Change")

