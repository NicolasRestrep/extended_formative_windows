
# cleaning/clean_anes_20_22.R

source("code/cleaning/utils.R")

clean_anes_20_22 <- function(path) {
  
  anes20 <- read_and_zap(path)
  
  cleaned <- anes20 %>%
    mutate(id = row_number()) %>%
    select(id, profile_age,
           weight_pre, #weight for wave 1 only
           weight_post, #weight for pre- and post-election combined
           w3weight, #weight for data including wave 3
           start, w2start, w3startdt,
           pid7x, w2pid7x, w3pid7x,
           lcself, w2lcself, w3lcself,
           hp_you, w2hp_you,
           ftdem, w2ftdem, w3ftdem,
           ftrep, w2ftrep, w3ftrep,
           w2ftgay, w3ftgay, 
           w2ftpolice, w3ftpolice,
           w2ftfeminists, 
           w3ftfeminists,
           rr2, w2rr2,
           rr1,	w2rr1) %>%
    mutate(weight_1 = as.character(weight_pre),
           weight_2 = as.character(weight_post),
           weight_3 = as.character(w3weight)) %>%
    mutate(age_1 = profile_age,
           age_2 = profile_age, 
           age_3 = profile_age + 2,
           across(age_1:age_3, ~as.character(.x))) %>%
    mutate(date_1 = paste(substr(start, 1, 4), substr(start, 5, 6),substr(start, 7, 8),
                          sep = "-"),
           date_2 = paste(substr(w2start, 1, 4), substr(w2start, 5, 6),substr(w2start, 7, 8),
                          sep = "-"),
           date_3 = as.character(w3startdt),
           across(c(pid7x, w2pid7x, w3pid7x), 
                  ~ifelse(.x %in% c(-7, -6), NA, .x - 1)),
           across(c(lcself, w2lcself, w3lcself,
                    hp_you, w2hp_you),
                  ~ifelse(.x %in% c(-7, -6, -1, 77, 98, 99), NA, .x-1)),
           across(c(ftdem, w2ftdem, w3ftdem,
                    ftrep, w2ftrep, w3ftrep,
                    w2ftgay, w3ftgay, 
                    w2ftpolice, w3ftpolice, 
                    w2ftfeminists,	w3ftfeminists,
                    rr2, w2rr2, 
                    rr1,	w2rr1),
                  ~ifelse(.x > 100 | .x < 0, NA, .x)),
           partyid_1 = as.character(pid7x), 
           partyid_2 = as.character(w2pid7x),
           partyid_3 = as.character(w3pid7x),
           polviews_1 = as.character(lcself),
           polviews_2 = as.character(w2lcself),
           polviews_3 = as.character(w3lcself),
           govins_1 = as.character(hp_you), 
           govins_2 = as.character(w2hp_you),
           ftdems_1 = as.character(ftdem),
           ftdems_2 = as.character(w2ftdem),
           ftdems_3 = as.character(w3ftdem),
           ftreps_1 = as.character(ftrep),
           ftreps_2 = as.character(w2ftrep),
           ftreps_3 = as.character(w3ftrep),
           fthomo_2 = as.character(w2ftgay),
           fthomo_3 = as.character(w3ftgay),
           ftcops_2 = as.character(w2ftpolice),
           ftcops_3 = as.character(w3ftpolice),
           ftfeminists_2 = as.character(w2ftfeminists),
           ftfeminists_3 = as.character(w3ftfeminists),
           slavediff_1 = as.character(rr2),
           slavediff_2 = as.character(w2rr2),
           wrkwayup_1 = as.character(rr1),
           wrkwayup_2 = as.character(w2rr1)
    ) %>%
    select(id, weight_1:weight_3, age_1:age_3, date_1:wrkwayup_2) %>%
    pivot_longer(weight_1:wrkwayup_2) %>%
    separate(name, into = c("measure", "wave")) %>%
    spread(measure, value) %>%
    mutate(date = as.Date(date),
           across(c(weight, age, ftcops:slavediff, wrkwayup),
                  ~as.numeric(.x)),
           partyid = (partyid/6)*100,
           across(c(polviews, govins),
                  ~(.x)/6*100),
           across(c(slavediff, 
                    wrkwayup), 
                  ~(.x-1)/4*100)) %>%
    pivot_longer(c(ftcops:slavediff, wrkwayup)) %>%
    filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
    mutate(df = "2020-22 ANES")
  
  return(cleaned)
}