
# cleaning/clean_gss_06_10.R

source("code/cleaning/utils.R")

clean_gss_06_10 <- function(path) {
  
  gss06 <- read_and_zap(path)
  
  cleaned <- gss06 %>%
    mutate(id = row_number()) %>%
    select(id, 
           wtpannr12, wtpannr123,
           age_1, age_2, age_3,
           dateintv_1, dateintv_2, dateintv_3,
           partyid_1, partyid_2, partyid_3,
           polviews_1, polviews_2, polviews_3,
           trust_1, trust_2, trust_3,
           fair_1, fair_2, fair_3,
           helpful_1, helpful_2, helpful_3,
           natenvir_1, natenvir_2, natenvir_3,
           nataid_1, nataid_2, nataid_3,
           natsoc_1, natsoc_2, natsoc_3,
           natchld_1, natchld_2, natchld_3,
           natfare_1, natfare_2, natfare_3,
           natcrime_1, natcrime_2, natcrime_3,
           nateduc_1, nateduc_2, nateduc_3, 
           natarms_1, natarms_2, natarms_3, 
           natrace_1, natrace_2, natrace_3, 
           cappun_1, cappun_2, cappun_3, 
           letin1a_1, letin1a_2, letin1a_3,
           tax_1, tax_2, tax_3,
           wrkwayup_1, wrkwayup_2, wrkwayup_3) %>%
    mutate(weight_2 = as.character(wtpannr12),
           weight_3 = as.character(wtpannr123),
           age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2 - 2, age_1),
           age_2 = ifelse(is.na(age_2), age_1 + 2, age_2),
           age_3 = ifelse(is.na(age_3), age_1 + 4, age_3),
           across(age_1:age_3, ~as.character(.x))) %>%
    mutate(date_1 = paste(2006,substr(dateintv_1, 1, 1),substr(dateintv_1, 2, 3),
                          sep = "-"),
           date_2 = paste(2008,substr(dateintv_2, 1, 1),substr(dateintv_2, 2, 3),
                          sep = "-"),
           date_2 = ifelse(date_2 == "2008-NA-NA", NA, date_2),
           date_3 = paste(2010,substr(dateintv_3, 1, 1),substr(dateintv_3, 2, 3),
                          sep = "-"),
           date_3 = ifelse(date_3 == "2010-NA-NA", NA, date_3),
           across(c(partyid_1, partyid_2, partyid_3), ~ifelse(.x %in% c(7), NA, .x)),
           across(c(tax_1, tax_2, tax_3), ~ifelse(.x %in% c(4), NA, .x)),
           across(c(trust_1, trust_2, trust_3,
                    fair_1, fair_2, fair_3,
                    helpful_1, helpful_2, helpful_3), 
                  ~ifelse(.x == 3, 1.5, .x)),
           across(partyid_1:wrkwayup_3,
                  ~as.character(.x))) %>%
    select(-c(wtpannr12, wtpannr123, dateintv_1, dateintv_2, dateintv_3)) %>%
    pivot_longer(c(weight_2, weight_3, age_1:date_3)) %>%
    separate(name, into = c("measure", "wave")) %>%
    spread(measure, value) %>%
    mutate(date = as.Date(date),
           across(c(weight, age, cappun, fair:wrkwayup),
                  ~as.numeric(.x)),
           partyid = (partyid/6)*100,
           polviews = (polviews-1)/6*100,
           across(c(trust, fair,helpful),
                  ~(.x-1)*100),
           across(c(natenvir, nataid, natsoc, natchld,
                    natfare, natcrime, nateduc, natarms, 
                    natrace, tax),
                  ~(.x-1)/2*100), 
           across(c(cappun), 
                  ~(.x)/2*100),
           across(c(letin1a, wrkwayup), 
                  ~(.x - 1)/4*100)) %>%
    pivot_longer(c(cappun, fair:trust, wrkwayup)) %>%
    filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
    mutate(df = "2006-10 GSS")
  
  return(cleaned)
  
}