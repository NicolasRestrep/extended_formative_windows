
# cleaning/clean_gss_16_20.R

source("code/cleaning/utils.R")

clean_gss_16_20 <- function(path) {
  
  gss16 <- read_and_zap(path)
  
  cleaned <- gss16 %>%
    mutate(id = row_number()) %>%
    select(id, 
           wtssnr_2,
           age_1a, age_1b, age_2,
           dateintv_1a, dateintv_1b, dateintv_2,
           partyid_1a, partyid_1b, partyid_2,
           polviews_1a, polviews_1b, polviews_2,
           trust_1a, trust_1b, trust_2,
           fair_1a, fair_1b, fair_2,
           helpful_1a, helpful_1b, helpful_2,
           natenvir_1a, natenvir_1b, natenvir_2,
           nataid_1a, nataid_1b, nataid_2,
           natsoc_1a, natsoc_1b, natsoc_2,
           natchld_1a, natchld_1b, natchld_2,
           natfare_1a, natfare_1b, natfare_2,
           natcrime_1a, natcrime_1b, natcrime_2, 
           nateduc_1a, nateduc_1b, nateduc_2, 
           natarms_1a, natarms_1b, natarms_2, 
           natrace_1a, natrace_1b, natrace_2, 
           cappun_1a, cappun_1b, cappun_2, 
           letin1a_1a, letin1a_1b, letin1a_2,
           tax_1a, tax_1b, tax_2,
           wrkwayup_1a, wrkwayup_1b, wrkwayup_2) %>%
    mutate(weight_2 = as.character(wtssnr_2)) %>%
    mutate(age_1a = ifelse(is.na(age_1a) & !is.na(age_2), age_2 - 4, age_1a),
           age_1b = ifelse(is.na(age_1b) & !is.na(age_2), age_2 - 2, age_1b),
           age_2 = ifelse(is.na(age_2) & !is.na(age_1a), age_1a + 4, age_2),
           age_2 = ifelse(is.na(age_2) & !is.na(age_1b), age_1b + 2, age_2),
           across(age_1a:age_2, ~as.character(.x))) %>%
    mutate(md = sprintf("%04d", dateintv_1a),
           date_1a = paste(2016, substr(md, 1, 2),substr(md, 3, 4),
                           sep = "-"),
           date_1a = ifelse(date_1a == "2016-00-NA", NA, date_1a),
           md = sprintf("%04d", dateintv_1b),
           date_1b = paste(2018, substr(md, 1, 2),substr(md, 3, 4),
                           sep = "-"),
           date_1b = ifelse(date_1b == "2018-00-NA", NA, date_1b),
           md = sprintf("%04d", dateintv_2),
           date_2 = paste(2020, substr(md, 1, 2),substr(md, 3, 4),
                          sep = "-"),
           date_2 = ifelse(date_2 == "2020-00-NA", NA, date_2),
           across(c(partyid_1a, partyid_1b, partyid_2), 
                  ~ifelse(.x %in% c(7,8,9), NA, .x)),
           across(c(trust_1a, trust_1b, trust_2,
                    fair_1a, fair_1b, fair_2,
                    helpful_1a, helpful_1b, helpful_2), 
                  ~ifelse(.x == 3, 1.5, .x)),
           across(partyid_1a:wrkwayup_2,
                  ~as.character(.x))) %>%
    select(id, weight_2, age_1a:age_2, date_1a, date_1b, date_2, partyid_1a:wrkwayup_2) %>% 
    pivot_longer(weight_2:wrkwayup_2) %>%
    separate(name, into = c("measure", "wave")) %>%
    mutate(wave = recode(wave, "1a"="1", "1b"="2", "2"="3")) %>%
    spread(measure, value) %>%
    mutate(date = as.Date(date),
           across(c(weight, age, cappun, fair:wrkwayup),
                  ~as.numeric(.x)),
           partyid = (partyid/6)*100,
           polviews = (polviews-1)/6*100,
           across(c(trust, fair, helpful),
                  ~(.x-1)*100),
           across(c(natenvir, nataid,
                    natsoc, natchld,
                    natfare, natcrime, nateduc, 
                    natarms, natrace,
                    tax),
                  ~(.x-1)/2*100), 
           across(c(cappun), 
                  ~(.x)/2*100), 
           across(c(letin1a, wrkwayup), 
                  ~(.x - 1)/4*100)) %>%
    pivot_longer(c(cappun, fair:trust, wrkwayup)) %>%
    filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
    mutate(df = "2016-20 GSS")
  
  return(cleaned)
  
}