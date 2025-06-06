
# cleaning/clean_anes_80.R

source("code/cleaning/utils.R")

clean_anes_1980 <- function(path) {
  
  anes8 <- read_and_zap(path)
  
  cleaned <- anes8 %>%
    mutate(id = row_number()) %>%
    select(
      id,
      VMP0325,
      VMP2404,
      VMP3386,
      VMP0578,
      VMP2425,
      VMP3571,
      VMP4105,
      VMP0191,
      VMP2212,
      VMP3212,
      VMP4022,
      VMP0121,
      VMP2125,
      VMP3213,
      VMP2192,
      VMP3284,
      VMP0144,
      VMP2159,
      VMP3254,
      VMP0133,
      VMP2137,
      VMP3234,
      VMP0052,
      VMP2052,
      VMP3109,
      VMP0053,
      VMP2053,
      VMP3110,
      # attention political campaigns
      VMP0005,
      VMP2001,
      VMP3001,
      VMP4001
    ) %>%
    mutate(
      across(c(VMP0325, VMP2404, VMP3386), ~ ifelse(.x == 0, NA, .x)),
      age_1 = VMP0325,
      age_2 = VMP2404,
      age_3 = VMP3386,
      age_1 = ifelse(is.na(age_1) & !is.na(age_2), age_2, age_1),
      age_1 = ifelse(is.na(age_1) & !is.na(age_3), age_3, age_1),
      age_2 = ifelse(is.na(age_2), age_1, age_2),
      age_3 = ifelse(is.na(age_3), age_1, age_3),
      age_4 = age_3,
      across(c(age_1:age_4), ~ as.character(.x))
    ) %>%
    mutate(
      date_str = sprintf("%06d", as.integer(VMP0578)),
      date_1 = paste(
        paste0("19", substr(date_str, 5, 6)),
        substr(date_str, 1, 2),
        substr(date_str, 3, 4),
        sep = "-"
      )
    ) %>%
    select(-c(date_str)) %>%
    mutate(
      date_str = sprintf("%06d", as.integer(VMP2425)),
      date_2 = paste(
        paste0("19", substr(date_str, 5, 6)),
        substr(date_str, 1, 2),
        substr(date_str, 3, 4),
        sep = "-"
      ),
      date_2 = ifelse(date_2 == "1999-99-99", NA, date_2)
    ) %>%
    select(-c(date_str)) %>%
    mutate(
      date_str = sprintf("%06d", as.integer(VMP3571)),
      date_3 = paste(
        paste0("19", substr(date_str, 5, 6)),
        substr(date_str, 1, 2),
        substr(date_str, 3, 4),
        sep = "-"
      ),
      date_3 = ifelse(date_3 %in% c("1900-00-00", "1999-99-99"), NA, date_3)
    ) %>%
    select(-c(date_str)) %>%
    mutate(
      date_str = sprintf("%06d", as.integer(VMP4105)),
      date_4 = paste(
        paste0("19", substr(date_str, 5, 6)),
        substr(date_str, 1, 2),
        substr(date_str, 3, 4),
        sep = "-"
      ),
      date_4 = ifelse(date_4 %in% c("1900-00-00", "1999-99-99"), NA, date_4)
    ) %>%
    select(-c(date_str)) %>%
    mutate(
      across(c(VMP0191, VMP2212, VMP3212, VMP4022), ~ ifelse(.x %in% c(7, 8, 9), NA, .x)),
      across(
        c(
          VMP0121,
          VMP2125,
          VMP3213,
          VMP0144,
          VMP2159,
          VMP3254,
          VMP0133,
          VMP2137,
          VMP3234, 
          VMP0005,
          VMP2001,
          VMP3001,
          VMP4001
        ),
        ~ ifelse(.x %in% c(0, 8, 9), NA, .x)
      ),
      across(c(VMP2192, VMP3284), ~ ifelse(.x %in% c(7, 8, 9), NA, .x)),
      across(
        c(VMP0052, VMP2052, VMP3109, #republicans
          VMP0053, VMP2053, VMP3110),
        ~ ifelse(.x %in% c(998, 999), NA, .x)
      )
    ) %>%
    mutate(
      partyid_1 = as.character(VMP0191),
      partyid_2 = as.character(VMP2212),
      partyid_3 = as.character(VMP3212),
      partyid_4 = as.character(VMP4022),
      polviews_1 = as.character(VMP0121),
      polviews_2 = as.character(VMP2125),
      polviews_3 = as.character(VMP3213),
      abortion_2 = as.character(VMP2192),
      abortion_3 = as.character(VMP3284),
      spendserv_1 = as.character(VMP0144),
      spendserv_2 = as.character(VMP2159),
      spendserv_3 = as.character(VMP3254),
      defscale_1 = as.character(VMP0133),
      defscale_2 = as.character(VMP2137),
      defscale_3 = as.character(VMP3234),
      ftdems_1 = as.character(VMP0052),
      ftdems_2 = as.character(VMP2052),
      ftdems_3 = as.character(VMP3109),
      ftreps_1 = as.character(VMP0053),
      ftreps_2 = as.character(VMP2053),
      ftreps_3 = as.character(VMP3110), 
      attentioncpg_1 = as.character(VMP0005),
      attentioncpg_2 = as.character(VMP2001),
      attentioncpg_3 = as.character(VMP3001),
      attentioncpg_4 = as.character(VMP4001)
    ) %>%
    select(c(id, age_1:age_4, date_1:attentioncpg_4)) %>%
    pivot_longer(age_1:attentioncpg_4) %>%
    separate(name, into = c("measure", "wave")) %>%
    spread(measure, value) %>%
    mutate(date = as.Date(date),
           across(c(age, abortion, defscale:spendserv),
                  ~as.numeric(.x)),
           partyid = (partyid/6)*100,
           across(c(polviews, spendserv,
                    defscale),
                  ~(.x-1)/6*100),
           abortion = (abortion-1)/3*100, 
           attentioncpg = as.numeric(attentioncpg), 
           attentioncpg = (attentioncpg-1)/4*100) %>%
    pivot_longer(c(abortion,attentioncpg, defscale:spendserv)) %>%
    filter(!is.na(age), !is.na(value), !is.na(date)) %>% arrange(id, name, date) %>%
    mutate(df = "1980 ANES") %>%
    mutate(weight = 1)
  
  return(cleaned)
  
}
