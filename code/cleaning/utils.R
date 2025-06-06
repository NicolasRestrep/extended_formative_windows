# cleaning/utils.R

library(tidyverse)
library(haven)

# Helper: Convert string like "19721105" to "1972-11-05"
convert_to_date <- function(date_str) {
  year <- substr(date_str, 1, 4)
  month <- substr(date_str, 5, 6)
  day <- substr(date_str, 7, 8)
  paste(year, month, day, sep = "-")
}

# Helper: Load and zap_labels
read_and_zap <- function(path) {
  read_dta(path) %>% zap_labels()
}

