library(tidyverse)

paps <- read.table("~/Dropbox/data/paps/paps.txt")



library(readr)
library(stringr)
library(dplyr)

codebook_path <- "~/Dropbox/data/paps/DS0001/09582-0001-Codebook copy.txt"  # adjust
cb <- read_lines(codebook_path)

rows <- list()
current <- NULL

for (ln in cb) {
  
  # Capture a VAR line (var id + short label)
  m_var <- str_match(ln, "^\\s*0\\s+VAR\\s+(\\d{4})\\s+(.+?)\\s+(?:MD=|NO MISSING DATA CODES)")
  if (!is.na(m_var[1,1])) {
    current <- list(
      var  = m_var[1,2],
      name = str_squish(m_var[1,3])
    )
    next
  }
  
  # Capture a REF line (loc + width) that follows a VAR
  m_ref <- str_match(ln, "^\\s*REF\\s+(\\d{4})\\s+LOC\\s+(\\d+)\\s+WIDTH\\s+(\\d+)")
  if (!is.na(m_ref[1,1]) && !is.null(current)) {
    
    # Safety: make sure REF #### matches last VAR ####
    if (m_ref[1,2] == current$var) {
      loc   <- as.integer(m_ref[1,3])
      width <- as.integer(m_ref[1,4])
      
      rows[[length(rows) + 1]] <- tibble(
        var   = current$var,
        name  = current$name,
        loc   = loc,
        width = width,
        start = loc,
        end   = loc + width - 1
      )
    }
    
    next
  }
}

meta <- bind_rows(rows) %>%
  mutate(name = make.names(name, unique = TRUE)) %>%
  distinct(var, loc, width, .keep_all = TRUE) %>%   # drops exact repeats
  arrange(start)

# Build a readr fixed-width spec
fwf_spec <- readr::fwf_positions(meta$start, meta$end, col_names = meta$name)

meta %>% select(var, name, start, end, width) %>% print(n = 20)

data_path <- "~/Dropbox/data/paps/paps.txt"

dat <- read_fwf(
  file = data_path,
  col_positions = fwf_spec,
  col_types = cols(.default = col_character())
)


paps_1974 <- dat |> 
  filter(COUNTRY == 5) |> 
  mutate(id = RESPONDENT.IDENTIFICTN,
         age_1 = Q.32B.R.S.AGE,
         age_2 = S1C.AGE.OF.RESPONDENT,
         birthyear_1 = Q.32A.YEAR.OF.BIRTH, 
         birthyear_2 = S1B.YEAR.OF.BIRTH, 
         date_1 = I.9.DATE.OF.INTERVIEW,
         date_2 = I10.DATE.OF.INTERVIEW,
         ftdems_1 = Q.16B.FLG.THM.MJR.LEFT,
         ftdems_2 = C5B.MAJOR.LEFT.PARTY,
         ftbiz_1 = Q.16D.FLG.THM.BIG.BUSN,
         ftbiz_2 = C5D.BIG.BUSINESS,
         ftcops_1 = Q.16E.FLG.THM.POLICE,
         ftcops_2 = C5E.POLICE,
         ftreps_1 = Q.16H.FLG.THM.MJR.RIGHT,
         ftreps_2 = C5H.MAJOR.RIGHT.PARTY,
         ftblacks_1 = Q.16I.FLG.THM.MINORITIES,
         ftblacks_2 = C5I.MINORITIES,
         ftfelib_1 = Q.16J.FLG.THM.WOMENS.LIB,
         ftfelib_2 = C5J.WOMENS.LIB.MOVMT,
         ftlabor_1 = Q.16A.FLG.THM.UNIONS,
         ftlabor_2 = C5A.LABOR.UNIONS,
         trustgov_1 = Q.31.TRUST.GVT.DO.RIGHT,
         trustgov_2 = E11.TRUST.GOVT.DO.RIGHT,
         runfew_1 = Q.30.FEW.BIG.INT.ALL.PPL,
         runfew_2 = E10.CTRY.RUN.BIG.INTERST,
         ppllikeme_1 = Q.29A.PPL.LK.ME.NO.SAY,
         ppllikeme_2 = E9A.PEOPLE.LKE.ME.NO.SAY,
         complicated_1 = Q.29C.POLT.TOO.COMPLICTD,
         complicated_2 = E9C.POL.GOVT.COMPLICATED,
         dontcare_1 = Q.29D.PBL.OFCLS.NOT.CARE,
         dontcare_2 = E9D.PUB.OFFCLS.DONT.CARE,
         partyid_1 = Q.22A.PARTY.ID.STRENGTH,
         partyid_2 = E1.STRENGTH.PARTISAN.ID,
         ) |> 
  select(id:partyid_2) |> 
  mutate(across(date_1:date_2, ~as.Date(.x, format = "%m%d%y"))) |> 
  mutate(across(c(age_1:birthyear_2, ftdems_1:partyid_2), ~as.numeric(.x))) |> 
  mutate(across(trustgov_1:partyid_2, ~if_else(.x > 6, NA, .x)),
         across(ftdems_1:ftlabor_2, ~if_else(.x > 100, NA, .x))) |> 
  mutate(across(c(trustgov_1,trustgov_2,ppllikeme_1,ppllikeme_2,
                  complicated_1, complicated_2,dontcare_1,dontcare_2),
                ~((.x - 1)/3*100)),
         across(c(runfew_1, runfew_2),
                ~((.x - 1)*100)),
         across(c(partyid_1, partyid_2),
                ~((.x)/6*100))) |> 
  pivot_longer(
    cols = matches("_(1|2)$"),
    names_to = c(".value", "wave"),
    names_pattern = "^(.*)_(1|2)$"
  ) %>%
  mutate(wave = as.integer(wave)) |> 
  pivot_longer(ftdems:partyid) |> 
  mutate(df = "1974-81 PAPS",
         id = as.numeric(id),
         wave = as.character(wave)) |> 
  arrange(id, name, wave)


