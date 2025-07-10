library(tidyverse)

dat_with_dosing <- readRDS(here::here("data/analysis_data/max_cows_data.rds")) |>
select(PATID, day, day_post_consent)

COW <- read.csv("data/COW.csv", colClasses = c(PATID = "character")) |>
  filter(PROT == "0097") |>
  mutate(across(c(COPULSE, COSWEAT, CORESTLS, COPUPIL, COBONJNT, CONOSEYE, COGIUPST, COTREMOR,
                  COYAWN, COANXITY, COGOOSKN), ~ coalesce(., 0))) |> # missing values = 0
  rowwise() |>
  mutate(cows_score = case_when(is.na(COCOWSCR) & is.na(COWSCRRT) == FALSE ~ COWSCRRT, # if missing COWS but retrospective available, use that
                                # patient notes indicating that these patients did not receive/finish COWS assessments at the visits
                                PATID == "02201009700118" & VISNO == "IN02" ~ as.numeric(NA),
                                PATID == "02076009700045" & VISNO == "IN08" ~ as.numeric(NA),
                                PATID == "02076009700335" & VISNO == "B00" ~ as.numeric(NA),
                                TRUE ~ sum(c(COPULSE, COSWEAT, CORESTLS, COPUPIL, COBONJNT, CONOSEYE, COGIUPST, COTREMOR, # some categories missing information but still taking sum (these were imputed with 0)
                                             COYAWN, COANXITY, COGOOSKN)) # issues with COCOWSCR variable -- sometimes doesn't match sum -- we will take sum for now (only 7 instances)
  ))

dat_with_dosing |>
  left_join(COW, by = c("PATID" = "PATID", 
                        "day" = "COWASMDT")) |>
  filter(day_post_consent <= 5,
         (COASMTM == "" | (is.na(COASMTM) & is.na(cows_score) == FALSE))) # 5 instances of missing timestamps

### CLONIDINE/CLONAZEPAM
cows_sows_data <- readRDS(here::here("data/cows_sows_data/joined_df.rds")) |>
  filter(time_diff_hours <= 15/60) # only within 15 minutes

## GETTING CLONIDINE AND CLONAZEPAM DATA

dat <- readRDS(here::here("data/analysis_data/ctn97_analysis_data_final.rds")) |>
  filter(DMAMDDT >= consent_DMAMDDT) |>
  mutate(day_post_consent = case_when(consent_DMAMDDT == DMAMDDT ~ 1,
                                      consent_DMAMDDT < DMAMDDT ~ DMAMDDT - consent_DMAMDDT + 1,
                                      TRUE ~ NA)) # creating new "day" variable

start_date <- as.Date("1970-01-01") # arbitrary start date to combine days with time -- treated as day 1 for everyone

clonidine_long <- dat |>
  select(PATID, day_post_consent, starts_with("DMCLDT"), starts_with("DMCLDD"), -DMCLDDTL) |>
  pivot_longer(
    cols = starts_with("DMCLDT"), 
    names_to = "time_var", 
    values_to = "time") |>
  pivot_longer(
    cols = starts_with("DMCLDD"),        
    names_to = "dose_var",            
    values_to = "dose") |>
  mutate(
    time_index = str_extract(time_var, "\\d+"),
    dose_index = str_extract(dose_var, "\\d+")
  ) |>
  filter(time_index == dose_index) |>
  select(PATID, day_post_consent, time, dose, time_var) |>
  mutate(time = ifelse(is.na(time) == FALSE, paste0(start_date + days(day_post_consent - 1), " ", time), time),
         time = ymd_hms(time)) |>
  filter(is.na(time) == TRUE, dose != 0) |>
  filter(day_post_consent <= 5) |>
  distinct() |>
  mutate(type = "clonidine")

clonidine_long |> nrow()

clonazepam_long <- dat |>
  select(PATID, day_post_consent, starts_with("DMCZPT"), starts_with("DMCZPD"), -DMCZPDTL) |>
  pivot_longer(
    cols = starts_with("DMCZPT"), 
    names_to = "time_var", 
    values_to = "time") |>
  pivot_longer(
    cols = starts_with("DMCZPD"),        
    names_to = "dose_var",            
    values_to = "dose") |>
  mutate(
    time_index = str_extract(time_var, "\\d+"),
    dose_index = str_extract(dose_var, "\\d+")
  ) |>
  filter(time_index == dose_index) |>
  select(PATID, day_post_consent, time, dose, time_var) |>
  mutate(time = ifelse(is.na(time) == FALSE, paste0(start_date + days(day_post_consent - 1), " ", time), time),
         time = ymd_hms(time)) |>
  filter(is.na(time) == TRUE, dose != 0) |>
  filter(day_post_consent <= 5) |>
  distinct() |>
  mutate(type = "clonazepam")

clonazepam_long |> nrow()