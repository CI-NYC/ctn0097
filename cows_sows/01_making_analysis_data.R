library(tidyverse)

cows_sows_data <- readRDS(here::here("data/cows_sows_data/joined_df.rds")) |>
  filter(time_diff_hours <= 15/60) # only within 15 minutes

## GETTING CLONIDINE AND CLONAZEPAM DATA

dat <- readRDS(here::here("data/analysis_data/ctn97_analysis_data_final.rds")) |>
  filter(DMAMDDT >= consent_DMAMDDT) 

start_date <- as.Date("1970-01-01") # arbitrary start date to combine days with time -- treated as day 1 for everyone

clonidine_long <- dat |>
  select(PATID, day, starts_with("DMCLDT"), starts_with("DMCLDD"), -DMCLDDTL) |>
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
  select(PATID, day, time, dose, time_var) |>
  mutate(time = ifelse(is.na(time) == FALSE, paste0(start_date + days(day - 1), " ", time), time),
         time = ymd_hms(time)) |>
  filter(is.na(time) == FALSE) |>
  distinct() |>
  mutate(type = "clonidine")

clonazepam_long <- dat |>
  select(PATID, day, starts_with("DMCZPT"), starts_with("DMCZPD"), -DMCZPDTL) |>
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
  select(PATID, day, time, dose, time_var) |>
  mutate(time = ifelse(is.na(time) == FALSE, paste0(start_date + days(day - 1), " ", time), time),
         time = ymd_hms(time)) |>
  filter(is.na(time) == FALSE) |>
  distinct() |>
  mutate(type = "clonazepam")

cows_sows_data_clonidine <- cows_sows_data |>
  left_join(clonidine_long, by = c("PATID" = "PATID")) |> 
  mutate(diff = case_when(cows_first == 1 ~ day_time_cows - time,
                          cows_first == 0 ~ day_time_sows - time
                          )) |>
  filter(abs(diff) <= 1440) |>
  distinct() |>
  group_by(PATID, days_from_admission, day_time_cows, cows_score, day_time_sows, sows_score, time_diff_hours, cows_first) |>
  summarize(sum_clonidine_24hrs = sum(dose)) |>
  distinct()

cows_sows_data_clonazepam <- cows_sows_data |>
  left_join(clonazepam_long, by = c("PATID" = "PATID")) |> 
  mutate(diff = case_when(cows_first == 1 ~ day_time_cows - time,
                          cows_first == 0 ~ day_time_sows - time
  )) |>
  filter(abs(diff) <= 1440) |>
  distinct() |>
  group_by(PATID, days_from_admission, day_time_cows, cows_score, day_time_sows, sows_score, time_diff_hours, cows_first) |>
  summarize(sum_clonazepam_24hrs = sum(dose)) |>
  distinct()

cows_sows_data <- cows_sows_data |>
  left_join(cows_sows_data_clonidine) |>
  left_join(cows_sows_data_clonazepam) |>
  mutate(sum_clonidine_24hrs = ifelse(is.na(sum_clonidine_24hrs), 0, sum_clonidine_24hrs),
         sum_clonazepam_24hrs = ifelse(is.na(sum_clonazepam_24hrs), 0, sum_clonazepam_24hrs)
         ) 

DMA <- read.csv("data/DMA.csv", colClasses = c(PATID = "character"), na.strings = "") |>
  select(PATID, DMAMDDT, DMBZODTL) |> 
  mutate(DMAMDDT = DMAMDDT - 1) |> # use previous day
  filter(is.na(PATID) == FALSE) |>
  distinct()

cows_sows_data <- cows_sows_data |>
  left_join(DMA, by = c("PATID" = "PATID",
                        "days_from_admission" = "DMAMDDT")) |>
  mutate(DMBZODTL = ifelse(is.na(DMBZODTL), 0, DMBZODTL)) |>
  rename("sum_benzo_previous_day" = "DMBZODTL")
  

# getting baseline covariates
baseline <- readRDS(here::here("data/analysis_data/analysis_data.rds"))

cows_sows_data <- cows_sows_data |>
 left_join(baseline, by = c("PATID" = "PATID"))

saveRDS(cows_sows_data, here::here("data/cows_sows_data/final_dat.rds"))


