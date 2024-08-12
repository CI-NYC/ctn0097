# cleaning script to find maximum COWS per patient-visit, then find if that COWS was eligible for more medication and create an indicator

library(tidyverse)
library(lubridate)

dat <- readRDS(here::here("data/analysis_data/ctn97_analysis_data_080624.rds")) |>
  filter(DMAMDDT >= consent_DMAMDDT) |> # filtering out to only after patient consented into study
  arrange(PATID, DMAMDDT) |>
  group_by(PATID) |>
  mutate(day_post_consent = row_number()) |> # creating new "day" variable
  ungroup() |>
  mutate(cows_score_1 = ifelse(is.na(cows_score_1) == FALSE & is.na(cows_time_1), as.numeric(NA), cows_score_1), # cases where cows score is present but time is missing -- these considered missing
         cows_score_2 = ifelse(is.na(cows_score_2) == FALSE & is.na(cows_time_2), as.numeric(NA), cows_score_2),
         cows_score_3 = ifelse(is.na(cows_score_3) == FALSE & is.na(cows_time_3), as.numeric(NA), cows_score_3),
         cows_score_4 = ifelse(is.na(cows_score_4) == FALSE & is.na(cows_time_4), as.numeric(NA), cows_score_4),
         cows_score_5 = ifelse(is.na(cows_score_5) == FALSE & is.na(cows_time_5), as.numeric(NA), cows_score_5),
         cows_score_6 = ifelse(is.na(cows_score_6) == FALSE & is.na(cows_time_6), as.numeric(NA), cows_score_6),
         cows_score_7 = ifelse(is.na(cows_score_7) == FALSE & is.na(cows_time_7), as.numeric(NA), cows_score_7),
         cows_score_8 = ifelse(is.na(cows_score_8) == FALSE & is.na(cows_time_8), as.numeric(NA), cows_score_8),
         cows_score_9 = ifelse(is.na(cows_score_9) == FALSE & is.na(cows_time_9), as.numeric(NA), cows_score_9),
         cows_score_10 = ifelse(is.na(cows_score_10) == FALSE & is.na(cows_time_10), as.numeric(NA), cows_score_10),
         cows_score_11 = ifelse(is.na(cows_score_11) == FALSE & is.na(cows_time_11), as.numeric(NA), cows_score_11),
         cows_score_12 = ifelse(is.na(cows_score_12) == FALSE & is.na(cows_time_12), as.numeric(NA), cows_score_12)) 

# dat <- dat |>
#   rowwise() |>
#   mutate(only_cows_indicator = as.numeric(sum(!is.na(c_across(cows_score_1:cows_score_12))) == 1)) |>
#   ungroup()

start_date <- as.Date("1970-01-01") # arbitrary start date to combine days with time -- treated as day 1 for everyone

# finding eligible COWS scores

# cows long
cows_long <-  dat |>
  select(PATID, day_post_consent, starts_with("cows")) |>
  pivot_longer(
    cols = starts_with("cows_time"), 
    names_to = "time_var", 
    values_to = "time") |>
  pivot_longer(
    cols = starts_with("cows_score"),        
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
  filter(is.na(time) == FALSE) |>
  distinct() |>
  mutate(type = "cows")

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
  filter(is.na(time) == FALSE) |>
  distinct() |>
  mutate(type = "clonidine")

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
  filter(is.na(time) == FALSE) |>
  distinct() |>
  mutate(type = "clonazepam")

all_long <- cows_long |>
  merge(clonidine_long, all = TRUE) |>
  merge(clonazepam_long, all = TRUE)

# looking at sum of clonazepam dosing in the 24 hours preceding each cows score

inelig_cows_clonazepam <- all_long |>
  filter(type == "cows") |>
  rowwise() |>
  mutate(
    start_window = time - days(1),
    end_window = time
  ) |>
  ungroup() |>
  left_join(all_long |> filter(type %in% c("clonazepam")), by = "PATID") |>
  filter(time.y >= start_window & time.y < end_window) |>
  group_by(PATID, day_post_consent.x, time.x, time_var.x) |>
  summarize(
    total_dose = sum(dose.y, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  rename(day_post_consent = day_post_consent.x, cows_time = time.x, total_clonazepam_dose_preceding_24h = total_dose, cows_index = time_var.x) |>
  filter(total_clonazepam_dose_preceding_24h >= 4) |>
  mutate(value = 1)

# looking at sum of clonidine dosing in the 24 hours preceding each cows score

inelig_cows_clonidine <- all_long |>
  filter(type == "cows") |>
  rowwise() |>
  mutate(
    start_window = time - days(1),
    end_window = time
  ) |>
  ungroup() |>
  left_join(all_long |> filter(type %in% c("clonidine")), by = "PATID") |>
  filter(time.y >= start_window & time.y < end_window) |>
  group_by(PATID, day_post_consent.x, time.x, time_var.x) |>
  summarize(
    total_dose = sum(dose.y, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  rename(day_post_consent = day_post_consent.x, cows_time = time.x, total_clonazepam_dose_preceding_24h = total_dose, cows_index = time_var.x) |>
  filter(total_clonazepam_dose_preceding_24h >= 1.2) |>
  mutate(value = 1)

# combining cows times ineligible due to clonazepam and clonidine
all_ineligible <- inelig_cows_clonazepam |> select(PATID, day_post_consent, cows_index, value) |>
  merge(inelig_cows_clonidine |> select(PATID, day_post_consent, cows_index, value), all = TRUE) |>
  distinct() |>
  pivot_wider(names_from = cows_index, values_from = value, values_fill = 0) |>
  rename_with(~ str_replace(., "^cows_time_", "ineligible_cows_time_"), starts_with("cows_time_")) |>
  select(PATID, day_post_consent, starts_with("ineligible"))

# pivoting data longer to make calculation of max COWS easier

dat_longer <- dat |>
   select(PATID, day_post_consent, starts_with("cows")) |>
   pivot_longer(
     cols = starts_with("cows"),
     names_to = c(".value", "time_index"),
     names_pattern = "(.*)_(.*)"
   ) 

# calculating maximum cows for each patient-day

max_cows_values_long <- dat_longer |>
   group_by(PATID, day_post_consent) |>
   summarize(max_cows = max(cows_score, na.rm = TRUE)) |>
   mutate(max_cows = ifelse(max_cows == -Inf, as.numeric(NA), max_cows))

# only looking at COWS equal to the maximum value for each patient (in the case of ties)

all_max_cows_long <- dat_longer |>
  left_join(max_cows_values_long) |>
  filter(cows_score == max_cows) 

# creating an indicator for COWS eligibility, taking the first maximum eligible COWS (if available) or the first non-eligible COWS (if no eligible are available)

max_cows_long_with_eligibility <- all_max_cows_long |>
  left_join(all_ineligible) |>
  mutate(across(starts_with("ineligible"), ~ replace_na(., 0))) |>
  mutate(max_cows_ineligible = case_when(time_index == "1" & ineligible_cows_time_1 == 1 ~ 1,
                                               time_index == "2" & ineligible_cows_time_2 == 1 ~ 1,
                                               time_index == "3" & ineligible_cows_time_3 == 1 ~ 1,
                                               time_index == "4" & ineligible_cows_time_4 == 1 ~ 1,
                                               time_index == "5" & ineligible_cows_time_5 == 1 ~ 1,
                                               time_index == "6" & ineligible_cows_time_6 == 1 ~ 1,
                                               time_index == "7" & ineligible_cows_time_7 == 1 ~ 1,
                                               TRUE ~ 0)) |>
  relocate(max_cows_ineligible, .after = max_cows) |>
  group_by(PATID, day_post_consent) |>
  arrange(PATID, day_post_consent, max_cows_ineligible, time_index) |>
  filter(max_cows_ineligible == 0 | !any(max_cows_ineligible == 0)) |> #taking the first maximum eligible COWS (if available) or the first non-eligible COWS (if no eligible are available)
  slice(1) |>
  ungroup() |>
  select(PATID, day_post_consent, max_cows, cows_time, time_index, max_cows_ineligible) |>
  rename("max_cows_time" = "cows_time")


# finding COWS time following maximum score
dat <- dat |>
  left_join(max_cows_long_with_eligibility) |>
  mutate(next_cows_time = case_when(time_index == "1" ~ cows_time_2,
                                    time_index == "2" ~ cows_time_3,
                                    time_index == "3" ~ cows_time_4,
                                    time_index == "4" ~ cows_time_5,
                                    time_index == "5" ~ cows_time_6,
                                    time_index == "6" ~ cows_time_7,
                                    time_index == "7" ~ cows_time_8,
                                    TRUE ~ hms::as_hms(NA))) 

# getting dosing in long format (finding any dosing info occurring after max COWS but prior to next COWS)
dat_bup_long <- dat |>
  select(PATID, day_post_consent, starts_with("DMBUP"), -DMBUPDTL) |>
  pivot_longer(
    cols = starts_with("DMBUP"),  
    names_to = c(".value", "pair"),   
    names_pattern = "^(DMBUPD|DMBUPT)(\\d+)$" 
  ) |>
  filter(is.na(DMBUPD) == FALSE) |>
  select(-pair)

dat_clonidine_long <- dat |>
  select(PATID, day_post_consent, starts_with("DMCLD"), -DMCLDDTL) |>
  pivot_longer(
    cols = starts_with("DMCLD"),  
    names_to = c(".value", "pair"),   
    names_pattern = "^(DMCLDD|DMCLDT)(\\d+)$" 
  ) |>
  filter(is.na(DMCLDD) == FALSE) |>
  select(-pair)

dat_clonazepam_long <- dat |>
  select(PATID, day_post_consent, starts_with("DMCZP"), -DMCZPDTL) |>
  pivot_longer(
    cols = starts_with("DMCZP"),  
    names_to = c(".value", "pair"),   
    names_pattern = "^(DMCZPD|DMCZPT)(\\d+)$" 
  ) |>
  filter(is.na(DMCZPD) == FALSE) |>
  select(-pair)

cows_info <- dat |>
  select(PATID, day_post_consent, max_cows_time, next_cows_time) |>
  filter(is.na(max_cows_time) == FALSE)

cows_info_clonidine <- cows_info |>
  left_join(dat_clonidine_long) |>
  rowwise() |>
  filter(max_cows_time <= DMCLDT & 
           (!is.na(next_cows_time) & next_cows_time > DMCLDT) | is.na(next_cows_time) | is.na(DMCLDT)) |>
  group_by(PATID, day_post_consent, max_cows_time) |>
  summarize(post_cows_clonidine_dose = sum(DMCLDD, na.rm = TRUE))

cows_info_clonazepam <- cows_info |>
  left_join(dat_clonazepam_long) |>
  rowwise() |>
  filter(max_cows_time <= DMCZPT & 
           (!is.na(next_cows_time) & next_cows_time > DMCZPT) | is.na(next_cows_time) | is.na(DMCZPT)) |>
  group_by(PATID, day_post_consent, max_cows_time) |>
  summarize(post_cows_clonazepam_dose = sum(DMCZPD, na.rm = TRUE))
  
dat_with_dosing <- dat |>
  left_join(cows_info_clonidine) |>
  left_join(cows_info_clonazepam) |>
  mutate(post_cows_clonidine_dose = ifelse(is.na(post_cows_clonidine_dose), 0, post_cows_clonidine_dose),
         post_cows_clonazepam_dose = ifelse(is.na(post_cows_clonazepam_dose), 0, post_cows_clonazepam_dose))

saveRDS(dat_with_dosing, here::here("data/analysis_data/max_cows_data.rds"))

dat_with_dosing |>
  group_by(day_post_consent, max_cows >= 5) |>
  summarize(count = n(),
            both_clonidine_and_clonazepam = sum(ifelse(post_cows_clonidine_dose >= 0.1 & 
                                                         post_cows_clonazepam_dose >= 1, 1, 0), na.rm = TRUE),
            clonidine_only = sum(ifelse(post_cows_clonidine_dose >= 0.1 & 
                                          post_cows_clonazepam_dose < 1, 1, 0), na.rm = TRUE),
            clonazepam_only = sum(ifelse(post_cows_clonidine_dose < 0.1 & 
                                           post_cows_clonazepam_dose >= 1, 1, 0), na.rm = TRUE)
  ) |>
  view()


dat_with_dosing |>
  filter(max_cows_ineligible == 1) |>
  group_by(day_post_consent, max_cows >= 5) |>
  summarize(count = n(),
            both_clonidine_and_clonazepam = sum(ifelse(post_cows_clonidine_dose >= 0.1 & 
                                                         post_cows_clonazepam_dose >= 1, 1, 0), na.rm = TRUE),
            clonidine_only = sum(ifelse(post_cows_clonidine_dose >= 0.1 & 
                                          post_cows_clonazepam_dose < 1, 1, 0), na.rm = TRUE),
            clonazepam_only = sum(ifelse(post_cows_clonidine_dose < 0.1 & 
                                           post_cows_clonazepam_dose >= 1, 1, 0), na.rm = TRUE)
            ) |>
  view()

dat_with_dosing |>
  filter(max_cows_ineligible == 0) |>
  group_by(day_post_consent, max_cows >= 5) |>
  summarize(count = n(),
            both_clonidine_and_clonazepam = sum(ifelse(post_cows_clonidine_dose >= 0.1 & 
                                                         post_cows_clonazepam_dose >= 1, 1, 0), na.rm = TRUE),
            clonidine_only = sum(ifelse(post_cows_clonidine_dose >= 0.1 & 
                                          post_cows_clonazepam_dose < 1, 1, 0), na.rm = TRUE),
            clonazepam_only = sum(ifelse(post_cows_clonidine_dose < 0.1 & 
                                           post_cows_clonazepam_dose >= 1, 1, 0), na.rm = TRUE)
  ) |>
  view()

  










