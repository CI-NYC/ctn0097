# cleaning script to find maximum COWS per patient-visit, then find if that COWS was eligible for more medication and create an indicator

library(tidyverse)
library(lubridate)

dat <- readRDS(here::here("data/analysis_data/ctn97_analysis_data_012825.rds")) |>
  filter(DMAMDDT >= consent_DMAMDDT) |> # filtering out to only after patient consented into study
  arrange(PATID, DMAMDDT) |>
  group_by(PATID) |>
  mutate(day_post_consent = row_number()) |> # creating new "day" variable
  ungroup() |>
  mutate(day_diff = day - day_post_consent,
         end_induction_day = end_induction_day - day_diff,
         naltrexone_injection_day = naltrexone_injection_day - day_diff) |>
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
  mutate(clonazepam_inelig = 1)

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
  rename(day_post_consent = day_post_consent.x, cows_time = time.x, total_clonidine_dose_preceding_24h = total_dose, cows_index = time_var.x) |>
  filter(total_clonidine_dose_preceding_24h >= 1.2) |>
  mutate(clonidine_inelig = 1)

# combining cows times ineligible due to clonazepam and clonidine
all_ineligible <- inelig_cows_clonazepam |> select(PATID, day_post_consent, cows_index, clonazepam_inelig) |>
  merge(inelig_cows_clonidine |> select(PATID, day_post_consent, cows_index, clonidine_inelig), all = TRUE) |>
  distinct() |>
  mutate(across(c(clonazepam_inelig, clonidine_inelig), ~ ifelse(is.na(.), 0, .))) |>
  mutate(cows_index = sub("^cows_time_", "", cows_index)) |>
  rename("time_index" = "cows_index") |>
  mutate(either_inelig = ifelse(clonazepam_inelig == 1 | clonidine_inelig == 1, 1, 0),
         both_inelig = ifelse(clonazepam_inelig == 1 & clonidine_inelig == 1, 1, 0)) 

# pivoting data longer to make calculation of max COWS easier

dat_longer <- dat |>
  select(PATID, day_post_consent, naltrexone_injection_day, naltrexone_injection_time, starts_with("cows")) |>
  pivot_longer(
    cols = starts_with("cows"),
    names_to = c(".value", "time_index"),
    names_pattern = "(.*)_(.*)"
  ) 

# calculating maximum cows for each patient-day (prior to injection)

max_cows_values_long <- dat_longer |>
  mutate(after_injection = case_when(day_post_consent == naltrexone_injection_day & naltrexone_injection_time < cows_time ~ 1,
                                     TRUE ~ 0)) |>
  mutate(cows_score = ifelse(after_injection == 1, as.numeric(NA), cows_score)) |> # only looking at scores before injection
  group_by(PATID, day_post_consent) |>
  summarize(max_cows = max(cows_score, na.rm = TRUE)) |>
  mutate(max_cows = ifelse(max_cows == -Inf, as.numeric(NA), max_cows))

# only looking at COWS equal to the maximum value for each patient (in the case of ties)

all_max_cows_long <- dat_longer |>
  left_join(max_cows_values_long) |>
  filter(cows_score == max_cows) |>
  mutate(cows_after_injection = ifelse(day_post_consent == naltrexone_injection_day & naltrexone_injection_time < cows_time, 1, 0)) |> # adding indicator for cows occurring after initiation
  filter(cows_after_injection == 0) |>
  select(-cows_after_injection)

# creating an indicator for COWS eligibility, taking the first maximum eligible COWS (if available) or the first non-eligible COWS (if no eligible are available)

max_cows_long_with_eligibility <- all_max_cows_long |>
  left_join(all_ineligible) |>
  mutate(across(ends_with("_inelig"), ~ replace_na(., 0))) |>
  group_by(PATID, day_post_consent) |>
  arrange(PATID, day_post_consent, either_inelig, time_index) |>
  filter(either_inelig == 0 | !any(either_inelig == 0)) |> #taking the first maximum eligible COWS (if available) or the first non-eligible COWS (if no eligible are available)
  slice(1) |>
  ungroup() |>
  select(PATID, day_post_consent, max_cows, cows_time, time_index, either_inelig, both_inelig, clonazepam_inelig, clonidine_inelig) |>
  rename("max_cows_time" = "cows_time")


# finding 2 hour period after max COWS
dat <- dat |>
  left_join(max_cows_long_with_eligibility) |>
  mutate(post_max_2_hours = ifelse(is.na(max_cows_time) == FALSE, max_cows_time + hms::as_hms("01:00:00"), max_cows_time), # be careful, this could go beyond 23:59
         post_max_2_hours = hms::as_hms(post_max_2_hours))

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

# finding medication occurring 2 hours after max cows (on same day)
cows_info_all <- dat |>
  select(PATID, day_post_consent, max_cows_time, post_max_2_hours) |>
  filter(is.na(max_cows_time) == FALSE)

cows_info <- cows_info_all |>
  filter(post_max_2_hours < hms::as_hms("23:59:59"))

cows_info_clonidine_same_day <- cows_info |>
  left_join(dat_clonidine_long) |>
  rowwise() |>
  filter(max_cows_time <= DMCLDT & 
           (!is.na(post_max_2_hours) & post_max_2_hours >= DMCLDT) | is.na(post_max_2_hours) | is.na(DMCLDT)) |>
  group_by(PATID, day_post_consent, max_cows_time) |>
  summarize(post_cows_clonidine_dose = sum(DMCLDD, na.rm = TRUE))

cows_info_clonazepam_same_day <- cows_info |>
  left_join(dat_clonazepam_long) |>
  rowwise() |>
  filter(max_cows_time <= DMCZPT & 
           (!is.na(post_max_2_hours) & post_max_2_hours >= DMCZPT) | is.na(post_max_2_hours) | is.na(DMCZPT)) |>
  group_by(PATID, day_post_consent, max_cows_time) |>
  summarize(post_cows_clonazepam_dose = sum(DMCZPD, na.rm = TRUE))

# finding medication occurring 2 hours after max cows (where ends on next day)

cows_info_day_after <- cows_info_all |>
  filter(post_max_2_hours >= hms::as_hms("23:59:59"))

# splitting into day x and day x + 1 portion

# SAME DAY PORTION
cows_info_day_after_x <- cows_info_day_after |>
  mutate(post_max_2_hours = hms::as_hms("23:59:59"))

cows_info_clonidine_x <- cows_info_day_after_x |>
  left_join(dat_clonidine_long) |>
  rowwise() |>
  filter(max_cows_time <= DMCLDT & 
           (!is.na(post_max_2_hours) & post_max_2_hours >= DMCLDT) | is.na(DMCLDT)) |>
  group_by(PATID, day_post_consent, max_cows_time) |>
  summarize(post_cows_clonidine_dose = sum(DMCLDD, na.rm = TRUE))

cows_info_clonazepam_x <- cows_info_day_after_x |>
  left_join(dat_clonazepam_long) |>
  rowwise() |>
  filter(max_cows_time <= DMCZPT & 
           (!is.na(post_max_2_hours) & post_max_2_hours >= DMCZPT) | is.na(DMCZPT)) |>
  group_by(PATID, day_post_consent, max_cows_time) |>
  summarize(post_cows_clonazepam_dose = sum(DMCZPD, na.rm = TRUE))

# PART FALLING INTO NEXT DAY
cows_info_day_after_x1 <- cows_info_day_after |>
  mutate(day_post_consent = day_post_consent + 1, 
    post_max_2_hours = post_max_2_hours - hms::as_hms("24:00:00"),
    post_max_2_hours = hms::as_hms(post_max_2_hours)
    )

cows_info_clonidine_x_1 <- cows_info_day_after_x1 |>
  left_join(dat_clonidine_long) |>
  rowwise() |>
  filter(post_max_2_hours >= DMCLDT | is.na(DMCLDT)) |>
  group_by(PATID, day_post_consent, max_cows_time) |>
  summarize(post_cows_clonidine_dose_x_1 = sum(DMCLDD, na.rm = TRUE)) |>
  mutate(day_post_consent = day_post_consent - 1) # return to normal

cows_info_clonazepam_x_1 <- cows_info_day_after_x1 |>
  left_join(dat_clonazepam_long) |>
  rowwise() |>
  filter(post_max_2_hours >= DMCZPT | is.na(DMCZPT)) |>
  group_by(PATID, day_post_consent, max_cows_time) |>
  summarize(post_cows_clonazepam_dose_x_1 = sum(DMCZPD, na.rm = TRUE)) |>
    mutate(day_post_consent = day_post_consent - 1) # return to normal
  
# combining day x to day x + 1
cows_info_clonidine_x <- cows_info_clonidine_x |>
  left_join(cows_info_clonidine_x_1) |>
  mutate(post_cows_clonidine_dose = sum(post_cows_clonidine_dose, post_cows_clonidine_dose_x_1, na.rm = TRUE)) |>
  select(-post_cows_clonidine_dose_x_1)
  
cows_info_clonazepam_x <- cows_info_clonazepam_x |>
  left_join(cows_info_clonazepam_x_1) |>
  mutate(post_cows_clonazepam_dose = sum(post_cows_clonazepam_dose, post_cows_clonazepam_dose_x_1, na.rm = TRUE)) |>
  select(-post_cows_clonazepam_dose_x_1)

cows_info_clonidine <- cows_info_clonidine_same_day |>
  merge(cows_info_clonidine_x, all = TRUE)

cows_info_clonazepam <- cows_info_clonazepam_same_day |>
  merge(cows_info_clonazepam_x, all = TRUE)

dat_with_dosing <- dat |>
  left_join(cows_info_clonidine) |>
  left_join(cows_info_clonazepam) |>
  mutate(post_cows_clonidine_dose = ifelse(is.na(post_cows_clonidine_dose), 0, post_cows_clonidine_dose),
         post_cows_clonazepam_dose = ifelse(is.na(post_cows_clonazepam_dose), 0, post_cows_clonazepam_dose))

pre_naltrexone_dosing <- dat_with_dosing |> 
  filter(naltrexone_injection_day == day_post_consent) |>
  select(PATID, day_post_consent, naltrexone_injection_day, naltrexone_injection_time, max_cows_time, post_max_2_hours) |>
  #left_join(dat_bup_long) |> # no BUP post-injection exists
  left_join(dat_clonidine_long) |>
  left_join(dat_clonazepam_long)

# finding medication occurring pre-injection and post-cows (if it exists) 
pre_naltrexone_dosing_clonidine <- pre_naltrexone_dosing |>
  group_by(PATID, day_post_consent, max_cows_time, post_max_2_hours, naltrexone_injection_time) |>
  filter((DMCLDT <= naltrexone_injection_time) & 
           (DMCLDT >= ifelse(is.na(max_cows_time) == FALSE, max_cows_time, TRUE)) &
           (DMCLDT <= ifelse(is.na(post_max_2_hours) == FALSE, post_max_2_hours, TRUE))) |>
  summarize(pre_injection_clonidine = sum(DMCLDD, na.rm = TRUE))

pre_naltrexone_dosing_clonazepam <- pre_naltrexone_dosing |>
  group_by(PATID, day_post_consent, max_cows_time, post_max_2_hours, naltrexone_injection_time) |>
  filter((DMCZPT <= naltrexone_injection_time) & 
           (DMCZPT >= ifelse(is.na(max_cows_time) == FALSE, max_cows_time, TRUE))) |>
  summarize(pre_injection_clonazepam = sum(DMCZPD, na.rm = TRUE))

dat_with_dosing <- dat_with_dosing |>
  left_join(pre_naltrexone_dosing_clonidine) |>
  left_join(pre_naltrexone_dosing_clonazepam) |>
  mutate(pre_injection_clonidine = ifelse(day_post_consent == naltrexone_injection_day & is.na(pre_injection_clonidine), 0, pre_injection_clonidine), 
         pre_injection_clonazepam = ifelse(day_post_consent == naltrexone_injection_day & is.na(pre_injection_clonazepam), 0, pre_injection_clonazepam))

saveRDS(dat_with_dosing, here::here("data/analysis_data/max_cows_data_2_hours.rds"))