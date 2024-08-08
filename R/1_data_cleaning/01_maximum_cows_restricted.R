library(tidyverse)
library(lubridate)

max_with_na <- function(...) {
  scores <- list(...)
  all_na <- all(sapply(scores, is.na))
  
  if (all_na) {
    return(NA)
  } else {
    return(do.call(pmax, c(scores, na.rm = TRUE)))
  }
}

dat <- readRDS(here::here("data/analysis_data/ctn97_analysis_data_080624.rds")) |>
  filter(DMAMDDT >= consent_DMAMDDT) |>
  arrange(PATID, DMAMDDT) |>
  group_by(PATID) |>
  mutate(day_post_consent = row_number()) |>
  ungroup() |>
  mutate(cows_score_1 = ifelse(is.na(cows_score_1) == FALSE & is.na(cows_time_1), as.numeric(NA), cows_score_1),
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

start_date <- as.Date("1970-01-01") # arbitrary start date -- doesn't matter

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
  merge(clonazepam_long, all = TRUE) |>
  filter(day_post_consent <= 5) # only looking at first 5 days

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
  rename(day_post_consent = day_post_consent.x, cows_time = time.x, total_clonazepam_dose_preceding_24h = total_dose, cows_index= time_var.x) |>
  filter(total_clonazepam_dose_preceding_24h >= 4) |>
  mutate(value = 1)

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
  rename(day_post_consent = day_post_consent.x, cows_time = time.x, total_clonazepam_dose_preceding_24h = total_dose, cows_index= time_var.x) |>
  filter(total_clonazepam_dose_preceding_24h >= 1.2) |>
  mutate(value = 1)

# combining cows times ineligible due to clonazepam and clonidine
all_ineligible <- inelig_cows_clonazepam |> select(PATID, day_post_consent, cows_index, value) |>
  merge(inelig_cows_clonidine |> select(PATID, day_post_consent, cows_index, value), all = TRUE) |>
  distinct() |>
  pivot_wider(names_from = cows_index, values_from = value, values_fill = 0) |>
  rename_with(~ str_replace(., "^cows_time_", "ineligible_cows_time_"), starts_with("cows_time_")) |>
  select(PATID, day_post_consent, ineligible_cows_time_1, ineligible_cows_time_2, ineligible_cows_time_3, 
         ineligible_cows_time_4, ineligible_cows_time_5, ineligible_cows_time_6, ineligible_cows_time_7)

dat_ineligle_filtered_out <- dat |>
  left_join(all_ineligible) |> # ineligible times
  mutate(across(starts_with("ineligible"), ~ replace_na(., 0))) |>
  mutate(cows_score_1 = ifelse(ineligible_cows_time_1 == 1, as.numeric(NA), cows_score_1),
         cows_score_2 = ifelse(ineligible_cows_time_2 == 1, as.numeric(NA), cows_score_2),
         cows_score_3 = ifelse(ineligible_cows_time_3 == 1, as.numeric(NA), cows_score_3),
         cows_score_4 = ifelse(ineligible_cows_time_4 == 1, as.numeric(NA), cows_score_4),
         cows_score_5 = ifelse(ineligible_cows_time_5 == 1, as.numeric(NA), cows_score_5),
         cows_score_6 = ifelse(ineligible_cows_time_6 == 1, as.numeric(NA), cows_score_6),
         cows_score_7 = ifelse(ineligible_cows_time_7 == 1, as.numeric(NA), cows_score_7),
         cows_time_1 = ifelse(ineligible_cows_time_1 == 1, hms::as_hms(NA), cows_time_1),
         cows_time_2 = ifelse(ineligible_cows_time_2 == 1, hms::as_hms(NA), cows_time_2),
         cows_time_3 = ifelse(ineligible_cows_time_3 == 1, hms::as_hms(NA), cows_time_3),
         cows_time_4 = ifelse(ineligible_cows_time_4 == 1, hms::as_hms(NA), cows_time_4),
         cows_time_5 = ifelse(ineligible_cows_time_5 == 1, hms::as_hms(NA), cows_time_5),
         cows_time_6 = ifelse(ineligible_cows_time_6 == 1, hms::as_hms(NA), cows_time_6),
         cows_time_7 = ifelse(ineligible_cows_time_7 == 1, hms::as_hms(NA), cows_time_7)
         ) |>
  mutate(across(starts_with("cows_time_"), ~ hms::as_hms(.)))

max_cows_by_pat_day_dat_ineligible <- dat_ineligle_filtered_out |>
  select(PATID, day_post_consent, starts_with("cows")) |>
  pivot_longer(
    cols = starts_with("cows"),
    names_to = c(".value", "set"),
    names_pattern = "(.*)_(.*)"
  ) |>
  group_by(PATID, day_post_consent) |>
  summarize(max_cows_score = max(cows_score, na.rm = TRUE),
            max_cows_time = cows_time[which.max(cows_score == max(cows_score, na.rm = TRUE) & !is.na(cows_score))]) |>
  mutate(max_cows_score = ifelse(max_cows_score == -Inf, as.numeric(NA), max_cows_score),
         max_cows_time = ifelse(max_cows_time == -Inf, hms::as_hms(NA), hms::as_hms(max_cows_time)))

max_cows_by_pat_day_dat_ineligible |>
  filter(day_post_consent <= 5) |>
  group_by(is.na(max_cows_score)) |>
  summarize(count = n())


# calculating maximum COWS score per day post consent
dat <- dat_ineligle_filtered_out |>
  group_by(day_post_consent) |>
  mutate(across(c(DMBUPD01, DMBUPD02, DMBUPD03, DMBUPD04, DMBUPD05, DMBUPD06, DMBUPD07, DMBUPDTL,
                  DMCLDD01, DMCLDD02, DMCLDD03, DMCLDD04, DMCLDD05, DMCLDD06, DMCLDDTL,
                  DMCZPD01, DMCZPD02, DMCZPD03, DMCZPD04, DMCZPD05, DMCZPD06, DMCZPDTL), ~ coalesce(., 0))) |>
  mutate(max_cows = max_with_na(cows_score_1, cows_score_2, cows_score_3, cows_score_4, cows_score_5, cows_score_6,
                        cows_score_7, cows_score_8, cows_score_9, cows_score_10, cows_score_11, cows_score_12)) |>
  mutate(max_cows_time = case_when(max_cows == cows_score_1 ~ cows_time_1,
                                   max_cows == cows_score_2 ~ cows_time_2,
                                   max_cows == cows_score_3 ~ cows_time_3,
                                   max_cows == cows_score_4 ~ cows_time_4,
                                   max_cows == cows_score_5 ~ cows_time_5,
                                   max_cows == cows_score_6 ~ cows_time_6,
                                   max_cows == cows_score_7 ~ cows_time_7,
                                   max_cows == cows_score_8 ~ cows_time_8,
                                   max_cows == cows_score_9 ~ cows_time_9,
                                   max_cows == cows_score_10 ~ cows_time_10,
                                   max_cows == cows_score_11 ~ cows_time_11,
                                   max_cows == cows_score_12 ~ cows_time_12
                                   )) |>
  mutate(next_bup_time = case_when(DMBUPT01 >= max_cows_time ~ DMBUPT01,
                                   DMBUPT02 >= max_cows_time ~ DMBUPT02,
                                   DMBUPT03 >= max_cows_time ~ DMBUPT03,
                                   DMBUPT04 >= max_cows_time ~ DMBUPT04,
                                   DMBUPT05 >= max_cows_time ~ DMBUPT05,
                                   DMBUPT06 >= max_cows_time ~ DMBUPT06,
                                   DMBUPT07 >= max_cows_time ~ DMBUPT07,
                                   TRUE ~ NA)) |>
  mutate(next_bup_dose = case_when(next_bup_time == DMBUPT01 ~ DMBUPD01,
                                   next_bup_time == DMBUPT02 ~ DMBUPD02,
                                   next_bup_time == DMBUPT03 ~ DMBUPD03,
                                   next_bup_time == DMBUPT04 ~ DMBUPD04,
                                   next_bup_time == DMBUPT05 ~ DMBUPD05,
                                   next_bup_time == DMBUPT06 ~ DMBUPD06,
                                   next_bup_time == DMBUPT07 ~ DMBUPD07,
                                   TRUE ~ NA)) |>
  mutate(next_clonidine_time = case_when(DMCLDT01 >= max_cows_time ~ DMCLDT01,
                                         DMCLDT02 >= max_cows_time ~ DMCLDT02,
                                         DMCLDT03 >= max_cows_time ~ DMCLDT03,
                                         DMCLDT04 >= max_cows_time ~ DMCLDT04,
                                         DMCLDT05 >= max_cows_time ~ DMCLDT05,
                                         DMCLDT06 >= max_cows_time ~ DMCLDT06,
                                         TRUE ~ NA)) |>
  mutate(next_clonidine_dose = case_when(next_clonidine_time == DMCLDT01 ~ DMCLDD01,
                                         next_clonidine_time == DMCLDT02 ~ DMCLDD02,
                                         next_clonidine_time == DMCLDT03 ~ DMCLDD03,
                                         next_clonidine_time == DMCLDT04 ~ DMCLDD04,
                                         next_clonidine_time == DMCLDT05 ~ DMCLDD05,
                                         next_clonidine_time == DMCLDT06 ~ DMCLDD06,
                                         TRUE ~ NA)) |>
  mutate(next_clonazepam_time = case_when(DMCZPT01 >= max_cows_time ~ DMCZPT01,
                                          DMCZPT02 >= max_cows_time ~ DMCZPT02,
                                          DMCZPT03 >= max_cows_time ~ DMCZPT03,
                                          DMCZPT04 >= max_cows_time ~ DMCZPT04,
                                          DMCZPT05 >= max_cows_time ~ DMCZPT05,
                                          DMCZPT06 >= max_cows_time ~ DMCZPT06,
                                          TRUE ~ NA)) |>
  mutate(next_clonazepam_dose = case_when(next_clonazepam_time == DMCZPT01 ~ DMCZPD01,
                                          next_clonazepam_time == DMCZPT02 ~ DMCZPD02,
                                          next_clonazepam_time == DMCZPT03 ~ DMCZPD03,
                                          next_clonazepam_time == DMCZPT04 ~ DMCZPD04,
                                          next_clonazepam_time == DMCZPT05 ~ DMCZPD05,
                                          next_clonazepam_time == DMCZPT06 ~ DMCZPD06,
                                          TRUE ~ NA)) |>
  relocate(day_post_consent, .before = day) |>
    relocate(max_cows, .after = consent_DMAMDDT) |>
  relocate(max_cows_time, .after = max_cows) |>
    relocate(next_bup_dose, .after = max_cows_time) |>
  relocate(next_bup_time, .after = next_bup_dose) |>
    relocate(next_clonidine_dose, .after = next_bup_time) |>
    relocate(next_clonidine_time, .after = next_clonidine_dose) |>
    relocate(next_clonazepam_dose, .after = next_clonidine_time) |>
    relocate(next_clonazepam_time, .after = next_clonazepam_dose)|>
  mutate(across(c(next_bup_dose, next_clonidine_dose, next_clonazepam_dose), ~ coalesce(., 0)))

saveRDS(dat, here::here("data/analysis_data/max_cows_data.rds"))


dat |>
  group_by(day_post_consent, max_cows >= 5) |>
  summarize(count = n(),
            both_clonidine_and_clonazepam = sum(ifelse(next_clonidine_dose >= 0.1 & 
                                                     next_clonazepam_dose >= 1, 1, 0), na.rm = TRUE),
            clonidine_only = sum(ifelse(next_clonidine_dose >= 0.1 & 
                                          next_clonazepam_dose < 1 &
                                          next_bup_dose == 0, 1, 0), na.rm = TRUE),
            clonazepam_only = sum(ifelse(next_clonidine_dose < 0.1 & 
                                           next_clonazepam_dose >= 1 &
                                           next_bup_dose == 0, 1, 0), na.rm = TRUE)
            )

  










