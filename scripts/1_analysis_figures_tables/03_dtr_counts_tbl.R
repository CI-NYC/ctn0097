library(tidyverse)

EOI <- read_csv(here::here("data/EOI.csv")) |>
  select(PATID, SITE) |>
  mutate(SITE = factor(SITE))

dat <- readRDS(here::here("data/analysis_data/analysis_data.rds")) |>
  left_join(EOI) |>
  mutate(follows_dtr_5_day_1 = case_when(max_cows_1 >= 5 & max_cows_eligible_1 == 1 & max_cows_missing_indicator_1 == 0 & adj_1 == 1 ~ 2, # followed DTR (trt)
                                         (max_cows_1 < 5 | max_cows_eligible_1 == 0 | max_cows_missing_indicator_1 == 1) & adj_1 == 0 ~ 1, # followed DTR (no trt)
                                         TRUE ~ 0), # did not follow DTR
         follows_dtr_5_day_2 = case_when(max_cows_2 >= 5 & max_cows_eligible_2 == 1 & max_cows_missing_indicator_2 == 0 & adj_2 == 1 ~ 2,
                                         (max_cows_2 < 5 | max_cows_eligible_2 == 0 | max_cows_missing_indicator_2 == 1) & adj_2 == 0 ~ 1,
                                         TRUE ~ 0),
         follows_dtr_5_day_3 = case_when(max_cows_3 >= 5 & max_cows_eligible_3 == 1 & max_cows_missing_indicator_3 == 0 & adj_3 == 1 ~ 2,
                                         (max_cows_3 < 5 | max_cows_eligible_3 == 0 | max_cows_missing_indicator_3 == 1) & adj_3 == 0 ~ 1,
                                         TRUE ~ 0),
         follows_dtr_5_day_4 = case_when(max_cows_4 >= 5 & max_cows_eligible_4 == 1 & max_cows_missing_indicator_4 == 0 & adj_4 == 1 ~ 2,
                                         (max_cows_4 < 5 | max_cows_eligible_4 == 0 | max_cows_missing_indicator_4 == 1) & adj_4 == 0 ~ 1,
                                         TRUE ~ 0),
         follows_dtr_5_day_5 = case_when(max_cows_5 >= 5 & max_cows_eligible_5 == 1 & max_cows_missing_indicator_5 == 0 & adj_5 == 1 ~ 2,
                                         (max_cows_5 < 5 | max_cows_eligible_5 == 0 | max_cows_missing_indicator_5 == 1) & adj_5 == 0 ~ 1,
                                         TRUE ~ 0),
         follows_dtr_3_day_1 = case_when(max_cows_1 >= 3 & max_cows_eligible_1 == 1 & max_cows_missing_indicator_1 == 0 & adj_1 == 1 ~ 2,
                                         (max_cows_1 < 3 | max_cows_eligible_1 == 0 | max_cows_missing_indicator_1 == 1) & adj_1 == 0 ~ 1,
                                         TRUE ~ 0),
         follows_dtr_3_day_2 = case_when(max_cows_2 >= 3 & max_cows_eligible_2 == 1 & max_cows_missing_indicator_2 == 0 & adj_2 == 1 ~ 2,
                                         (max_cows_2 < 3 | max_cows_eligible_2 == 0 | max_cows_missing_indicator_2 == 1) & adj_2 == 0 ~ 1,
                                         TRUE ~ 0),
         follows_dtr_3_day_3 = case_when(max_cows_3 >= 3 & max_cows_eligible_3 == 1 & max_cows_missing_indicator_3 == 0 & adj_3 == 1 ~ 2,
                                         (max_cows_3 < 3 | max_cows_eligible_3 == 0 | max_cows_missing_indicator_3 == 1) & adj_3 == 0 ~ 1,
                                         TRUE ~ 0),
         follows_dtr_3_day_4 = case_when(max_cows_4 >= 3 & max_cows_eligible_4 == 1 & max_cows_missing_indicator_4 == 0 & adj_4 == 1 ~ 2,
                                         (max_cows_4 < 3 | max_cows_eligible_4 == 0 | max_cows_missing_indicator_4 == 1) & adj_1 == 0 ~ 1,
                                         TRUE ~ 0),
         follows_dtr_3_day_5 = case_when(max_cows_5 >= 3 & max_cows_eligible_5 == 1 & max_cows_missing_indicator_5 == 0 & adj_5 == 1 ~ 2,
                                         (max_cows_5 < 3 | max_cows_eligible_5 == 0 | max_cows_missing_indicator_5 == 1) & adj_5 == 0 ~ 1,
                                         TRUE ~ 0),
         follows_dtr_always_day_1 = case_when(max_cows_eligible_1 == 1 & adj_1 == 1 ~ 2,
                                              (max_cows_eligible_1 == 0) & adj_1 == 0 ~ 1,
                                              TRUE ~ 0),
         follows_dtr_always_day_2 = case_when(max_cows_eligible_2 == 1 & adj_2 == 1 ~ 2,
                                              (max_cows_eligible_2 == 0) & adj_2 == 0 ~ 1,
                                              TRUE ~ 0),
         follows_dtr_always_day_3 = case_when(max_cows_eligible_3 == 1 & adj_3 == 1 ~ 2,
                                              (max_cows_eligible_3 == 0) & adj_3 == 0 ~ 1,
                                              TRUE ~ 0),
         follows_dtr_always_day_4 = case_when(max_cows_eligible_4 == 1 & adj_4 == 1 ~ 2,
                                              (max_cows_eligible_4 == 0) & adj_4 == 0 ~ 1,
                                              TRUE ~ 0),
         follows_dtr_always_day_5 = case_when(max_cows_eligible_5 == 1 & adj_5 == 1 ~ 2,
                                              (max_cows_eligible_5 == 0) & adj_5 == 0 ~ 1,
                                              TRUE ~ 0))

day_1 <- dat |>
  group_by(follows_dtr_3_day_1) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_1") |>
  full_join(dat |>
              group_by(follows_dtr_5_day_1) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_1")) |>
  full_join(dat |>
              group_by(follows_dtr_always_day_1) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_1")) |>
  mutate(day = 1)

day_2 <- dat |>
  filter(C_1 == 1, Y_1 == 0) |>
  group_by(follows_dtr_3_day_2) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_2") |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              group_by(follows_dtr_5_day_2) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_2")) |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              group_by(follows_dtr_always_day_2) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_2")) |>
  mutate(day = 2)

day_3 <- dat |>
  filter(C_2 == 1, Y_2 == 0) |>
  group_by(follows_dtr_3_day_3) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_3") |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              group_by(follows_dtr_5_day_3) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_3")) |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              group_by(follows_dtr_always_day_3) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_3")) |>
  mutate(day = 3)

day_4 <- dat |>
  filter(C_3 == 1, Y_3 == 0) |>
  group_by(follows_dtr_3_day_4) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_4") |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              group_by(follows_dtr_5_day_4) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_4")) |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              group_by(follows_dtr_always_day_4) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_4")) |>
  mutate(day = 4)

day_5 <- dat |>
  filter(C_4 == 1, Y_4 == 0) |>
  group_by(follows_dtr_3_day_5) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_5") |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              group_by(follows_dtr_5_day_5) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_5")) |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              group_by(follows_dtr_always_day_5) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_5")) |>
  mutate(day = 5)

dtr_counts <- day_1 |>
  merge(day_2, all = TRUE) |>
  merge(day_3, all = TRUE) |>
  merge(day_4, all = TRUE) |>
  merge(day_5, all = TRUE) |>
  arrange(day, desc(follows_dtr_1)) |>
  relocate(day, .before = follows_dtr_1) |>
  relocate(count_3, .after = count_5) |>
  pivot_wider(names_from = follows_dtr_1, values_from= c("count_5", "count_3", "count_always")) 

dtr_counts

knitr::kable(dtr_counts, format = "latex", booktabs = TRUE)

# SITE 1

day_1_02018 <- dat |>
  filter(SITE == "02018") |>
  group_by(follows_dtr_3_day_1) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_1") |>
  full_join(dat |>
              filter(SITE == "02018") |>
              group_by(follows_dtr_5_day_1) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_1")) |>
  full_join(dat |>
              filter(SITE == "02018") |>
              group_by(follows_dtr_always_day_1) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_1")) |>
  mutate(day = 1)

day_2_02018 <- dat |>
  filter(C_1 == 1, Y_1 == 0) |>
  filter(SITE == "02018") |>
  group_by(follows_dtr_3_day_2) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_2") |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02018") |>
              group_by(follows_dtr_5_day_2) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_2")) |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02018") |>
              group_by(follows_dtr_always_day_2) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_2")) |>
  mutate(day = 2)

day_3_02018 <- dat |>
  filter(C_2 == 1, Y_2 == 0) |>
  filter(SITE == "02018") |>
  group_by(follows_dtr_3_day_3) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_3") |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02018") |>
              group_by(follows_dtr_5_day_3) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_3")) |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02018") |>
              group_by(follows_dtr_always_day_3) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_3")) |>
  mutate(day = 3)

day_4_02018 <- dat |>
  filter(C_3 == 1, Y_3 == 0) |>
  filter(SITE == "02018") |>
  group_by(follows_dtr_3_day_4) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_4") |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02018") |>
              group_by(follows_dtr_5_day_4) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_4")) |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02018") |>
              group_by(follows_dtr_always_day_4) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_4")) |>
  mutate(day = 4)

day_5_02018 <- dat |>
  filter(C_4 == 1, Y_4 == 0) |>
  filter(SITE == "02018") |>
  group_by(follows_dtr_3_day_5) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_5") |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02018") |>
              group_by(follows_dtr_5_day_5) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_5")) |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02018") |>
              group_by(follows_dtr_always_day_5) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_5")) |>
  mutate(day = 5)

dtr_counts_02018 <- day_1_02018 |>
  merge(day_2_02018, all = TRUE) |>
  merge(day_3_02018, all = TRUE) |>
  merge(day_4_02018, all = TRUE) |>
  merge(day_5_02018, all = TRUE) |>
  arrange(day, desc(follows_dtr_1)) |>
  relocate(day, .before = follows_dtr_1) |>
  relocate(count_3, .after = count_5) |>
  pivot_wider(names_from = follows_dtr_1, values_from= c("count_5", "count_3", "count_always")) |>
  mutate(SITE = "02018")

dtr_counts_02018

knitr::kable(dtr_counts_02018, format = "latex", booktabs = TRUE)

# SITE 2
day_1_02076 <- dat |>
  filter(SITE == "02076") |>
  group_by(follows_dtr_3_day_1) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_1") |>
  full_join(dat |>
              filter(SITE == "02076") |>
              group_by(follows_dtr_5_day_1) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_1")) |>
  full_join(dat |>
              filter(SITE == "02076") |>
              group_by(follows_dtr_always_day_1) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_1")) |>
  mutate(day = 1)

day_2_02076 <- dat |>
  filter(C_1 == 1, Y_1 == 0) |>
  filter(SITE == "02076") |>
  group_by(follows_dtr_3_day_2) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_2") |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02076") |>
              group_by(follows_dtr_5_day_2) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_2")) |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02076") |>
              group_by(follows_dtr_always_day_2) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_2")) |>
  mutate(day = 2)

day_3_02076 <- dat |>
  filter(C_2 == 1, Y_2 == 0) |>
  filter(SITE == "02076") |>
  group_by(follows_dtr_3_day_3) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_3") |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02076") |>
              group_by(follows_dtr_5_day_3) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_3")) |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02076") |>
              group_by(follows_dtr_always_day_3) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_3")) |>
  mutate(day = 3)

day_4_02076 <- dat |>
  filter(C_3 == 1, Y_3 == 0) |>
  filter(SITE == "02076") |>
  group_by(follows_dtr_3_day_4) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_4") |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02076") |>
              group_by(follows_dtr_5_day_4) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_4")) |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02076") |>
              group_by(follows_dtr_always_day_4) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_4")) |>
  mutate(day = 4)

day_5_02076 <- dat |>
  filter(C_4 == 1, Y_4 == 0) |>
  filter(SITE == "02076") |>
  group_by(follows_dtr_3_day_5) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_5") |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02076") |>
              group_by(follows_dtr_5_day_5) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_5")) |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02076") |>
              group_by(follows_dtr_always_day_5) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_5")) |>
  mutate(day = 5)

dtr_counts_02076 <- day_1_02076 |>
  merge(day_2_02076, all = TRUE) |>
  merge(day_3_02076, all = TRUE) |>
  merge(day_4_02076, all = TRUE) |>
  merge(day_5_02076, all = TRUE) |>
  arrange(day, desc(follows_dtr_1)) |>
  relocate(day, .before = follows_dtr_1) |>
  relocate(count_3, .after = count_5) |>
  pivot_wider(names_from = follows_dtr_1, values_from= c("count_5", "count_3", "count_always")) |>
  mutate(SITE = "02076")

dtr_counts_02076

knitr::kable(dtr_counts_02076, format = "latex", booktabs = TRUE)

# SITE 3

day_1_02201 <- dat |>
  filter(SITE == "02201") |>
  group_by(follows_dtr_3_day_1) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_1") |>
  full_join(dat |>
              filter(SITE == "02201") |>
              group_by(follows_dtr_5_day_1) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_1")) |>
  full_join(dat |>
              filter(SITE == "02201") |>
              group_by(follows_dtr_always_day_1) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_1")) |>
  mutate(day = 1)

day_2_02201 <- dat |>
  filter(C_1 == 1, Y_1 == 0) |>
  filter(SITE == "02201") |>
  group_by(follows_dtr_3_day_2) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_2") |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02201") |>
              group_by(follows_dtr_5_day_2) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_2")) |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02201") |>
              group_by(follows_dtr_always_day_2) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_2")) |>
  mutate(day = 2)

day_3_02201 <- dat |>
  filter(C_2 == 1, Y_2 == 0) |>
  filter(SITE == "02201") |>
  group_by(follows_dtr_3_day_3) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_3") |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02201") |>
              group_by(follows_dtr_5_day_3) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_3")) |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02201") |>
              group_by(follows_dtr_always_day_3) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_3")) |>
  mutate(day = 3)

day_4_02201 <- dat |>
  filter(C_3 == 1, Y_3 == 0) |>
  filter(SITE == "02201") |>
  group_by(follows_dtr_3_day_4) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_4") |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02201") |>
              group_by(follows_dtr_5_day_4) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_4")) |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02201") |>
              group_by(follows_dtr_always_day_4) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_4")) |>
  mutate(day = 4)

day_5_02201 <- dat |>
  filter(C_4 == 1, Y_4 == 0) |>
  filter(SITE == "02201") |>
  group_by(follows_dtr_3_day_5) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_5") |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02201") |>
              group_by(follows_dtr_5_day_5) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_5")) |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02201") |>
              group_by(follows_dtr_always_day_5) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_5")) |>
  mutate(day = 5)

dtr_counts_02201 <- day_1_02201 |>
  merge(day_2_02201, all = TRUE) |>
  merge(day_3_02201, all = TRUE) |>
  merge(day_4_02201, all = TRUE) |>
  merge(day_5_02201, all = TRUE) |>
  arrange(day, desc(follows_dtr_1)) |>
  relocate(day, .before = follows_dtr_1) |>
  relocate(count_3, .after = count_5) |>
  pivot_wider(names_from = follows_dtr_1, values_from= c("count_5", "count_3", "count_always")) |>
  mutate(SITE = "02201")

dtr_counts_02201

knitr::kable(dtr_counts_02201, format = "latex", booktabs = TRUE)

# SITE 4

day_1_02207 <- dat |>
  filter(SITE == "02207") |>
  group_by(follows_dtr_3_day_1) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_1") |>
  full_join(dat |>
              filter(SITE == "02207") |>
              group_by(follows_dtr_5_day_1) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_1")) |>
  full_join(dat |>
              filter(SITE == "02207") |>
              group_by(follows_dtr_always_day_1) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_1")) |>
  mutate(day = 1)

day_2_02207 <- dat |>
  filter(C_1 == 1, Y_1 == 0) |>
  filter(SITE == "02207") |>
  group_by(follows_dtr_3_day_2) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_2") |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02207") |>
              group_by(follows_dtr_5_day_2) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_2")) |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02207") |>
              group_by(follows_dtr_always_day_2) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_2")) |>
  mutate(day = 2)

day_3_02207 <- dat |>
  filter(C_2 == 1, Y_2 == 0) |>
  filter(SITE == "02207") |>
  group_by(follows_dtr_3_day_3) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_3") |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02207") |>
              group_by(follows_dtr_5_day_3) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_3")) |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02207") |>
              group_by(follows_dtr_always_day_3) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_3")) |>
  mutate(day = 3)

day_4_02207 <- dat |>
  filter(C_3 == 1, Y_3 == 0) |>
  filter(SITE == "02207") |>
  group_by(follows_dtr_3_day_4) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_4") |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02207") |>
              group_by(follows_dtr_5_day_4) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_4")) |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02207") |>
              group_by(follows_dtr_always_day_4) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_4")) |>
  mutate(day = 4)

day_5_02207 <- dat |>
  filter(C_4 == 1, Y_4 == 0) |>
  filter(SITE == "02207") |>
  group_by(follows_dtr_3_day_5) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_5") |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02207") |>
              group_by(follows_dtr_5_day_5) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_5")) |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02207") |>
              group_by(follows_dtr_always_day_5) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_5")) |>
  mutate(day = 5)

dtr_counts_02207 <- day_1_02207 |>
  merge(day_2_02207, all = TRUE) |>
  merge(day_3_02207, all = TRUE) |>
  merge(day_4_02207, all = TRUE) |>
  merge(day_5_02207, all = TRUE) |>
  arrange(day, desc(follows_dtr_1)) |>
  relocate(day, .before = follows_dtr_1) |>
  relocate(count_3, .after = count_5) |>
  pivot_wider(names_from = follows_dtr_1, values_from= c("count_5", "count_3", "count_always")) |>
  mutate(SITE = "02207")

dtr_counts_02207

knitr::kable(dtr_counts_02207, format = "latex", booktabs = TRUE)

# SITE 5

day_1_02217 <- dat |>
  filter(SITE == "02217") |>
  group_by(follows_dtr_3_day_1) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_1") |>
  full_join(dat |>
              filter(SITE == "02217") |>
              group_by(follows_dtr_5_day_1) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_1")) |>
  full_join(dat |>
              filter(SITE == "02217") |>
              group_by(follows_dtr_always_day_1) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_1")) |>
  mutate(day = 1)

day_2_02217 <- dat |>
  filter(C_1 == 1, Y_1 == 0) |>
  filter(SITE == "02217") |>
  group_by(follows_dtr_3_day_2) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_2") |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02217") |>
              group_by(follows_dtr_5_day_2) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_2")) |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02217") |>
              group_by(follows_dtr_always_day_2) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_2")) |>
  mutate(day = 2)

day_3_02217 <- dat |>
  filter(C_2 == 1, Y_2 == 0) |>
  filter(SITE == "02217") |>
  group_by(follows_dtr_3_day_3) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_3") |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02217") |>
              group_by(follows_dtr_5_day_3) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_3")) |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02217") |>
              group_by(follows_dtr_always_day_3) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_3")) |>
  mutate(day = 3)

day_4_02217 <- dat |>
  filter(C_3 == 1, Y_3 == 0) |>
  filter(SITE == "02217") |>
  group_by(follows_dtr_3_day_4) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_4") |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02217") |>
              group_by(follows_dtr_5_day_4) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_4")) |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02217") |>
              group_by(follows_dtr_always_day_4) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_4")) |>
  mutate(day = 4)

day_5_02217 <- dat |>
  filter(C_4 == 1, Y_4 == 0) |>
  filter(SITE == "02217") |>
  group_by(follows_dtr_3_day_5) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_5") |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02217") |>
              group_by(follows_dtr_5_day_5) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_5")) |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02217") |>
              group_by(follows_dtr_always_day_5) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_5")) |>
  mutate(day = 5)

dtr_counts_02217 <- day_1_02217 |>
  merge(day_2_02217, all = TRUE) |>
  merge(day_3_02217, all = TRUE) |>
  merge(day_4_02217, all = TRUE) |>
  merge(day_5_02217, all = TRUE) |>
  arrange(day, desc(follows_dtr_1)) |>
  relocate(day, .before = follows_dtr_1) |>
  relocate(count_3, .after = count_5) |>
  pivot_wider(names_from = follows_dtr_1, values_from= c("count_5", "count_3", "count_always")) |>
  mutate(SITE = "02217")

dtr_counts_02217

knitr::kable(dtr_counts_02217, format = "latex", booktabs = TRUE)

# SITE 6
day_1_02221 <- dat |>
  filter(SITE == "02221") |>
  group_by(follows_dtr_3_day_1) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_1") |>
  full_join(dat |>
              filter(SITE == "02221") |>
              group_by(follows_dtr_5_day_1) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_1")) |>
  full_join(dat |>
              filter(SITE == "02221") |>
              group_by(follows_dtr_always_day_1) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_1")) |>
  mutate(day = 1)

day_2_02221 <- dat |>
  filter(C_1 == 1, Y_1 == 0) |>
  filter(SITE == "02221") |>
  group_by(follows_dtr_3_day_2) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_2") |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02221") |>
              group_by(follows_dtr_5_day_2) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_2")) |>
  full_join(dat |>
              filter(C_1 == 1, Y_1 == 0) |>
              filter(SITE == "02221") |>
              group_by(follows_dtr_always_day_2) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_2")) |>
  mutate(day = 2)

day_3_02221 <- dat |>
  filter(C_2 == 1, Y_2 == 0) |>
  filter(SITE == "02221") |>
  group_by(follows_dtr_3_day_3) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_3") |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02221") |>
              group_by(follows_dtr_5_day_3) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_3")) |>
  full_join(dat |>
              filter(C_2 == 1, Y_2 == 0) |>
              filter(SITE == "02221") |>
              group_by(follows_dtr_always_day_3) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_3")) |>
  mutate(day = 3)

day_4_02221 <- dat |>
  filter(C_3 == 1, Y_3 == 0) |>
  filter(SITE == "02221") |>
  group_by(follows_dtr_3_day_4) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_4") |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02221") |>
              group_by(follows_dtr_5_day_4) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_4")) |>
  full_join(dat |>
              filter(C_3 == 1, Y_3 == 0) |>
              filter(SITE == "02221") |>
              group_by(follows_dtr_always_day_4) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_4")) |>
  mutate(day = 4)

day_5_02221 <- dat |>
  filter(C_4 == 1, Y_4 == 0) |>
  filter(SITE == "02221") |>
  group_by(follows_dtr_3_day_5) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_5") |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02221") |>
              group_by(follows_dtr_5_day_5) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_5")) |>
  full_join(dat |>
              filter(C_4 == 1, Y_4 == 0) |>
              filter(SITE == "02221") |>
              group_by(follows_dtr_always_day_5) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_5")) |>
  mutate(day = 5)

dtr_counts_02221 <- day_1_02221 |>
  merge(day_2_02221, all = TRUE) |>
  merge(day_3_02221, all = TRUE) |>
  merge(day_4_02221, all = TRUE) |>
  merge(day_5_02221, all = TRUE) |>
  arrange(day, desc(follows_dtr_1)) |>
  relocate(day, .before = follows_dtr_1) |>
  relocate(count_3, .after = count_5) |>
  pivot_wider(names_from = follows_dtr_1, values_from= c("count_5", "count_3", "count_always")) |>
  mutate(SITE = "02221")

dtr_counts_02221

knitr::kable(dtr_counts_02221, format = "latex", booktabs = TRUE)

dtr_counts_all <- dtr_counts_02018 |>
  rbind(dtr_counts_02076) |>
  rbind(dtr_counts_02201) |>
  rbind(dtr_counts_02207) |>
  rbind(dtr_counts_02217) |>
  rbind(dtr_counts_02221) |>
  relocate(SITE, .before = day)

knitr::kable(dtr_counts_all, format = "latex", booktabs = TRUE)

