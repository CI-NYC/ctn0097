library(tidyverse)

dat <- readRDS(here::here("data/analysis_data/analysis_data.rds")) |>
  mutate(follows_dtr_5_day_1 = case_when(max_cows_1 >= 5 & max_cows_eligible_1 == 1 & max_cows_missing_indicator_1 == 0 & adj_1 == 1 ~ 2,
                                         (max_cows_1 < 5 | max_cows_eligible_1 == 0 | max_cows_missing_indicator_1 == 1) & adj_1 == 0 ~ 1,
                                         TRUE ~ 0),
         follows_dtr_5_day_2 = case_when(max_cows_2 >= 5 & max_cows_eligible_2 == 1 & max_cows_missing_indicator_2 == 0 & adj_2 == 1 ~ 2,
                                         (max_cows_2 < 5 | max_cows_eligible_2 == 0 | max_cows_missing_indicator_2== 1) & adj_2 == 0 ~ 1,
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
  left_join(dat |>
              group_by(follows_dtr_5_day_1) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_1")) |>
  left_join(dat |>
              group_by(follows_dtr_always_day_1) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_1")) |>
  mutate(day = 1)

day_2 <- dat |>
  group_by(follows_dtr_3_day_2) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_2") |>
  left_join(dat |>
              group_by(follows_dtr_5_day_2) |>
              summarize(count_5 = n()) |>
              rename("follows_dtr_1" = "follows_dtr_5_day_2")) |>
  left_join(dat |>
              group_by(follows_dtr_always_day_2) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_2")) |>
  mutate(day = 2)

day_3 <- dat |>
  group_by(follows_dtr_3_day_3) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_3") |>
  left_join(dat |>
              group_by(follows_dtr_5_day_3) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_3")) |>
  left_join(dat |>
              group_by(follows_dtr_always_day_3) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_3")) |>
  mutate(day = 3)

day_4 <- dat |>
  group_by(follows_dtr_3_day_4) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_4") |>
  left_join(dat |>
              group_by(follows_dtr_5_day_4) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_4")) |>
  left_join(dat |>
              group_by(follows_dtr_always_day_4) |>
              summarize(count_always = n()) |>
              rename("follows_dtr_1" = "follows_dtr_always_day_4")) |>
  mutate(day = 4)

day_5 <- dat |>
  group_by(follows_dtr_3_day_5) |>
  summarize(count_3 = n()) |>
  rename("follows_dtr_1" = "follows_dtr_3_day_5") |>
  left_join(dat |>
              group_by(follows_dtr_5_day_5) |>
              summarize(count_5 = n())|>
              rename("follows_dtr_1" = "follows_dtr_5_day_5")) |>
  left_join(dat |>
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

