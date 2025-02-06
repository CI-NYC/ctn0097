library(tidyverse)
library(gtsummary)

dat <- readRDS(here::here("data/analysis_data/pre_imputed_analysis_data.rds")) |>
  # mutate(max_cows_1 = ifelse(max_cows_missing_indicator_1 == 1, as.numeric(NA), max_cows_1),
  #        max_cows_2 = ifelse(max_cows_missing_indicator_2 == 1, as.numeric(NA), max_cows_2),
  #        max_cows_3 = ifelse(max_cows_missing_indicator_3 == 1, as.numeric(NA), max_cows_3),
  #        max_cows_4 = ifelse(max_cows_missing_indicator_4 == 1, as.numeric(NA), max_cows_4),
  #        max_cows_5 = ifelse(max_cows_missing_indicator_5 == 1, as.numeric(NA), max_cows_5)
  #        ) |>
  mutate(across(starts_with("C_"), ~ ifelse(. == 0, 1, ifelse(. == 1, 0, .))))

W <- c("days_from_admission_to_consent",
       # demographics
       "DESEX",
       "age",
       "is_hispanic",
       "DEWHITE",
       "DEBLACK",
       "DEOTHER",
       "alcohol_use_disorder", #missing
       "amphetamine_use_disorder", #missing
       "cannabis_use_disorder", #missing
       "cocaine_use_disorder", #missing
       "sedative_use_disorder", #missing
       "injection_opioid_use",
       "years_since_first_opioid_use",
       "anxiety", #missing
       "bipolar", #missing,
       "depression"#, #missing
       )

A <- list(c("adj_1"),
          c("adj_2"),
          c("adj_3"),
          c("adj_4"),
          c("adj_5")
)


L <- list(c("max_cows_1", 
            "max_cows_eligible_1", 
            "max_cows_missing_indicator_1",
            "L3_1"
            ), 
          c("max_cows_2", 
            "max_cows_eligible_2", 
            "max_cows_missing_indicator_2",
            "L3_2"
            ), 
          c("max_cows_3", 
            "max_cows_eligible_3", 
            "max_cows_missing_indicator_3",
            "L3_3"
            ), 
          c("max_cows_4", 
            "max_cows_eligible_4", 
            "max_cows_missing_indicator_4",
            "L3_4"
            ), 
          c("max_cows_5", 
            "max_cows_eligible_5", 
            "max_cows_missing_indicator_5",
            "L3_5"
            )
)

table1 <- dat |>
  select(W, unlist(L), unlist(A), "C_14", "Y_14") |>
  tbl_summary(
    statistic = list(all_continuous() ~ "{median} ({p25}, {p75})",
                     all_categorical() ~ "{n} ({p}%)"),
    type = list(days_from_admission_to_consent ~ "continuous"),
    missing = "ifany"
  )

table1

latex_table <- table1 |>
  as_kable(format = "latex")

cat(latex_table)

# getting missing counts for ineligibility

dat_long <- readRDS(here::here("data/analysis_data/max_cows_data.rds")) |>
  select(PATID, day_post_consent, DMEFF24H, DMDROWSY, DMDIZZY, both_inelig) |>
  mutate(DMDROWSY = case_when(DMEFF24H == 0 ~ 0,
                              TRUE ~ DMDROWSY),
         DMDIZZY = case_when(DMEFF24H == 0 ~ 0,
                              TRUE ~ DMDIZZY)
         ) |>
  filter(day_post_consent <= 5)

# counting ineligible (severely drowsy, dizzy, OR ineligible due to dose)
# dat_long_ineligible <- dat_long |>
#   filter(DMDROWSY == 3 | DMDIZZY == 3 | both_inelig == 1)
# 
# inelig_tbl <- dat_long_ineligible |> group_by(day_post_consent) |> summarize(count_inelig = n())
# 
# # counting eligible (not severely drowsy, dizzy, NOR ineligible due to dose) -- if any of these are missing, then considered missing variable
# dat_long_eligible <- dat_long |>
#   filter(DMDROWSY %in% c(0, 1, 2, 3) & DMDIZZY %in% c(0, 1, 2, 3) & both_inelig == 0)
# 
# elig_tbl <- dat_long_eligible |> group_by(day_post_consent) |> summarize(count_elig = n())
# 
# tbl <- elig_tbl |>
#   left_join(inelig_tbl) |>
#   mutate(sum_not_missing = rowSums(cbind(count_elig, count_inelig))) |>
#   mutate(n = case_when(day_post_consent == 1 ~ 415,
#                        day_post_consent == 2 ~ 415 - 7,
#                        day_post_consent == 3 ~ 415 - 37,
#                        day_post_consent == 4 ~ 415 - 74,
#                        day_post_consent == 5 ~ 415 - 144
#                        )) |>
#   mutate(missing = n - sum_not_missing,
#          perc_eligible = count_elig/sum_not_missing)
# 
# tbl










