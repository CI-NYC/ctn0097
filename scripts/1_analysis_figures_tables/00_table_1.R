library(tidyverse)
library(gtsummary)

dat <- readRDS(here::here("data/analysis_data/pre_imputed_analysis_data.rds")) |>
  mutate(max_cows_1 = ifelse(max_cows_missing_indicator_1 == 1, as.numeric(NA), max_cows_1),
         max_cows_2 = ifelse(max_cows_missing_indicator_2 == 1, as.numeric(NA), max_cows_2),
         max_cows_3 = ifelse(max_cows_missing_indicator_3 == 1, as.numeric(NA), max_cows_3),
         max_cows_4 = ifelse(max_cows_missing_indicator_4 == 1, as.numeric(NA), max_cows_4),
         max_cows_5 = ifelse(max_cows_missing_indicator_5 == 1, as.numeric(NA), max_cows_5)
         ) |>
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
            "max_cows_ineligible_1", 
            "max_cows_missing_indicator_1",
            #"L3_1"
            ), 
          c("max_cows_2", 
            "max_cows_ineligible_2", 
            "max_cows_missing_indicator_2",
            #"L3_2"
            ), 
          c("max_cows_3", 
            "max_cows_ineligible_3", 
            "max_cows_missing_indicator_3",
            #"L3_3"
            ), 
          c("max_cows_4", 
            "max_cows_ineligible_4", 
            "max_cows_missing_indicator_4",
            #"L3_4"
            ), 
          c("max_cows_5", 
            "max_cows_ineligible_5", 
            "max_cows_missing_indicator_5",
            #"L3_5"
            )
)

table1 <- dat |>
  select(W, unlist(L), unlist(A), starts_with("C"), starts_with("Y")) |>
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



