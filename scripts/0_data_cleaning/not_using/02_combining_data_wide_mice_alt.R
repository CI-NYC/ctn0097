
#devtools::install_github("nt-williams/lmtp@competing-risks")
library(lmtp)
library(tidyverse)
library(mice)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)

# loading data and making into wide format

dat_long <- readRDS(here::here("data/analysis_data/max_cows_data_alt.rds")) |>
  mutate(A1 = case_when(day_post_consent == naltrexone_injection_day & pre_injection_clonidine >= 0.1 ~ 1, # if injection day, only look at pre-injection 
                        DMCLDDTL >= 0.1 ~ 1, #& DMCLDDTL >= 0.1 ~ 1, # if cows missing then do all medications over the day
                        is.na(max_cows) == FALSE & post_cows_clonidine_dose >= 0.1 ~ 1, # if cows not missing, then look at post-cows dose
                        TRUE ~ 0), # otherwise, did not receive medication
         A2 = case_when(day_post_consent == naltrexone_injection_day & pre_injection_clonazepam >= 1 ~ 1,  # if injection day, only look at pre-injection 
                        DMCZPDTL >= 1 ~ 1, #& DMCZPDTL >= 1 ~ 1, # if cows missing then do all medications over the day
                        is.na(max_cows) == FALSE & post_cows_clonazepam_dose >= 1 ~ 1, # if cows not missing, then look at post-cows dose
                        TRUE ~ 0), # otherwise, did not receive medication
         A3 = case_when(DMBZODTL > 0 ~ 1,
                        TRUE ~ 0),
         L1 = ifelse(DMBUPDTL > 0, 1, 0),
         L2 = ifelse(DMDROWSY == 3 | DMDIZZY == 3, 1, 0)) |>
  filter(day_post_consent <= 14) |>
  mutate_at(vars(starts_with("L")), ~ if_else(is.na(.), 0, .)) |>
  rename("max_cows_ineligible" = "both_inelig") |>
  mutate(naltrexone_injection_day_shift = case_when(naltrexone_injection_day == day_post_consent & is.na(max_cows_time) & pre_injection_clonidine == 0 & pre_injection_clonazepam == 0 & is.na(A3) ~ 1, # if no cows on day x, then injection counted as previous day
                                                    TRUE ~ 0),
         adj = ifelse(rowSums(cbind(A1, A2, A3), na.rm = TRUE) >= 1, 1, 0)) |>
  select(PATID, PROTSEG, naltrexone_injection_day, naltrexone_injection_time, end_induction_day, 
         received_naltrexone_injection, day_post_consent, max_cows, max_cows_time, max_cows_ineligible, ends_with("inelig"), A1, A2, A3, adj, L1, L2, naltrexone_injection_day_shift) |>
  group_by(PATID) |>
  mutate(naltrexone_injection_day = ifelse(any(naltrexone_injection_day_shift) == 1, naltrexone_injection_day - 1, naltrexone_injection_day),
         end_induction_day = ifelse(any(naltrexone_injection_day_shift) == 1, end_induction_day - 1, end_induction_day)) |>
  select(-naltrexone_injection_day_shift) |>
  filter(day_post_consent <= end_induction_day) |>
  mutate(max_cows_missing_indicator = ifelse(is.na(max_cows), 1, 0)) # replacing max cows with 0 

saveRDS(dat_long, "data/analysis_data/dat_long_alt.rds")

dat <- dat_long |>
  pivot_wider(names_from = day_post_consent, values_from = c(max_cows, max_cows_time, max_cows_missing_indicator, max_cows_ineligible, ends_with("inelig"), A1, A2, A3, L1, L2, adj)) |>
  mutate(PROTSEG = ifelse(PROTSEG == "C", 0, 1))

# making outcome variables

dat <- dat |>
  mutate(Y_1 = 0,
         Y_2 = case_when(is.na(naltrexone_injection_day) ~ 0,
                         naltrexone_injection_day == 2 ~ 1,
                         TRUE ~ 0),
         Y_3 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_2 == 1 ~ 1,
                         naltrexone_injection_day == 3 ~ 1,
                         TRUE ~ 0),
         Y_4 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_3 == 1 ~ 1,
                         naltrexone_injection_day == 4 ~ 1,
                         TRUE ~ 0),
         Y_5 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_4 == 1 ~ 1,
                         naltrexone_injection_day == 5 ~ 1,
                         TRUE ~ 0),
         Y_6 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_5 == 1 ~ 1,
                         naltrexone_injection_day == 6 ~ 1,
                         TRUE ~ 0),
         Y_7 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_6 == 1 ~ 1,
                         naltrexone_injection_day == 7 ~ 1,
                         TRUE ~ 0),
         Y_8 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_7 == 1 ~ 1,
                         naltrexone_injection_day == 8 ~ 1,
                         TRUE ~ 0),
         Y_9 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                         Y_8 == 1 ~ 1,
                         naltrexone_injection_day == 9 ~ 1,
                         TRUE ~ 0),
         Y_10 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_9 == 1 ~ 1,
                          naltrexone_injection_day == 10 ~ 1,
                          TRUE ~ 0),
         Y_11 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_10 == 1 ~ 1,
                          naltrexone_injection_day == 11 ~ 1,
                          TRUE ~ 0),
         Y_12 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_11 == 1 ~ 1,
                          naltrexone_injection_day == 12 ~ 1,
                          TRUE ~ 0),
         Y_13 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_12 == 1 ~ 1,
                          naltrexone_injection_day == 13 ~ 1,
                          TRUE ~ 0),
         Y_14 = case_when(is.na(naltrexone_injection_day) ~ 0, 
                          Y_13 == 1 ~ 1,
                          naltrexone_injection_day == 14 ~ 1,
                          TRUE ~ 0))

# making censoring variables

dat <- dat |>
  mutate(C_1 = ifelse(end_induction_day == 1 & received_naltrexone_injection == 0, 0, 1),
         C_2 = case_when(C_1 == 0 ~ 0,
                         end_induction_day == 2 & received_naltrexone_injection == 0 ~ 0,
                         TRUE ~ 1),
         C_3 = case_when(C_2 == 0 ~ 0,
                         end_induction_day == 3 & received_naltrexone_injection == 0 ~ 0,
                         TRUE ~ 1),
         C_4 = case_when(C_3 == 0 ~ 0,
                         end_induction_day == 4 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_5 = case_when(C_4 == 0 ~ 0,
                         end_induction_day == 5 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_6 = case_when(C_5 == 0 ~ 0,
                         end_induction_day == 6 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_7 = case_when(C_6 == 0 ~ 0,
                         end_induction_day == 7 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_8 = case_when(C_7 == 0 ~ 0,
                         end_induction_day == 8 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_9 = case_when(C_8 == 0 ~ 0,
                         end_induction_day == 9 & received_naltrexone_injection == 0 ~ 0, 
                         TRUE ~ 1),
         C_10 = case_when(C_9 == 0 ~ 0,
                          end_induction_day == 10 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1),
         C_11 = case_when(C_10 == 0 ~ 0,
                          end_induction_day == 11 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1),
         C_12 = case_when(C_11 == 0 ~ 0,
                          end_induction_day == 12 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1),
         C_13 = case_when(C_12 == 0 ~ 0,
                          end_induction_day == 13 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1),
         C_14 = case_when(C_13 == 0 ~ 0,
                          end_induction_day == 14 & received_naltrexone_injection == 0 ~ 0, 
                          TRUE ~ 1))  |>
  select(PATID, PROTSEG, received_naltrexone_injection,
         starts_with("max_cows_"),
         starts_with("adj_"),
         starts_with("L"),
         starts_with("C_"),
         starts_with("Y_"),
  )

# carry-forward previous COWS if missing, we are now imputing with 0

# dat <- dat |>
#   mutate(max_cows_2 = ifelse(is.na(max_cows_2) & C_2 == 1, max_cows_1, max_cows_2),
#          max_cows_3 = ifelse(is.na(max_cows_3) & C_3 == 1, max_cows_2, max_cows_3),
#          max_cows_4 = ifelse(is.na(max_cows_4) & C_4 == 1, max_cows_3, max_cows_4),
#          max_cows_5 = ifelse(is.na(max_cows_5) & C_5 == 1, max_cows_4, max_cows_5),
#          max_cows_6 = ifelse(is.na(max_cows_6) & C_6 == 1, max_cows_5, max_cows_6),
#          max_cows_7 = ifelse(is.na(max_cows_7) & C_7 == 1, max_cows_6, max_cows_7),
#          max_cows_8 = ifelse(is.na(max_cows_8) & C_8 == 1, max_cows_7, max_cows_8),
#          max_cows_9 = ifelse(is.na(max_cows_9) & C_9 == 1, max_cows_8, max_cows_9),
#          max_cows_10 = ifelse(is.na(max_cows_10) & C_10 == 1, max_cows_9, max_cows_10),
#          max_cows_11 = ifelse(is.na(max_cows_11) & C_11 == 1, max_cows_10, max_cows_11),
#          max_cows_12 = ifelse(is.na(max_cows_12) & C_12 == 1, max_cows_11, max_cows_12),
#          max_cows_13 = ifelse(is.na(max_cows_13) & C_13 == 1, max_cows_12, max_cows_13),
#          max_cows_14 = ifelse(is.na(max_cows_14) & C_14 == 1, max_cows_13, max_cows_14),
#          max_cows_ineligible_1 = ifelse(is.na(max_cows_ineligible_1), 0, max_cows_ineligible_1), 
#          max_cows_ineligible_2 = ifelse(is.na(max_cows_ineligible_2) & C_2 == 1, max_cows_ineligible_1, max_cows_ineligible_2),
#          max_cows_ineligible_3 = ifelse(is.na(max_cows_ineligible_3) & C_3 == 1, max_cows_ineligible_2, max_cows_ineligible_3),
#          max_cows_ineligible_4 = ifelse(is.na(max_cows_ineligible_4) & C_4 == 1, max_cows_ineligible_3, max_cows_ineligible_4),
#          max_cows_ineligible_5 = ifelse(is.na(max_cows_ineligible_5) & C_5 == 1, max_cows_ineligible_4, max_cows_ineligible_5),
#          max_cows_ineligible_6 = ifelse(is.na(max_cows_ineligible_6) & C_6 == 1, max_cows_ineligible_5, max_cows_ineligible_6),
#          max_cows_ineligible_7 = ifelse(is.na(max_cows_ineligible_7) & C_7 == 1, max_cows_ineligible_6, max_cows_ineligible_7),
#          max_cows_ineligible_8 = ifelse(is.na(max_cows_ineligible_8) & C_8 == 1, max_cows_ineligible_7, max_cows_ineligible_8)
#   )

# demographic vars
DEM <- read.csv(here::here("data/DEM.csv"), colClasses = c(PATID = "character"), na.strings = "") |>
  mutate(is_hispanic = case_when(DEHISPNC == 1 ~ 1,
                                 DEHISPNC == 0 ~ 0,
                                 DEHISPNC == 97 ~ as.numeric(NA),
                                 TRUE ~ as.numeric(NA)),
         DERACE_missing = case_when(DERACEDK == 1 | DERACERF == 1 ~ 1, 
                                    TRUE ~ 0),
         DEWHITE = case_when(DEWHITE == 1 ~ 1,
                             DERACE_missing == 1 ~ as.numeric(NA),
                             TRUE ~ 0),
         DEBLACK = case_when(DEBLACK == 1 ~ 1,
                             DERACE_missing == 1 ~ as.numeric(NA),
                             TRUE ~ 0),
         DEAMEIND = case_when(DEAMEIND == 1 ~ 1,
                              DERACE_missing == 1 ~ as.numeric(NA),
                              TRUE ~ 0),
         DEAAPI = case_when(DEHAWAII == 1 | DEASIAN == 1 ~ 1,
                            DERACE_missing == 1 ~ as.numeric(NA),
                            TRUE ~ 0)
  ) |>
  select(PATID, age, DESEX, is_hispanic, DEWHITE, DEBLACK, DEAMEIND, DEAAPI, DERACE_missing)

# mental health vars
MHX <- read.csv(here::here("data/MHX.csv"), colClasses = c(PATID = "character"), na.strings = "") |>
  select(PATID, MHANXH, MHBPLRH, MHMDDH) |>
  rename("anxiety" = "MHANXH",
         "bipolar" = "MHBPLRH",
         "depression" = "MHMDDH")

# substance use vars
DSM <- read.csv(here::here("data/DSM.csv"), colClasses = c(PATID = "character"), na.strings = "") |>
  mutate(alcohol_use_disorder = ifelse(DSALCSCO < 4, 1, 0),
         amphetamine_use_disorder = ifelse(DSAMPSCO < 4, 1, 0),
         cannabis_use_disorder = ifelse(DSTHCSCO < 4, 1, 0),
         cocaine_use_disorder = ifelse(DSCOCSCO < 4, 1, 0),
         sedative_use_disorder = ifelse(DSSEDSCO < 4, 1, 0)
  ) |>
  select(PATID, ends_with("disorder"))

ASU <- read.csv(here::here("data/ASU.csv"), colClasses = c(PATID = "character"), na.strings = "") |>
  mutate(age_first_opioid_use = case_when(AUPNKAGE < AUHERAGE & is.na(AUPNKAGE) == FALSE & is.na(AUHERAGE) == FALSE ~ AUPNKAGE, 
                                          AUPNKAGE >= AUHERAGE & is.na(AUPNKAGE) == FALSE & is.na(AUHERAGE) == FALSE ~ AUHERAGE, 
                                          is.na(AUPNKAGE) == FALSE & is.na(AUHERAGE) == TRUE ~ AUPNKAGE,
                                          is.na(AUPNKAGE) == TRUE & is.na(AUHERAGE) == FALSE ~ AUHERAGE,
                                          TRUE ~ as.numeric(NA)),
         injection_opioid_use = case_when(AUPNKRTE == 4 | AUPNKRTE == 5 | AUHERRTE == 4 | AUHERRTE == 5 ~ 1, #IV or non-IV injcection
                                          (AUPNKLFT == 1 & is.na(AUPNKRTE)) | ((AUHERLFT == 1 | is.na(AUHERLFT)) & is.na(AUHERRTE)) ~ as.numeric(NA),
                                          TRUE ~ 0)) |>
  select(PATID, age_first_opioid_use, injection_opioid_use)

process_column <- function(text) {
  text <- iconv(text, from = "latin1", to = "UTF-8", sub = "byte")
  text <- tolower(text)
  split_values <- unlist(strsplit(text, ",\\s*"))
  unique_values <- unique(split_values)
  return(unique_values)
}

# finding all types of pain killers
unique_values_vector <- process_column(ASU$AUPNKLSP)

# non-opioid pain killers - ativan, tylenol, valium -- people with these had opioids prescribed anyways

Mode <- function(x, na.rm = TRUE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# joining data and imputing missing values
dat <- dat |>
  left_join(DEM) |>
  left_join(MHX) |>
  left_join(DSM) |>
  left_join(ASU) |>
  mutate(DESEX = ifelse(DESEX == 2, 1, 0),
         years_since_first_opioid_use = case_when(is.na(age_first_opioid_use) == FALSE ~ age - age_first_opioid_use,
                                                  TRUE ~ as.numeric(NA)))
dat <- dat |>  
  ungroup() |>
  # mutate(DEBLACK = ifelse(is.na(DEBLACK), Mode(DEBLACK), DEBLACK),
  #        DEWHITE = ifelse(is.na(DEWHITE), Mode(DEWHITE), DEWHITE),
  #        DEAMEIND = ifelse(is.na(DEAMEIND), Mode(DEAMEIND), DEAMEIND),
  #        DEAAPI = ifelse(is.na(DEAAPI), Mode(DEAAPI), DEAAPI),
  #        is_hispanic_missing = ifelse(is.na(is_hispanic), 1, 0),
  #        is_hispanic = ifelse(is_hispanic_missing == 1, Mode(is_hispanic), is_hispanic),
  #        alcohol_use_disorder_missing = ifelse(is.na(alcohol_use_disorder), 1, 0),
  #        alcohol_use_disorder = ifelse(alcohol_use_disorder_missing == 1, Mode(alcohol_use_disorder), alcohol_use_disorder),
  #        amphetamine_use_disorder_missing = ifelse(is.na(amphetamine_use_disorder), 1, 0),
  #        amphetamine_use_disorder = ifelse(amphetamine_use_disorder_missing == 1, Mode(amphetamine_use_disorder), amphetamine_use_disorder),
  #        cannabis_use_disorder_missing = ifelse(is.na(cannabis_use_disorder), 1, 0),
#        cannabis_use_disorder = ifelse(cannabis_use_disorder_missing == 1, Mode(cannabis_use_disorder), cannabis_use_disorder),
#        cocaine_use_disorder_missing = ifelse(is.na(cocaine_use_disorder), 1, 0),
#        cocaine_use_disorder = ifelse(cannabis_use_disorder_missing == 1, Mode(cocaine_use_disorder), cocaine_use_disorder),
#        sedative_use_disorder_missing = ifelse(is.na(sedative_use_disorder), 1, 0),
#        sedative_use_disorder = ifelse(cannabis_use_disorder_missing == 1, Mode(sedative_use_disorder), sedative_use_disorder),
#        anxiety_missing = ifelse(is.na(anxiety), 1, 0),
#        anxiety = ifelse(anxiety_missing == 1, Mode(anxiety), anxiety),
#        bipolar_missing = ifelse(is.na(bipolar), 1, 0),
#        bipolar = ifelse(bipolar_missing == 1, Mode(bipolar), bipolar),
#        depression_missing = ifelse(is.na(depression), 1, 0),
#        depression = ifelse(depression_missing == 1, Mode(depression), depression),
#        years_since_first_opioid_use_missing = ifelse(is.na(years_since_first_opioid_use), 1, 0),
#        years_since_first_opioid_use = ifelse(years_since_first_opioid_use_missing == 1, median(years_since_first_opioid_use, na.rm = TRUE), years_since_first_opioid_use),
#        injection_opioid_use_missing = ifelse(is.na(injection_opioid_use), 1, 0),
#        injection_opioid_use = ifelse(injection_opioid_use_missing == 1, Mode(injection_opioid_use), injection_opioid_use)) |>
select(-age_first_opioid_use,
       -starts_with("max_cows_time"))

# removing any variables coming after baseline
dat_remove_max <- dat |>
  select(-c("max_cows_2",
            "max_cows_3",
            "max_cows_4",
            "max_cows_5",
            "max_cows_6",
            "max_cows_7",
            "max_cows_8",
            "max_cows_9",
            "max_cows_10",
            "max_cows_11",
            "max_cows_12",
            "max_cows_13",
            "max_cows_14",
            "max_cows_missing_indicator_1", "max_cows_missing_indicator_2", "max_cows_missing_indicator_3", "max_cows_missing_indicator_4",
            "max_cows_missing_indicator_5", "max_cows_missing_indicator_6", "max_cows_missing_indicator_7", "max_cows_missing_indicator_8",
            "max_cows_missing_indicator_9", "max_cows_missing_indicator_10", "max_cows_missing_indicator_11", "max_cows_missing_indicator_12",
            "max_cows_missing_indicator_13", "max_cows_missing_indicator_14",
            "max_cows_ineligible_1", "max_cows_ineligible_2", "max_cows_ineligible_3", "max_cows_ineligible_4", "max_cows_ineligible_5",
            "max_cows_ineligible_6", "max_cows_ineligible_7", "max_cows_ineligible_8", "max_cows_ineligible_9", "max_cows_ineligible_10",
            "max_cows_ineligible_11", "max_cows_ineligible_12", "max_cows_ineligible_13", "max_cows_ineligible_14", 
            "adj_1", "adj_2", "adj_3", "adj_4", "adj_5", "adj_6", "adj_7", "adj_8", "adj_9", "adj_10", "adj_11", "adj_12", "adj_13", "adj_14",
            "L1_1", "L1_2", "L1_3", "L1_4", "L1_5", "L1_6", "L1_7", "L1_8", "L1_9", "L1_10", "L1_11", "L1_12", "L1_13", "L1_14", 
            "L2_1", "L2_2", "L2_3", "L2_4", "L2_5", "L2_6", "L2_7", "L2_8", "L2_9", "L2_10", "L2_11", "L2_12", "L2_13", "L2_14",
            "C_1", "C_2", "C_3", "C_4", "C_5", "C_6", "C_7", "C_8", "C_9", "C_10", "C_11", "C_12", "C_13", "C_14",
            "Y_1", "Y_2", "Y_3", "Y_4",  "Y_5", "Y_6", "Y_7", "Y_8", "Y_9", "Y_10", "Y_11", "Y_12", "Y_13", "Y_14"))


set.seed(9)
dat_mice <- mice(dat_remove_max, 
                 m = 10,
                 maxit = 10,
                 exclude = c("PATID"), 
                 method = "cart", 
                 seed = 9)

# getting all 10 datasets
completed_datasets <- lapply(1:dat_mice$m, function(i) complete(dat_mice, action = i))

completed_datasets <- lapply(completed_datasets, function(dat_final) {
  dat_final |>
    left_join(dat |> select(PATID,
                            "max_cows_2",
                            "max_cows_3",
                            "max_cows_4",
                            "max_cows_5",
                            "max_cows_6",
                            "max_cows_7",
                            "max_cows_8",
                            "max_cows_9",
                            "max_cows_10",
                            "max_cows_11",
                            "max_cows_12",
                            "max_cows_13",
                            "max_cows_14",
                            "max_cows_missing_indicator_1", "max_cows_missing_indicator_2", "max_cows_missing_indicator_3", "max_cows_missing_indicator_4",
                            "max_cows_missing_indicator_5", "max_cows_missing_indicator_6", "max_cows_missing_indicator_7", "max_cows_missing_indicator_8",
                            "max_cows_missing_indicator_9", "max_cows_missing_indicator_10", "max_cows_missing_indicator_11", "max_cows_missing_indicator_12",
                            "max_cows_missing_indicator_13", "max_cows_missing_indicator_14",
                            "max_cows_ineligible_1", "max_cows_ineligible_2", "max_cows_ineligible_3", "max_cows_ineligible_4", "max_cows_ineligible_5",
                            "max_cows_ineligible_6", "max_cows_ineligible_7", "max_cows_ineligible_8", "max_cows_ineligible_9", "max_cows_ineligible_10",
                            "max_cows_ineligible_11", "max_cows_ineligible_12", "max_cows_ineligible_13", "max_cows_ineligible_14", 
                            "adj_1", "adj_2", "adj_3", "adj_4", "adj_5", "adj_6", "adj_7", "adj_8", "adj_9", "adj_10", "adj_11", "adj_12", "adj_13", "adj_14",
                            "L1_1", "L1_2", "L1_3", "L1_4", "L1_5", "L1_6", "L1_7", "L1_8", "L1_9", "L1_10", "L1_11", "L1_12", "L1_13", "L1_14", 
                            "L2_1", "L2_2", "L2_3", "L2_4", "L2_5", "L2_6", "L2_7", "L2_8", "L2_9", "L2_10", "L2_11", "L2_12", "L2_13", "L2_14",
                            "C_1", "C_2", "C_3", "C_4", "C_5", "C_6", "C_7", "C_8", "C_9", "C_10", "C_11", "C_12", "C_13", "C_14",
                            "Y_1", "Y_2", "Y_3", "Y_4",  "Y_5", "Y_6", "Y_7", "Y_8", "Y_9", "Y_10", "Y_11", "Y_12", "Y_13", "Y_14")) |>
    select(PATID, PROTSEG, received_naltrexone_injection, age, DESEX, is_hispanic,
           DEWHITE, DEBLACK, DEAMEIND, DEAAPI, DERACE_missing, anxiety, bipolar, 
           depression, alcohol_use_disorder, amphetamine_use_disorder, cannabis_use_disorder,
           cocaine_use_disorder, sedative_use_disorder, injection_opioid_use,
           years_since_first_opioid_use, #is_hispanic_missing, alcohol_use_disorder_missing,
           #amphetamine_use_disorder_missing, cannabis_use_disorder_missing, 
           #cocaine_use_disorder_missing, sedative_use_disorder_missing, anxiety_missing,
           #bipolar_missing, depression_missing, years_since_first_opioid_use_missing,
           #injection_opioid_use_missing,
           max_cows_1, max_cows_missing_indicator_1, max_cows_ineligible_1, L1_1, L2_1, adj_1, C_1, Y_1,
           max_cows_2, max_cows_missing_indicator_2, max_cows_ineligible_2, L1_2, L2_2, adj_2, C_2, Y_2,
           max_cows_3, max_cows_missing_indicator_3, max_cows_ineligible_3, L1_3, L2_3, adj_3, C_3, Y_3,
           max_cows_4, max_cows_missing_indicator_4, max_cows_ineligible_4, L1_4, L2_4, adj_4, C_4, Y_4,
           max_cows_5, max_cows_missing_indicator_5, max_cows_ineligible_5, L1_5, L2_5, adj_5, C_5, Y_5,
           max_cows_6, max_cows_missing_indicator_6, max_cows_ineligible_6, L1_6, L2_6, adj_6, C_6, Y_6,
           max_cows_7, max_cows_missing_indicator_7, max_cows_ineligible_7, L1_7, L2_7, adj_7, C_7, Y_7,
           max_cows_8, max_cows_missing_indicator_8, max_cows_ineligible_8, L1_8, L2_8, adj_8, C_8, Y_8,
           max_cows_9, max_cows_missing_indicator_9, max_cows_ineligible_9, L1_9, L2_9, adj_9, C_9, Y_9,
           max_cows_10, max_cows_missing_indicator_10, max_cows_ineligible_10, L1_10, L2_10, adj_10, C_10, Y_10,
           max_cows_11, max_cows_missing_indicator_11, max_cows_ineligible_11, L1_11, L2_11, adj_11, C_11, Y_11,
           max_cows_12, max_cows_missing_indicator_12, max_cows_ineligible_12, L1_12, L2_12, adj_12, C_12, Y_12,
           max_cows_13, max_cows_missing_indicator_13, max_cows_ineligible_13, L1_13, L2_13, adj_13, C_13, Y_13,
           max_cows_14, max_cows_missing_indicator_14, max_cows_ineligible_14, L1_14, L2_14, adj_14, C_14, Y_14,
           C_6, Y_6,
           C_7, Y_7,
           C_8, Y_8,
           C_9, Y_9,
           C_10, Y_10,
           C_11, Y_11,
           C_12, Y_12,
           C_13, Y_13,
           C_14, Y_14)
})
#|>
#   mutate(max_cows_2 = ifelse(is.na(max_cows_2) & C_1 == 1, max_cows_1, max_cows_2),
#          max_cows_3 = ifelse(is.na(max_cows_3) & C_2 == 1, max_cows_2, max_cows_3),
#          max_cows_4 = ifelse(is.na(max_cows_4) & C_3== 1, max_cows_3, max_cows_4),
#          max_cows_5 = ifelse(is.na(max_cows_5) & C_4 == 1, max_cows_4, max_cows_5),
#          max_cows_6 = ifelse(is.na(max_cows_6) & C_5 == 1, max_cows_5, max_cows_6),
#          max_cows_7 = ifelse(is.na(max_cows_7) & C_6 == 1, max_cows_6, max_cows_7),
#          max_cows_8 = ifelse(is.na(max_cows_8) & C_7 == 1, max_cows_7, max_cows_8),
#          max_cows_9 = ifelse(is.na(max_cows_9) & C_8 == 1, max_cows_8, max_cows_9),
#          max_cows_10 = ifelse(is.na(max_cows_10) & C_9 == 1, max_cows_9, max_cows_10),
#          max_cows_11 = ifelse(is.na(max_cows_11) & C_10 == 1, max_cows_10, max_cows_11),
#          max_cows_12 = ifelse(is.na(max_cows_12) & C_11 == 1, max_cows_11, max_cows_12),
#          max_cows_13 = ifelse(is.na(max_cows_13) & C_12 == 1, max_cows_12, max_cows_13),
#          max_cows_14 = ifelse(is.na(max_cows_14) & C_13 == 1, max_cows_13, max_cows_14),
#          max_cows_ineligible_2 = ifelse(is.na(max_cows_ineligible_2) & C_1 == 1, max_cows_ineligible_1, max_cows_ineligible_2),
#          max_cows_ineligible_3 = ifelse(is.na(max_cows_ineligible_3) & C_2 == 1, max_cows_ineligible_2, max_cows_ineligible_3),
#          max_cows_ineligible_4 = ifelse(is.na(max_cows_ineligible_4) & C_3 == 1, max_cows_ineligible_3, max_cows_ineligible_4),
#          max_cows_ineligible_5 = ifelse(is.na(max_cows_ineligible_5) & C_4 == 1, max_cows_ineligible_4, max_cows_ineligible_5),
#          max_cows_ineligible_6 = ifelse(is.na(max_cows_ineligible_6) & C_5 == 1, max_cows_ineligible_5, max_cows_ineligible_6),
#          max_cows_ineligible_7 = ifelse(is.na(max_cows_ineligible_7) & C_6 == 1, max_cows_ineligible_6, max_cows_ineligible_7),
#          max_cows_ineligible_8 = ifelse(is.na(max_cows_ineligible_8) & C_7 == 1, max_cows_ineligible_7, max_cows_ineligible_8),
#          max_cows_ineligible_9 = ifelse(is.na(max_cows_ineligible_9) & C_8 == 1, max_cows_ineligible_8, max_cows_ineligible_9),
#          max_cows_ineligible_10 = ifelse(is.na(max_cows_ineligible_10) & C_9 == 1, max_cows_ineligible_9, max_cows_ineligible_10),
#          max_cows_ineligible_11 = ifelse(is.na(max_cows_ineligible_11) & C_10 == 1, max_cows_ineligible_10, max_cows_ineligible_11),
#          max_cows_ineligible_12 = ifelse(is.na(max_cows_ineligible_12) & C_11 == 1, max_cows_ineligible_11, max_cows_ineligible_12),
#          max_cows_ineligible_13 = ifelse(is.na(max_cows_ineligible_13) & C_12 == 1, max_cows_ineligible_12, max_cows_ineligible_13),
#          max_cows_ineligible_14 = ifelse(is.na(max_cows_ineligible_14) & C_13 == 1, max_cows_ineligible_13, max_cows_ineligible_14))
# })

# once experienced competing risk, all subsequent values should be missing (except cr/outcome which will always be 1/0 - this is manually changed later)
cols_to_modify <- c("C_1", "C_2", "C_3", "C_4", "C_5", "C_6", "C_7",
                    "C_8", "C_9", "C_10", "C_11", "C_12", "C_13", "C_14")
for (i in 1:10)
{
  for (col in cols_to_modify) {
    cols_after <- which(names(completed_datasets[[i]]) == col):ncol(completed_datasets[[i]])
    cols_after <- cols_after[-1]
    completed_datasets[[i]] <- completed_datasets[[i]] |>
      mutate(across(all_of(cols_after), ~ if_else(completed_datasets[[i]][[col]] == 0, NA, .))) |>
      mutate(across(starts_with("C_"), ~replace_na(.x, 0))) |>
      mutate(across(starts_with("Y_"), ~replace_na(.x, 0)))
  }
}

# once experienced outcome, all subsequent values should be missing (except cr/outcome which will always be 0/1 - this is manually changed later)
cols_to_modify <- c("Y_1", "Y_2", "Y_3", "Y_4", "Y_5", "Y_6", "Y_7",
                    "Y_8", "Y_9", "Y_10", "Y_11", "Y_12", "Y_13", "Y_14")
for (i in 1:10)
{
  for (col in cols_to_modify) {
    cols_after <- which(names(completed_datasets[[i]]) == col):ncol(completed_datasets[[i]])
    cols_after <- cols_after[-1]
    completed_datasets[[i]] <- completed_datasets[[i]] |>
      mutate(across(all_of(cols_after), ~ if_else(completed_datasets[[i]][[col]] == 1, NA, .))) |>
      mutate(across(starts_with("C_"), ~replace_na(.x, 1))) |>
      mutate(across(starts_with("Y_"), ~replace_na(.x, 1)))
  }
}

completed_datasets <- lapply(completed_datasets, function(data){
  data |>
    mutate(C_1 = ifelse(is.na(C_1), 0, C_1),
           C_2 = ifelse(is.na(C_2), 0, C_2),
           C_3 = ifelse(is.na(C_3), 0, C_3),
           C_4 = ifelse(is.na(C_4), 0, C_4),
           C_5 = ifelse(is.na(C_5), 0, C_5),
           C_6 = ifelse(is.na(C_6), 0, C_6),
           C_7 = ifelse(is.na(C_7), 0, C_7),
           C_8 = ifelse(is.na(C_8), 0, C_8),
           C_9 = ifelse(is.na(C_9), 0, C_9),
           C_10 = ifelse(is.na(C_10), 0, C_10),
           C_11 = ifelse(is.na(C_11), 0, C_11),
           C_12 = ifelse(is.na(C_12), 0, C_12),
           C_13 = ifelse(is.na(C_13), 0, C_13),
           C_14 = ifelse(is.na(C_14), 0, C_14)) |>
    mutate(C_1 = ifelse(is.na(C_1), 0, C_1),
           C_2 = ifelse(is.na(C_2), 0, C_2),
           C_3 = ifelse(is.na(C_3), 0, C_3),
           C_4 = ifelse(is.na(C_4), 0, C_4),
           C_5 = ifelse(is.na(C_5), 0, C_5),
           C_6 = ifelse(is.na(C_6), 0, C_6),
           C_7 = ifelse(is.na(C_7), 0, C_7),
           C_8 = ifelse(is.na(C_8), 0, C_8),
           C_9 = ifelse(is.na(C_9), 0, C_9),
           C_10 = ifelse(is.na(C_10), 0, C_10),
           C_11 = ifelse(is.na(C_11), 0, C_11),
           C_12 = ifelse(is.na(C_12), 0, C_12),
           C_13 = ifelse(is.na(C_13), 0, C_13),
           C_14 = ifelse(is.na(C_14), 0, C_14)) |>
    mutate(adj_2 = ifelse(C_1 == 0, NA, adj_2),
           max_cows_2 = ifelse(C_1 == 0, NA, max_cows_2),
           L1_2 = ifelse(C_1 == 0, NA, L1_2),
           L2_2 = ifelse(C_1 == 0, NA, L2_2),
           adj_3 = ifelse(C_2 == 0, NA, adj_3),
           max_cows_3 = ifelse(C_2 == 0, NA, max_cows_3),
           L1_3 = ifelse(C_2 == 0, NA, L1_3),
           L2_3 = ifelse(C_2 == 0, NA, L2_3),
           adj_4 = ifelse(C_3 == 0, NA, adj_4),
           max_cows_4 = ifelse(C_3 == 0, NA, max_cows_4),
           L1_4 = ifelse(C_3 == 0, NA, L1_4),
           L2_4 = ifelse(C_3 == 0, NA, L2_4),
           adj_5 = ifelse(C_4 == 0, NA, adj_5),
           max_cows_5 = ifelse(C_4 == 0, NA, max_cows_5),
           L1_5 = ifelse(C_4 == 0, NA, L1_5),
           L2_5 = ifelse(C_4 == 0, NA, L2_5)
    ) |>
    mutate(max_cows_2 = ifelse(is.na(max_cows_2) & C_1 == 1 & Y_1 == 0, max_cows_1, max_cows_2),
           max_cows_3 = ifelse(is.na(max_cows_3) & C_2 == 1 & Y_2 == 0, max_cows_2, max_cows_3),
           max_cows_4 = ifelse(is.na(max_cows_4) & C_3 == 1 & Y_3 == 0, max_cows_3, max_cows_4),
           max_cows_5 = ifelse(is.na(max_cows_5) & C_4 == 1 & Y_4 == 0, max_cows_4, max_cows_5),
           max_cows_ineligible_1 = 0,
           max_cows_ineligible_2 = ifelse(is.na(max_cows_ineligible_2) & C_1 == 1 & Y_1 == 0, max_cows_ineligible_1, max_cows_ineligible_2),
           max_cows_ineligible_3 = ifelse(is.na(max_cows_ineligible_3) & C_2 == 1 & Y_2 == 0, max_cows_ineligible_2, max_cows_ineligible_3),
           max_cows_ineligible_4 = ifelse(is.na(max_cows_ineligible_4) & C_3 == 1 & Y_3 == 0, max_cows_ineligible_3, max_cows_ineligible_4),
           max_cows_ineligible_5 = ifelse(is.na(max_cows_ineligible_5) & C_4 == 1 & Y_4 == 0, max_cows_ineligible_4, max_cows_ineligible_5)
    )
})

saveRDS(completed_datasets, here::here("data/analysis_data/completed_datasets_alt.rds"))