library(tidyverse)
library(lmtp)
library(gtsummary)

dat_final <- readRDS(here::here("data/analysis_data/dat_final_analysis_wide_no_carry.rds")) |>
  mutate(across(starts_with("C_"), ~ ifelse(. == 0, 1, ifelse(. == 1, 0, .))))


W <- c(# demographics
  "DESEX",
  "age",
  "is_hispanic",
  #"is_hispanic_missing",
  "DEWHITE",
  "DEBLACK",
  "DEAMEIND",
  "DEAAPI",
  #"DERACE_missing",
  #"PROTSEG",
  # substance use
  "alcohol_use_disorder", #missing
  #"alcohol_use_disorder_missing",
  "amphetamine_use_disorder", #missing
  #"amphetamine_use_disorder_missing",
  "cannabis_use_disorder", #missing
  #"cannabis_use_disorder_missing",
  "cocaine_use_disorder", #missing
  #"cocaine_use_disorder_missing",
  "sedative_use_disorder", #missing
  #"sedative_use_disorder_missing",
  "injection_opioid_use",
  #"injection_opioid_use_missing",
  "years_since_first_opioid_use",
  #"years_since_first_opioid_use_missing",
  # mental health
  "anxiety", #missing
  #"anxiety_missing",
  "bipolar", #missing,
  #"bipolar_missing",
  "depression"#, #missing
  #"depression_missing"
)

A <- list(c("adj_1"),
          c("adj_2"),
          c("adj_3"),
          c("adj_4"),
          c("adj_5")#,
          #c("adj_6"),
          #c("adj_7"),
          #c("adj_8")
)


L <- list(c("max_cows_1", #"max_cows_ineligible_1", 
            "L1_1", "L2_1"), 
           c("max_cows_2", #"max_cows_ineligible_2", 
             "L1_2", "L2_2"), 
           c("max_cows_3", "max_cows_ineligible_3", 
             "L1_3", "L2_3"), 
           c("max_cows_4", "max_cows_ineligible_4", 
             "L1_4", "L2_4"), 
           c("max_cows_5", "max_cows_ineligible_5", 
             "L1_5", "L2_5")#,
           # c("max_cows_6", #"max_cows_ineligible_6", 
           #   "L1_6", "L2_6"),
           # c("max_cows_7", #"max_cows_ineligible_7", 
           #   "L1_7", "L2_7"),
           # c("max_cows_8", #"max_cows_ineligible_8", 
           #   "L1_8", "L2_8")
)

dat_shifted_8 <- dat_final |>
  mutate(adj_1 = ifelse(max_cows_1 >= 8 & L2_1 == 0 & max_cows_ineligible_1 == 0, 1, 0),
         adj_2 = ifelse(max_cows_2 >= 8 & L2_2 == 0 & max_cows_ineligible_2 == 0, 1, 0),
         adj_3 = ifelse(max_cows_3 >= 8 & L2_3 == 0 & max_cows_ineligible_3 == 0, 1, 0),
         adj_4 = ifelse(max_cows_4 >= 8 & L2_4 == 0 & max_cows_ineligible_4 == 0, 1, 0),
         adj_5 = ifelse(max_cows_5 >= 8 & L2_5 == 0 & max_cows_ineligible_5 == 0, 1, 0),
         adj_6 = ifelse(max_cows_6 >= 8 & L2_6 == 0 & max_cows_ineligible_6 == 0, 1, 0),
         adj_7 = ifelse(max_cows_7 >= 8 & L2_7 == 0 & max_cows_ineligible_7 == 0, 1, 0),
         adj_8 = ifelse(max_cows_8 >= 8 & L2_8 == 0 & max_cows_ineligible_8 == 0, 1, 0))

dat_shifted_5 <- dat_final |>
  mutate(adj_1 = ifelse(max_cows_1 >= 5 & L2_1 == 0 & max_cows_ineligible_1 == 0, 1, 0),
         adj_2 = ifelse(max_cows_2 >= 5 & L2_2 == 0 & max_cows_ineligible_2 == 0, 1, 0),
         adj_3 = ifelse(max_cows_3 >= 5 & L2_3 == 0 & max_cows_ineligible_3 == 0, 1, 0),
         adj_4 = ifelse(max_cows_4 >= 5 & L2_4 == 0 & max_cows_ineligible_4 == 0, 1, 0),
         adj_5 = ifelse(max_cows_5 >= 5 & L2_5 == 0 & max_cows_ineligible_5 == 0, 1, 0),
         adj_6 = ifelse(max_cows_6 >= 5 & L2_6 == 0 & max_cows_ineligible_6 == 0, 1, 0),
         adj_7 = ifelse(max_cows_7 >= 5 & L2_7 == 0 & max_cows_ineligible_7 == 0, 1, 0),
         adj_8 = ifelse(max_cows_8 >= 5 & L2_8 == 0 & max_cows_ineligible_8 == 0, 1, 0)) 

dat_shifted_3 <- dat_final |>
  mutate(adj_1 = ifelse(max_cows_1 >= 3 & L2_1 == 0 & max_cows_ineligible_1 == 0, 1, 0),
         adj_2 = ifelse(max_cows_2 >= 3 & L2_2 == 0 & max_cows_ineligible_2 == 0, 1, 0),
         adj_3 = ifelse(max_cows_3 >= 3 & L2_3 == 0 & max_cows_ineligible_3 == 0, 1, 0),
         adj_4 = ifelse(max_cows_4 >= 3 & L2_4 == 0 & max_cows_ineligible_4 == 0, 1, 0),
         adj_5 = ifelse(max_cows_5 >= 3 & L2_5 == 0 & max_cows_ineligible_5 == 0, 1, 0),
         adj_6 = ifelse(max_cows_6 >= 3 & L2_6 == 0 & max_cows_ineligible_6 == 0, 1, 0),
         adj_7 = ifelse(max_cows_7 >= 3 & L2_7 == 0 & max_cows_ineligible_7 == 0, 1, 0),
         adj_8 = ifelse(max_cows_8 >= 3 & L2_8 == 0 & max_cows_ineligible_8 == 0, 1, 0))

dat_shifted_always <- dat_final |>
  mutate(adj_1 = 1,
         adj_2 = 1,
         adj_3 = 1,
         adj_4 = 1,
         adj_5 = 1,
         adj_6 = 1,
         adj_7 = 1,
         adj_8 = 1)

dat_shifted_never <- dat_final |>
  mutate(adj_1 = 0,
         adj_2 = 0,
         adj_3 = 0,
         adj_4 = 0,
         adj_5 = 0,
         adj_6 = 0,
         adj_7 = 0,
         adj_8 = 0)

learners <- c("SL.mean", "SL.glm", "SL.earth", "SL.xgboost")
learners <- c("SL.mean", "SL.glm")


# function for running lmtp
run_lmtp <-  function(data, day = 5, shift = NULL, learners = learners, folds = 2)
{
  if (day <= 5) {
    outcome_nodes <- 1:day
  } else {
    outcome_nodes <- c(1, 2, 3, 4, day)
  }
  
  result <- lmtp_sdr(
    data = data, 
    trt = A,
    competing_risk = paste0("C_", outcome_nodes),
    time_vary = L,
    baseline = W,
    outcome = paste0("Y_", outcome_nodes), 
    shifted = shift, 
    outcome_type = "survival",
    learners_outcome = learners,
    learners_trt = learners,
    folds = folds, 
    control = lmtp_control(.learners_outcome_folds = folds,
                           .learners_trt_folds = folds,
                           .trim = 0.95),
    mtp = FALSE,
    id = NULL)
  
  result
}

set.seed(9)

for (i in 5:14)
{
set.seed(9)

results_shift_8 <- run_lmtp(data = dat_final,
                                 day = i,
                                 shift = dat_shifted_8,
                                 learners = learners,
                                 folds = 5
)

saveRDS(results_shift_8, here::here(paste0("results/results_shift_8_day_", i, ".rds")))

set.seed(9)
results_shift_5 <- run_lmtp(data = dat_final,
                                 day = i,
                                 shift = dat_shifted_5,
                                 learners = learners,
                                 folds = 5
)

saveRDS(results_shift_5, here::here(paste0("results/results_shift_5_day_", i, ".rds")))

set.seed(9)
results_shift_3 <- run_lmtp(data = dat_final,
                            day = i,
                            shift = dat_shifted_3,
                            learners = learners,
                            folds = 5
)

saveRDS(results_shift_3, here::here(paste0("results/results_shift_3_day_", i, ".rds")))

set.seed(9)
results_shift_always <- run_lmtp(data = dat_final, 
                            day = i,
                            shift = dat_shifted_always,
                            learners = learners, 
                            folds = 5
)

saveRDS(results_shift_always, here::here(paste0("results/results_shift_always_day_", i, ".rds")))

set.seed(9)
results_shift_never <- run_lmtp(data = dat_final, 
                                 day = i,
                                 shift = dat_shifted_never,
                                 learners = learners, 
                                 folds = 5
)

saveRDS(results_shift_never, here::here(paste0("results/results_shift_never_day_", i, ".rds")))
}
