#remotes::install_github("nt-williams/lmtp@curve")
#remotes::install_github("mlr-org/mlr3extralearners@*release")
library(tidyverse)
library(lmtp)
library(mlr3extralearners)
library(xgboost)
library(earth)

dat <- readRDS(here::here("data/analysis_data/analysis_data.rds"))

dat <- dat |>
    mutate(across(starts_with("C_"), ~ ifelse(. == 0, 1, ifelse(. == 1, 0, .)))) # alternating to match competing risks format

W <- c("days_from_admission_to_consent",
  # demographics
  "DESEX",
  "age",
  "is_hispanic",
  #"is_hispanic_missing",
  "DEWHITE",
  "DEBLACK",
  #"DEAMEIND",
  #"DEAAPI",
  "DEOTHER",
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
          c("adj_5")
)


L <- list(c("max_cows_1", 
            "max_cows_ineligible_1", 
            "max_cows_missing_indicator_1",
            #"L1_1", #buprenorphine
            "L3_1"), #benzo
          c("max_cows_2", 
            "max_cows_ineligible_2", 
            "max_cows_missing_indicator_2",
            #"L1_2", #buprenorphine
            "L3_2"), #benzo
          c("max_cows_3", 
            "max_cows_ineligible_3", 
            "max_cows_missing_indicator_3",
            #"L1_3", #buprenorphine
            "L3_3"), #benzo
          c("max_cows_4", 
            "max_cows_ineligible_4", 
            "max_cows_missing_indicator_4",
            #"L1_4", #buprenorphine
            "L3_4"), #benzo
          c("max_cows_5", 
            "max_cows_ineligible_5", 
            "max_cows_missing_indicator_5",
            #"L1_5", #buprenorphine
            "L3_5") #benzo
)

dat_shifted_5 <- dat |>
    mutate(adj_1 = ifelse(max_cows_1 >= 5 & max_cows_ineligible_1 == 0 & max_cows_missing_indicator_1 == 0
                          , 1, 0),
           adj_2 = ifelse(max_cows_2 >= 5 & max_cows_ineligible_2 == 0 & max_cows_missing_indicator_2 == 0
                          , 1, 0),
           adj_3 = ifelse(max_cows_3 >= 5 & max_cows_ineligible_3 == 0 & max_cows_missing_indicator_3 == 0
                          , 1, 0),
           adj_4 = ifelse(max_cows_4 >= 5 & max_cows_ineligible_4 == 0 & max_cows_missing_indicator_4 == 0
                          , 1, 0),
           adj_5 = ifelse(max_cows_5 >= 5 & max_cows_ineligible_4 == 0 & max_cows_missing_indicator_5 == 0
                          , 1, 0))

dat_shifted_3 <- dat |>
    mutate(adj_1 = ifelse(max_cows_1 >= 3 & max_cows_ineligible_1 == 0 & max_cows_missing_indicator_1 == 0
                          , 1, 0),
           adj_2 = ifelse(max_cows_2 >= 3 & max_cows_ineligible_2 == 0 & max_cows_missing_indicator_2 == 0
                          , 1, 0),
           adj_3 = ifelse(max_cows_3 >= 3 & max_cows_ineligible_3 == 0 & max_cows_missing_indicator_3 == 0
                          , 1, 0),
           adj_4 = ifelse(max_cows_4 >= 3 & max_cows_ineligible_4 == 0 & max_cows_missing_indicator_4 == 0
                          , 1, 0),
           adj_5 = ifelse(max_cows_5 >= 3 & max_cows_ineligible_4 == 0 & max_cows_missing_indicator_5 == 0
                          , 1, 0))

dat_shifted_always <- dat  |>
    mutate(adj_1 = 1,
           adj_2 = ifelse(C_1 == 0, 1, NA),
           adj_3 = ifelse(C_2 == 0, 1, NA),
           adj_4 = ifelse(C_3 == 0, 1, NA),
           adj_5 = ifelse(C_4 == 0, 1, NA))

learners <- list("mean", "glm", 
                 "earth",
                 "xgboost",
                 list("xgboost",
                      min_child_weight = 5,
                      id = "xgboost1"),
                 "ranger",
                 list("ranger",
                      num.trees = 1000,
                      id = "ranger1")
                 )

# function for running lmtp
run_lmtp <-  function(data, day = 5, shift = NULL, learners = learners, folds = 20)
{
  if (day <= 5) {
    outcome_nodes <- 1:day
  } else {
    outcome_nodes <- c(1, 2, 3, 4, day)
  }
  
  result <- lmtp_sdr(
    data = data, 
    trt = A,
    compete = paste0("C_", outcome_nodes),
    time_vary = L,
    baseline = W,
    outcome = paste0("Y_", outcome_nodes), 
    shifted = shift, 
    outcome_type = "survival",
    learners_outcome = learners,
    learners_trt = learners,
    folds = folds, 
    control = lmtp_control(.learners_outcome_folds = 10,
                           .learners_trt_folds = 10,
                           .trim = 0.95), # look at trim
    mtp = FALSE,
    id = NULL)
  
  result
}

set.seed(9)
for (i in 14:5)
  {
    
    # set.seed(9)
    # results_shift_obs <- run_lmtp(data = completed_datasets[[j]],
    #                             day = i,
    #                             shift = NULL,
    #                             learners = learners,
    #                             folds = 20
    # )
    # 
    # saveRDS(results_shift_obs, here::here(paste0("v/results_obs_day_", i, ".rds")))
    # 
set.seed(9)
results_shift_5 <- run_lmtp(data = dat,
                            day = i,
                            shift = dat_shifted_5,
                            learners = learners,
                            folds = 20
)

saveRDS(results_shift_5, here::here(paste0("results_final/results_shift_5_day_", i, ".rds")))

# set.seed(9)
# results_shift_3 <- run_lmtp(data = dat,
#                             day = i,
#                             shift = dat_shifted_3,
#                             learners = learners,
#                             folds = 20
# )
# 
# saveRDS(results_shift_3, here::here(paste0("results_final/results_shift_3_day_", i, ".rds")))

    # set.seed(9)
    # results_shift_always <- run_lmtp(data = dat,
    #                             day = i,
    #                             shift = dat_shifted_always,
    #                             learners = learners,
    #                             folds = 20
    # )
    # saveRDS(results_shift_always, here::here(paste0("results_final/results_shift_always_day_", i,".rds")))
  }
