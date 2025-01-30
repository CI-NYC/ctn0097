#remotes::install_github("nt-williams/lmtp@curve")
#remotes::install_github("mlr-org/mlr3extralearners@*release")
library(tidyverse)
library(lmtp)
library(mlr3extralearners)
library(xgboost)
library(earth)

completed_datasets <- readRDS(here::here("data/analysis_data/completed_datasets_2_hours.rds")) 

completed_datasets <- lapply(completed_datasets, function(data) {
  data |>
  mutate(across(starts_with("C_"), ~ ifelse(. == 0, 1, ifelse(. == 1, 0, .)))) |> # alternating to match competing risks format
  mutate(DEOTHER = ifelse(DEAMEIND == 1 | DEAAPI == 1, 1, 0))
})

W <- c(# demographics
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
          c("adj_5")#,
          #c("adj_6"),
          #c("adj_7"),
          #c("adj_8")
)


L <- list(c("max_cows_1", #"max_cows_ineligible_1", 
            "L1_1", "L2_1"), 
           c("max_cows_2", #"max_cows_ineligible_2", 
             "L1_2", "L2_2"), 
           c("max_cows_3", #"max_cows_ineligible_3", 
             "L1_3", "L2_3"), 
           c("max_cows_4", #"max_cows_ineligible_4", 
             "L1_4", "L2_4"), 
           c("max_cows_5", #"max_cows_ineligible_5", 
             "L1_5", "L2_5")#,
           # c("max_cows_6", #"max_cows_ineligible_6", 
           #   "L1_6", "L2_6"),
           # c("max_cows_7", #"max_cows_ineligible_7", 
           #   "L1_7", "L2_7"),
           # c("max_cows_8", #"max_cows_ineligible_8", 
           #   "L1_8", "L2_8")
)

completed_datasets_8 <- lapply(completed_datasets, function(data){
data |>
  mutate(adj_1 = ifelse(max_cows_1 >= 8 & L2_1 == 0 & max_cows_ineligible_1 == 0
                        , 1, 0),
         adj_2 = ifelse(max_cows_2 >= 8 & L2_2 == 0 & max_cows_ineligible_2 == 0
                        , 1, 0),
         adj_3 = ifelse(max_cows_3 >= 8 & L2_3 == 0 & max_cows_ineligible_3 == 0
                        , 1, 0),
         adj_4 = ifelse(max_cows_4 >= 8 & L2_4 == 0 & max_cows_ineligible_4 == 0
                        , 1, 0),
         adj_5 = ifelse(max_cows_5 >= 8 & L2_4 == 0 & max_cows_ineligible_4 == 0
                 , 1, 0))
})

completed_datasets_5 <- lapply(completed_datasets, function(data){
  data |>
    mutate(adj_1 = ifelse(max_cows_1 >= 5 & L2_1 == 0 & max_cows_ineligible_1 == 0
                          , 1, 0),
           adj_2 = ifelse(max_cows_2 >= 5 & L2_2 == 0 & max_cows_ineligible_2 == 0
                          , 1, 0),
           adj_3 = ifelse(max_cows_3 >= 5 & L2_3 == 0 & max_cows_ineligible_3 == 0
                          , 1, 0),
           adj_4 = ifelse(max_cows_4 >= 5 & L2_4 == 0 & max_cows_ineligible_4 == 0
                          , 1, 0),
           adj_5 = ifelse(max_cows_5 >= 5 & L2_4 == 0 & max_cows_ineligible_4 == 0
                          , 1, 0))
})

completed_datasets_3 <- lapply(completed_datasets, function(data){
  data |>
    mutate(adj_1 = ifelse(max_cows_1 >= 3 & L2_1 == 0 & max_cows_ineligible_1 == 0
                          , 1, 0),
           adj_2 = ifelse(max_cows_2 >= 3 & L2_2 == 0 & max_cows_ineligible_2 == 0
                          , 1, 0),
           adj_3 = ifelse(max_cows_3 >= 3 & L2_3 == 0 & max_cows_ineligible_3 == 0
                          , 1, 0),
           adj_4 = ifelse(max_cows_4 >= 3 & L2_4 == 0 & max_cows_ineligible_4 == 0
                          , 1, 0),
           adj_5 = ifelse(max_cows_5 >= 3 & L2_4 == 0 & max_cows_ineligible_4 == 0
                          , 1, 0))
})

completed_datasets_always <- lapply(completed_datasets, function(data){
  data  |>
  mutate(adj_1 = 1,
         adj_2 = ifelse(C_1 == 0, 1, NA),
         adj_3 = ifelse(C_2 == 0, 1, NA),
         adj_4 = ifelse(C_3 == 0, 1, NA),
         adj_5 = ifelse(C_4 == 0, 1, NA))
})

completed_datasets_never <- lapply(completed_datasets, function(data){
  data  |>
    mutate(adj_1 = 0,
           adj_2 = ifelse(C_1 == 0, 0, NA),
           adj_3 = ifelse(C_2 == 0, 0, NA),
           adj_4 = ifelse(C_3 == 0, 0, NA),
           adj_5 = ifelse(C_4 == 0, 0, NA))
})

learners <- list("mean", "glm", #"cv_glmnet",
              "earth",
              "xgboost",
              list("xgboost", 
                   min_child_weight = 10, 
                   id = "xgboost1"),
              "glmnet",
              "ranger")

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
    control = lmtp_control(.learners_outcome_folds = 5,
                           .learners_trt_folds = 5,
                           .trim = 0.95,
                           .discrete = FALSE), # look at trim
    mtp = FALSE,
    id = NULL)
  
  result
}

set.seed(9)

for (j in 1:1)
{
for (i in 14:5)
{

  # set.seed(9)
  # results_shift_obs <- run_lmtp(data = completed_datasets[[j]],
  #                             day = i,
  #                             shift = NULL,
  #                             learners = learners,
  #                             folds = 5
  # )
  # 
  # saveRDS(results_shift_obs, here::here(paste0("results_012825/results_obs_day_", i, "_", j, ".rds")))

set.seed(9)
results_shift_5 <- run_lmtp(data = completed_datasets[[j]],
                                 day = i,
                                 shift = completed_datasets_5[[j]],
                                 learners = learners,
                                 folds = 5
)

saveRDS(results_shift_5, here::here(paste0("results_012825/results_shift_5_day_", i, "_", j, ".rds")))

# set.seed(9)
# results_shift_3 <- run_lmtp(data = completed_datasets[[j]],
#                             day = i,
#                             shift = completed_datasets_3[[j]],
#                             learners = learners,
#                             folds = 5
# )
# 
# saveRDS(results_shift_3, here::here(paste0("results_012825/results_shift_3_day_", i, "_", j, ".rds")))

# set.seed(9)
# results_shift_always <- run_lmtp(data = completed_datasets[[j]],
#                             day = i,
#                             shift = completed_datasets_always[[j]],
#                             learners = learners,
#                             folds = 5
# )
# saveRDS(results_shift_always, here::here(paste0("results_012825/results_shift_always_day_", i, "_", j, ".rds")))
}
}
