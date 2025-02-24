library(tidyverse)
library(glmnet)

cows_sows_data <- readRDS(here::here("data/cows_sows_data/final_dat.rds"))

data <- cows_sows_data |>
  select(sows_score, 
         time_diff_hours, 
         cows_first,
         days_from_admission,
         "days_from_admission_to_consent",
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
         "injection_opioid_use_missing",
         "years_since_first_opioid_use",
         "years_since_first_opioid_use_missing",
         # mental health
         "anxiety", #missing
         #"anxiety_missing",
         "bipolar", #missing,
         #"bipolar_missing",
         "depression", #missing
         #"depression_missing"
         sum_clonidine_24hrs,
         sum_clonazepam_24hrs,
         sum_benzo_previous_day,
         cows_score)

# getting interaction between SOWS and all variables
predictor_names <- names(data)[-ncol(data)] # everything but outcome (COWS score)
interaction_names <- setdiff(predictor_names, "sows_score") # all predictors but SOWS

for (var in interaction_names) {
  data[[paste0("sows_score:", var)]] <- data$sows_score * data[[var]]
}

analysis_mat <- as.matrix(data[, c(predictor_names, paste0("sows_score:", interaction_names))])

summary(analysis_mat)

set.seed(8)
cvfit <- cv.glmnet(x = analysis_mat, 
                   y = data$cows_score, 
                   alpha = 1,
                   nfolds = 40)

coefficients <- coef(cvfit, s = "lambda.1se") # use 1SE lambda

non_zero <- as.matrix(coefficients)
non_zero <- non_zero[non_zero != 0, , drop = FALSE]

non_zero

saveRDS(non_zero, here::here("data/cows_sows_data/model_coef.rds"))

set.seed(8)
cvfit <- cv.glmnet(x = analysis_mat, 
                   y = data$cows_score, 
                   alpha = 1,
                   nfolds = 40)

coefficients <- coef(cvfit, s = "lambda.min") # use 1SE lambda

non_zero <- as.matrix(coefficients)
non_zero <- non_zero[non_zero != 0, , drop = FALSE]

non_zero

saveRDS(non_zero, here::here("data/cows_sows_data/model_coef_min.rds"))

