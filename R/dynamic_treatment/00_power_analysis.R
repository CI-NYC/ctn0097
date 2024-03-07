#devtools::install_github("nt-williams/lmtp@multivariate-exposure")

library(tidyverse)
library(lmtp)
library(mlr3superlearner)
library(mlr3extralearners)
library(mice)

source("R/dynamic_treatment/utils_adapted.R")

trts <- c("A1.0", "A2.0", "A1.1", "A2.1", "A1.2", "A2.2", "A1.3", "A2.3", "A1.4", "A2.4", "A1.5", "A2.5")

dat <-read_csv("data/ctn0097dat_wide.csv") |>
  as.data.frame() |>
  select(-c(L1, L2, L3, L4)) |>
  mutate(A1.0 = ifelse(A1.0 >= 1, 1, 0), #dichotomize A 
         A2.0 = ifelse(A2.0 >= 1, 1, 0),
         A1.1 = ifelse(A1.1 >= 1, 1, 0),
         A2.1 = ifelse(A2.1 >= 1, 1, 0),
         A1.2 = ifelse(A1.2 >= 1, 1, 0),
         A2.2 = ifelse(A2.2 >= 1, 1, 0),
         A1.3 = ifelse(A1.3 >= 1, 1, 0),
         A2.3 = ifelse(A2.3 >= 1, 1, 0),
         A1.4 = ifelse(A1.4 >= 1, 1, 0),
         A2.4 = ifelse(A2.4 >= 1, 1, 0),
         A1.5 = ifelse(A1.5 >= 1, 1, 0),
         A2.5 = ifelse(A2.5 >= 1, 1, 0)) |>
  mutate(C.0 = ifelse(is.na(Y.0) & is.na(Y.1) & is.na(Y.2) & is.na(Y.3) & is.na(Y.4) & is.na(Y.5), 0, 1),
         C.1 = ifelse(is.na(Y.1) & is.na(Y.2) & is.na(Y.3) & is.na(Y.4) & is.na(Y.5), 0, 1),
         C.2 = ifelse(is.na(Y.2) & is.na(Y.3) & is.na(Y.4) & is.na(Y.5), 0, 1),
         C.3 = ifelse(is.na(Y.3) & is.na(Y.4) & is.na(Y.5), 0, 1),
         C.4 = ifelse(is.na(Y.4) & is.na(Y.5), 0, 1),
         C.5 = ifelse(is.na(Y.5), 0, 1)) |>
  mutate_at(trts, factor)

dat <- dat |>
  mutate(DEBLACK_missing = ifelse(is.na(DEBLACK), 1, 0),
         DEBLACK = ifelse(is.na(DEBLACK), 0, DEBLACK),
         DEWHITE_missing = ifelse(is.na(DEWHITE), 1, 0),
         DEWHITE = ifelse(is.na(DEWHITE), 0, DEWHITE),
         DEEDUCTN_missing = ifelse(DEEDUCTN == 97, 1, 0),
         DEEDUCTN = case_when(DEEDUCTN == 97 ~ 13, #replacing missing with mode
                                DEEDUCTN < 13 ~ 12, # collapsing under high school graduates
                              DEEDUCTN == 16 | DEEDUCTN == 17 ~ 16, #collapsing associates degrees
                              DEEDUCTN == 18 | DEEDUCTN == 19 ~ 18, #collapsing bachelor + degrees
                              TRUE ~ DEEDUCTN), 
         DEJOB = ifelse(DEJOB == 5 | DEJOB == 6 | DEJOB == 7 | DEJOB == 99, 5, DEJOB), # disabled, keeping house, student, other as OTHER
         DEMARTL_missing = ifelse(DEMARTL == 97, 1, 0),
         DEMARTL = case_when(DEMARTL == 97 ~ 5, #replacing missing with mode
                             DEMARTL == 2 ~ 2, # combining widowed + divorced + separated
                             DEMARTL == 3 ~ 2, # combining widowed + divorced + separated
                             DEMARTL == 4 ~ 2, # combining widowed + divorced + separated
                             TRUE ~ DEMARTL), 
         MHANXH_missing = ifelse(is.na(MHANXH), 1, 0),
         MHANXH = ifelse(is.na(MHANXH), 1, MHANXH),
         MHBPLRH_missing = ifelse(is.na(MHBPLRH), 1, 0),
         MHBPLRH = ifelse(is.na(MHBPLRH), 1, MHBPLRH),
         MHMDDH_missing = ifelse(is.na(MHMDDH), 1, 0),
         MHMDDH = ifelse(is.na(MHMDDH), 1, MHMDDH),
         MHSCHZH_missing = ifelse(is.na(MHSCHZH), 1, 0),
         MHSCHZH = ifelse(is.na(MHSCHZH), 1, MHSCHZH))

dat <- fastDummies::dummy_cols(dat, select_columns = c("DEEDUCTN","DEJOB", "DEMARTL")) #convert factors to dummy vars


set.seed(9)
dat_mice <- mice(dat, exclude = "Y.5", method = "cart")

set.seed(9)
dat_final <- complete(dat_mice)
dat_final$Y.5 <- dat$Y.5 # keep original Y5

lrnrs <- c("mean", "glm", "earth", "ranger", "gbm", "bartMachine") 

W <- c("DESEX",
       "DEHISPNC",
       "DEBLACK", #missing
       "DEBLACK_missing",
       "DEWHITE", #missing
       "DEWHITE_missing",
       "DEEDUCTN_12", #under 12th grade
       "DEEDUCTN_13", #high school graduate
       "DEEDUCTN_14", #GED or equiv
       "DEEDUCTN_15", #some college, no degree
       "DEEDUCTN_16", #associate's degree
       "DEEDUCTN_18", #bachelor's degree + 
       "DEEDUCTN_missing",
       "DEJOB_1", #working now
       "DEJOB_2", #temporarily laid off
       "DEJOB_3", #looking for work
       "DEJOB_5", #disabled, keeping house, student, other
       "DEMARTL_1", #married
       "DEMARTL_2", #widowed, divorced, or separated
       "DEMARTL_5", #never married
       "DEMARTL_6", #living with partner
       "DEMARTL_missing",
       "MHANXH", #missing
       "MHANXH_missing",
       "MHBPLRH", #missing,
       "MHBPLRH_missing",
       "MHMDDH", #missing,
       "MHMDDH_missing",
       "age")

A <- list(c("A1.0", "A2.0"),
          c("A1.1", "A2.1"),
          c("A1.2", "A2.2"),
          c("A1.3", "A2.3"),
          c("A1.4", "A2.4"),
          c("A1.5", "A2.5"))

C <- c("C.0", "C.1", "C.2", "C.3", "C.4", "C.5")

L <- list(c(W), c("Y.0"), c("Y.1"), c("Y.2"), c("Y.3"), c("Y.4"))

Y <- c("Y.5")

dat_shifted <- dat_final |>
  mutate(A1.1 = ifelse(Y.0 > 10, 1, 0),
         A2.1 = ifelse(Y.0 > 10, 1, 0),
         A1.2 = ifelse(Y.1 > 10, 1, 0),
         A2.2 = ifelse(Y.1 > 10, 1, 0),
         A1.3 = ifelse(Y.2 > 10, 1, 0),
         A2.3 = ifelse(Y.2 > 10, 1, 0),
         A1.4 = ifelse(Y.3 > 10, 1, 0),
         A2.4 = ifelse(Y.3 > 10, 1, 0),
         A1.5 = ifelse(Y.4 > 10, 1, 0),
         A2.5 = ifelse(Y.4 > 10, 1, 0)) |>
  mutate(C.0 = 1,
         C.1 = 1,
         C.2 = 1,
         C.3 = 1, 
         C.4 = 1,
         C.5 = 1) |>
  mutate_at(trts, factor)

set.seed(9)
results <- lmtp_sdr(
  data = dat_final, 
  trt = A,
  cens = C,
  time_vary = L,
  outcome = Y, 
  shifted = dat_shifted, 
  outcome_type = "continuous", 
  learners_outcome = lrnrs,
  learners_trt = lrnrs,
  folds = 20,
  .SL_folds = 20,
  mtp = FALSE
  )

#saveRDS(results, "data/results.rds")

set.seed(9)
results_origin <- lmtp_sdr(
  data = dat_final, 
  trt = A,
  cens = C,
  time_vary = L,
  outcome = Y, 
  outcome_type = "continuous", 
  learners_outcome = lrnrs,
  learners_trt = lrnrs,
  folds = 20,
  .SL_folds = 20,
  mtp = FALSE
)

#saveRDS(results_origin, "data/results_origin.rds")


trt <- list(readRDS("results.rds"))
ctl <- list(readRDS("results_origin.rds"))

# estimating difference
summarize_results(trt, ctl, ci_level = 0.95, ci_type = "marginal")$diff_est |>
  filter(time == 1)

