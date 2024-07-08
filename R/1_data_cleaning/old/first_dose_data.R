library(tidyverse)
library(tidyr)
library(lme4)
library(ggplot2)
library(data.table)

### read the data
data <- read_csv("data/ctn0097dat_botharms.csv") |>
  as.data.table()


### cleaning
# patient IDs
data$PATID <- as.character(data$PATID)

# extracting numbers from VISNO identifiers
data$VISNO_num <- as.numeric(gsub("\\D", "", data$VISNO))

# filter PROTSEG = "D"
data <- data[PROTSEG == "D"]


### converting the data from long to wide using the following criteria:
# O=(L0, A1_0, A2_0, C_0, Y_1, L1, A1_1, A2_1, C_1, Y_2, ...,  L4, A1_4, A2_4, C_4, Y_5)
# where L0 = baseline covariates
# A1, A2 are clonidine and clonazepam doses over the course of day _t.
# C_t is censoring at day t
# Y_t is COWs score at day t
# leaving L_1 - L_4 empty for now

# storing the baseline variables for later
L0 <- c("PATID",
        "DESEX",
        "DEHISPNC",
        "DEBLACK",
        "DEWHITE",
        "DEEDUCTN",
        "DEJOB",
        "DEMARTL",
        "age",
        "MHANXH",
        "MHBPLRH",
        "MHMDDH",
        "MHSCHZH")
baseline_vars <- unique(data[, ..L0])

# taking the first (non-NA, if possible) COWs score for each patient at each visit number
COWS_SCORE <- data[VISNO_num <= 5, .(Y = ifelse(is.na(first(COCOWSCR)) == FALSE, first(COCOWSCR), 
                                                first(na.omit(COCOWSCR)))), by=c("PATID", "VISNO_num")]

COWS_SCORE_LAST <- data[VISNO_num == 5, .(last_Y = ifelse(is.na(last(COCOWSCR)) == FALSE, last(COCOWSCR), 
                                                last(na.omit(COCOWSCR)))), by=c("PATID", "VISNO_num")]

max_COWS_SCORE <- data[VISNO_num <= 5, .(Z = max(COCOWSCR, na.rm = TRUE)), by=c("PATID", "VISNO_num")]
COWS_SCORE <- COWS_SCORE[!is.infinite(Y)]

max_COWS_SCORE <- max_COWS_SCORE[!is.infinite(Z)]

COWS_SCORE <- COWS_SCORE[max_COWS_SCORE, on = c("PATID", "VISNO_num")]

# including the visits only where doses are administered
data <- data[VISNO %in% c("B00","IN01","IN02","IN03","IN04","IN05")]

# counting how many doses are given each visit
num_doses <- data.frame(A1 = rowSums(!is.na(data[, c("DMCLDD01", "DMCLDD02", "DMCLDD03", "DMCLDD04", "DMCLDD05", "DMCLDD06")])),
                        A2 = rowSums(!is.na(data[, c("DMCZPD01", "DMCZPD02", "DMCZPD03", "DMCZPD04", "DMCZPD05", "DMCZPD06")])),
                        A1_dose_sum = data[, c("DMCLDDTL")],
                        A2_dose_sum = data[, c("DMCZPDTL")]) |>
  rename("A1_dose_sum" = "DMCLDDTL",
         "A2_dose_sum" = "DMCZPDTL")

data <- cbind(data, num_doses)

# joining the dosage columns with the first COWS scores from above
measures <- data[, c("PATID","VISNO_num","A1","A2", "A1_dose_sum", "A2_dose_sum")][COWS_SCORE, on = c("PATID", "VISNO_num")]

# reshape from long to wide
data_wide <- reshape(measures, idvar="PATID", timevar="VISNO_num", direction="wide")

# adding censoring variables and other covariates
additional_vars <- data.frame(C.0 = as.numeric(!is.na(data_wide$Y.1)), # fix these
                              C.1 = as.numeric(!is.na(data_wide$Y.2)),
                              C.2 = as.numeric(!is.na(data_wide$Y.3)),
                              C.3 = as.numeric(!is.na(data_wide$Y.4)),
                              C.4 = as.numeric(!is.na(data_wide$Y.5)),
                              L1 = NA,
                              L2 = NA,
                              L3 = NA,
                              L4 = NA)

data_wide <- cbind(data_wide, additional_vars)

# Adding back in the baseline covariates and changing the column order 
data_wide <- data_wide[baseline_vars, on="PATID"][,
                                                  c("PATID",
                                                    "DESEX",
                                                    "DEHISPNC",
                                                    "DEBLACK",
                                                    "DEWHITE",
                                                    "DEEDUCTN",
                                                    "DEJOB",
                                                    "DEMARTL",
                                                    "MHANXH",
                                                    "MHBPLRH",
                                                    "MHMDDH",
                                                    "MHSCHZH",
                                                    "age",
                                                    "Y.0",
                                                    "Z.0", 
                                                    "A1.0",
                                                    "A1_dose_sum.0",
                                                    "A2.0",
                                                    "A2_dose_sum.0",
                                                    "C.0",
                                                    "Y.1",
                                                    "L1",
                                                    "A1.1",
                                                    "A1_dose_sum.1",
                                                    "A2.1",
                                                    "A2_dose_sum.1",
                                                    "C.1",
                                                    "Y.2",
                                                    "L2",
                                                    "A1.2",
                                                    "A1_dose_sum.2",
                                                    "A2.2",
                                                    "A2_dose_sum.2",
                                                    "C.2",
                                                    "Y.3",
                                                    "L3",
                                                    "A1.3",
                                                    "A1_dose_sum.3",
                                                    "A2.3",
                                                    "A2_dose_sum.3",
                                                    "C.3",
                                                    "Y.4",
                                                    "L4",
                                                    "A1.4",
                                                    "A1_dose_sum.4",
                                                    "A2.4",
                                                    "A2_dose_sum.4",
                                                    "C.4",
                                                    "Y.5",
                                                    "A1.5",
                                                    "A1_dose_sum.5",
                                                    "A2.5",
                                                    "A2_dose_sum.5"
                                                  )] 

# replacing NAs in every "dose count" column with zeros
data_wide[, c("A1.0",
              "A2.0",
              "A1.1",
              "A2.1",
              "A1.2",
              "A2.2",
              "A1.3",
              "A2.3",
              "A1.4",
              "A2.4",
              "A1.5",
              "A2.5")] <- replace(data_wide[, c("A1.0",
                                                "A2.0",
                                                "A1.1",
                                                "A2.1",
                                                "A1.2",
                                                "A2.2",
                                                "A1.3",
                                                "A2.3",
                                                "A1.4",
                                                "A2.4",
                                                "A1.5",
                                                "A2.5")], is.na(data_wide[, c("A1.0",
                                                                              "A2.0",
                                                                              "A1.1",
                                                                              "A2.1",
                                                                              "A1.2",
                                                                              "A2.2",
                                                                              "A1.3",
                                                                              "A2.3",
                                                                              "A1.4",
                                                                              "A2.4",
                                                                              "A1.5",
                                                                              "A2.5")]), 0)

data_wide <- data_wide |>
  mutate(A1_dose_sum.0 = ifelse(is.na(A1_dose_sum.0), 0, A1_dose_sum.0),
         A1_dose_sum.1 = ifelse(is.na(A1_dose_sum.1), 0, A1_dose_sum.1),
         A1_dose_sum.2 = ifelse(is.na(A1_dose_sum.2), 0, A1_dose_sum.2),
         A1_dose_sum.3 = ifelse(is.na(A1_dose_sum.3), 0, A1_dose_sum.3),
         A1_dose_sum.4 = ifelse(is.na(A1_dose_sum.4), 0, A1_dose_sum.4),
         A1_dose_sum.5 = ifelse(is.na(A1_dose_sum.5), 0, A1_dose_sum.5)) |>
  mutate(A2_dose_sum.0 = ifelse(is.na(A2_dose_sum.0), 0, A2_dose_sum.0),
         A2_dose_sum.1 = ifelse(is.na(A2_dose_sum.1), 0, A2_dose_sum.1),
         A2_dose_sum.2 = ifelse(is.na(A2_dose_sum.2), 0, A2_dose_sum.2),
         A2_dose_sum.3 = ifelse(is.na(A2_dose_sum.3), 0, A2_dose_sum.3),
         A2_dose_sum.4 = ifelse(is.na(A2_dose_sum.4), 0, A2_dose_sum.4),
         A2_dose_sum.5 = ifelse(is.na(A2_dose_sum.5), 0, A2_dose_sum.5)) |>
  left_join(COWS_SCORE_LAST |> select(PATID, last_Y), by = c("PATID" = "PATID"))

# saving the data

write.csv(data_wide, "data/first_COWS/first_COWS_armd.csv")
