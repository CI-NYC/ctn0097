library(tidyverse)
library(tidyr)
library(lme4)
library(ggplot2)
library(data.table)

### read the data
data <- read_csv("data/ctn0097dat_botharms.csv")  |>
  filter(PROTSEG == "D") |>
  mutate(PATID = as.character(PATID)) |>
  as.data.table()


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

# daily mediation data
DMA <- read_csv("data/DMA.csv") |>
  filter(PROTSEG == "D") |>
  select(PATID, VISNO, DMBUPDTL, DMBUPD01, DMBUPD02, DMBUPD03, DMBUPD04, DMBUPD05, DMBUPD06) |>
  mutate(PATID = as.character(as.numeric(PATID))) |>
  filter(VISNO %in% c("B00","IN01","IN02","IN03","IN04","IN05"))

# joining buprenorphine data to original data
data <- data |>
  full_join(DMA,
            by = c("PATID" = "PATID",
                   "VISNO" = "VISNO")) |>
  as.data.table()


### cleaning
# extracting numbers from VISNO identifiers
data$VISNO_num <- as.numeric(gsub("\\D", "", data$VISNO))

### converting the data from long to wide using the following criteria:
# O=(L0, A1_0, A2_0, C_0, Y_1, L1, A1_1, A2_1, C_1, Y_2, ...,  L4, A1_4, A2_4, C_4, Y_5)
# where L0 = baseline covariates
# A1, A2 are clonidine and clonazepam doses over the course of day _t.
# C_t is censoring at day t
# Y_t is COWs score at day t
# leaving L_1 - L_4 empty for now

# taking the first (non-NA, if possible) COWs score for each patient at each visit number
COWS_SCORE <- data[VISNO_num <= 5, .(Y = ifelse(max(COCOWSCR, na.rm = TRUE) == -Inf, COWSCRRT, #if COWS missing, use retrospective
                                                max(COCOWSCR, na.rm = TRUE))), by=c("PATID", "VISNO_num")]

# including the visits only where doses are administered
data <- data[VISNO %in% c("B00","IN01","IN02","IN03","IN04","IN05")]

# counting how many doses are given each visit
num_doses <- data.frame(A1 = rowSums(!is.na(data[, c("DMCLDD01", "DMCLDD02", "DMCLDD03", "DMCLDD04", "DMCLDD05", "DMCLDD06")])),
                        A2 = rowSums(!is.na(data[, c("DMCZPD01", "DMCZPD02", "DMCZPD03", "DMCZPD04", "DMCZPD05", "DMCZPD06")])),
                        A3 = rowSums(!is.na(data[, c("DMBUPD01", "DMBUPD02", "DMBUPD03", "DMBUPD04", "DMBUPD05", "DMBUPD06")])),
                        A1_dose_sum = data[, c("DMCLDDTL")],
                        A2_dose_sum = data[, c("DMCZPDTL")],
                        A3_dose_sum = data[, c("DMBUPDTL")]) |>
  rename("A1_dose_sum" = "DMCLDDTL",
         "A2_dose_sum" = "DMCZPDTL",
         "A3_dose_sum" = "DMBUPDTL")

data <- cbind(data, num_doses)

# joining the dosage columns with the max COWS scores from above
measures <- data[, c("PATID","VISNO_num","A1","A2", "A3", "A1_dose_sum", "A2_dose_sum", "A3_dose_sum")] |>
  full_join(COWS_SCORE, by = c("PATID" = "PATID", "VISNO_num" = "VISNO_num"))

# reshape from long to wide
data_wide <- reshape(measures, idvar="PATID", timevar="VISNO_num", direction="wide")

# adding censoring variables and other covariates
additional_vars <- data.frame(C.0 = NA, # fix these
                              C.1 = NA,
                              C.2 = NA,
                              C.3 = NA,
                              C.4 = NA,
                              L1 = NA,
                              L2 = NA,
                              L3 = NA,
                              L4 = NA)

data_wide <- cbind(data_wide, additional_vars)

baseline_vars <- baseline_vars 

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
                                                    "A1.0",
                                                    "A1_dose_sum.0",
                                                    "A2.0",
                                                    "A2_dose_sum.0",
                                                    "A3.0",
                                                    "A3_dose_sum.0",
                                                    "C.0",
                                                    "Y.1",
                                                    "L1",
                                                    "A1.1",
                                                    "A1_dose_sum.1",
                                                    "A2.1",
                                                    "A2_dose_sum.1",
                                                    "A3.1",
                                                    "A3_dose_sum.1",
                                                    "C.1",
                                                    "Y.2",
                                                    "L2",
                                                    "A1.2",
                                                    "A1_dose_sum.2",
                                                    "A2.2",
                                                    "A2_dose_sum.2",
                                                    "A3.2",
                                                    "A3_dose_sum.2",
                                                    "C.2",
                                                    "Y.3",
                                                    "L3",
                                                    "A1.3",
                                                    "A1_dose_sum.3",
                                                    "A2.3",
                                                    "A2_dose_sum.3",
                                                    "A3.3",
                                                    "A3_dose_sum.3",
                                                    "C.3",
                                                    "Y.4",
                                                    "L4",
                                                    "A1.4",
                                                    "A1_dose_sum.4",
                                                    "A2.4",
                                                    "A2_dose_sum.4",
                                                    "A3.4",
                                                    "A3_dose_sum.4",
                                                    "C.4",
                                                    "Y.5",
                                                    "A1.5",
                                                    "A1_dose_sum.5",
                                                    "A2.5",
                                                    "A2_dose_sum.5",
                                                    "A3.5",
                                                    "A3_dose_sum.5"
                                                  )] 

# replacing NAs in every "dose count" column with zeros
data_wide[, c("A1.0",
              "A2.0",
              "A3.0",
              "A1.1",
              "A2.1",
              "A3.1",
              "A1.2",
              "A2.2",
              "A3.2",
              "A1.3",
              "A2.3",
              "A3.3",
              "A1.4",
              "A2.4",
              "A3.4",
              "A1.5",
              "A2.5",
              "A3.5")] <- replace(data_wide[, c("A1.0",
                                                "A2.0",
                                                "A3.0",
                                                "A1.1",
                                                "A2.1",
                                                "A3.1",
                                                "A1.2",
                                                "A2.2",
                                                "A3.2",
                                                "A1.3",
                                                "A2.3",
                                                "A3.3",
                                                "A1.4",
                                                "A2.4",
                                                "A3.4",
                                                "A1.5",
                                                "A2.5",
                                                "A3.5")], is.na(data_wide[, c("A1.0",
                                                                              "A2.0",
                                                                              "A3.0",
                                                                              "A1.1",
                                                                              "A2.1",
                                                                              "A3.1",
                                                                              "A1.2",
                                                                              "A2.2",
                                                                              "A3.2",
                                                                              "A1.3",
                                                                              "A2.3",
                                                                              "A3.3",
                                                                              "A1.4",
                                                                              "A2.4",
                                                                              "A3.4",
                                                                              "A1.5",
                                                                              "A2.5",
                                                                              "A3.5")]), 0)

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
  mutate(A3_dose_sum.0 = ifelse(is.na(A3_dose_sum.0), 0, A3_dose_sum.0),
         A3_dose_sum.1 = ifelse(is.na(A3_dose_sum.1), 0, A3_dose_sum.1),
         A3_dose_sum.2 = ifelse(is.na(A3_dose_sum.2), 0, A3_dose_sum.2),
         A3_dose_sum.3 = ifelse(is.na(A3_dose_sum.3), 0, A3_dose_sum.3),
         A3_dose_sum.4 = ifelse(is.na(A3_dose_sum.4), 0, A3_dose_sum.4),
         A3_dose_sum.5 = ifelse(is.na(A3_dose_sum.5), 0, A3_dose_sum.5))

EOI <- read_csv("data/EOI.csv") |>
  select(PATID, EINTXIND, EOIINJDT) |>
  mutate(PATID = as.character(as.numeric(PATID)))

data_wide <- data_wide |>
  left_join(EOI, by = c("PATID" = "PATID"))

# saving the data

write.csv(data_wide, "data/max_COWS/max_COWS_armd.csv")
