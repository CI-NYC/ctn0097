library(tidyverse)

COW <- read.csv("data/COW.csv", colClasses = c(PATID = "character")) |>
  filter(is.na(COASMTM) == FALSE) |>
  mutate(across(c(COPULSE, COSWEAT, CORESTLS, COPUPIL, COBONJNT, CONOSEYE, COGIUPST, COTREMOR,
                  COYAWN, COANXITY, COGOOSKN), ~ coalesce(., 0))) |> # missing values = 0
  rowwise() |>
  mutate(cows_score = case_when(is.na(COCOWSCR) & is.na(COWSCRRT) == FALSE ~ COWSCRRT, # if missing COWS but retrospective available, use that
                                # patient notes indicating that these patients did not receive/finish COWS assessments at the visits
                                PATID == "02201009700118" & VISNO == "IN02" ~ as.numeric(NA),
                                PATID == "02076009700045" & VISNO == "IN08" ~ as.numeric(NA),
                                PATID == "02076009700335" & VISNO == "B00" ~ as.numeric(NA),
                                TRUE ~ sum(c(COPULSE, COSWEAT, CORESTLS, COPUPIL, COBONJNT, CONOSEYE, COGIUPST, COTREMOR, # some categories missing information but still taking sum (these were imputed with 0)
                                             COYAWN, COANXITY, COGOOSKN)) # issues with COCOWSCR variable -- sometimes doesn't match sum -- we will take sum for now (only 7 instances)
  )) |>
  filter(!((PATID == "02201009700118" & COWASMDT == 0 & COASMTM == "02:57") | COASMTM == "")) |>
  select(PATID, COWASMDT, COASMTM, cows_score) |>
  rename("days_from_admission" = "COWASMDT") |>
  mutate(COASMTM = ymd_hm(paste("2024-01-01", COASMTM)), # choose arbitrary day
         day_time_cows = COASMTM + days(days_from_admission)) |>
  select(PATID, day_time_cows, cows_score)

SOW <- read.csv("data/SBW.csv", colClasses = c(PATID = "character")) |>
  filter(SBASMTM != "") |> # 26 cases of SOWS missing but individual scores present, using sum
  rowwise() |>
  mutate(sows_score = sum(SBANXIUS,
         SBYAWN,
         SBSWEAT,
         SBTEARY,
         SBNOSRUN,
         SBGSBMPS,
         SBSHAKE,
         SBHTFLSH,
         SBCDFLSH,
         SBMSACHE,
         SBRSTLS,
         SBNAUS,
         SBVOMIT,
         SBMSCLTW,
         SBCRAMPS,
         SBUSENOW, na.rm = TRUE)) |>
  select(PATID, SBWASMDT, SBASMTM, sows_score) |>
  rename("days_from_admission" = "SBWASMDT") |>
  mutate(SBASMTM = ymd_hm(paste("2024-01-01", SBASMTM)), # choose arbitrary day
         day_time_sows = SBASMTM + days(days_from_admission)) |>
  select(PATID, day_time_sows, sows_score)

joined_df <- COW |>
  inner_join(SOW, by = c("PATID" = "PATID")) |>
  rowwise() |>
  mutate(
    time_diff_hours = abs(as.numeric(difftime(day_time_cows, day_time_sows, units = "hours")))
  ) |>
  ungroup() |>
  filter(time_diff_hours <= 12) # only those within a half day

# exact time
joined_df |> filter(time_diff_hours == 0) |> nrow() #51

# less than or equal to 1 minutes
joined_df |> filter(time_diff_hours <= 1/60) |> nrow() #73

# less than or equal to 2 minutes
joined_df |> filter(time_diff_hours <= 2/60) |> nrow() #104

# less than or equal to 3 minutes
joined_df |> filter(time_diff_hours <= 3/60) |> nrow() #141

# less than or equal to 5 minutes
joined_df |> filter(time_diff_hours <= 5/60) |> nrow() #191

# less than or equal to 10 minutes
joined_df |> filter(time_diff_hours <= 10/60) |> nrow() #244

# less than or equal to 15 minutes
joined_df |> filter(time_diff_hours <= 15/60) |> nrow() #269

# less than or equal to 30 minutes
joined_df |> filter(time_diff_hours <= 30/60) |> nrow() #320

# less than or equal to 1 hour
joined_df |> filter(time_diff_hours <= 1) |> nrow() #401

# less than or equal to 2 hour
joined_df |> filter(time_diff_hours <= 2) |> nrow() #526

# less than or equal to 3 hour
joined_df |> filter(time_diff_hours <= 3) |> nrow() #658







