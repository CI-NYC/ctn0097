library(tidyverse)

# COWS scores
COW <- read.csv("data/COW.csv", colClasses = c(PATID = "character")) |>
  filter(is.na(COASMTM) == FALSE) |> #if time is missing, remove
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
  filter(!((PATID == "02201009700118" & COWASMDT == 0 & COASMTM == "02:57") | COASMTM == "")) |> # remove COWS with issues
  select(PATID, COWASMDT, COASMTM, cows_score) |>
  rename("days_from_admission" = "COWASMDT") |>
  mutate(COASMTM = ymd_hm(paste("1970-01-01", COASMTM)), # choose arbitrary day
         day_time_cows = COASMTM + days(days_from_admission)) |>
  select(PATID,days_from_admission, day_time_cows, cows_score)

# SOWS scores
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
  mutate(SBASMTM = ymd_hm(paste("1970-01-01", SBASMTM)), # choose arbitrary day
         day_time_sows = SBASMTM + days(days_from_admission)) |>
  select(PATID, day_time_sows, sows_score)

joined_df <- COW |>
  inner_join(SOW, by = c("PATID" = "PATID")) |>
  rowwise() |>
  mutate(
    time_diff_hours = abs(as.numeric(difftime(day_time_cows, day_time_sows, units = "hours")))
  ) |>
  ungroup() |>
  filter(time_diff_hours <= 12) |> # only those within a half day
  mutate(cows_first = ifelse(day_time_cows < day_time_sows, 1, 0)) |>
  distinct()

# finding number of COWS and SOWS mathced by patient within x minutes/hours
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
joined_df |> filter(time_diff_hours <= 10/60) |> nrow() #243

# less than or equal to 15 minutes
joined_df |> filter(time_diff_hours <= 15/60) |> nrow() #268

# less than or equal to 30 minutes
joined_df |> filter(time_diff_hours <= 30/60) |> nrow() #319

# less than or equal to 1 hour
joined_df |> filter(time_diff_hours <= 1) |> nrow() #400

# less than or equal to 2 hour
joined_df |> filter(time_diff_hours <= 2) |> nrow() #525

# less than or equal to 3 hour
joined_df |> filter(time_diff_hours <= 3) |> nrow() #657

# try keeping within 15 minutes
joined_df_15 <- joined_df |>
  filter(time_diff_hours <= 15/60) # only within 15 minutes

hist(joined_df_15$cows_score)
hist(joined_df_15$sows_score)

plot(joined_df_15$sows_score, 
     joined_df_15$cows_score, 
     main = "Scatterplot of SOWS v. COWS",
     xlab = "SOWS", 
     ylab = "COWS",
     pch = 19,      
     col = "black")   

# looks like linear fit is appropriate

saveRDS(joined_df, here::here("data/cows_sows_data/joined_df.rds"))


