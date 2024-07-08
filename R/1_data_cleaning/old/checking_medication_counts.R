library(tidyverse)

cols <- c("DMCLDD01", "DMCLDD02", "DMCLDD03", "DMCLDD04", "DMCLDD05", "DMCLDD06")

# daily medication data

DMA <- read_csv("data/DMA.csv") |>
  filter(PROTSEG == "D" |
           PROTSEG == "C") |>
  #rowwise() %>%
  #mutate(summ = sum(c_across(all_of(cols)), na.rm = TRUE),
  #       ind = ifelse(summ == DMCLDDTL, 0, 1)) |>
  mutate(day = case_when(grepl("B00", VISNO) ~ 0,
                         grepl("01", VISNO) ~ 1,
                         grepl("02", VISNO) ~ 2,
                         grepl("03", VISNO) ~ 3,
                         grepl("04", VISNO) ~ 4, 
                         grepl("05", VISNO) ~ 5,
                         grepl("06", VISNO) ~ 6,
                         grepl("07", VISNO) ~ 7,
                         grepl("08", VISNO) ~ 8,
                         grepl("09", VISNO) ~ 9,
                         grepl("10", VISNO) ~ 10,
                         grepl("11", VISNO) ~ 11,
                         grepl("12", VISNO) ~ 12,
                         grepl("13", VISNO) ~ 13,
                         grepl("14", VISNO) ~ 14,
                         grepl("15", VISNO) ~ 15,
                         grepl("16", VISNO) ~ 16,
                         grepl("17", VISNO) ~ 17,
                         grepl("18", VISNO) ~ 18,
                         grepl("19", VISNO) ~ 19,
                         grepl("20", VISNO) ~ 20,
                         grepl("21", VISNO) ~ 21,
                         grepl("22", VISNO) ~ 22,
                         grepl("23", VISNO) ~ 23,
                         grepl("24", VISNO) ~ 24,
                         grepl("25", VISNO) ~ 25,
                         grepl("26", VISNO) ~ 26,
                         grepl("27", VISNO) ~ 27,
                         grepl("28", VISNO) ~ 28,
                         grepl("29", VISNO) ~ 29,
                         TRUE ~ NA)) |>
  relocate(day, .before = VISNO) #|>
  #mutate(PATID = as.character(as.numeric(PATID)))# |>
  #filter(VISNO %in% c("B00","IN01","IN02","IN03","IN04","IN05"))

# DMA missing 3 people in arm C?

#dat <- read_csv("data/ctn0097dat_botharms.csv")

DMA |>
  filter(PROTSEG == "D") |> # filter to rapid induction
  complete(PATID, day) |>
  group_by(day) |> # group by VISNO day
  summarize(buprenorphine = sum(ifelse(DMBUPDTL > 0, 1, 0), na.rm = TRUE),  # if pat received any buprenorphine then yes
            buprenorphine2 = sum(ifelse(DMBUPD01 > 0 |
                                         DMBUPD02 > 0 |
                                         DMBUPD03 > 0 |
                                         DMBUPD04 > 0 |
                                         DMBUPD05 > 0 |
                                         DMBUPD06 > 0, 1, 0), na.rm = TRUE),
            clonidine = sum(ifelse(DMCLDDTL > 0, 1, 0), na.rm = TRUE), # if pat received any clonidine then yes
            clonazepam = sum(ifelse(DMCZPDTL > 0, 1, 0), na.rm = TRUE)) # if pat received any clonazepam then yes
  
  
DMA |>
  filter(PROTSEG == "D") |> # filter to rapid induction
  group_by(day) |> # group by date of medication administration
  rowwise() |>
  mutate(buprenorphine_sum = sum(DMBUPD01, DMBUPD02, DMBUPD03, DMBUPD04, DMBUPD05, DMBUPD06, na.rm = TRUE),
         bup_diff = ifelse(DMBUPDTL == buprenorphine_sum, 0, 1))  |>
  select(PATID, day, VISNO, DMAMDDT, DMBUPDTL, buprenorphine_sum, bup_diff) |>
  filter(bup_diff == 1)


# COWS data

COW <- read_csv("data/COW.csv") |>
  mutate(day = case_when(grepl("B00", VISNO) ~ 0,
                         grepl("01", VISNO) ~ 1,
                         grepl("02", VISNO) ~ 2,
                         grepl("03", VISNO) ~ 3,
                         grepl("04", VISNO) ~ 4, 
                         grepl("05", VISNO) ~ 5,
                         grepl("06", VISNO) ~ 6,
                         grepl("07", VISNO) ~ 7,
                         grepl("08", VISNO) ~ 8,
                         grepl("09", VISNO) ~ 9,
                         grepl("10", VISNO) ~ 10,
                         grepl("11", VISNO) ~ 11,
                         grepl("12", VISNO) ~ 12,
                         grepl("13", VISNO) ~ 13,
                         grepl("14", VISNO) ~ 14,
                         grepl("15", VISNO) ~ 15,
                         grepl("16", VISNO) ~ 16,
                         grepl("17", VISNO) ~ 17,
                         grepl("18", VISNO) ~ 18,
                         grepl("19", VISNO) ~ 19,
                         grepl("20", VISNO) ~ 20,
                         grepl("21", VISNO) ~ 21,
                         grepl("22", VISNO) ~ 22,
                         grepl("23", VISNO) ~ 23,
                         grepl("24", VISNO) ~ 24,
                         grepl("25", VISNO) ~ 25,
                         grepl("26", VISNO) ~ 26,
                         grepl("27", VISNO) ~ 27,
                         grepl("28", VISNO) ~ 28,
                         grepl("29", VISNO) ~ 29,
                         TRUE ~ NA)) |>
  relocate(day, .before = VISNO)

COW |>
  filter(PROTSEG == "C") |>
  group_by(day, PATID) |>
  summarize(count = n()) |>
  group_by(day) |>
  summarize(count = n()) |>
  view()

EOI <- read_csv("data/EOI.csv") |>
  select(PATID, EINTXIND, EOIINJDT) #|>
  #mutate(PATID = as.character(as.numeric(PATID)))

enrollment <- read_csv("data/EC0097B.csv") |>
  select(PATID, STARTDT) #|>
  #mutate(PATID = as.character(as.numeric(PATID)))

ctn97 <- DMA |>
  complete(PATID, day) |>
  inner_join(EOI, by = c("PATID" = "PATID")) |>
  inner_join(enrollment, by = c("PATID" = "PATID")) |>
  relocate(c(STARTDT, EINTXIND, EOIINJDT), .after = DMAMDDT)

ctn97_TEST <- ctn97|>
  group_by(PATID) |>
  filter(row_number() >= which(DMAMDDT == STARTDT)[1]) |>
  filter(is.na(EOIINJDT) | row_number() <= which(DMAMDDT == EOIINJDT)[1]) |>
  ungroup() |>
  filter(VISNO != "B00" | is.na(VISNO)
         ) |>
  group_by(PATID) |>
  mutate(day_cleaned = row_number()) |>
  ungroup() |>
  relocate(day_cleaned, .after = day)
  
ctn97_TEST |>
  filter(PROTSEG == "D") |> # filter to rapid induction
  group_by(day_cleaned) |> # group by "Date of medication admin" variable
  summarize(buprenorphine = sum(ifelse(DMBUPDTL > 0, 1, 0), na.rm = TRUE),  # if pat received any buprenorphine then yes
            clonidine = sum(ifelse(DMCLDDTL > 0, 1, 0), na.rm = TRUE), # if pat received any clonidine then yes
            clonazepam = sum(ifelse(DMCZPDTL > 0, 1, 0), na.rm = TRUE)) # if pat received any clonazepam then yes