library(tidyverse)

adverse1 <- read.csv(here::here("data/AD1.csv"), colClasses = c(PATID = "character"), na.strings = "") 
adverse2 <- read.csv(here::here("data/AD2.csv"), colClasses = c(PATID = "character"), na.strings = "") 
adverse3 <- read.csv(here::here("data/AD3.csv"), colClasses = c(PATID = "character"), na.strings = "")