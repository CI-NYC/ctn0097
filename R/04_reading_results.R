library(tidyverse)
library(lmtp)
library(ggpubr)

read_results <- function(day, shift){
  data <- readRDS(here::here(paste0("results_5_days_trt/results_shift_", shift, "_day_", day, ".rds"))) |>
    tidy() |>
    rename(old.conf.low = conf.low,
           old.conf.high = conf.high) |>
    mutate(estimate = 1-estimate,
           conf.low = 1-old.conf.high,
           conf.high = 1-old.conf.low)
  
  data
}


combined_results_df <- data.frame()

for (i in c(3, 8))
{
  for (j in 5:14)
  {
    result_df <- read_results(j, i) |>
      mutate(shift = i, day = j)
    
    # Combine the result with the existing combined dataframe
    combined_results_df <- rbind(combined_results_df, result_df)
  }
}

results_plot <- ggplot(data = combined_results_df, aes(x = factor(day), y = estimate, color = factor(shift), group = factor(shift))) +
  geom_point(position = position_dodge(width = 0.2)) +  # Plot points
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.2)) +  # Add error bars
  labs(x = "Day", y = "Incidence", title = "XR-NTX Initiation Incidence by Day") +
  theme_minimal()

read_results_contrast <- function(day){
  data_5 <- readRDS(here::here(paste0("results_5_days_trt/results_shift_", "5", "_day_", day, ".rds"))) 
  data_8 <- readRDS(here::here(paste0("results_5_days_trt/results_shift_", "8", "_day_", day, ".rds"))) 
  contrast <- lmtp_contrast(data_5, ref = data_8) # opposite because survival
  
  contrast$vals
}


contrast_results_df <- data.frame()

for (j in 5:14)
  {
    result_df <- read_results_contrast(j) |>
      mutate(day = j)
    
    # Combine the result with the existing combined dataframe
    contrast_results_df <- rbind(contrast_results_df, result_df)
  }

contrast_plot <- ggplot(data = contrast_results_df, aes(x = factor(day), y = theta)) +
  geom_point(position = position_dodge(width = 0.2)) +  # Plot points
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.2)) +  # Add error bars
  labs(x = "Day", y = "Difference", title = "Contrast by Day") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()

ggarrange(results_plot, contrast_plot, ncol = 1)




