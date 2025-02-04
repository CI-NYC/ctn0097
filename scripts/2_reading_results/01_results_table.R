library(tidyverse)
library(knitr)

for (p in c("results_final", "results_alt"))
{
combined_results_df <- readRDS(here::here(paste0(p, "/combined_results_df_", p, ".rds"))) |>
  mutate(estimate = paste0(round(estimate, 4), " (", round(conf.low, 4), ", ", round(conf.high, 4), ")")) |>
  select(day, shift, estimate)
contrast_df <- readRDS(here::here(paste0(p, "/contrast_results_df_", p, ".rds"))) |>
  mutate(contrast = paste0(round(estimate, 4), " (", round(conf.low, 4), ", ", round(conf.high, 4), ")")) |>
  select(day, shift, contrast)

final_results <- combined_results_df |>
  left_join(contrast_df)

latex <- kable(final_results, format = "latex", booktabs = TRUE)

print(p)
print(latex)
}
