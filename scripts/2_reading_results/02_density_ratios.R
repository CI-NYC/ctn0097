library(tidyverse)
library(knitr)

density_df <- data.frame(analysis = character(),
                 shift = character(),
                 day_1 = numeric(),
                 day_2 = numeric(),
                 day_3 = numeric(),
                 day_4 = numeric(),
                 day_5 = numeric())

for (a in c("results_r1", "results_r1_alt", "results_r1_shift", "results_r1_site_exclusion"))
{
for (i in c("always", "5", "3"))
{

density <- readRDS(paste0(a, "/results_shift_", i, "_day_14.rds"))

row_cumprod <- t(apply(density$density_ratios, 1, cumprod))

print(a)
print(i)

max_density <- round(apply(row_cumprod, 2, max), 2)

density_row <- data.frame(analysis = a,
                         shift = i,
                         day_1 = max_density[[1]],
                         day_2 = max_density[[2]],
                         day_3 = max_density[[3]],
                         day_4 = max_density[[4]],
                         day_5 = max_density[[5]])

density_df <- rbind(density_df, density_row)

}
}

density_df <- density_df |>
  mutate(analysis = factor(analysis, levels = c("results_r1", "results_r1_alt", "results_r1_shift", "results_r1_site_exclusion")),
         shift = factor(shift, levels = c("5", "3", "always")),
         ) |>
  arrange(analysis, shift)


latex <- kable(density_df, format = "latex", booktabs = TRUE)

print(latex)

