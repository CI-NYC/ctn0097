library(tidyverse)
library(lmtp)
library(ggpubr)

tidy.lmtp_survival <- function(x, ...) {
  out <- do.call("rbind", lapply(x, tidy))
  out$t <- 1:length(x)
  out[, c(ncol(out), 1:ncol(out) - 1)]
}

isotonic_projection <- function(x, alpha = 0.05) {
  cv <- abs(qnorm(p = alpha / 2))
  estim <- tidy.lmtp_survival(x)
  iso_fit <- isotone::gpava(1:length(x), 1 - estim$estimate)
  for (i in seq_along(x)) {
    x[[i]]$estimate@x <- iso_fit$x[i]
  }
  x
}

read_results <- function(day, shift){
  data <- readRDS(here::here(paste0("results_012825/results_shift_", shift, "_day_", day, "_1", ".rds")))
}


combined_results_df <- data.frame()

both_results <- list()
for (i in c("always", 5, 3))
{
  results <- list()
  for (j in 5:14)
  {
    results[[j - 4]] <- read_results(j, i) 
  }
  
  both_results[[i]] <- isotonic_projection(results) 
}

# getting contrast of lmtp objects
contrast_always_5 <- map2(both_results[[1]], both_results[[2]], ~lmtp_contrast(.x, ref = .y))
contrast_always_3 <- map2(both_results[[1]], both_results[[3]], ~lmtp_contrast(.x, ref = .y))

# tidying lmtp object for dataframe
for (i in c("always", 5, 3))
{
  both_results[[i]] <- lapply(both_results[[i]], tidy)
}

df1 <- do.call(rbind, both_results[[1]]) |>
  mutate(shift = "always",
         day = row_number() + 4)

df2 <- do.call(rbind, both_results[[2]]) |>
  mutate(shift = 5, 
         day = row_number() + 4)

df3 <- do.call(rbind, both_results[[3]]) |>
  mutate(shift = 3, 
         day = row_number() + 4)


combined_results_df <- df1 |>
  merge(df2, all = TRUE) |>
  merge(df3, all = TRUE) |>
  mutate(shift = factor(shift, levels = c("always", "3", "5")))


tidied_results <- map(both_results, ~ map_dfr(.x, rbind))

df1 <- tidied_results[[1]] |>
  mutate(shift = "always",
         day = row_number() + 4)

df2 <- tidied_results[[2]]|>
  mutate(shift = 5, 
         day = row_number() + 4)

df3 <- tidied_results[[3]]|>
  mutate(shift = 3, 
         day = row_number() + 4)

combined_results_df <- df1 |>
  merge(df2, all = TRUE) |>
  merge(df3, all = TRUE) |>
  mutate(shift = factor(shift, levels = c("always", "3", "5")))

combined_vals_always_5 <- map_dfr(contrast_always_5, ~ {
  data.frame(vals = .x$estimates)  # Extract $vals and convert to data frame
}) |>
  mutate(day = row_number() + 4)

colnames(combined_vals_always_5) <- gsub("\\vals.", "", colnames(combined_vals_always_5))

combined_vals_always_3 <- map_dfr(contrast_always_3, ~ {
  data.frame(vals = .x$estimates)  # Extract $vals and convert to data frame
}) |>
  mutate(day = row_number() + 4)

colnames(combined_vals_always_3) <- gsub("\\vals.", "", colnames(combined_vals_always_3))

results_plot <- ggplot(data = combined_results_df, aes(x = factor(day), y = estimate, color = factor(shift), group = factor(shift))) +
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) + 
  ylim(0, 0.7) +
  labs(x = "Day", y = "Incidence", title = "XR-NTX Initiation Incidence by Day") +
  labs(color = "Regime") + 
  theme_minimal() + 
  theme(
    legend.position =  c(0.85, 0.15),
    legend.key.height = unit(0.5, "lines"),
    legend.key.width = unit(0.5, "lines"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.25), 
    legend.title = element_text(face = "bold") 
  )

contrast_plot_always_5 <- ggplot(data = combined_vals_always_5, aes(x = factor(day), y = estimate)) +
  geom_point(position = position_dodge(width = 0.2)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.2)) + 
   ylim(-0.2, 0.3) + 
  labs(x = "Day", y = "Difference", title = "Contrast by Day (Always vs. 5)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()

contrast_plot_always_3 <- ggplot(data = combined_vals_always_3, aes(x = factor(day), y = estimate)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.2)) + 
  ylim(-0.2, 0.3) + 
  labs(x = "Day", y = "Difference", title = "Contrast by Day (Always vs. 3)") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_minimal()


ggarrange(results_plot, 
          ggarrange(contrast_plot_always_5, contrast_plot_always_3, ncol = 2), 
          nrow = 2)




