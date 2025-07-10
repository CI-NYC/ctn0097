library(tidyverse)
library(ggplot2)

dat <- readRDS(here::here("data/analysis_data/analysis_data.rds")) |>
  select(SITE,
         PROTSEG,
         Y_14)

dat |> mutate(Arm = ifelse(PROTSEG == 1, "Rapid", "Standard")) |> group_by(SITE, Arm) |> summarize(count = n())

predictions <- 1 - readRDS("results_final/predicted_outcomes.rds")

dat <- dat |>
  cbind(predictions[,5]) |>
  rename("predicted_vals" = "predictions[, 5]") |>
  mutate(residual = Y_14 - predicted_vals) |>
  mutate(SITE = factor(SITE))

result <- aov(residual ~ SITE, data = dat)
summary(result)

result_arm <- aov(residual ~ SITE * PROTSEG, data = dat)
summary(result_arm)

result_without_SITE02076 <- aov(residual ~ SITE, data = dat |> filter(SITE != "02076"))
summary(result_without_SITE02076)

result_without_SITE02201 <- aov(residual ~ SITE, data = dat |> filter(SITE != "02201"))
summary(result_without_SITE02201)

result_without_SITE_fewer <- aov(residual ~ SITE, data = dat |> filter(SITE != "02076", SITE != "02201"))
summary(result_without_SITE_fewer)

result_without_SITE_fewer_arm <- aov(residual ~ SITE * PROTSEG, data = dat |> filter(SITE != "02201", SITE != "02217"))
summary(result_without_SITE_fewer_arm)

ggplot(dat, aes(x = residual)) +
  geom_boxplot(binwidth = 1, fill = "skyblue", color = "black") +
  facet_wrap(~SITE) +
  theme_minimal() +
  labs(title = "Boxplots of Residuals by Site", x = "Residuals", y = "")

