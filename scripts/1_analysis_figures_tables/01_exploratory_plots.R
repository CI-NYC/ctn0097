library(tidyverse)

dat_long <- readRDS("data/analysis_data/dat_long.rds")

# tables
ctn97_medication_tbl_8 <- dat_long |> group_by(day_post_consent, max_cows >= 8) |>
  summarize(count = n(),
            at_least_one_of_clonidine_clonazepam_benzo = sum(ifelse(A1 == 1 | A2 == 1 | A3 == 1, 1, 0), na.rm = TRUE),
            both_clonidine_and_clonazepam_and_other_benzo = sum(ifelse(A1 == 1 & A2 == 1 & A3 == 1, 1, 0), na.rm = TRUE),
            both_clonidine_and_clonazepam_no_benzo = sum(ifelse(A1 == 1 & A2 == 1 & A3 == 0, 1, 0), na.rm = TRUE),
            clonidine_only = sum(ifelse(A1 == 1 & A2 == 0, 1, 0), na.rm = TRUE),
            clonazepam_only = sum(ifelse(A1 == 0 & A2 == 1, 1, 0), na.rm = TRUE)
  )

write.csv(ctn97_medication_tbl_8, here::here("results/ctn97_medication_tbl_8.csv"))

ctn97_medication_tbl_5 <- dat_long |> group_by(day_post_consent, max_cows >= 5) |>
  summarize(count = n(),
            at_least_one_of_clonidine_clonazepam_benzo = sum(ifelse(A1 == 1 | A2 == 1 | A3 == 1, 1, 0), na.rm = TRUE),
            both_clonidine_and_clonazepam_and_other_benzo = sum(ifelse(A1 == 1 & A2 == 1 & A3 == 1, 1, 0), na.rm = TRUE),
            both_clonidine_and_clonazepam_no_benzo = sum(ifelse(A1 == 1 & A2 == 1 & A3 == 0, 1, 0), na.rm = TRUE),
            clonidine_only = sum(ifelse(A1 == 1 & A2 == 0, 1, 0), na.rm = TRUE),
            clonazepam_only = sum(ifelse(A1 == 0 & A2 == 1, 1, 0), na.rm = TRUE)
  )

write.csv(ctn97_medication_tbl_5, here::here("results/ctn97_medication_tbl_5.csv"))

ctn97_medication_tbl_3 <- dat_long |> group_by(day_post_consent, max_cows > 3) |>
  summarize(count = n(),
            at_least_one_of_clonidine_clonazepam_benzo = sum(ifelse(A1 == 1 | A2 == 1 | A3 == 1, 1, 0), na.rm = TRUE),
            both_clonidine_and_clonazepam_and_other_benzo = sum(ifelse(A1 == 1 & A2 == 1 & A3 == 1, 1, 0), na.rm = TRUE),
            both_clonidine_and_clonazepam_no_benzo = sum(ifelse(A1 == 1 & A2 == 1 & A3 == 0, 1, 0), na.rm = TRUE),
            clonidine_only = sum(ifelse(A1 == 1 & A2 == 0, 1, 0), na.rm = TRUE),
            clonazepam_only = sum(ifelse(A1 == 0 & A2 == 1, 1, 0), na.rm = TRUE)
  )

write.csv(ctn97_medication_tbl_3, here::here("results/ctn97_medication_tbl_3.csv"))

sample_sizes <- dat_long |>
  filter(is.na(max_cows) == FALSE) |>
  group_by(day_post_consent, adj) |>
  summarise(n = n()) |>
  arrange(day_post_consent, desc(adj))

# boxplot
boxplot <- ggplot(dat_long, aes(x = factor(day_post_consent), y = max_cows, fill = factor(adj, levels = c("1", "0")))) +
  geom_boxplot(position = position_dodge(width = 0.9)) +
  labs(title = "Maximum COWS by Day and Medication (Pre-Imputation)",
       x = "Day (post-consent)",
       y = "Maximum daily COWS",
       fill = "Received clonidine, clonazepam, or other benzos after maximum COWS but prior to next COWS") +
  scale_x_discrete(name = "Day", breaks = 1:14, labels = paste("Day", 1:14))  +
  geom_text(data = sample_sizes, aes(x = factor(day_post_consent), y = -1, 
                                     label = paste0("n=", n), 
                                     group = factor(adj, levels = c("1", "0"))), 
            position = position_dodge(width = 0.75), 
            vjust = -0.5,
            size = 2.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.65, 0.9)) +
  scale_fill_manual(values = c("#FF9999", "#99CCFF"),
                    labels = c("Yes", "No"))

ggsave("figs/medication_boxplot.png", plot = boxplot, width = 16, height = 8, dpi = 300, bg="white")
