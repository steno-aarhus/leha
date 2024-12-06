# Calculate differences between completed questionnaire dates for each participant


library(dplyr)
library(magrittr)
library(tidyverse)
data <- targets::tar_read(eligible_participants)

data <- data %>% mutate(
  ques_comp_t1 = as.Date(ques_comp_t1),
  ques_comp_t2 = as.Date(ques_comp_t2),
  ques_comp_t3 = as.Date(ques_comp_t3),
  ques_comp_t4 = as.Date(ques_comp_t4)
)


data <- data %>%
  rowwise() %>%
  mutate(
    # diff_t0_t1 = as.numeric(difftime(ques_comp_t1, ques_comp_t0, units = "days")),
    diff_t1_t2 = as.numeric(difftime(ques_comp_t2, ques_comp_t1, units = "days")),
    diff_t2_t3 = as.numeric(difftime(ques_comp_t3, ques_comp_t2, units = "days")),
    diff_t3_t4 = as.numeric(difftime(ques_comp_t4, ques_comp_t3, units = "days"))
  ) %>%
  ungroup()

# Combine all differences into a single column, ignoring NAs
all_differences <- data %>%
  select(diff_t1_t2, diff_t2_t3, diff_t3_t4) %>%
  pivot_longer(cols = everything(), values_to = "interval") %>%
  filter(!is.na(interval)) %>%
  pull(interval)

# Calculate mean and standard deviation
mean_interval <- mean(all_differences)
sd_interval <- sd(all_differences)

# Print results
cat("Mean interval:", mean_interval, "days\n")
cat("SD of interval:", sd_interval, "days\n")
