# Additional calculations

library(dplyr)
library(magrittr)
library(tidyverse)



# Calculate differences between completed questionnaire dates for each participant

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



# printing knots in alcohol splines
data <- targets::tar_read(id_data)

data <- data %>% mutate(
  alcohol_intake = rowSums(pick(matches("p26030")), na.rm = TRUE),
  alcohol_daily = alcohol_intake / p20077,
  alcohol_weekly = alcohol_daily * 7,
  alc_spline = splines::bs(alcohol_weekly, knots = 4, degree = 3)
)

# Calculate internal knots based on quantiles
nknots <- 4  # Number of knots (adjust as needed)
internal_knots <- quantile(data$alcohol_weekly, probs = seq(0.25, 0.75, length.out = nknots), na.rm = TRUE)

# Generate the spline basis using specified knots
spline_basis <- splines::bs(data$alcohol_weekly, knots = internal_knots, degree = 3)

# Extract and print the knot positions
knots <- attr(spline_basis, "knots")

cat("knots are placed at:", knots, "\n")


