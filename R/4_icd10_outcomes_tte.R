# This script will split p41270 (ICD10-codes) and p41271 (ICD9-codes) into
# columns and match the date of diagnosis with the diagnosis code for that
# specific data. This is useful when using ICD10 diagnoses as outcomes in
# time-to-event analyses

# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)
library(here)
library(ggplot2)

# Load data
data <- read_csv(here("data/data.csv"))


# ICD10 codes ---------------------------------------------------------
# Split the diagnosis-variable into separate columns based on delimiter "|"
icd <- data %>%
  select(starts_with("p41270"), starts_with("p41280"), "id") %>%
  separate_wider_delim(p41270,
                       delim = "|",
                       names = paste0("p41270var_a", 0:258), too_few = "debug")

# Transform from wide to long format to match ICD-codes with date of diagnosis
icd10_subset <- icd %>%
  select(matches("p41270|p41280|id")) %>%
  pivot_longer(cols = matches("_a[0-9]*$"),
               names_to = c(".value", "a"),
               names_sep = "_")

# Defining dates of NAFLD
icd10_nafld <- icd10_subset %>%
  mutate(icd10_nafld_date = ifelse(str_detect(p41270var, "K76.0"),
                                   as.character(c_across(starts_with("p41280"))),
                                   NA),
         icd10_nafld_date = as.Date(icd10_nafld_date, format = "%Y-%m-%d"))

first_non_na_nafld <- icd10_nafld %>%
  filter(!is.na(icd10_nafld_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_nafld %>% select(id, icd10_nafld_date), by = "id")

# Defining dates of NASH
icd10_nash <- icd10_subset %>%
  mutate(icd10_nash_date = ifelse(str_detect(p41270var, "K75.8"),
                                   as.character(c_across(starts_with("p41280"))),
                                   NA),
         icd10_nash_date = as.Date(icd10_nash_date, format = "%Y-%m-%d"))

first_non_na_nash <- icd10_nash %>%
  filter(!is.na(icd10_nash_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_nash %>% select(id, icd10_nash_date), by = "id")

# Delete old ICD10 diagnosis and dates ------------------------------------
delete <- c("p41280", "p41270")
data <- data %>%
  select(-matches(paste0(delete)))




# ICD9 codes ---------------------------------------------------------
# Split the diagnosis-variable into separate columns based on delimiter "|"
icd9 <- data %>%
  select(starts_with("p41271"), starts_with("p41281"), "id") %>%
  separate_wider_delim(p41271,
                       delim = "|",
                       names = paste0("p41271var_a", 0:46), too_few = "debug")

# Transform from wide to long format to match ICD-codes with date of diagnosis
icd9_subset <- icd9 %>%
  select(matches("p41271|p41281|id")) %>%
  pivot_longer(cols = matches("_a[0-9]*$"),
               names_to = c(".value", "a"),
               names_sep = "_")

# Defining dates of NAFLD
icd9_nafld <- icd9_subset %>%
  mutate(icd9_nafld_date = ifelse(str_detect(p41271var, "5718"),
                                   as.character(c_across(starts_with("p41281"))),
                                   NA),
         icd9_nafld_date = as.Date(icd9_nafld_date, format = "%Y-%m-%d"))

first_non_na_nafld <- icd9_nafld %>%
  filter(!is.na(icd9_nafld_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_nafld %>% select(id, icd9_nafld_date), by = "id")

# Defining dates of NASH
icd9_nash <- icd9_subset %>%
  mutate(icd9_nash_date = ifelse(str_detect(p41271var, "5715"),
                                  as.character(c_across(starts_with("p41281"))),
                                  NA),
         icd9_nash_date = as.Date(icd9_nash_date, format = "%Y-%m-%d"))

first_non_na_nash <- icd9_nash %>%
  filter(!is.na(icd9_nash_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_nash %>% select(id, icd9_nash_date), by = "id")

# Delete old ICD9 diagnosis and dates ------------------------------------
delete <- c("p41281", "p41271")
data <- data %>%
  select(-matches(paste0(delete)))


# time of last completed 24h recall as baseline date
data <- data %>%
  mutate(ques_comp_t0 = p105010_i0,
         ques_comp_t1 = p105010_i1,
         ques_comp_t2 = p105010_i2,
         ques_comp_t3 = p105010_i3,
         ques_comp_t4 = p105010_i4,
         # Removing specific time stamp
         ques_comp_t0 = substr(ques_comp_t0, 1, 10),
         ques_comp_t1 = substr(ques_comp_t1, 1, 10),
         ques_comp_t2 = substr(ques_comp_t2, 1, 10),
         ques_comp_t3 = substr(ques_comp_t3, 1, 10),
         ques_comp_t4 = substr(ques_comp_t4, 1, 10)
  )


# New column with baseline start date as last completed questionnaire
data <- data %>%
  # Convert ques_0 to ques_4 to date format
  mutate(across(starts_with("ques_"), as.Date)) %>%
  # Gather all columns into key-value pairs
  pivot_longer(cols = starts_with("ques_"), names_to = "questionnaire", values_to = "date_filled") %>%
  # Group by participant ID and select the row with the latest date_filled for each participant
  group_by(id) %>%
  slice(which.max(date_filled)) %>%
  ungroup() %>%
  # Rename the remaining column to indicate the last filled questionnaire
  rename(last_filled_questionnaire = questionnaire)


data <- data %>%
  mutate(date_filled = as.Date(date_filled))

remove <- c("p105010_i0", "p105010_i1", "p105010_i2", "p105010_i3","p105010_i4")
data <- data %>%
  select(-matches(remove))


# Define variables for survival analysis ----------------------------------
# Date of death and loss to follow up
data <- data %>%
  mutate(date_of_death = if_else(!is.na(p40000_i0), p40000_i0, p40000_i1),
         date_of_death = as.Date(date_of_death),
         loss_to_follow_up = p191,
         loss_to_follow_up = as.Date(loss_to_follow_up))

# remove p-values
remove <- c("p191", "p40000_i0", "p40000_i1")
data <- data %>%
  select(-matches(remove))

# Defining birth date as origin for survival analysis
# Merging birth year and month of birth into one column:

month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

data <- data %>%
  mutate(month_of_birth_num = sprintf("%02d", match(p52, month_names)))

data <- data %>%
  unite(date_birth, p34, month_of_birth_num, sep = "-")

remove(month_names)


# adding 15 as DD for all participants:
data$date_birth <- as.Date(paste0(data$date_birth, "-15"))


# Set cut-off date for follow-up ------------------------------------------
# Estimated last follow-up date for ICD10 codes (stagnation of diagnoses)
# Create the plot
ggplot(data, aes(x = icd10_nafld_date, y = id)) +
  geom_point() + # Use points to show individual data points
  geom_smooth(method = "lm", se = FALSE) + # Add linear regression line
  annotate("text", x = max(data$icd10_nafld_date), y = min(data$id),
           label = paste("Last observed date:", max(data$icd10_nafld_date)),
           hjust = 1, vjust = -0.5, size = 4) +  # Add annotation for the last observed date
  labs(title = "Dates of Disease Over Time", x = "Date of Disease", y = "Participant ID")

# The density is very high in the right of the plot - I will estimate the last
# diagnosis date in data:

dates <- data %>%
  subset(!is.na(icd10_nafld_date))

# Find the last date of diagnosis
last_date <- max(dates$icd10_nafld_date)

# Print or use the last date as needed
print(last_date)

# Administrative censoring at October 31st, 2022
data <- data %>%
  mutate(censoring = as.Date("2022-10-31"))


# estimate survival time
data <- data %>%
  mutate(
    survival_time_tmp = case_when(
      !is.na(icd10_nafld_date) ~ as.numeric(difftime(icd10_nafld_date, date_birth, units = "days")),
      !is.na(icd10_nash_date) ~ as.numeric(difftime(icd10_nash_date, date_birth, units = "days")),
      !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
      !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
      TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
    ),
    # Use min to get the minimum survival time across columns
    survival_time = pmin(survival_time_tmp, na.rm = TRUE),
    survival_time = survival_time/365.25,
    # Remove temporary variable
    survival_time_tmp = NULL
  )

# binary variable to indicate if nafld happened
data <- data %>%
  mutate(nafld = case_when(
    !is.na(icd10_nafld_date) | !is.na(icd10_nash_date) |
      !is.na(icd9_nafld_date) | !is.na(icd9_nash_date) ~ 1,
    TRUE ~ 0))

# counting and removing those with event before baseline
# defining time in study
data <- data %>%
  mutate(
    survival_time_nafld = case_when(
      !is.na(icd10_nafld_date) ~ as.numeric(difftime(icd10_nafld_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_nash = case_when(
      !is.na(icd10_nash_date) ~ as.numeric(difftime(icd10_nash_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_ltfu = case_when(
      !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_death = case_when(
      !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_cenc = difftime(censoring, date_filled, units = "days"),
    time = pmin(survival_time_death, survival_time_cenc, survival_time_ltfu,
                survival_time_nash, survival_time_nafld, na.rm = TRUE),
    time = time/365.25
  )

# counting and removing those with event before baseline
data_time <- data %>%
  subset(data$time<0)

nafld_nash <-sum(!is.na(data_time$survival_time_nafld)
            | !is.na(data_time$survival_time_nash))

ltfu <- sum(!is.na(data_time$survival_time_ltfu)
            & is.na(data_time$survival_time_nafld)
            & is.na(data_time$survival_time_nash)
            & is.na(data_time$survival_time_death))

death <- sum(!is.na(data_time$survival_time_death)
             & is.na(data_time$survival_time_nafld)
             & is.na(data_time$survival_time_nash)
             & is.na(data_time$survival_time_ltfu))


# remove those with event before baseline
data <- data %>%
  subset(data$time>=0)


# Save data ---------------------------------------------------------------
arrow::write_parquet(data, here("data/data.parquet"))
ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")
