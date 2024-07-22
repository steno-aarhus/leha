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
# data <- read_csv(here("data/data.csv"))
before_outcome <- data
data <- before_outcome

# ICD10 codes ---------------------------------------------------------
icd10_diagnoses <- function(data) {
  icd10_subset <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), "id") %>%

    # splitting diagnoses string-variable each time a | is in the string
    separate_wider_delim(p41270, delim = "|", names = paste0("p41270var_a", 0:258), too_few = "debug") %>%
    select(matches("p41270|p41280|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%

    # creating outcome variables with date info from p41280
    mutate(
      #NAFLD
      icd10_nafld_date = ifelse(str_detect(p41270var, "K76.0"), as.character(c_across(starts_with("p41280"))), NA),
      icd10_nafld_date = as.Date(icd10_nafld_date, format = "%Y-%m-%d"),
      #NASH
      icd10_nash_date = ifelse(str_detect(p41270var, "K75.8"), as.character(c_across(starts_with("p41280"))), NA),
      icd10_nash_date = as.Date(icd10_nash_date, format = "%Y-%m-%d")) %>%

    # retrieve first diagnosis date
    select(id, icd10_nafld_date, icd10_nash_date) %>%
    pivot_longer(cols = starts_with("icd10_"), names_to = "condition", values_to = "date") %>%
    filter(!is.na(date)) %>%
    group_by(id, condition) %>%
    slice(1) %>%
    pivot_wider(names_from = condition, values_from = date) %>%
    ungroup()

  data <- data %>%
    left_join(icd10_subset, by = "id")

  return(data)
}
data <- icd10_diagnoses(data)


# Delete old ICD10 diagnosis and dates ------------------------------------
delete <- c("p41280", "p41270")
data <- data %>%
  select(-matches(paste0(delete)))


# ICD9 diagnoses codes
icd9_diagnoses <- function(data) {
  icd9_subset <- data %>%
    select(starts_with("p41271"), starts_with("p41281"), "id") %>%
    # splitting diagnoses string-variable each time a | is in the string
    separate_wider_delim(p41271, delim = "|", names = paste0("p41271var_a", 0:46), too_few = "debug") %>%
    select(matches("p41271|p41281|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%

    # creating outcome variables with date info from p41281
    mutate(
      # NAFLD
      icd9_nafld_date = ifelse(str_detect(p41271var, "5718"), as.character(c_across(starts_with("p41281"))), NA),
      icd9_nafld_date = as.Date(icd9_nafld_date, format = "%Y-%m-%d"),
      # NASH
      icd9_nash_date = ifelse(str_detect(p41271var, "5715"), as.character(c_across(starts_with("p41281"))), NA),
      icd9_nash_date = as.Date(icd9_nash_date, format = "%Y-%m-%d")) %>%

    # retrieve first diagnosis date
    select(id, icd9_nafld_date, icd9_nash_date) %>%
    pivot_longer(cols = starts_with("icd9_"), names_to = "condition", values_to = "date") %>%
    filter(!is.na(date)) %>%
    group_by(id, condition) %>%
    slice(1) %>%
    pivot_wider(names_from = condition, values_from = date) %>%
    ungroup()

  data <- data %>%
    left_join(icd9_subset, by = "id")

  return(data)
}

data <- icd9_diagnoses(data)


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


# New column with baseline start date set as last completed questionnaire
baseline_date <- function(data) {
  baseline_start_date <- data %>%
    select(p20077, starts_with("ques_comp_t"), id) %>%
    pivot_longer(
      cols = starts_with("ques_comp_t"),
      names_to = "instance",
      values_to = "completion_date"
    ) %>%
    filter(!is.na(completion_date)) %>%
    group_by(id) %>%
    arrange(completion_date, .by_group = TRUE) %>%
    slice_tail() %>%
    rename(baseline_start_date = completion_date) %>%
    ungroup() %>%
    select(id, baseline_start_date)
  data <- data %>%
    left_join(baseline_start_date, by = "id")

  data <- data %>%
    filter(!is.na(baseline_start_date))

  return(data)
}

data <- baseline_date(data)

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

# # remove p-values
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
# Estimated last follow-up date for
clean_data <- data %>%
  filter(!is.na(icd10_nafld_date), !is.na(id))  # Remove rows with NA values
last_date <- max(clean_data$icd10_nafld_date) %>% print()

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
    !is.na(icd10_nafld_date) | !is.na(icd10_nash_date)  ~ 1,
    TRUE ~ 0))

# counting and removing those with event before baseline
# defining time in study
data <- data %>%
  mutate(
    survival_time_nafld = case_when(
      !is.na(icd10_nafld_date) ~ as.numeric(difftime(icd10_nafld_date, baseline_start_date, units = "days")),
      TRUE ~ NA),
    survival_time_nash = case_when(
      !is.na(icd10_nash_date) ~ as.numeric(difftime(icd10_nash_date, baseline_start_date, units = "days")),
      TRUE ~ NA),
    survival_time_ltfu = case_when(
      !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, baseline_start_date, units = "days")),
      TRUE ~ NA),
    survival_time_death = case_when(
      !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, baseline_start_date, units = "days")),
      TRUE ~ NA),
    survival_time_cenc = difftime(censoring, baseline_start_date, units = "days"),
    time = pmin(survival_time_death, survival_time_cenc, survival_time_ltfu,
                survival_time_nash, survival_time_nafld, na.rm = TRUE),
    time = time/365.25
  )
######################################################
# counting and removing those with event before baseline

data_time <- data %>%
  filter(data$time<=0)

#those with event before baseline
nafld_nash <-sum(!is.na(data_time$survival_time_nafld)
            | !is.na(data_time$survival_time_nash)) %>%
  print()

ltfu_or_dead <- sum(!is.na(data_time$survival_time_ltfu)
            | !is.na(data_time$survival_time_death)
            & is.na(data_time$survival_time_nafld)
            & is.na(data_time$survival_time_nash)
            & is.na(data_time$survival_time_death)) %>%
  print()

# removing those with no time in study
data <- data %>%
  subset(data$time>0)

table(data$sex)
# Save data ---------------------------------------------------------------
# arrow::write_parquet(data, here("data/data.parquet"))
# ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")
