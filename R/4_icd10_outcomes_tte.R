# This script will split p41270 (ICD10-codes) and p41271 (ICD9-codes) into
# columns and match the date of diagnosis with the diagnosis code for that
# specific data. This is useful when using ICD10 diagnoses as outcomes in
# time-to-event analyses

# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)
library(here)


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



# Date of death
data <- data %>%
  mutate(date_of_death = if_else(!is.na(p40000_i0), p40000_i0, p40000_i1),
         date_of_death = as.Date(date_of_death),
         loss_to_follow_up = p191,
         loss_to_follow_up = as.Date(loss_to_follow_up))

# Setting baseline age at last questionnaire completed --------------------

# Merging birth year and month of birth into one column:
data <- data %>%
  mutate(month_of_birth = p52,
         year_of_birth = p34,
         ques_comp_t0 = p105010_i0,
         ques_comp_t1 = p105010_i1,
         ques_comp_t2 = p105010_i2,
         ques_comp_t3 = p105010_i3,
         ques_comp_t4 = p105010_i4
  )
months <- c("January", "February", "March", "April", "May", "June",
            "July", "August", "September", "October", "November", "December")

data <- data %>%
  mutate(month_of_birth_num = sprintf("%02d", match(month_of_birth, months)))

data <- data %>%
  unite(birth, year_of_birth, month_of_birth_num, sep = "-")

remove(months)
data <- data %>%
  select(-month_of_birth)

# adding 15 as birth date for all participants:

data$birth <- as.Date(paste0(data$birth, "-15"))

# Removing specific time stamp from date of completed questionnaires:
data <- data %>%
  mutate(ques_comp_t0 = substr(ques_comp_t0, 1, 10),
         ques_comp_t1 = substr(ques_comp_t1, 1, 10),
         ques_comp_t2 = substr(ques_comp_t2, 1, 10),
         ques_comp_t3 = substr(ques_comp_t3, 1, 10),
         ques_comp_t4 = substr(ques_comp_t4, 1, 10)
  )

# New column with baseline start date as last completed questionnaire
data <- data %>%
  # Gather questionnaire dates into long format
  pivot_longer(cols = starts_with("ques_comp_t"),
               names_to = "questionnaire",
               values_to = "completion_date") %>%
  # Remove rows with NA completion dates
  filter(!is.na(completion_date)) %>%
  group_by(id) %>%
  # Arrange completion date for each participant
  arrange(completion_date) %>%
  # Create a lagged column to find the last completed questionnaire
  mutate(last_questionnaire_date = lag(completion_date)) %>%
  # Keep only the last completed questionnaire for each participant
  filter(is.na(lead(completion_date))) %>%
  # Rename the columns to match the desired output
  rename(baseline_start_date = completion_date) %>%
  select(-starts_with("ques_comp_t"))

# Creating age at baseline
data <- data %>%
  mutate(age_at_baseline = year(baseline_start_date) - year(birth) -
           ifelse(month(baseline_start_date) < month(birth) |
                    (month(baseline_start_date) == month(birth) &
                       day(baseline_start_date) < day(birth)), 1, 0))
# Save data ---------------------------------------------------------------
arrow::write_parquet(data, here("data/data.parquet"))
ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")
