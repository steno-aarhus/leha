# This script will split p41270 (ICD10-codes) into columns and match the date of
# diagnosis with the diagnosis code for that specific data. This is useful when
# using ICD10 diagnoses as outcomes in time-to-event analyses

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


# Save data ---------------------------------------------------------------
arrow::write_parquet(data, here("data/data.parquet"))


# Delete old ICD10 diagnosis and dates ------------------------------------
delete <- c("p41280", "p41270")
data <- data %>%
  select(-matches(paste0(delete)))

# Save data ---------------------------------------------------------------
arrow::write_parquet(data, here("data/data.parquet"))



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

# Save data ---------------------------------------------------------------
arrow::write_parquet(data, here("data/data.parquet"))
