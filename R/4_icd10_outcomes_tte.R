# This script will split p41270 (ICD10-codes) into columns and match the date of
# diagnosis with the diagnosis code for that specific data. This is useful when
# using ICD10 diagnoses as outcomes in time-to-event analyses

# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)
library(here)

# Load data ---------------------------------------------------------------
sorted_data <- ukbAid::read_parquet(here("data/sorted_data.parquet"))
# # Converting the dataset into a tibble to work with for analyses
sorted_data <-tibble::as_tibble(sorted_data)

# Full data frame ---------------------------------------------------------
# Split the diagnosis-variable into separate columns based on delimiter "|"
icd <- sorted_data %>%
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

sorted_data <- sorted_data %>%
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

sorted_data <- sorted_data %>%
  left_join(first_non_na_nash %>% select(id, icd10_nash_date), by = "id")


# Save data ---------------------------------------------------------------
arrow::write_parquet(sorted_data, here("data/sorted_data.parquet"))


# Delete old ICD10 diagnosis and dates ------------------------------------
delete <- c("p41280", "p41270")
sorted_data <- sorted_data %>%
  select(-matches(paste0(delete)))

# Save data ---------------------------------------------------------------
arrow::write_parquet(sorted_data, here("data/sorted_data.parquet"))


# Set cut-off date for follow-up ------------------------------------------
# Estimated last follow-up date for ICD10 codes (stagnation of diagnoses)
proportionen for hvornår der ikke kommer flere bælgfrugtsindtag
Den første måling (folk nogensinde har) og antallet der spiser bælgfrugter?
  % der spiser bælg - vælge start ud fra stagnering
JA % - 25 - 30 - 40 - 40 -
  Nej

NASH, K75.8
NAFLD, K76.0,
