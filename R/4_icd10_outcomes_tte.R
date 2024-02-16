# This script will split p41270 (ICD10-codes) into columns and match the date of
# diagnosis with the diagnosis code for that specific data. This is useful when
# using ICD10 diagnoses as outcomes in time-to-event analyses

# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)

# Load sorted_data ---------------------------------------------------------------
sorted_data <- ukbAid::read_parquet(here("data/sorted_data.parquet"))
# # Converting the dataset into a tibble to work with for analyses
sorted_data <-tibble::as_tibble(sorted_data)

# On a subset of data -----------------------------------------------------
# subset of data for overview
test <- sorted_data %>%
  slice(115000:120000) %>%
  select(starts_with("p41270"), starts_with("p41280"), "id") %>%
# Split the diagnosis-variable into separate column based on delimiter "|"
  separate_wider_delim(p41270,
                       delim = "|",
                       names = paste0("p41270var_a", 0:258), too_few = "debug")

# Transform from wide to long format to match ICD-codes with date of diagnosis
icd10_subset <- test %>%
  select(matches("p41270|p41280|id")) %>%
  pivot_longer(cols = matches("_a[0-9]*$"),
               names_to = c(".value", "a"),
               names_sep = "_")

# Filter for relevant rows and convert dates
icd10_nafld <- icd10_subset %>%
  mutate(icd10_nafld_date = ifelse(str_detect(p41270var, "K76.0"), as.character(p41280), NA),
         icd10_nafld_date = as.Date(icd10_nafld_date, format = "%Y-%m-%d"))

# Now you can merge the 'icd10_nafld_date' back into the original wide-format data frame
# Aggregate icd10_nafld by participant ID to match the number of rows in test
aggregated_icd10_nafld <- icd10_nafld %>%
  group_by(id) %>%
  summarise(icd10_nafld_date = first(icd10_nafld_date))  # Or use another summarization method

# Now you can merge the 'aggregated_icd10_nafld' back into the 'test' data frame
merged_data <- test %>%
  left_join(aggregated_icd10_nafld, by = "id")


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

# Filter for relevant rows and convert dates
icd10_nafld <- icd10_subset %>%
  mutate(icd10_nafld_date = ifelse(str_detect(p41270var, "K76.0"), as.character(p41280), NA),
         icd10_nafld_date = as.Date(icd10_nafld_date, format = "%Y-%m-%d"))

# Now you can merge the 'icd10_nafld_date' back into the original wide-format data frame
# Aggregate icd10_nafld by participant ID to match the number of rows in test
aggregated_icd10_nafld <- icd10_nafld %>%
  group_by(id) %>%
  summarise(icd10_nafld_date = first(icd10_nafld_date))  # Or use another summarization method

# Now you can merge the 'aggregated_icd10_nafld' back into the 'test' data frame
merged_data <- sorted_data %>%
  left_join(aggregated_icd10_nafld, by = "id")

# Set cut-off date for follow-up ------------------------------------------
# Estimated last follow-up date for ICD10 codes (stagnation of diagnoses)
proportionen for hvornår der ikke kommer flere bælgfrugtsindtag
Den første måling (folk nogensinde har) og antallet der spiser bælgfrugter?
  % der spiser bælg - vælge start ud fra stagnering
JA % - 25 - 30 - 40 - 40 -
  Nej

NASH, K75.8
NAFLD, K76.0,
