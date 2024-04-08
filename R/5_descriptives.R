#3. Descriptive

# Load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(magrittr)
library(tidyr)
library(splines)
library(gtsummary)
library(flextable)

# Load data ---------------------------------------------------------------
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))

# Table 1 -----------------------------------------------------------------
table1 <- data %>%
  select(nafld, age, sex, yearly_income, education, deprivation, cohabitation, ethnicity, physical_activity, smoking, alcohol_daily, region, bmi30, diabetes, non_cancer_illness, cancer, family_illness) %>%
  tbl_summary(by = nafld,
              statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 1. Baseline characteristics of participants in the UK Biobank Cohort") %>%
  as_flex_table()

# flextable::save_as_html(table1, path = here("doc", "table1.html"))
flextable::save_as_docx(table1, path = here("doc", "table1.docx"))

# Counting missing information on covariates
filtered_data <- subset(data, !is.na(age_strata))
filtered_data <- subset(filtered_data, !is.na(region))
filtered_data <- subset(filtered_data, !is.na(sex))
filtered_data <- subset(filtered_data, !is.na(ethnicity)) #126770
filtered_data <- subset(filtered_data, !is.na(deprivation)) #126621
filtered_data <- subset(filtered_data, !is.na(education)) #126261
filtered_data <- subset(filtered_data, !is.na(cohabitation)) # 125641
filtered_data <- subset(filtered_data, !is.na(physical_activity)) # no change
filtered_data <- subset(filtered_data, !is.na(smoking)) # no change
filtered_data <- subset(filtered_data, !is.na(related_disease)) # no change
filtered_data <- subset(filtered_data, !is.na(disease_family)) # no change
filtered_data <- subset(filtered_data, !is.na(yearly_income)) # no change
summary(filtered_data$sex=="Male")
filtered_data <- subset(filtered_data, !is.na(bmi30)) #123821
summary(filtered_data$sex=="Male")

# Table 1 after exclusions-----------------------------------------------------------------
table1_excl <- filtered_data %>%
  select(nafld, age, sex, yearly_income, education, deprivation, cohabitation, ethnicity, physical_activity, smoking, alcohol_daily, region, bmi30, diabetes, non_cancer_illness, cancer, family_illness) %>%
  tbl_summary(by = nafld,
              statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 1. Baseline characteristics of participants in the UK Biobank Cohort") %>%
  as_flex_table()

flextable::save_as_html(table1_excl, path = here("doc", "table1_excl.html"))
flextable::save_as_docx(table1_excl, path = here("doc", "table1_excl.docx"))

#save data without outliers
data <- filtered_data
arrow::write_parquet(data, here("data/data.parquet"))

# Upload to the project RAP folder.
ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")
