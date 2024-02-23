#3. Descriptive

# Load packages -----------------------------------------------------------


install.packages(glue)
library(dplyr)
library(magrittr)
library(tidyr)
library(splines)
install.packages("openxlsx")
library(openxlsx)
install.packages("gtsummary")
library(gtsummary)
install.packages("flextable")
library(flextable)


# Load data ---------------------------------------------------------------
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))



# Table 1 -----------------------------------------------------------------




data <- data %>%
  mutate(nafld = case_when(
    !is.na(icd10_nafld_date) | !is.na(icd10_nash_date) |
      !is.na(icd9_nafld_date) | !is.na(icd9_nash_date) ~ 1,
    TRUE ~ 0))

table1 <- data %>%
  select(nafld, age, sex, yearly_income, education, deprivation, cohabitation, ethnicity, physical_activity, smoking, alcohol_daily, region, bmi30, diabetes, non_cancer_illness, cancer, family_illness) %>%
  tbl_summary(by = nafld,
              statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 1.") %>%
  as_flex_table()

save_as_html(table1, path = here("data", "table1.html"))

