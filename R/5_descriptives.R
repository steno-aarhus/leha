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
  select(nafld, age, sex, yearly_income, education, deprivation, cohabitation, ethnicity, physical_activity, smoking, alcohol_daily, region, bmi30, related_disease, cancer, disease_family) %>%
  tbl_summary(by = nafld,
              statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 1. Baseline characteristics of participants in the UK Biobank Cohort") %>%
  as_flex_table()

flextable::save_as_html(table1, path = here("doc", "table1.html"))
flextable::save_as_docx(table1, path = here("doc", "table1.docx"))

# food group intakes
table_foods <- data %>%
  select(nafld, legume_weekly, meats_weekly, poultry_weekly, fish_weekly,
         cereal_refined_weekly, whole_grain_weekly, mixed_dish_weekly,
         dairy_weekly, fats_weekly, fruit_weekly, nut_weekly, veggie_weekly,
         potato_weekly, egg_weekly, non_alc_beverage_weekly,
         alc_beverage_weekly, snack_weekly, sauce_weekly, weight_weekly) %>%
  tbl_summary(by = nafld,
              statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table X. Median and 10-90 percentiles of weekly intake of food groups included in substitution analyses") %>%
  as_flex_table()

flextable::save_as_html(table_foods, path = here("doc", "table_foods.html"))



# Person-years of follow-up -----------------------------------------------

sum(data$time)
summary(data$time)
