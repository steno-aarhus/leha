#5. Descriptive

# Load packages -----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(magrittr)
library(tidyr)
library(splines)
library(gtsummary)
library(flextable)
library(here)

# Load data
data <- read_csv(here("data/data.csv"))

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

# baseline characteristics across legume intakes
# subset to only include consumers
consumers <- data %>%
  subset(legume_weekly != 0)
# define and print the tertile of legume consumption among consumers
cons <- consumers %>%
  mutate(legume_tert = ntile(legume_weekly, 3))
tertile_breaks <- quantile(cons$legume_weekly, probs = seq(0, 1, 1/3))
print(tertile_breaks)

# include the tertiles in the dataframe
data <- data %>% mutate(
  legume_groups = case_when(legume_weekly == 0 ~ 0,
                            legume_weekly >0 & legume_weekly < 163.3 ~ 1,
                            legume_weekly >= 163.3 & legumes_weekly < 358.75 ~ 2,
                            legume_weekly >= 358.75 ~ 3
  )
)

# making the descriptive table
suppl_table2 <- data %>%
  select(legume_groups, legume_weekly, nafld, age, sex, yearly_income, education,
         deprivation, cohabitation, ethnicity, physical_activity, smoking,
         alcohol_weekly, region, bmi30, related_disease, cancer, disease_family,
         meats_weekly, poultry_weekly, fish_weekly,
         cereal_refined_weekly, whole_grain_weekly, mixed_dish_weekly,
         dairy_weekly, fats_weekly, fruit_weekly, nut_weekly, veggie_weekly,
         potato_weekly, egg_weekly, non_alc_beverage_weekly,
         alc_beverage_weekly, snack_weekly, sauce_weekly, food_weight_weekly) %>%
  tbl_summary(by = legume_groups,
              statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 1,
              missing_text = "n missing") %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Supplementary Table 2. Baseline characteristics across consumption of legumes in the UK Biobank cohort") %>%
  as_flex_table()

flextable::save_as_html(suppl_table2, path = here("doc", "suppl_table2.html"))



# Person-years of follow-up -----------------------------------------------

sum(data$time)
summary(data$time)

# correlation between touchscreen and WebQ
# Estimating correlation between habitual and total food intakes
# spearman
meat_correlation <- cor(data$total_meat, data$habitual_meat, use = "complete.obs", method = c("spearman")) %>% print()
poultry_correlation <- cor(data$total_poultry, data$habitual_poultry, use = "complete.obs", method = c("spearman")) %>% print()
fish_correlation <- cor(data$total_fish, data$habitual_fish, use = "complete.obs", method = c("spearman")) %>% print()
# pearson
meat_correlation <- cor(data$total_meat, data$habitual_meat, use = "complete.obs", method = c("pearson")) %>% print()
poultry_correlation <- cor(data$total_poultry, data$habitual_poultry, use = "complete.obs", method = c("pearson")) %>% print()
fish_correlation <- cor(data$total_fish, data$habitual_fish, use = "complete.obs", method = c("pearson")) %>% print()
