# Only using 2 recalls to maximize number of events

# 1. change /targets.R to "csv", run targets::tar_make(), and convert to parquet (without saving data on RAP)
# 2. run 1_data_start.R
# 3. run 2_data_management.R (without saving data on RAP)
# 4. run 3_diet_data.R (without saving data on RAP)

# 5. run 4_icd10_outcomes_tte.R code with some moderations (below)
# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)
library(here)
library(broom)
library(survival)
library(ggsurvfit)
library(ggplot2)


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

#OLD CODE
# data <- data %>%
#   # Convert ques_0 to ques_4 to date format
#   mutate(across(starts_with("ques_"), as.Date)) %>%
#   # Gather all columns into key-value pairs
#   pivot_longer(cols = starts_with("ques_"), names_to = "questionnaire", values_to = "date_filled") %>%
#   # Group by participant ID and select the row with the latest date_filled for each participant
#   group_by(id) %>%
#   slice(which.max(date_filled)) %>%
#   ungroup() %>%
#   # Rename the remaining column to indicate the last filled questionnaire
#   rename(last_filled_questionnaire = questionnaire)

#NEW CODE
data <- data %>%
  # Convert ques_0 to ques_4 to date format
  mutate(across(starts_with("ques_"), as.Date)) %>%
  # Gather all columns into key-value pairs
  pivot_longer(cols = starts_with("ques_"), names_to = "questionnaire", values_to = "date_filled") %>%
  # Group by participant ID and select the row with the second latest date_filled for each participant
  group_by(id) %>%
  arrange(id, desc(date_filled)) %>%  # Sort by date within each participant in descending order
  slice(2) %>%  # Select the second entry (second latest date_filled) for each participant
  ungroup() %>%
  # Rename the remaining column to indicate the second filled questionnaire
  rename(second_filled_questionnaire = questionnaire)


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

# Run analyses
# model 2 -----------------------------------------------------------------

# meats
meat_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly +
                       #other variables
                       age + region + sex +
                       alcohol_weekly + ethnicity + deprivation + education +
                       cohabitation + physical_activity + smoking +
                       related_disease + disease_family + yearly_income,
                     data = data, ties='breslow')

meat_model2 <- tidy(meat_model2, exponentiate = TRUE, conf.int = TRUE, digits = 2) # 2 digits doesn't work

# poultry
poultry_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly +
                          #other variables
                          age + region + sex +
                          alcohol_weekly + ethnicity + deprivation + education +
                          cohabitation + physical_activity + smoking +
                          related_disease + disease_family + yearly_income,
                        data = data, ties='breslow')

poultry_model2 <- tidy(poultry_model2, exponentiate = TRUE, conf.int = TRUE, digits = 2)


# fish
fish_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly +
                       #other variables
                       age + region + sex +
                       alcohol_weekly + ethnicity + deprivation + education +
                       cohabitation + physical_activity + smoking +
                       related_disease + disease_family + yearly_income,
                     data = data, ties='breslow')

fish_model2 <- tidy(fish_model2, exponentiate = TRUE, conf.int = TRUE, digits = 2)


