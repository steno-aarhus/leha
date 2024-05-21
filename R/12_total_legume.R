# 12. HR for NAFLD for each increase in portions of legumes/week

# load packages
library(survival)
library(tidyverse)
library(broom)
library(ggsurvfit)
library(dplyr)
library(here)

# Load data
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))

# main analyses
data <- data %>%
  mutate(
    legumes80 = legume_weekly / 80,
  )

# model 2
total_model2 <- coxph(
  Surv(survival_time, nafld == 1) ~ legumes80 +
    # removing meat
    # other food components
    cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
    dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
    veggie_weekly + potato_weekly + egg_weekly +
    non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
    sauce_weekly + meats_weekly + poultry_weekly + fish_weekly+
    # other variables
    sex +
    alcohol_weekly + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata),
  data = data, ties = "breslow"
)
total <- tidy(total_model2, exponentiate = TRUE, conf.int = TRUE)

