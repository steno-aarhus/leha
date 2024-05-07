# 11. population attributable fractions (PAF)

# load packages
library(splines)
library(survival)
library(graphPAF)
library(tidyverse)
library(broom)
library(Hmisc)
library(survival)
library(gtsummary)
library(ggsurvfit)
library(ggplot2)
library(dplyr)
library(here)

# load data
# Load data --------------------------------------------------------
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))


# main analyses
data <- data %>%
  mutate(
    legumes80 = legume_weekly / 80,
    meats80 = meats_weekly / 80,
    poultry80 = poultry_weekly / 80,
    fish80 = fish_weekly / 80
  )

# meats
meat_model2 <- coxph(
  Surv(survival_time, nafld == 1) ~
    # removing meat
    legumes80 + poultry80 + fish80 +
    # other food components
    cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
    dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
    veggie_weekly + potato_weekly + egg_weekly +
    non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
    sauce_weekly + weight_weekly +
    # other variables
    sex +
    alcohol_weekly + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata),
  data = data, ties = "breslow"
)

# poultry
poultry_model2 <- coxph(
  Surv(survival_time, nafld == 1) ~
    # removing meat
    legumes80 + meats80 + fish80 +
    # other food components
    cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
    dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
    veggie_weekly + potato_weekly + egg_weekly +
    non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
    sauce_weekly + weight_weekly +
    # other variables
    sex +
    alcohol_weekly + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata),
  data = data, ties = "breslow"
)

# fish
fish_model2 <- coxph(
  Surv(survival_time, nafld == 1) ~
    # removing meat
    legumes80 + meats80 + poultry80 +
    # other food components
    cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
    dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
    veggie_weekly + potato_weekly + egg_weekly +
    non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
    sauce_weekly + weight_weekly +
    # other variables
    sex +
    alcohol_weekly + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata),
  data = data, ties = "breslow"
)


# PAF
paf_meat <- PAF_calc_continuous(model=meat_model2, riskfactor_vec = c("legumes80"), # how to add meat?
                                q_vec = c(0.01), calculation_method = "D",
                                data = data, ci = TRUE, boot_rep = 50, verbose=TRUE,
                                ci_level = 0.95, ci_type = c("norm"),
                                t_vector=65) #how is time added in model? from baseline or from birth?
