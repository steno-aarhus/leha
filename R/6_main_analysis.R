# 6. Main analyses

# Load packages
library(tidyverse)
library(broom)
library(Hmisc)
library(survival)
library(gtsummary)
library(ggsurvfit)
library(ggplot2)
library(dplyr)
library(here)


# Load data --------------------------------------------------------
# targets::tar_make()
# # Restart session
# source(here::here("R/1_data_start.R"))

# Weekly substituting 80 g legumes (NHS 1 portion beans = 80 g) https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# defining 80 g/week variable for each food
data <- data %>%
  mutate(
    legumes80 = legume_weekly / 80,
    meats80 = meats_weekly / 80,
    poultry80 = poultry_weekly / 80,
    fish80 = fish_weekly / 80
  )

create_formula <- function(xvars, covars) {
  outcome <- "Surv(survival_time, nafld == 1)"
  reformulate(c(xvars, covars), response = outcome)
}

# model 1 -----------------------------------------------------------------

covariates_model1 <- (~ # other food components
                        cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                        dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                        veggie_weekly + potato_weekly + egg_weekly +
                        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                        sauce_weekly + food_weight_weekly +
                        # baseline strata
                        strata(region, age_strata, sex)) |>
  all.vars()

model1_formulas <- list(
  meat_model1 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model1),
  poultry_model1 = create_formula(c("legumes80","meats80", "fish80"), covariates_model1),
  fish_model1 = create_formula(c("legumes80", "meats80", "poultry80"), covariates_model1)
)

model1_results <- model1_formulas |>
  map(~ coxph(.x, data = data, ties = "breslow")) |>
  map2(names(model1_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
         mutate(across(where(is.numeric), ~ round(.x, 2))) |>
         mutate(model = .y)) |>
  print()

  # print(model_results$meat_model1[1,2][1,6][1,7]) # not running


# model 2 -----------------------------------------------------------------

covariates_model2 <- (~ # other food components
                        cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                        dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                        veggie_weekly + potato_weekly + egg_weekly +
                        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                        sauce_weekly + food_weight_weekly +
                        # other variables
                        alc_spline + ethnicity + deprivation + education +
                        cohabitation + physical_activity + smoking +
                        related_disease + disease_family + yearly_income +
                        # baseline strata
                        strata(region, age_strata, sex)) |>
  all.vars()

model2_formulas <- list(
  meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model2),
  poultry_model2 = create_formula(c("legumes80","meats80", "fish80"), covariates_model2),
  fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covariates_model2)
)

model2_results <- model2_formulas |>
  map(~ coxph(.x, data = data, ties = "breslow")) |>
  map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
         mutate(across(where(is.numeric), ~ round(.x, 2))) |>
         mutate(model = .y)) |>
  print()


# model 3 -----------------------------------------------------------------
covariates_model3 <- (~ # other food components
                        cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                        dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                        veggie_weekly + potato_weekly + egg_weekly +
                        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                        sauce_weekly + food_weight_weekly +
                        # other variables
                        alc_spline + ethnicity + deprivation + education +
                        cohabitation + physical_activity + smoking +
                        related_disease + disease_family + yearly_income +
                        # anthropometry
                        bmi30 +
                        # baseline strata
                        strata(region, age_strata, sex)) |>
  all.vars()

model3_formulas <- list(
  meat_model3 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model3),
  poultry_model3 = create_formula(c("legumes80","meats80", "fish80"), covariates_model3),
  fish_model3 = create_formula(c("legumes80", "meats80", "poultry80"), covariates_model3)
)

model3_results <- model3_formulas |>
  map(~ coxph(.x, data = data, ties = "breslow")) |>
  map2(names(model3_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
         mutate(across(where(is.numeric), ~ round(.x, 2))) |>
         mutate(model = .y)) |>
  print()




# Model assumptions proportional hazards ------------------------------------------
# Schoenfeld residuals
data <- data %>%
  mutate(
    legumes80 = legume_weekly / 80,
    meats80 = meats_weekly / 80,
    poultry80 = poultry_weekly / 80,
    fish80 = fish_weekly / 80
  )

meat_model2 <- coxph(
  Surv(survival_time, nafld == 1) ~
    # removing meat
    legumes80 + poultry80 + fish80 +
    # other food components
    cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
    dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
    veggie_weekly + potato_weekly + egg_weekly +
    non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
    sauce_weekly + food_weight_weekly +
    # other variables
    alc_spline + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata, sex),
  data = data, ties = "breslow"
)

ph_test1 <- cox.zph(meat_model2)
print(ph_test1)

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
    sauce_weekly + food_weight_weekly +
    # other variables
    alc_spline + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata, sex),
  data = data, ties = "breslow"
)

ph_test2 <- cox.zph(poultry_model2)
print(ph_test2)

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
    sauce_weekly + food_weight_weekly +
    # other variables
    alc_spline + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata, sex),
  data = data, ties = "breslow"
)

ph_test3 <- cox.zph(fish_model2)
print(ph_test3)
