#7. Sensitivity analyses

# Load packages
library(tidyverse)
library(Hmisc)
library(survival)
library(ggsurvfit)
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(broom)

# Load data --------------------------------------------------------
# targets::tar_make()
# # Restart session
# source(here::here("R/1_data_start.R"))

# Legumes including peas ------------------------------
data <- data %>%
  mutate(legumepea80 = legume_pea_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)

create_formula <- function(xvars, covars) {
  outcome <- "Surv(survival_time, nafld == 1)"
  reformulate(c(xvars, covars), response = outcome)
}

covariates_pea <- (~ # other food components
                        cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                        dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                        veggie_pea_weekly + potato_weekly + egg_weekly +
                        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                        sauce_weekly + food_weight_weekly +
                        # other variables
                        alc_spline + ethnicity + deprivation + education +
                        cohabitation + physical_activity + smoking +
                        related_disease + disease_family + yearly_income +
                        # baseline strata
                        strata(region, age_strata, sex)) |>
  all.vars()

model2_formulas_pea <- list(
  meat_model2 = create_formula(c("legumepea80", "poultry80", "fish80"), covariates_pea),
  poultry_model2 = create_formula(c("legumepea80","meats80", "fish80"), covariates_pea),
  fish_model2 = create_formula(c("legumepea80", "meats80", "poultry80"), covariates_pea)
)

model2_results_pea <- model2_formulas_pea |>
  map(~ coxph(.x, data = data, ties = "breslow")) |>
  map2(names(model2_formulas_pea), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
         mutate(across(where(is.numeric), ~ round(.x, 2))) |>
         mutate(model = .y)) |>
  print()



# Removing soymilk from legumes -----------------------------------------------
data <- data %>%
  mutate(legumenosoy80 = legume_no_soymilk/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)


covariates_nosoy <- (~ # other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly +
                       non_alc_beverage_soymilk_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + food_weight_weekly +
                       # other variables
                       alc_spline + ethnicity + deprivation + education +
                       cohabitation + physical_activity + smoking +
                       related_disease + disease_family + yearly_income +
                       # baseline strata
                       strata(region, age_strata, sex)) |>
  all.vars()

model2_formulas_nosoy <- list(
  meat_model2 = create_formula(c("legumenosoy80", "poultry80", "fish80"), covariates_nosoy),
  poultry_model2 = create_formula(c("legumenosoy80","meats80", "fish80"), covariates_nosoy),
  fish_model2 = create_formula(c("legumenosoy80", "meats80", "poultry80"), covariates_nosoy)
)

model2_results_nosoy <- model2_formulas_nosoy |>
  map(~ coxph(.x, data = data, ties = "breslow")) |>
  map2(names(model2_formulas_nosoy), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
         mutate(across(where(is.numeric), ~ round(.x, 2))) |>
         mutate(model = .y)) |>
  print()



# Varying 24h recalls -----------------------------------------------------
data <- data %>%
  mutate(legumes80 = legume_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)

data3 <- data %>%
  subset(p20077>=3)

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

model2_results_3recalls <- model2_formulas |>
  map(~ coxph(.x, data = data3, ties = "breslow")) |>
  map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
         mutate(across(where(is.numeric), ~ round(.x, 2))) |>
         mutate(model = .y)) |>
  print()



# Removing high ALT from analysis-----------------------------------------------

# Subsetting data
normal_liver <- data %>%
  subset(alt < 40)

## Running main analysis on subsample --------------------------------------
normal_liver <- normal_liver %>%
  mutate(legumes80 = legume_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)

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

model2_results_liver <- model2_formulas |>
  map(~ coxph(.x, data = normal_liver, ties = "breslow")) |>
  map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
         mutate(across(where(is.numeric), ~ round(.x, 2))) |>
         mutate(model = .y)) |>
  print()


# Remove high alcohol intake --------------------------------------------------
percentile_90 <- quantile(data$alcohol_weekly, probs = 0.90, na.rm = TRUE)
lower_alc <- data %>%
  subset(alcohol_weekly < percentile_90)

## main analysis model 2 on subset -----------------------------------------------------------------
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

model2_results_alc <- model2_formulas |>
  map(~ coxph(.x, data = lower_alc, ties = "breslow")) |>
  map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
         mutate(across(where(is.numeric), ~ round(.x, 2))) |>
         mutate(model = .y)) |>
  print()
