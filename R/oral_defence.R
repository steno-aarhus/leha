#For oral defence

#NAFLD diagnosed from proton density fat fraction (PDFF) - data field 40061

#PDFF of 5.4 or higher indicate mild steatosis. This threshold will be used in main
# analysis, model 2, as outcome (https://pmc.ncbi.nlm.nih.gov/articles/PMC8108484/)
# will run for 2014 and 2019 PDFF measures separately

# Make PDFF binary cutting at 5.4 -----------------------------------------
library(magrittr)
library(dplyr)
library(survival)
library(broom)
library(purrr)

oral_defence <- sorted_data %>% mutate(
  pdff54_2014 = ifelse (p40061_i2 >= 5.4, 1, 0),
  pdff54_2014 = as.numeric(pdff54_2014),
  pdff54_2019 = ifelse (p40061_i3 >= 5.4, 1, 0),
  pdff54_2019 = as.numeric(pdff54_2019)
)


# Analyses
create_formula <- function(xvars, covars) {
  outcome <- "Surv(survival_time, pdff54_2014 == 1)"
  reformulate(c(xvars, covars), response = outcome)
}

define_exposure_variables <- function(data) {
  data <- data %>%
    mutate(
      legumes80 = legume_weekly / 80,
      meats80 = meats_weekly / 80,
      poultry80 = poultry_weekly / 80,
      fish80 = fish_weekly / 80
    )
  return(data)
}

# Main analyses, 2014 measure -----------------------------------------------------------
model2_pdff14<- function(data) {
  covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
               "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
               "veggie_weekly", "potato_weekly", "egg_weekly",
               "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
               "sauce_weekly", "food_weight_weekly", "alc_spline", "ethnicity",
               "deprivation", "education", "cohabitation", "physical_activity",
               "smoking", "related_disease", "disease_family", "yearly_income",
               "strata(region, age_strata, sex)")


  model2_formulas <- list(
    meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
    poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
    fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
  )

  model2_results <- model2_formulas |>
    purrr::map(~ coxph(.x, data = data, ties = "breslow")) |>
    purrr::map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
           mutate(across(where(is.numeric), ~ round(.x, 2))) |>
           mutate(model = .y))

  return(model2_results)
}

results14 <- model2_pdff14(oral_defence)

# Main analyses 2019 measure------------------------------------------------------
create_formula <- function(xvars, covars) {
  outcome <- "Surv(survival_time, pdff54_2019 == 1)"
  reformulate(c(xvars, covars), response = outcome)
}

define_exposure_variables <- function(data) {
  data <- data %>%
    mutate(
      legumes80 = legume_weekly / 80,
      meats80 = meats_weekly / 80,
      poultry80 = poultry_weekly / 80,
      fish80 = fish_weekly / 80
    )
  return(data)
}

# Main analyses -----------------------------------------------------------
model2_pdff19 <- function(data) {
  covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
               "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
               "veggie_weekly", "potato_weekly", "egg_weekly",
               "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
               "sauce_weekly", "food_weight_weekly", "alc_spline", "ethnicity",
               "deprivation", "education", "cohabitation", "physical_activity",
               "smoking", "related_disease", "disease_family", "yearly_income",
               "strata(region, age_strata, sex)")


  model2_formulas <- list(
    meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
    poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
    fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
  )

  model2_results <- model2_formulas |>
    purrr::map(~ coxph(.x, data = data, ties = "breslow")) |>
    purrr::map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
           mutate(across(where(is.numeric), ~ round(.x, 2))) |>
           mutate(model = .y))

  return(model2_results)
}

results19 <- model2_pdff19(oral_defence)
