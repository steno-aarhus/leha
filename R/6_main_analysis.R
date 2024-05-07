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
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))

# Weekly substituting 80 g legumes (NHS 1 portion beans = 80 g) https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# defining 80 g/week variable for each food
data <- data %>%
  mutate(
    legumes80 = legume_weekly / 80,
    meats80 = meats_weekly / 80,
    poultry80 = poultry_weekly / 80,
    fish80 = fish_weekly / 80
  )

# covariates_model1 <- (~ cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
#   dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
#   veggie_weekly + potato_weekly + egg_weekly +
#   non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
#   sauce_weekly + weight_weekly + age + region + sex) |>
#   all.vars()
#
# create_formula <- function(xvars, covars) {
#   outcome <- "Surv(survival_time, nafld == 1)"
#   reformulate(c(xvars, covars), response = outcome)
# }
#
# # model 1 -----------------------------------------------------------------
# # meats
# model_formulas <- list(
#   meat_model1 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model1),
#   poultry_model1 = create_formula(c("meats80", "legumes80", "fish80"), covariates_model1),
#   fish_model1 = create_formula(c("meats80", "legumes80", "poultry80"), covariates_model1),
# )
#
# model_results <- model_formulas |>
#   map(~ coxph(.x, data = data, ties = "breslow")) |>
#   map2(names(model_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
#     mutate(model = .y))

# Model 1
# meats
meat_model1 <- coxph(
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
    sex + strata(region, age_strata),
  data = data, ties = "breslow"
)

meat_model1 <- tidy(meat_model1, exponentiate = TRUE, conf.int = TRUE)

# poultry
poultry_model1 <- coxph(
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
    sex + strata(region, age_strata),
  data = data, ties = "breslow"
)

poultry_model1 <- tidy(poultry_model1, exponentiate = TRUE, conf.int = TRUE, digits = 2)


# fish
fish_model1 <- coxph(
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
    sex + strata(region, age_strata),
  data = data, ties = "breslow"
)

fish_model1 <- tidy(fish_model1, exponentiate = TRUE, conf.int = TRUE, digits = 2)


# model 2 -----------------------------------------------------------------

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

meat_model2 <- tidy(meat_model2, exponentiate = TRUE, conf.int = TRUE)


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

poultry_model2 <- tidy(poultry_model2, exponentiate = TRUE, conf.int = TRUE, digits = 2)


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

fish_model2 <- tidy(fish_model2, exponentiate = TRUE, conf.int = TRUE, digits = 2)



# model 3 -----------------------------------------------------------------
# meats
meat_model3 <- coxph(
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
    related_disease + disease_family + yearly_income + bmi30 +
    strata(region, age_strata),
  data = data, ties = "breslow"
)

meat_model3 <- tidy(meat_model3, exponentiate = TRUE, conf.int = TRUE, digits = 2)

# poultry
poultry_model3 <- coxph(
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
    related_disease + disease_family + yearly_income + bmi30 +
    strata(region, age_strata),
  data = data, ties = "breslow"
)

poultry_model3 <- tidy(poultry_model3, exponentiate = TRUE, conf.int = TRUE, digits = 2)


# fish
fish_model3 <- coxph(
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
    related_disease + disease_family + yearly_income + bmi30 +
    strata(region, age_strata),
  data = data, ties = "breslow"
)

fish_model3 <- tidy(fish_model3, exponentiate = TRUE, conf.int = TRUE, digits = 2)



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
    sauce_weekly + weight_weekly +
    # other variables
    sex +
    alcohol_weekly + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata),
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
    sauce_weekly + weight_weekly +
    # other variables
    sex +
    alcohol_weekly + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata),
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
    sauce_weekly + weight_weekly +
    # other variables
    sex +
    alcohol_weekly + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata),
  data = data, ties = "breslow"
)

ph_test3 <- cox.zph(fish_model2)
print(ph_test3)
