# Analyses
create_formula <- function(xvars, covars) {
  outcome <- "Surv(survival_time, nafld == 1)"
  reformulate(c(xvars, covars), response = outcome)
}

define_exposure_variables <- function(data) {
  data <- data %>%
    mutate(
      legumes80 = legume_weekly / 80,
      legumepea80 = legume_pea_weekly / 80,
      legumenosoy80 = legume_no_soymilk / 80,
      meats80 = meats_weekly / 80,
      poultry80 = poultry_weekly / 80,
      fish80 = fish_weekly / 80
    )
  return(data)
}

# Main analyses -----------------------------------------------------------
main_model1 <- function(data) {
  covariates_model1 <- all.vars(
    ~ # other food components
      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
      veggie_weekly + potato_weekly + egg_weekly +
      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
      sauce_weekly + food_weight_weekly +
      # baseline strata
      strata(region, age_strata, sex)
  )
covars1 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
            "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
            "veggie_weekly", "potato_weekly", "egg_weekly",
            "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
            "sauce_weekly", "food_weight_weekly", "strata(region, age_strata, sex)")
  
  
  model1_formulas <- list(
    meat_model1 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model1),
    poultry_model1 = create_formula(c("legumes80", "meats80", "fish80"), covariates_model1),
    fish_model1 = create_formula(c("legumes80", "meats80", "poultry80"), covariates_model1)
  )

  model1_results <- model1_formulas |>
    map(~ coxph(.x, data = data, ties = "breslow")) |>
    map2(names(model1_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      mutate(model = .y))

  return(model1_results)
}

main_model2<- function(data) {
  covariates_model2 <- (
    ~ # other food components
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
      strata(region, age_strata, sex)
  ) |>
    all.vars()

  model2_formulas <- list(
    meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model2),
    poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covariates_model2),
    fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covariates_model2)
  )

  model2_results <- model2_formulas |>
    map(~ coxph(.x, data = data, ties = "breslow")) |>
    map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      mutate(model = .y))

  return(model2_results)
}

main_model3<- function(data) {
  covariates_model3 <- (
    ~ # other food components
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
      strata(region, age_strata, sex)
  ) |>
    all.vars()

  model3_formulas <- list(
    meat_model3 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model3),
    poultry_model3 = create_formula(c("legumes80", "meats80", "fish80"), covariates_model3),
    fish_model3 = create_formula(c("legumes80", "meats80", "poultry80"), covariates_model3)
  )

  model3_results <- model3_formulas |>
    map(~ coxph(.x, data = data, ties = "breslow")) |>
    map2(names(model3_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      mutate(model = .y))
  return(model3_results)
}


# secondary analyses ------------------------------------------------------

consumers_analyses<- function(data) {
  consumers <- data %>%
    subset(legume_weekly > 0)

  covariates_model2 <- (
    ~ # other food components
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
      strata(region, age_strata, sex)
  ) |>
    all.vars()

    model2_formulas <- list(
      meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model2),
      poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covariates_model2),
      fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covariates_model2)
    )

    model2_results_3recalls <- model2_formulas |>
      map(~ coxph(.x, data = consumers, ties = "breslow")) |>
      map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
        mutate(across(where(is.numeric), ~ round(.x, 2))) |>
        mutate(model = .y))

    return(model_results_3recalls)
}

total_intake <- function(data) {
  consumers <- data %>%
    subset(legume_weekly > 0)

  total_model2 <- coxph(
    Surv(survival_time, nafld == 1) ~ legumes80 +
      # other food components
      cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
      veggie_weekly + potato_weekly + egg_weekly +
      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
      sauce_weekly + meats_weekly + poultry_weekly + fish_weekly +
      # other variables
      alc_spline + ethnicity + deprivation + education +
      cohabitation + physical_activity + smoking +
      related_disease + disease_family + yearly_income +
      strata(region, age_strata, sex),
    data = consumers, ties = "breslow"
  )
  total <- tidy(total_model2, exponentiate = TRUE, conf.int = TRUE) %>%
    mutate(across(where(is.numeric), ~ round(.x, 2)))
  return(total)
}



# sensitivity analyses ----------------------------------------------------
# peas included in legume component
legumes_and_peas<- function(data) {
  covariates_pea <- (
    ~ # other food components
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
      strata(region, age_strata, sex)
  ) |>
    all.vars()

  model2_formulas_pea <- list(
    meat_model2 = create_formula(c("legumepea80", "poultry80", "fish80"), covariates_pea),
    poultry_model2 = create_formula(c("legumepea80", "meats80", "fish80"), covariates_pea),
    fish_model2 = create_formula(c("legumepea80", "meats80", "poultry80"), covariates_pea)
  )

  model2_results_pea <- model2_formulas_pea |>
    map(~ coxph(.x, data = data, ties = "breslow")) |>
    map2(names(model2_formulas_pea), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      mutate(model = .y))

  return(model2_results_pea)
}

# soy excluded from legume component
legumes_without_soy<- function(data) {
  data <- data %>%
    mutate(
      legumenosoy80 = legume_no_soymilk / 80,
      meats80 = meats_weekly / 80,
      poultry80 = poultry_weekly / 80,
      fish80 = fish_weekly / 80
    )


  covariates_nosoy <- (
    ~ # other food components
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
      strata(region, age_strata, sex)
  ) |>
    all.vars()

  model2_formulas_nosoy <- list(
    meat_model2 = create_formula(c("legumenosoy80", "poultry80", "fish80"), covariates_nosoy),
    poultry_model2 = create_formula(c("legumenosoy80", "meats80", "fish80"), covariates_nosoy),
    fish_model2 = create_formula(c("legumenosoy80", "meats80", "poultry80"), covariates_nosoy)
  )

  model2_results_nosoy <- model2_formulas_nosoy |>
    map(~ coxph(.x, data = data, ties = "breslow")) |>
    map2(names(model2_formulas_nosoy), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      mutate(model = .y))

  return(model2_results_nosoy)
}

# excluding those above 90% percentile of alcohol_weekly
alcohol_restricted_analyses <- function(data) {
  percentile_90 <- quantile(data$alcohol_weekly, probs = 0.90, na.rm = TRUE)
  lower_alc <- data %>%
    subset(alcohol_weekly < percentile_90)

  covariates_model2 <- (
    ~ # other food components
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
      strata(region, age_strata, sex)
  ) |>
    all.vars()

  model2_formulas <- list(
    meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model2),
    poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covariates_model2),
    fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covariates_model2)
  )

  model2_results_alc <- model2_formulas |>
    map(~ coxph(.x, data = lower_alc, ties = "breslow")) |>
    map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      mutate(model = .y))

  return(model2_results_alc)
}

# excluding those with ALT levels above 40 UL
normal_liver_analyses<- function(data) {
  normal_liver <- data %>%
    subset(alt < 40)

  covariates_model2 <- (
    ~ # other food components
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
      strata(region, age_strata, sex)
  ) |>
    all.vars()

  model2_formulas <- list(
    meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model2),
    poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covariates_model2),
    fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covariates_model2)
  )

  model2_results_liver <- model2_formulas |>
    map(~ coxph(.x, data = normal_liver, ties = "breslow")) |>
    map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      mutate(model = .y))

  return(model2_results_liver)
}

# excluding those with <3 24-hour dietary assessments
three_recalls_analyses <- function(data) {
  data3 <- data %>%
    subset(p20077 >= 3)

  covariates_model2 <- (
    ~ # other food components
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
      strata(region, age_strata, sex)
  ) |>
    all.vars()

  model2_formulas <- list(
    meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covariates_model2),
    poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covariates_model2),
    fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covariates_model2)
  )

  model2_results_3recalls <- model2_formulas |>
    map(~ coxph(.x, data = data3, ties = "breslow")) |>
    map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      mutate(model = .y))

  return(model2_results_3recalls)
}
