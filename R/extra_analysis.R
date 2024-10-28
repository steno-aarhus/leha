# Extra analysis
# excluding those with alcohol intake > 20 g/day for women and >30 g/day for men
low_alc_analyses <- function(data) {

  low_alc <- subset(data, (sex == 0 & alcohol_intake < 20) |
                               (sex == 1 & alcohol_intake < 30))

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

  model2_low_alc <- model2_formulas |>
    map(~ coxph(.x, data = low_alc, ties = "breslow")) |>
    map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      mutate(model = .y))

  return(model2_low_alc)
}


# excluding only cases with alcohol intake > 20 g/day for women and >30 g/day for men
low_alc_cases_analyses <- function(data) {

  low_alc_cases <- subset(data, !(nafld == 1 &
                               ((sex == 0 & alcohol_intake >= 20) |
                                (sex == 0 & alcohol_intake >= 30))))

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

  model2_low_alc_cases <- model2_formulas |>
    map(~ coxph(.x, data = low_alc_cases, ties = "breslow")) |>
    map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
      mutate(across(where(is.numeric), ~ round(.x, 2))) |>
      mutate(model = .y))

  return(model2_low_alc_cases)
}
