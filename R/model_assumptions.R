# model assumptions


# model control ------------------------------------------------------
# Proportional hazards assumption - Schoenfeld residuals
proportionality_assumption <- function(data) {
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
      alcohol_weekly + ethnicity + deprivation + education +
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
      alcohol_weekly + ethnicity + deprivation + education +
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
      alcohol_weekly + ethnicity + deprivation + education +
      cohabitation + physical_activity + smoking +
      related_disease + disease_family + yearly_income +
      strata(region, age_strata, sex),
    data = data, ties = "breslow"
  )

  ph_test3 <- cox.zph(fish_model2) %>% print()
  return(data)
}


# Linearity assumption
# Investigating the association between weekly intakes of legumes, red and processed meat,
# poultry, and fish and NAFLD for non-linearity using restricted cubic splines at the
# 10th, 50th and 90th percentiles as proposed in: https://pubmed.ncbi.nlm.nih.gov/37694448/

likelihood_ratio <- function(data) {

  # Knots
  knots_legume  <- quantile(data$legume_weekly,  c(0.1, 0.5, 0.9), na.rm = TRUE)
  knots_meat    <- quantile(data$meats_weekly,   c(0.1, 0.5, 0.9), na.rm = TRUE)
  knots_poultry <- quantile(data$poultry_weekly, c(0.1, 0.5, 0.9), na.rm = TRUE)
  knots_fish    <- quantile(data$fish_weekly,    c(0.1, 0.5, 0.9), na.rm = TRUE)

  # Splines
  data <- data %>%
    mutate(
      legume_spline  = splines::bs(legume_weekly,  knots = knots_legume,  degree = 3),
      meat_spline    = splines::bs(meats_weekly,   knots = knots_meat,    degree = 3),
      poultry_spline = splines::bs(poultry_weekly, knots = knots_poultry, degree = 3),
      fish_spline    = splines::bs(fish_weekly,    knots = knots_fish,    degree = 3)
    )

  # Base model
  cox_model <- Surv(survival_time, event = nafld) ~ alc_spline + ethnicity +
    deprivation + education + cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata, sex)

  # Helper
  lrt_p <- function(f_linear, f_spline) {
    fit1 <- coxph(update(cox_model, f_linear), data = data, ties = "breslow")
    fit2 <- coxph(update(cox_model, f_spline), data = data, ties = "breslow")
    anova(fit1, fit2, test = "LRT")$`Pr(>|Chi|)`[2]
  }

  tibble(
    lrt_legume  = round(lrt_p(~ legume_weekly  + ., ~ legume_spline  + .), 3),
    lrt_meat    = round(lrt_p(~ meats_weekly   + ., ~ meat_spline    + .), 3),
    lrt_poultry = round(lrt_p(~ poultry_weekly + ., ~ poultry_spline + .), 3),
    lrt_fish    = round(lrt_p(~ fish_weekly    + ., ~ fish_spline    + .), 3)
  )
}
