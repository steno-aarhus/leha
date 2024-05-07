# 11. Incidence rates

# Overall incidence rate


# Incidence rate difference when replacing meat with pulses

# Incidence rate difference when replacing poultry with pulses

# Incidence rate difference when replacing fish with pulses


# PAF

library(splines)
library(survival)
library(graphPAF)
> model_exercise <- clogit(formula = case ~ age + education +exercise +
                             ns(diet, df = 3) + smoking + alcohol + stress +
                             ns(lipids,df = 3) + ns(waist_hip_ratio, df = 3) +
                             high_blood_pressure +strata(strata),data=stroke_reduced )
ThePAFforphysical inactivity can then be calculated using the function PAF_calc_discrete.
Note that the reference value of the risk factor variable in R, that is the value corresponding
to no risk factor exposure, is specified below as refval.
> PAF_calc_discrete(model = model_exercise, riskfactor = "exercise", refval = 0, data = stroke_reduced )


data <- data %>%
  mutate(
    legumes80 = legume_weekly / 80,
    meats80 = meats_weekly / 80,
    poultry80 = poultry_weekly / 80,
    fish80 = fish_weekly / 80
  )

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

PAF_calc_continuous(model = meat_model1, riskfactor = "legumes80", refval = 0, data = data )

paf_meat <- PAF_calc_continuous(model=meat_model1, riskfactor_vec = c("legumes80", "poultry80", "fish80"),
                                q_vec = c(0.01, 0.1, 0.3, 0.5, 0.7, 0.9), ci = TRUE, calculation_method = "D",
                                data = data, boot_rep = 50, verbose=TRUE )
