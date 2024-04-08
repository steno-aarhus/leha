#8. Secondary analyses

# Load packages
library(tidyverse)
library(Hmisc)
library(survival)
library(ggsurvfit)
library(dplyr)
library(here)
library(splines)
library(broom)
library(eventglm)

# Pseudo observational method ---------------------------------------------
# using eventglm package
# https://www.rdocumentation.org/packages/eventglm/versions/1.2.2

# using survival model from main analysis
# defining 80 g/week variable for each food
data <- data %>%
  mutate(legumes80 = legume_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)

data <- data %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = 4, degree = 3, knots = NULL)))

# defining time in study
data <- data %>%
  mutate(
    survival_time_nafld = case_when(
      !is.na(icd10_nafld_date) ~ as.numeric(difftime(icd10_nafld_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_nash = case_when(
      !is.na(icd10_nash_date) ~ as.numeric(difftime(icd10_nash_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_ltfu = case_when(
      !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_death = case_when(
      !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_cenc = difftime(censoring, date_filled, units = "days"),
    time = pmin(survival_time_death, survival_time_cenc, survival_time_ltfu,
                survival_time_nash, survival_time_nafld, na.rm = TRUE),
    time = time/365.25
  )


# meats


fit_meat <- eventglm::cumincglm(Surv(time, nafld == 1) ~
                                  # removing meat
                                  legumes80 + poultry80 + fish80+
                                  #other food components
                                  cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                                  dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                                  veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                                  non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                                  sauce_weekly + weight_weekly +
                                  #other variables
                                  age_strata + region + sex +
                                  alcohol_spline + ethnicity + deprivation + education +
                                  cohabitation + physical_activity + smoking +
                                  related_disease + disease_family + yearly_income,
                                time = 5, data = data)

meat_pseudo <- tidy(fit_meat, exponentiate = TRUE, conf.int = TRUE, digits = 2)
fit_meat <- tidy(fit_meat, exponentiate = FALSE, conf.int = TRUE, digits = 2)


# poultry
fit_poultry <- eventglm::cumincglm(Surv(time, nafld == 1) ~
                          # removing poultry
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly +
                          #other variables
                          age_strata + region + sex +
                          alcohol_spline + ethnicity + deprivation + education +
                          cohabitation + physical_activity + smoking +
                          related_disease + disease_family + yearly_income,
                        time = 5, data = data)

poultry_pseudo <- tidy(fit_poultry, exponentiate = TRUE, conf.int = TRUE, digits = 2)
fit_poultry <- tidy(fit_poultry, exponentiate = FALSE, conf.int = TRUE, digits = 2)

# fish
fit_fish <- eventglm::cumincglm(Surv(time, nafld == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly +
                    #other variables
                    age_strata + region + sex +
                    alcohol_spline + ethnicity + deprivation + education +
                    cohabitation + physical_activity + smoking +
                    related_disease + disease_family + yearly_income,
                  time = 5, data = data)

fish_pseudo <- tidy(fit_fish, exponentiate = TRUE, conf.int = TRUE, digits = 2)
fit_fish <- tidy(fit_fish, exponentiate = FALSE, conf.int = TRUE, digits = 2)

# Non specific substitutions ----------------------------------------------
# Leaving one portion of legumes (80g) out weekly
data <- data %>%
  mutate(legumes80 = legume_weekly/80)


## model 2 -----------------------------------------------------------------
#alcohol spline
df <- 4
data <- data %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = df, degree = 3, knots = NULL)))

fit_nonspecific <- coxph(Surv(survival_time, nafld == 1) ~ legumes80 +
                              weight_weekly + age_strata + region + sex +
                              alcohol_spline + ethnicity + deprivation + education +
                              cohabitation + physical_activity + smoking +
                              related_disease + disease_family + yearly_income,
                            data = data, ties='breslow')

nonspecific_pseudo <- tidy(fit_nonspecific, exponentiate = TRUE, conf.int = TRUE, digits = 2)
