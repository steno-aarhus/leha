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
                                  age + region + sex +
                                  alcohol_weekly + ethnicity + deprivation + education +
                                  cohabitation + physical_activity + smoking +
                                  related_disease + disease_family + yearly_income,
                                time = 10, data = data)

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
                          age + region + sex +
                          alcohol_weekly + ethnicity + deprivation + education +
                          cohabitation + physical_activity + smoking +
                          related_disease + disease_family + yearly_income,
                        time = 10, data = data)

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
                    age + region + sex +
                    alcohol_weekly + ethnicity + deprivation + education +
                    cohabitation + physical_activity + smoking +
                    related_disease + disease_family + yearly_income,
                  time = 10, data = data)

fit_fish <- tidy(fit_fish, exponentiate = FALSE, conf.int = TRUE, digits = 2)

# Non specific substitutions ----------------------------------------------
# Leaving one portion of legumes (80g) out weekly
data <- data %>%
  mutate(legumes80 = legume_weekly/80)


## model 2 -----------------------------------------------------------------
fit_nonspecific <- coxph(Surv(survival_time, nafld == 1) ~ legumes80 +
                              weight_weekly + age + region + sex +
                              alcohol_weekly + ethnicity + deprivation + education +
                              cohabitation + physical_activity + smoking +
                              related_disease + disease_family + yearly_income,
                            data = data, ties='breslow')

nonspecific<- tidy(fit_nonspecific, exponentiate = TRUE, conf.int = TRUE, digits = 2)
