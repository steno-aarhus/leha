#8. Secondary analyses

# Load packages
library(tidyverse)
library(Hmisc)
library(survival)
library(ggsurvfit)
library(dplyr)
library(here)
library(broom)

# Non specific substitutions ----------------------------------------------
# Leaving one portion of legumes (80g) out weekly
data <- data %>%
  mutate(legumes80 = legume_weekly/80)


## model 2 -----------------------------------------------------------------
fit_nonspecific <- coxph(Surv(survival_time, nafld == 1) ~ legumes80 +
                              weight_weekly + strata(age_strata, region, sex) +
                              alcohol_weekly + ethnicity + deprivation + education +
                              cohabitation + physical_activity + smoking +
                              related_disease + disease_family + yearly_income,
                            data = data, ties='breslow')

nonspecific<- tidy(fit_nonspecific, exponentiate = TRUE, conf.int = TRUE, digits = 2)



# Legume as continuous exposure 80 g/week ---------------------------------

# one serving of legumes/week (80 g)
data <- data %>%
  mutate(
    legumes80 = legume_weekly / 80
  )

# model 2
total_model2 <- coxph(
  Surv(survival_time, nafld == 1) ~ legumes80 +
    # other food components
    cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
    dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
    veggie_weekly + potato_weekly + egg_weekly +
    non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
    sauce_weekly + meats_weekly + poultry_weekly + fish_weekly+
    # other variables
    alcohol_weekly + ethnicity + deprivation + education +
    cohabitation + physical_activity + smoking +
    related_disease + disease_family + yearly_income +
    strata(region, age_strata, sex),
  data = data, ties = "breslow"
)
total <- tidy(total_model2, exponentiate = TRUE, conf.int = TRUE)


