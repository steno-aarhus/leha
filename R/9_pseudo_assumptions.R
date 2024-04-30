# This script tests the model assumptions for Pseudo Observational analyses
# Load packages
# Load packages
library(tidyverse)
library(Hmisc)
library(survival)
library(ggsurvfit)
library(dplyr)
library(here)
library(broom)
library(eventglm)

# Pseudo model
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

# Cox model
meat_model2 <- coxph(Surv(survival_time, nafld == 1) ~
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
                     data = data, ties='breslow')

meat_model2 <- tidy(meat_model2, exponentiate = TRUE, conf.int = TRUE, digits = 2) # 2 digits doesn't work

# Assumption 2: Event risk is independent of censoring (The independent censoring assumption)
# This assumption can only be examined with regard to administrative censoring
# Test: Investigate the association between baseline date and event risk within strata of baseline age using Cox
# regression.

# individuals who were administratively censored
data <- data %>% mutate(
  were_censored = case_when(nafld != 1 ~ "yes",
                            nafld == 1 ~ "no")
)
# Start of follow-up = date_filled
# Baseline age
data <- data %>%
  mutate(baseline_age = (date_filled - date_birth)/365.25)
# age strata
age0 <- data %>%
  subset(age_strata==0)
age1 <- data %>%
  subset(age_strata==1)
age2 <- data %>%
  subset(age_strata==2)
age3 <- data %>%
  subset(age_strata==3)
age4 <- data %>%
  subset(age_strata==4)
age5 <- data %>%
  subset(age_strata==5)

# Linear association between baseline date and event risk stratified on age
# Hvor kommer den del med censoring ind?
model0 <- lm(nafld ~ date_filled, data = age0)
# model1 <- lm(nafld ~ date_filled, data = age1)
# model2 <- lm(nafld ~ date_filled, data = age2)
# model3 <- lm(nafld ~ date_filled, data = age3)
# model4 <- lm(nafld ~ date_filled, data = age4)
# model5 <- lm(nafld ~ date_filled, data = age5)

# plots


# Assumption 4: Censoring is independent of covariates
# Test: Investigate the association between covariates and risk of censoring using Cox regression with the
# relevant time axis. The outcome can be defined as a composite of censoring due to emigration and
# administrative censoring.

# Covariates
# cereal_refined_weekly
# whole_grain_weekly
# mixed_dish_weekly
# dairy_weekly
# fats_weekly
# fruit_weekly
# nut_weekly
# veggie_weekly
# potato_weekly
# egg_weekly
# meat_sub_weekly
# non_alc_beverage_weekly
# alc_beverage_weekly
# snack_weekly
# sauce_weekly
# weight_weekly
# age
# region
# sex
# alcohol_weekly
# ethnicity
# deprivation
# education
# cohabitation
# physical_activity
# smoking
# related_disease
# disease_family
# yearly_income
