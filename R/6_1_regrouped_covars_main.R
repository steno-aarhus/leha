# regrouping covariates

library(tidyverse)
library(broom)
library(Hmisc)
library(survival)
library(gtsummary)
library(ggsurvfit)
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(splines)
library(kableExtra)

# Load data --------------------------------------------------------
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))


data <- data %>%
  mutate(
    related_disease = case_when(
      str_detect(non_cancer_illness, "hypertension") | str_detect(non_cancer_illness, "mi") |
        str_detect(non_cancer_illness, "stroke") | str_detect(non_cancer_illness, "angina") |
      str_detect(non_cancer_illness, "cholesterolemia") | str_detect(non_cancer_illness, "gbd")|
      str_detect(non_cancer_illness, "alcoholic liver disease") | diabetes == "yes" ~ "yes",
      non_cancer_illness == "none of the above" |  diabetes == "no" |
        diabetes == "no answer" |diabetes == "don't know" ~ "none of the above"
      ),
    disease_family = case_when(
      family_illness == "diabetes" | family_illness == "hypertension" |
        family_illness == "stroke" | family_illness == "heart disease" ~ "yes",
      family_illness == "none of the above" ~ "no"
      )
  )



family_illness = as.factor(family_illness),
cancer = case_when(
  str_detect(p2453_i0, "Do not know") ~ "don't know",
  str_detect(p2453_i0, "Yes") ~ "yes",
  p2453_i0 == "No" ~ "no",
  str_detect(p2453_i0, "answer") ~ "no answer",
  TRUE ~ "no answer"
),
data <- data %>%
  mutate(legumes80 = legume_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)
df <- 4
data <- data %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = df, degree = 3, knots = NULL)))

meat_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation + education +
                       cohabitation + physical_activity + smoking +
                       related_disease + disease_family + yearly_income,
                     data = data, ties='breslow')
meat_model <- tidy(meat_model2, exponentiate = TRUE, conf.int = TRUE, digits = 2) # not 2 digits?
