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

# Status variable
data <- data %>%
  mutate(
    status = case_when(
      !is.na(icd10_nafld_date) &
        (icd10_nafld_date < icd10_nash_date |
           icd10_nafld_date < date_of_death |
           icd10_nafld_date < loss_to_follow_up) ~ "icd10_nafld_date",
      !is.na(icd10_nash_date) &
        (icd10_nash_date < icd10_nafld_date |
           icd10_nash_date < date_of_death |
           icd10_nash_date < loss_to_follow_up) ~ "icd10_nash_date",
      !is.na(date_of_death) &
        (date_of_death < icd10_nafld_date |
           date_of_death < icd10_nash_date |
           date_of_death < loss_to_follow_up) ~ "date_of_death",
      !is.na(loss_to_follow_up) &
        (loss_to_follow_up < icd10_nafld_date |
           loss_to_follow_up < icd10_nash_date |
           loss_to_follow_up < date_of_death) ~ "loss_to_follow_up",
      TRUE ~ "censored"
    )
  )
data <- data %>%
  mutate(censored = case_when(
    status == "censored" ~ "yes",
    status != "censored" ~ "no"
  ))

# Baseline age
data <- data %>%
  mutate(baseline_age = (date_filled - date_birth)/365.25)
# age strata
data <- data %>%
  mutate(base_age_strata = ntile(baseline_age, 5))

# Assumption 1: Entry is independent of event risk (The independent entry assumption)
# Test: Investigate the association between baseline age and event risk using Cox regression

assump1 <- coxph(Surv(time, nafld == 1) ~ date_filled,
                 data = data, ties='breslow')

# Assumption 2: Event risk is independent of censoring (The independent censoring assumption)
# This assumption can only be examined with regard to administrative censoring
# Test: Investigate the association between baseline date and event risk within strata of baseline age using Cox
# regression.

assump2_age <- coxph(Surv(time, nafld ==1) ~ baseline_age,
                     data = data, ties='breslow')
assump2_baseline_dato <- coxph(Surv(time, nafld == 1) ~ date_filled,
                 data = data, ties='breslow')


# Cox model
cox <- coxph(Surv(time, nafld == 1) ~ date_filled,
                     data = data, ties='breslow')
cox <- tidy(cox, exponentiate = TRUE, conf.int = TRUE)
# association between censoring and outcome

# within age strata
cox1 <- data %>% filter(base_age_strata==3) %>% coxph(Surv(survival_time, nafld == 1) ~ date_filled,
                                              data = ., ties='breslow')
cox1 <- tidy(cox1, exponentiate = TRUE, conf.int = TRUE)
# Aldersstrata er væsentlige og bør tilføjes til Pseudo koden nedenfor for at fjerne
# afhængighed mellem event risk og censoring.

##### PSEUDO - justeret for alle covariate med strata af alder i modellen
data <- data %>%
  mutate(
    legumes80 = legume_weekly / 80,
    meats80 = meats_weekly / 80,
    poultry80 = poultry_weekly / 80,
    fish80 = fish_weekly / 80
  )

# hvad skal eventstatus være? censurering eller sygdom?
fit_meat2 <- eventglm::cumincglm(Surv(time, nafld == 1) ~
                                  # removing meat
                                  legumes80 + poultry80 + fish80+
                                  #other food components
                                  cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                                  dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                                  veggie_weekly + potato_weekly + egg_weekly +
                                  non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                                  sauce_weekly + weight_weekly +
                                  #other variables
                                  baseline_age + region + sex +
                                  alcohol_weekly + ethnicity + deprivation + education +
                                  cohabitation + physical_activity + smoking +
                                  related_disease + disease_family + yearly_income,
                                time = 10, cause = "censored",
                                model.censoring = "stratified",
                                formula.censoring = ~ base_age_strata, data = data) #skal det være strataet jeg har lavet, eller skal det bare være den kontinuære variabel "age"?

fit_meat2 <- tidy(fit_meat2, exponentiate = FALSE, conf.int = TRUE)


# Linear association between baseline date and event risk stratified on age
# plots??









# Assumption 4: Censoring is independent of covariates
# Test: Investigate the association between covariates and risk of censoring using Cox regression with the
# relevant time axis. The outcome can be defined as a composite of censoring due to emigration and
# administrative censoring.

# association between covariates and censoring
# age
assump4_age <- coxph(Surv(survival_time, censored == "yes") ~ base_age_strata, # eller baseline_age? og within strata?
              data = data, ties = 'breslow')
assump4_age <- tidy(assump4_age, exponentiate = TRUE, conf.int = TRUE)
# within each age strata
cox_age <- function(df) {
  cox_model <- coxph(Surv(survival_time, censored == "yes") ~ baseline_age, data = df, ties = 'breslow')
  tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)
}

# Group by base_age_strata, fit Cox model, and tidy the results for each group
cox_results <- data %>%
  group_by(base_age_strata) %>%
  nest() %>%
  mutate(cox_results = map(data, cox_age)) %>%
  unnest(cox_results)
# View the results
print(cox_results)

# sex
assump4_sex <- coxph(Surv(survival_time, censored == "yes") ~ sex,
                     data = data, ties = 'breslow')
assump4_sex <- tidy(assump4_sex, exponentiate = TRUE, conf.int = TRUE)
# Within age strata
cox_sex <- function(df) {
  cox_model <- coxph(Surv(survival_time, censored == "yes") ~ sex, data = df, ties = 'breslow')
  tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)
}

# Group by base_age_strata, fit Cox model, and tidy the results for each group
cox_results <- data %>%
  group_by(base_age_strata) %>%
  nest() %>%
  mutate(cox_results = map(data, cox_sex)) %>%
  unnest(cox_results)
# View the results
print(cox_results)


assump4_region <- coxph(Surv(survival_time, censored == "yes") ~ region, # eller baseline_age? og within strata?
                     data = data, ties = 'breslow')
assump4_region <- tidy(assump4_region, exponentiate = TRUE, conf.int = TRUE)
# within each age strata
cox_region <- function(df) {
  cox_model <- coxph(Surv(survival_time, censored == "yes") ~ region, data = df, ties = 'breslow')
  tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)
}

# Group by base_age_strata, fit Cox model, and tidy the results for each group
cox_results <- data %>%
  group_by(base_age_strata) %>%
  nest() %>%
  mutate(cox_results = map(data, cox_region)) %>%
  unnest(cox_results)
# View the results
print(cox_results)


# Covariates
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



# Pseudo model
fit_meat <- eventglm::cumincglm(Surv(time, nafld == 1) ~
                                  # removing meat
                                  legumes80 + poultry80 + fish80+
                                  #other food components
                                  cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                                  dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                                  veggie_weekly + potato_weekly + egg_weekly +
                                  non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                                  sauce_weekly + weight_weekly +
                                  #other variables
                                  age + region + sex +
                                  alcohol_weekly + ethnicity + deprivation + education +
                                  cohabitation + physical_activity + smoking +
                                  related_disease + disease_family + yearly_income,
                                time = 10, data = data)

fit_meat <- tidy(fit_meat, exponentiate = FALSE, conf.int = TRUE, digits = 2)
