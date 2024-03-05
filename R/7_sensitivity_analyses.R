#7. Sensitivity analyses

#Load packages
install.packages("patchwork")
install.packages("Hmisc")
install.packages("survival")
install.packages("lubridate")
install.packages("Publish")
install.packages("gtsummary")
install.packages("ggsurvfit")
install.packages("kableExtra")

library(tidyverse)
library(patchwork)
library(Hmisc)
library(survival)
library(lubridate)
library(Publish)
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


# Legumes including peas; substitution models ------------------------------
# Weekly substituting 80 g legumes including peas (NHS 1 portion beans = 80 g)
# https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# defining 80 g/week variable for each food
data <- data %>%
  mutate(legumepea80 = legume_pea_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)


## Crude analysis ----------------------------------------------------------
# leave one out model
# meats
cox_meat_ua <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumepea80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly, data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(cox_meat_ua)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(cox_meat_ua)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_ua <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
cox_poultry_ua <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumepea80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly, data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(cox_poultry_ua)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(cox_poultry_ua)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_ua <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
cox_fish_ua <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumepea80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly, data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(cox_fish_ua)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(cox_fish_ua)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_ua <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])



## model 1 -----------------------------------------------------------------
# meats
meat_model1 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumepea80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex,
                     data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_model1)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_model1)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_model1 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_model1 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumepea80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex,
                        data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(poultry_model1)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(poultry_model1)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_model1 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
fish_model1 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumepea80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex,
                     data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(fish_model1)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(fish_model1)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_model1 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])



## model 2 -----------------------------------------------------------------
# Alcohol as spline with 4 knots for adjustment
df <- 4
data <- data %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = df, degree = 3, knots = NULL)))

# meats
meat_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumepea80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income + alcohol_spline,
                     data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumepea80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex +
                          alcohol_spline + ethnicity + deprivation_quint + education +
                          cohabitation + physical_activity + smoking + diabetes + cancer +
                          non_cancer_illness + family_illness + yearly_income + alcohol_spline,
                        data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(poultry_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(poultry_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
fish_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumepea80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income + alcohol_spline,
                     data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(fish_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(fish_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])



## model 3 -----------------------------------------------------------------
# meats
meat_model3 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumepea80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income + alcohol_spline
                     + bmi30,
                     data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_model3)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_model3)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_model3 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_model3 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumepea80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex +
                          alcohol_spline + ethnicity + deprivation_quint + education +
                          cohabitation + physical_activity + smoking + diabetes + cancer +
                          non_cancer_illness + family_illness + yearly_income + alcohol_spline
                        + bmi30,
                        data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(poultry_model3)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(poultry_model3)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_model3 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
fish_model3 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumepea80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_pea_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income + alcohol_spline
                     + bmi30,
                     data = data, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(fish_model3)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(fish_model3)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_model3 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])



### Extract results ---------------------------------------------------------
# combine first row from each model in df
first_row_df1 <- meat_ua[1, ]
first_row_df2 <- meat_model1[1, ]
first_row_df3 <- meat_model2[1, ]
first_row_df4 <- meat_model3[1, ]
first_row_df5 <- poultry_ua[1, ]
first_row_df6 <- poultry_model1[1, ]
first_row_df7 <- poultry_model2[1, ]
first_row_df8 <- poultry_model3[1, ]
first_row_df9 <- fish_ua[1, ]
first_row_df10 <- fish_model1[1, ]
first_row_df11 <- fish_model2[1, ]
first_row_df12 <- fish_model3[1, ]
first_rows_combined <- rbind(first_row_df1, first_row_df2, first_row_df3, first_row_df4,
                             first_row_df5, first_row_df6, first_row_df7, first_row_df8,
                             first_row_df9, first_row_df10, first_row_df11, first_row_df12)
rownames(first_rows_combined) <- c("meat_ua", "meat_model1", "meat_model2", "meat_model3",
                                   "poultry_ua", "poultry_model1", "poultry_model2", "poultry_model3",
                                   "fish_ua", "fish_model1", "fish_model2", "fish_model3")

#create html table
legume_pea_analysis <- first_rows_combined %>%
  kable("html") %>%
  kable_styling()
# flextable::save_as_html(legume_pea_analysis, path = here("doc", "sensitivity1.html"))




# Removing high ALT and AST from analysis-----------------------------------------------
# removing high ALT and AST
data <- data %>% mutate(
  alt = case_when(
    p30620 <= 45 & sex == "Female" | p30620 <= 70 & sex == "Male" ~ 0,
    p30620 > 45 & sex == "Female" | p30620 > 70 & sex == "Male" ~ 1
  ),
  ast = case_when(
    p30650 <= 35 & sex == "Female" | p30650 <= 45 & sex == "Male" ~ 0,
    p30650 > 35 & sex == "Female" | p30650 > 45 & sex == "Male" ~ 1
  )
)

# Subsetting data
normal_liver <- data %>%
  subset(alt !=1 & ast != 1)


## Running main analysis on subsample --------------------------------------

# Weekly substituting 80 g legumes (NHS 1 portion beans = 80 g) https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# defining 80 g/week variable for each food
normal_liver <- normal_liver %>%
  mutate(legumes80 = legume_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)


## Crude analysis ----------------------------------------------------------
# leave one out model
# meats
cox_meat_ua <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly, data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(cox_meat_ua)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(cox_meat_ua)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_ua <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
cox_poultry_ua <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly, data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(cox_poultry_ua)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(cox_poultry_ua)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_ua <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
cox_fish_ua <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly, data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(cox_fish_ua)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(cox_fish_ua)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_ua <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])



## model 1 -----------------------------------------------------------------
# meats
meat_model1 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex,
                     data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_model1)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_model1)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_model1 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_model1 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex,
                        data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(poultry_model1)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(poultry_model1)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_model1 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
fish_model1 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex,
                     data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(fish_model1)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(fish_model1)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_model1 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])



## model 2 -----------------------------------------------------------------
# Alcohol as spline with 4 knots for adjustment
df <- 4
normal_liver <- normal_liver %>%
  mutate(alcohol_spline = predict(bs(alcohol_weekly, df = df, degree = 3, knots = NULL)))

# meats
meat_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income,
                     data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex +
                          alcohol_spline + ethnicity + deprivation_quint + education +
                          cohabitation + physical_activity + smoking + diabetes + cancer +
                          non_cancer_illness + family_illness + yearly_income,
                        data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(poultry_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(poultry_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
fish_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income,
                     data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(fish_model2)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(fish_model2)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_model2 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])



## model 3 -----------------------------------------------------------------
# meats
meat_model3 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income + bmi30,
                     data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(meat_model3)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(meat_model3)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
meat_model3 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])

# poultry
poultry_model3 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly + age_strata + region + sex +
                          alcohol_spline + ethnicity + deprivation_quint + education +
                          cohabitation + physical_activity + smoking + diabetes + cancer +
                          non_cancer_illness + family_illness + yearly_income + bmi30,
                        data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(poultry_model3)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(poultry_model3)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
poultry_model3 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])


# fish
fish_model3 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly + age_strata + region + sex +
                       alcohol_spline + ethnicity + deprivation_quint + education +
                       cohabitation + physical_activity + smoking + diabetes + cancer +
                       non_cancer_illness + family_illness + yearly_income + bmi30,
                     data = normal_liver, ties='breslow')

# Extract HR and 95% CI for the first coefficient
coef_summary <- summary(fish_model3)$coefficients
row_name <- rownames(coef_summary)
HR <- exp(coef_summary[1, "coef"])
CI <- confint(fish_model3)[1, ]
CI <- exp(CI)
# Round to two decimals
HR <- round(HR, 2)
CI <- round(CI, 2)
# Convert to dataframe
fish_model3 <- data.frame(row_name = row_name, HR = HR, Lower_CI = CI[1], Upper_CI = CI[2])



### Extract results ---------------------------------------------------------
# combine first row from each model in df
first_row_df1 <- meat_ua[1, ]
first_row_df2 <- meat_model1[1, ]
first_row_df3 <- meat_model2[1, ]
first_row_df4 <- meat_model3[1, ]
first_row_df5 <- poultry_ua[1, ]
first_row_df6 <- poultry_model1[1, ]
first_row_df7 <- poultry_model2[1, ]
first_row_df8 <- poultry_model3[1, ]
first_row_df9 <- fish_ua[1, ]
first_row_df10 <- fish_model1[1, ]
first_row_df11 <- fish_model2[1, ]
first_row_df12 <- fish_model3[1, ]
first_rows_combined <- rbind(first_row_df1, first_row_df2, first_row_df3, first_row_df4,
                             first_row_df5, first_row_df6, first_row_df7, first_row_df8,
                             first_row_df9, first_row_df10, first_row_df11, first_row_df12)
rownames(first_rows_combined) <- c("meat_ua", "meat_model1", "meat_model2", "meat_model3",
                                   "poultry_ua", "poultry_model1", "poultry_model2", "poultry_model3",
                                   "fish_ua", "fish_model1", "fish_model2", "fish_model3")

#create html table
normal_liver <- first_rows_combined %>%
  kable("html") %>%
  kable_styling()
# flextable::save_as_html(normal_liver, path = here("doc", "sensitivity2.html"))
