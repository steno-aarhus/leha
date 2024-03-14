#6. Main analyses

#Load packages
install.packages("Hmisc")
install.packages("survival")
install.packages("gtsummary")
install.packages("ggsurvfit")
install.packages("kableExtra")

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


# Weekly substituting 80 g legumes (NHS 1 portion beans = 80 g) https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/
# defining 80 g/week variable for each food
data <- data %>%
    mutate(legumes80 = legume_weekly/80,
           meats80 = meats_weekly/80,
           poultry80 = poultry_weekly/80,
           fish80 = fish_weekly/80)


# Crude analysis ----------------------------------------------------------
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
                         sauce_weekly + weight_weekly, data = data, ties='breslow')

meat_ua <- tidy(cox_meat_ua, exponentiate = TRUE, conf.int = TRUE)

# poultry
cox_poultry_ua <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + meats80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly, data = data, ties='breslow')

poultry_ua <- tidy(cox_poultry_ua, exponentiate = TRUE, conf.int = TRUE)


# fish
cox_fish_ua <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + poultry80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly, data = data, ties='breslow')

fish_ua <- tidy(cox_fish_ua, exponentiate = TRUE, conf.int = TRUE)



# model 1 -----------------------------------------------------------------
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
                     data = data, ties='breslow')

meat_model1 <- tidy(meat_model1, exponentiate = TRUE, conf.int = TRUE)

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
                        data = data, ties='breslow')

poultry_model1 <- tidy(poultry_model1, exponentiate = TRUE, conf.int = TRUE)


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
                     data = data, ties='breslow')

fish_model1 <- tidy(fish_model1, exponentiate = TRUE, conf.int = TRUE)


# model 2 -----------------------------------------------------------------
# Alcohol as spline with 4 knots for adjustment
df <- 4
data <- data %>%
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
                     data = data, ties='breslow')

meat_model2 <- tidy(meat_model2, exponentiate = TRUE, conf.int = TRUE)

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
                        data = data, ties='breslow')

poultry_model2 <- tidy(poultry_model2, exponentiate = TRUE, conf.int = TRUE)


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
                     data = data, ties='breslow')

fish_model2 <- tidy(fish_model2, exponentiate = TRUE, conf.int = TRUE)



# model 3 -----------------------------------------------------------------
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
                     data = data, ties='breslow')

meat_model3 <- tidy(meat_model3, exponentiate = TRUE, conf.int = TRUE)

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
                        data = data, ties='breslow')

poultry_model3 <- tidy(poultry_model3, exponentiate = TRUE, conf.int = TRUE)


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
                     data = data, ties='breslow')

fish_model3 <- tidy(fish_model3, exponentiate = TRUE, conf.int = TRUE)

# Extract results ---------------------------------------------------------
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
main_analysis <- first_rows_combined %>%
  kable("html") %>%
  kable_styling()
writeLines(as.character(main_analysis), "doc/main_analysis_table.html")
