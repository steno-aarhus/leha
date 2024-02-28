#4. Main analyses

#Load packages
library(tidyverse)
install.packages("patchwork")
library(patchwork)
install.packages("Hmisc")
library(Hmisc)
install.packages("survival")
library(survival)
install.packages("lubridate")
library(lubridate)
install.packages("Publish")
library(Publish)
install.packages("gtsummary")
library(gtsummary)
install.packages("ggsurvfit")
library(ggsurvfit)
library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(splines)


# Load data --------------------------------------------------------
targets::tar_make()
# Restart session
source(here::here("R/1_data_start.R"))




# Daily substituting 30 g legumes for 30g meat, poultry and fish
# defining 30 g/day variable for each food
# Changing unit to 30 g (nutri epi inspired)

data <- data %>%
    mutate(legumes30 = legume_daily/30,
           meats30 = meats_daily/30,
           poultry30 = poultry_daily/30,
           fish30 = fish_daily/30)

# HR for NAFLD when substituting 30g meat for 30g legumes
# unadjusted analysis (ua) partition model (alpha1-alpha2)
cox_meat_ua <- coxph(Surv(survival_time, nafld == 1) ~
                       # how to add the 30 g substitution?
                       legume_daily + meats_daily + poultry_daily + fish_daily +
                       #other food components
                       cereal_refined_daily + whole_grain_daily + mixed_dish_daily +
                       dairy_daily + fats_daily + fruit_daily + nut_daily +
                       veggie_daily + potato_daily + egg_daily + meat_sub_daily +
                       non_alc_beverage_daily + alc_beverage_daily + snack_daily +
                       sauce_daily + total_weight_food, data = data, ties='breslow')

summary(cox_meat_ua)
Publish::publish(cox_meat_ua)

# subtracting legumes from meats (same as dividing OR1 with OR2)
estimate1 <- hr1[legumes30]/hr1[meats30]
CI??



# Model check



# Alcohol as spline with 4 knots for adjustment
df <- 4
data <- data %>%
  mutate(alcohol_spline = predict(bs(alcohol_daily, df = df, degree = 3, knots = NULL)))


                     cereal_refined_weekly = cereal_refined_daily * 7,
                     whole_grain_weekly = whole_grain_daily * 7,
                     mixed_dish_weekly = mixed_dish_daily * 7,
                     dairy_weekly = dairy_daily * 7,
                     fats_weekly = fats_daily * 7,
                     fruit_weekly = fruit_daily * 7,
                     nut_weekly = nut_daily*7,
                     veggie_weekly = veggie_daily * 7,
                     potato_weekly = potato_daily * 7,
                     egg_weekly = egg_daily * 7,
                     meat_sub_weekly = meat_sub_daily * 7,
                     non_alc_beverages_weekly = non_alc_beverage_daily * 7,
                     alc_beverage_weekly = alc_beverage_daily * 7,
                     snack_weekly = snack_daily * 7,
                     sauce_weekly = sauce_daily * 7,
                     meats_weekly
                     poultry_weekly = poultry_daily * 7,
                     fish_weekly = fish_daily * 7,
                     # total weight of all foods
                     total_weight_food = rowSums(select(., starts_with("p26000"))),



# Weekly substituting 80 g legumes for meat (NHS 1 portion beans = 80 g) https://www.nhs.uk/live-well/eat-well/5-a-day/5-a-day-what-counts/




**Model 1** will be minimally adjusted for strata of age at recruitment
(\<45, 45-49, 50-54, 55-59, 60-64, ≤65 years) and geographical region of
recruitment (ten UK regions), sex, and intake of all other dietary
components apart from the substitute components (red and processed
meats; poultry; fish). When substituting g legumes/day, the unit for all
dietary components will be g/day and the analyses will be adjusted for
total amount of consumed foods in g/day.

**Model 2** will be further adjusted for alcohol consumption, ethnic
group (white, mixed, Asian, black, other, unknown), socioeconomic status
(Townsend deprivation score, educational level), living with a wife or
partner (yes, no), physical activity (low \[0-9.9 METs/week\], moderate
                                      \[10-49.9 METs/week\], and high \[≥50 METs/week\], unknown), smoking
status (never, former, current 1-15 cigarettes per day, current ≥15
        cigarettes per day, current but number of cigarettes per day unknown,
        and smoking status unknown), and self-reported diagnosis of diabetes,
hypertension, or high cholesterol (yes, no, unknown).

**Model 3** will further adjust for anthropometry (BMI ≥ 30 kg/m2), as
obesity may either confound or mediate the association between replacing
red and processed meats, poultry, or fish with legumes and risk of MASLD


#Cox regression
# Examples:
# cox <- coxph(Surv(time,event= death) ~ cenc0, <-- alle variable i modellen (kost, bælg, confoundere) kød/fisk/poultry er ikke med som variabel i analysen. Her skal man skalere sit indtag så det passer med modellen, fx pr 30g eller 30kcal
#              data=azat,
#              ties='breslow')
# summary(cox) #gives estimate as log(HR)=coef, and HR=exp(coef)
# publish(cox) #to get estimate as HR with 95%CI
# tbl_regression(cox, exponentiate=TRUE) #to get estimate as HR with 95%CI
#
# fit.azat <- survfit(Surv(time,event= death) ~ cenc0,
#                     data=azat,
#                     conf.type = 'log-log')
#
# #log-log plot
# plot(fit.azat,
#      col=1:2,
#      fun='cloglog',
#      main='Ex 8.2, log-log plot')
# And to check the second assumption of linearity between log hazards (=HR) and each covariate (residual plot)
#
# azat$res<-predict(cox)
# g1<-azat %>%
#     ggplot(aes(res,age)) +
#     geom_point()+
#     geom_smooth(method='coxph', formula= y~x)
#
# g2<-azat %>%
#     ggplot(aes(res,logb0)) +
#     geom_point()+
#     geom_smooth(method='coxph', formula= y~x)
#
# g3<-azat %>%
#     ggplot(aes(res,alb0)) +
#     geom_point()+
#     geom_smooth(method='coxph', formula= y~x)
#
# g1+g2+g3




# Create table with results

table2 <- data %>%
  select([outcome], [results]) %>%
  tbl_summary(by = nafld,
              statistic = list(all_continuous() ~  "{coef} ({ci_lower}, {ci_upper})", # for incidence rate
                                all_continuous() ~  "{coef} ({ci_lower}, {ci_upper})", # for HR
                               all_categorical() ~ "{n} ({p}%)"),
              digits = all_continuous() ~ 2) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_caption("Table 2. Risk of incident non-alcoholic fatty liver disease when substituting 30 g/day legumes for meat in UK Biobank cohort (N=126812)") %>%
  as_flex_table()

flextable::save_as_html(table2, path = here("doc", "table2.html"))
