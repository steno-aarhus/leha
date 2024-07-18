# descriptives

# Descriptive analyses ------------------------------------------------------------
# baseline characteristics across legume consumption
baseline_table <- function(data) {
  # subset to only include consumers
  consumers <- data %>%
    subset(legume_weekly != 0) %>%
    mutate(legume_tert = ntile(legume_weekly, 3))
  tertile_breaks <- quantile(consumers$legume_weekly, probs = seq(0, 1, 1/3))

  #grouping consumption in tertiles
  data <- data %>% mutate(
    legume_groups = case_when(
      legume_weekly == 0 ~ 0,
      legume_weekly >0 & legume_weekly < tertile_breaks[2] ~ 1,
      legume_weekly >= tertile_breaks[2] & legume_weekly < tertile_breaks[3] ~ 2,
      legume_weekly >= tertile_breaks[3] ~ 3)
  )

  # making the descriptive table
  table1 <- data %>%
    select(legume_groups, legume_weekly, nafld, age, sex, yearly_income, education,
           deprivation, cohabitation, ethnicity, physical_activity, smoking,
           alcohol_weekly, region, bmi30, related_disease, cancer, disease_family,
           meats_weekly, poultry_weekly, fish_weekly,
           cereal_refined_weekly, whole_grain_weekly, mixed_dish_weekly,
           dairy_weekly, fats_weekly, fruit_weekly, nut_weekly, veggie_weekly,
           potato_weekly, egg_weekly, non_alc_beverage_weekly,
           alc_beverage_weekly, snack_weekly, sauce_weekly, food_weight_weekly) %>%
    tbl_summary(by = legume_groups,
                statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                                 all_categorical() ~ "{n} ({p}%)"),
                digits = all_continuous() ~ 1,
                missing_text = "n missing") %>%
    add_overall() %>%
    bold_labels() %>%
    modify_caption("Table 1. Baseline characteristics across consumption of legumes in the UK Biobank cohort") %>%
    as_flex_table()

  flextable::save_as_html(table1, path = here("doc", "table1.html"))
}


# baseline characteristics across outcome status
supplementary_baseline_table <- function(data){
  suppl_base <- data %>%
    select(nafld, age, sex, yearly_income, education, deprivation, cohabitation, ethnicity, physical_activity, smoking, alcohol_daily, region, bmi30, related_disease, cancer, disease_family) %>%
    tbl_summary(by = nafld,
                statistic = list(all_continuous() ~  "{median} ({p10}, {p90})",
                                 all_categorical() ~ "{n} ({p}%)"),
                digits = all_continuous() ~ 1,
                missing_text = "n missing") %>%
    add_overall() %>%
    bold_labels() %>%
    modify_caption("Supplementary Baseline Table. Baseline characteristics of participants in the UK Biobank Cohort across NAFLD outcome") %>%
    as_flex_table()

  flextable::save_as_html(suppl_base, path = here("doc", "suppl_base.html"))
  return(data)
}

# Person-years of follow-up
person_years_followup <- function(data) {
  print(sum(data$time))
  print(summary(data$time))
  return(data)
}

# correlation between touchscreen and WebQ reported foods
spearman_correlation <- function(data){
  meat_correlation <- cor(data$total_meat, data$habitual_meat, use = "complete.obs", method = c("spearman")) %>% print()
  poultry_correlation <- cor(data$total_poultry, data$habitual_poultry, use = "complete.obs", method = c("spearman")) %>% print()
  fish_correlation <- cor(data$total_fish, data$habitual_fish, use = "complete.obs", method = c("spearman")) %>% print()
  return(data)
}

pearson_correlation <- function(data){
  meat_correlation <- cor(data$total_meat, data$habitual_meat, use = "complete.obs", method = c("pearson")) %>% print()
  poultry_correlation <- cor(data$total_poultry, data$habitual_poultry, use = "complete.obs", method = c("pearson")) %>% print()
  fish_correlation <- cor(data$total_fish, data$habitual_fish, use = "complete.obs", method = c("pearson")) %>% print()
  return(data)
}
