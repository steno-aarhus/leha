# Descriptive analyses ------------------------------------------------------------
# baseline characteristics across legume consumption
baseline_table <- function(data) {
  # subset to only include consumers
  consumers <- data %>%
    subset(legume_weekly != 0) %>%
    mutate(legume_tert = ntile(legume_weekly, 3))
  tertile_breaks <- quantile(consumers$legume_weekly, probs = seq(0, 1, 1 / 3))

  # grouping consumption in tertiles
  data <- data %>% mutate(
    legume_groups = case_when(
      legume_weekly == 0 ~ 0,
      legume_weekly > 0 & legume_weekly < tertile_breaks[2] ~ 1,
      legume_weekly >= tertile_breaks[2] & legume_weekly < tertile_breaks[3] ~ 2,
      legume_weekly >= tertile_breaks[3] ~ 3
    )
  )

  # making the descriptive table
  table1 <- data %>%
    select(
      legume_groups, legume_weekly, nafld, age, sex, yearly_income, education,
      deprivation, cohabitation, ethnicity, physical_activity, smoking,
      alcohol_daily, bmi30, related_disease, disease_family,
      meats_weekly, poultry_weekly, fish_weekly,
      cereal_refined_weekly, whole_grain_weekly, mixed_dish_weekly,
      dairy_weekly, fats_weekly, fruit_weekly, nut_weekly, veggie_weekly,
      potato_weekly, egg_weekly, non_alc_beverage_weekly,
      alc_beverage_weekly, snack_weekly, sauce_weekly, food_weight_weekly
    ) %>%
    tbl_summary(
      by = legume_groups,
      statistic = list(
        all_continuous() ~ "{median} ({p10}, {p90})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 1,
      missing_text = "n missing"
    ) %>%
    add_overall() %>%
    bold_labels() %>%
    modify_caption("Table 1. Baseline characteristics across consumption of legumes in the UK Biobank cohort") %>%
    as_flex_table()

  flextable::save_as_html(table1, path = here("doc", "table1.html"))
}


# baseline characteristics across outcome status
supplementary_baseline_table <- function(data) {
  suppl_base <- data %>%
    select(
      nafld, age, sex, yearly_income, education, deprivation, cohabitation,
      ethnicity, physical_activity, smoking, alcohol_daily, region, bmi30,
      related_disease, disease_family,
      legume_weekly, meats_weekly, poultry_weekly, fish_weekly,
      cereal_refined_weekly, whole_grain_weekly, mixed_dish_weekly,
      dairy_weekly, fats_weekly, fruit_weekly, nut_weekly, veggie_weekly,
      potato_weekly, egg_weekly, non_alc_beverage_weekly,
      alc_beverage_weekly, snack_weekly, sauce_weekly, food_weight_weekly
    ) %>%
    tbl_summary(
      by = nafld,
      statistic = list(
        all_continuous() ~ "{median} ({p10}, {p90})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      digits = all_continuous() ~ 1,
      missing_text = "n missing"
    ) %>%
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

diff_time_webQ <- function(data) {
  data <- data %>%
    mutate(
      ques_comp_t1 = as.Date(ques_comp_t1),
      ques_comp_t2 = as.Date(ques_comp_t2),
      ques_comp_t3 = as.Date(ques_comp_t3),
      ques_comp_t4 = as.Date(ques_comp_t4)
    ) %>%
    rowwise() %>%
    mutate(
      diff_t1_t2 = as.numeric(difftime(ques_comp_t2, ques_comp_t1, units = "days")),
      diff_t2_t3 = as.numeric(difftime(ques_comp_t3, ques_comp_t2, units = "days")),
      diff_t3_t4 = as.numeric(difftime(ques_comp_t4, ques_comp_t3, units = "days"))
    ) %>%
    ungroup()

  all_differences <- data %>%
    select(diff_t1_t2, diff_t2_t3, diff_t3_t4) %>%
    pivot_longer(
      cols = everything(),
      values_to = "interval"
    ) %>%
    filter(!is.na(interval)) %>%
    pull(interval)

  tibble(
    mean_interval_days = mean(all_differences),
    sd_interval_days   = sd(all_differences),
    n_intervals        = length(all_differences)
  )
}

# cut-offs for alcohol splines
compute_alcohol_spline <- function(data) {
  data <- data %>% mutate(
    alcohol_intake = rowSums(pick(matches("p26030")), na.rm = TRUE),
    alcohol_daily = alcohol_intake / p20077,
    alcohol_weekly = alcohol_daily * 7,
    alc_spline = splines::bs(alcohol_weekly, knots = 4, degree = 3)
  )

  # Knots based on quantiles
  nknots <- 4  # Number of knots
  internal_knots <- quantile(data$alcohol_weekly, probs = seq(0.25, 0.75, length.out = nknots), na.rm = TRUE)

  # Generate spline using specified knots
  spline_basis <- splines::bs(data$alcohol_weekly, knots = internal_knots, degree = 3)

  # Extract and print the knot positions
  knots <- attr(spline_basis, "knots")

  cat("knots are placed at:", knots, "\n")
}

