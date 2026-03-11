# table for results
# analyses first:
library(broom)
create_formula <- function(xvars, covars) {
  outcome <- "Surv(survival_time, nafld == 1)"
  reformulate(c(xvars, covars), response = outcome)
}
# Main analyses -----------------------------------------------------------
main_model1 <- function(data) {
  covars1 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
               "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
               "veggie_weekly", "potato_weekly", "egg_weekly",
               "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
               "sauce_weekly", "food_weight_weekly", "strata(region, age_strata, sex)")


  model1_formulas <- list(
    meat_model1 = create_formula(c("legumes80", "poultry80", "fish80"), covars1),
    poultry_model1 = create_formula(c("legumes80", "meats80", "fish80"), covars1),
    fish_model1 = create_formula(c("legumes80", "meats80", "poultry80"), covars1)
  )

  model1_results <- model1_formulas |>
    map(~ coxph(.x, data = data, ties = "breslow")) |>
    map2(names(model1_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
           mutate(across(where(is.numeric), ~ round(.x, 2))) |>
           mutate(model = .y))

  return(model1_results)
}

main_model2<- function(data) {
  covars2 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
               "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
               "veggie_weekly", "potato_weekly", "egg_weekly",
               "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
               "sauce_weekly", "food_weight_weekly", "alc_spline", "ethnicity",
               "deprivation", "education", "cohabitation", "physical_activity",
               "smoking", "related_disease", "disease_family", "yearly_income",
               "strata(region, age_strata, sex)")


  model2_formulas <- list(
    meat_model2 = create_formula(c("legumes80", "poultry80", "fish80"), covars2),
    poultry_model2 = create_formula(c("legumes80", "meats80", "fish80"), covars2),
    fish_model2 = create_formula(c("legumes80", "meats80", "poultry80"), covars2)
  )

  model2_results <- model2_formulas |>
    map(~ coxph(.x, data = data, ties = "breslow")) |>
    map2(names(model2_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
           mutate(across(where(is.numeric), ~ round(.x, 2))) |>
           mutate(model = .y))

  return(model2_results)
}

main_model3<- function(data) {
  covars3 <- c("cereal_refined_weekly", "whole_grain_weekly", "mixed_dish_weekly",
               "dairy_weekly", "fats_weekly", "fruit_weekly", "nut_weekly",
               "veggie_weekly", "potato_weekly", "egg_weekly",
               "non_alc_beverage_weekly", "alc_beverage_weekly", "snack_weekly",
               "sauce_weekly", "food_weight_weekly", "alc_spline", "ethnicity",
               "deprivation", "education", "cohabitation", "physical_activity",
               "smoking", "related_disease", "disease_family", "yearly_income",
               "bmi30", "strata(region, age_strata, sex)")


  model3_formulas <- list(
    meat_model3 = create_formula(c("legumes80", "poultry80", "fish80"), covars3),
    poultry_model3 = create_formula(c("legumes80", "meats80", "fish80"), covars3),
    fish_model3 = create_formula(c("legumes80", "meats80", "poultry80"), covars3)
  )

  model3_results <- model3_formulas |>
    map(~ coxph(.x, data = data, ties = "breslow")) |>
    map2(names(model3_formulas), ~ tidy(.x, exponentiate = TRUE, conf.int = TRUE) |>
           mutate(across(where(is.numeric), ~ round(.x, 2))) |>
           mutate(model = .y))
  return(model3_results)
}
model1_results <- main_model1(data)
model2_results <- main_model2(data)
model3_results <- main_model3(data)

# then I want to create a table where I can paste my results
# for each list, I want to copy and paste the first line {estimate "("conf.low";" conf.high")"}
# for the list starting with meat, I want the result in position [2,2] of my table

meat1estimate <- model1_results$meat_model1[1,2]
meat1ci_low <- model1_results$meat_model1[1,6]
meat1ci_high <- model1_results$meat_model1[1,7]

meat2estimate <- model2_results$meat_model2[1,2]
meat2ci_low <- model2_results$meat_model2[1,6]
meat2ci_high <- model2_results$meat_model2[1,7]

meat3estimate <- model3_results$meat_model3[1,2]
meat3ci_low <- model3_results$meat_model3[1,6]
meat3ci_high <- model3_results$meat_model3[1,7]

data <- data %>%
  mutate(meat1 = glue::glue("{meat1estimate} ({meat1ci_low}; {meat1ci_high})"),
         meat2 = glue::glue("{meat2estimate} ({meat2ci_low}; {meat2ci_high})"),
         meat3 = glue::glue("{meat3estimate} ({meat3ci_low}; {meat3ci_high})"))


table_df <- data.frame(
  meat = glue::glue("{meat1estimate} ({meat1ci_low}; {meat1ci_high})"))

table_df <- data.frame(
  model = c("Model 1", "Model 2", "Model 3"),
  meat = data$meat1, data$meat2, data$meat3,
  )

# From Luke:
# Instead of working with three (or more) dataframes, combine them together into
# one dataframe via bind_rows() or similar and then use filter() to take out what
# you want.


table_df <- data.frame(
  model = c("Model 1", "Model 2", "Model 3"),
  meat = c(data$meat1),
  poultry = c(meat1, meat2, meat3),
  fish = c(meat1, meat2, meat3)
)
knitr::kable(table_df, col.names = c("Statistical model", "Red and processed meat"), row.names = TRUE)





all_results <- bind_rows(
  model1_results %>% bind_rows(.id = "model_type") %>% mutate(model = "Model 1"),
  model2_results %>% bind_rows(.id = "model_type") %>% mutate(model = "Model 2"),
  model3_results %>% bind_rows(.id = "model_type") %>% mutate(model = "Model 3")
)

table_df <- all_results %>%
  filter(grepl("meat", model_type)) %>%  # select rows for meat
  select(model, estimate, conf.low, conf.high) %>%
  mutate(meat = paste0(estimate, " (", conf.low, "; ", conf.high, ")")) %>%
  select(model, meat)


table_all <- all_results %>%
  filter(grepl("legumes|meat|poultry|fish", model_type)) %>%
  mutate(value = paste0(estimate, " (", conf.low, "; ", conf.high, ")")) %>%
  select(model, model_type, value) %>%
  tidyr::pivot_wider(names_from = model_type, values_from = value)


# 1️⃣ Helper function to extract the first row of each model
extract_first_row <- function(model_results, model_name) {
  bind_rows(model_results, .id = "model_type") %>%
    slice(1) %>%  # take only the first row
    mutate(model = model_name,
           value = paste0(estimate, " (", conf.low, "; ", conf.high, ")")) %>%
    select(model, model_type, value)
}

# 2️⃣ Extract first rows for each model
first_rows1 <- extract_first_row(model1_results, "Model 1")
first_rows2 <- extract_first_row(model2_results, "Model 2")
first_rows3 <- extract_first_row(model3_results, "Model 3")

# 3️⃣ Combine all first rows
all_first_rows <- bind_rows(first_rows1, first_rows2, first_rows3)

# 4️⃣ Pivot wider to get meat, poultry, fish in separate columns
table_clean <- all_first_rows %>%
  mutate(exposure = case_when(
    grepl("meat", model_type) ~ "meat",
    grepl("poultry", model_type) ~ "poultry",
    grepl("fish", model_type) ~ "fish"
  )) %>%
  select(model, exposure, value) %>%
  pivot_wider(names_from = exposure, values_from = value)

# 5️⃣ View final table
table_clean

