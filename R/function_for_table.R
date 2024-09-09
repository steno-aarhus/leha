# table for results
# analyses first:
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

then I want to create a table where I can paste my results
for each list, I want to copy and paste the first line {estimate "("conf.low";" conf.high")"}
for the list starting with meat, I want the result in position [2,2] of my table

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

#
# table_df <- data.frame(
#   model = c("Model 1", "Model 2", "Model 3"),
#   meat = c(data$meat1),
#   poultry = c(meat1, meat2, meat3),
#   fish = c(meat1, meat2, meat3)
# )
# knitr::kable(table_df, col.names = c("Statistical model", "Red and processed meat"), row.names = TRUE)




