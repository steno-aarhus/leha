# Results tables


# Main analyses -----------------------------------------------------------
main_extract_first_rows <- function(model_results, model_name) {
  bind_rows(lapply(names(model_results), function(x) {
    row <- model_results[[x]][1, ]  # first row of each list
    tibble(
      model_type = x,
      estimate = row$estimate,
      conf.low = row$conf.low,
      conf.high = row$conf.high
    )
  })) %>%
    mutate(
      model = model_name,
      value = paste0(estimate, " (", conf.low, "; ", conf.high, ")"),
      exposure = case_when(
        grepl("meat", model_type) ~ "Red and processed meat",
        grepl("poultry", model_type) ~ "Poultry",
        grepl("fish", model_type) ~ "Fish"
      )
    ) %>%
    select(model, exposure, value)
}

# Apply to all three models
rows1 <- main_extract_first_rows(model1_results, "Model 1")
rows2 <- main_extract_first_rows(model2_results, "Model 2")
rows3 <- main_extract_first_rows(model3_results, "Model 3")

table <- bind_rows(rows1, rows2, rows3) %>%
  pivot_wider(names_from = exposure, values_from = value)

print(table)



# Secondary analyses ------------------------------------------------------
main_extract_first_rows <- function(model_results, model_name) {
  bind_rows(lapply(names(model_results), function(x) {
    row <- model_results[[x]][1, ]  # first row of each list
    tibble(
      model_type = x,
      estimate = row$estimate,
      conf.low = row$conf.low,
      conf.high = row$conf.high
    )
  })) %>%
    mutate(
      model = model_name,
      value = paste0(estimate, " (", conf.low, "; ", conf.high, ")"),
      exposure = case_when(
        grepl("meat", model_type) ~ "Red and processed meat",
        grepl("poultry", model_type) ~ "Poultry",
        grepl("fish", model_type) ~ "Fish"
      )
    ) %>%
    select(model, exposure, value)
}

# Apply to all three models
rows1 <- main_extract_first_rows(model1_results, "Model 1")
rows2 <- main_extract_first_rows(model2_results, "Model 2")
rows3 <- main_extract_first_rows(model3_results, "Model 3")

table <- bind_rows(rows1, rows2, rows3) %>%
  pivot_wider(names_from = exposure, values_from = value)

print(table)

main_extract_first_rows <- function(model_results, model_name) {
  bind_rows(lapply(names(model_results), function(x) {
    row <- model_results[[x]][1, ]  # first row of each list
    tibble(
      model_type = x,
      estimate = row$estimate,
      conf.low = row$conf.low,
      conf.high = row$conf.high
    )
  })) %>%
    mutate(
      model = model_name,
      value = paste0(estimate, " (", conf.low, "; ", conf.high, ")"),
      exposure = case_when(
        grepl("meat", model_type) ~ "Red and processed meat",
        grepl("poultry", model_type) ~ "Poultry",
        grepl("fish", model_type) ~ "Fish"
      )
    ) %>%
    select(model, exposure, value)
}

# Apply to all three models
rows1 <- main_extract_first_rows(model1_results, "Model 1")
rows2 <- main_extract_first_rows(model2_results, "Model 2")
rows3 <- main_extract_first_rows(model3_results, "Model 3")

table_main <- bind_rows(rows1, rows2, rows3) %>%
  pivot_wider(names_from = exposure, values_from = value)

table_main
# Secondary analyses ------------------------------------------------------
rows1_sec <- extract(consumers_analyses(data), "Consumers only")
rows2_sec <- extract(legumes_and_peas(data), "Legumes and peas")
rows3_sec <- extract(legumes_without_soy(data), "Legumes without soymilk")

table_secondary <- bind_rows(rows1_sec, rows2_sec, rows3_sec) %>%
pivot_wider(names_from = exposure, values_from = value)

table_secondary



# Sensitivity analyses ----------------------------------------------------

rows1_sens <- extract(alcohol_restricted_analyses(data), "Alcohol below 90 percentile")
rows2_sens <- extract(normal_liver_analyses(data), "ALT below threshold")
rows3_sens <- extract(three_recalls_analyses(data), ">3 completed Oxford WebQ")
rows4_sens <- extract(low_alc_cases_analyses(data), "NAFLD diagnosis + alcohol intake below threshold")


table_sensitivity <- bind_rows(rows1_sens, rows2_sens, rows3_sens, rows4_sens) %>%
  pivot_wider(names_from = exposure, values_from = value)

table_sensitivity
