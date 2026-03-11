# Function to extract data into tables  ------------------------------------------------------

## Extract results  ------------------------------------------------------
extract <- function(model_results, model_name) {
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
      Model = model_name,
      value = paste0(estimate, " (", conf.low, "; ", conf.high, ")"),
      exposure = case_when(
        grepl("meat", model_type) ~ "Red and processed meat",
        grepl("poultry", model_type) ~ "Poultry",
        grepl("fish", model_type) ~ "Fish"
      )
    ) %>%
    select(Model, exposure, value)
}

## Apply to analyses  ------------------------------------------------------

### Main analyses -----------------------------------------------------------
table_main_analyses <- function(data){
rows1 <- extract(main_model1(data), "Model 1")
rows2 <- extract(main_model2(data), "Model 2")
rows3 <- extract(main_model3(data), "Model 3")

table_main <- bind_rows(rows1, rows2, rows3) %>%
  pivot_wider(names_from = exposure, values_from = value) %>%
  print()
}
### Secondary analyses ------------------------------------------------------
table_secondary_analyses <- function(data){
rows1_sec <- extract(consumers_analyses(data), "Consumers only")
rows2_sec <- extract(legumes_and_peas(data), "Legumes and peas")
rows3_sec <- extract(legumes_without_soy(data), "Legumes without soymilk")

table_secondary <- bind_rows(rows1_sec, rows2_sec, rows3_sec) %>%
pivot_wider(names_from = exposure, values_from = value) %>%
  print()
}

### Sensitivity analyses ----------------------------------------------------
table_sensitivity_analyses <- function(data){
rows1_sens <- extract(alcohol_restricted_analyses(data), "Alcohol below 90 percentile")
rows2_sens <- extract(normal_liver_analyses(data), "ALT below threshold")
rows3_sens <- extract(three_recalls_analyses(data), ">3 completed Oxford WebQ")
rows4_sens <- extract(low_alc_cases_analyses(data), "NAFLD diagnosis + alcohol intake below threshold")


table_sensitivity <- bind_rows(rows1_sens, rows2_sens, rows3_sens, rows4_sens) %>%
  pivot_wider(names_from = exposure, values_from = value) %>%
  print()
}

