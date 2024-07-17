# Diet data
# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)

# Load data
# data <- read_csv(here("data/data.csv"))

# Average weekly dietary intake of food groups -----------------------------------
# Food groups intakes are summed across rows and divided by the number of 24h recalls
# completed (p20077) and multiplied with 7 to estimate the weekly intake of each food.

calculate_weekly_diet <- function(variables, number_recalls) {
  rowSums(dplyr::pick(tidyselect::matches(variables)), na.rm = TRUE) / number_recalls * 7
}

data <- data %>%
  # creating food groups from UKB Aurora Perez
  mutate(
    legume_weekly = calculate_weekly_diet("p26086|p26101|p26136|p26137", p20077),
    meats_weekly = calculate_weekly_diet("p26066|p26100|p26104|p26117|p26122", p20077),
    poultry_weekly = calculate_weekly_diet("p26121|p26069", p20077),
    fish_weekly = calculate_weekly_diet("p26070|p26109|p26132|p26149", p20077),
    cereal_refined_weekly = calculate_weekly_diet("p26113|p26079|p26071|p26072|p26073|p26075|p26068|p26083", p20077),
    whole_grain_weekly = calculate_weekly_diet("p26074|p26076|p26077|p26078|p26105|p26114", p20077),
    mixed_dish_weekly = calculate_weekly_diet("p26128|p26097|p26116|p26135|p26139|p26145", p20077),
    dairy_weekly = calculate_weekly_diet("p26154|p26087|p26096|p26102|p26103|p26099|p26131|p26133|p26150", p20077),
    fats_weekly = calculate_weekly_diet("p26112|p26062|p26063|p26155|p26110|p26111", p20077),
    fruit_weekly= calculate_weekly_diet("p26089|p26090|p26091|p26092|p26093|p26094", p20077),
    nut_weekly = calculate_weekly_diet("p26107|p26108", p20077),
    veggie_weekly = calculate_weekly_diet("p26065|p26098|p26115|p26123|p26125|p26143|p26146|p26147|p26144", p20077),
    potato_weekly = calculate_weekly_diet("p26118|p26119|p26120", p20077),
    egg_weekly = calculate_weekly_diet("p26088", p20077),
    non_alc_beverage_weekly = calculate_weekly_diet("p26124|p26141|p26142|p26148|p26081|p26082|p26095|p26126|p26127", p20077),
    alc_beverage_weekly = calculate_weekly_diet("p26151|p26152|p26153|p26067|p26138", p20077),
    snack_weekly = calculate_weekly_diet("p26106|p26140|p26134|p26084|p26085|p26064|p26080", p20077),
    sauce_weekly = calculate_weekly_diet("p26129|p26130", p20077),
    food_weight_weekly = legume_weekly + meats_weekly + poultry_weekly + fish_weekly + cereal_refined_weekly + whole_grain_weekly +
      mixed_dish_weekly + dairy_weekly + fats_weekly + fruit_weekly + nut_weekly + veggie_weekly + potato_weekly + egg_weekly +
      non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly + sauce_weekly,
    legume_pea_weekly = calculate_weekly_diet("p26086|p26101|p26136|p26137|peas", p20077),
    veggie_pea = ((rowSums(pick(matches("p26065|p26098|p26147|p26123|p26125|p26143|p26146")), na.rm = TRUE) - peas) / p20077) * 7
  )

# estimating total intakes of certain foods based on 24 h dietary assessments and touchscreens
data <- data %>% mutate(
  total_meat = rowSums(pick(matches("p26066|p26100|p26104|p26117|p26122")), na.rm = TRUE),
  total_poultry = rowSums(pick(matches("p26121|p26069")), na.rm = TRUE),
  total_fish = rowSums(pick(matches("p26070|p26109|p26132|p26149")), na.rm = TRUE),
)



transform_touchscreen <- function(data) {
  convert_frequency <- function(column) {
    case_when(
      str_detect(column, "Never") | str_detect(column, "know")  ~ 0,
      str_detect(column, "Less") ~ 1,
      column == "Once a week" ~ 2,
      str_detect(column, "2-4 times") ~ 3,
      str_detect(column, "5-6 times") ~ 4,
      str_detect(column, "Once or more") ~ 5,
      TRUE ~ NA
    ) %>% as.numeric()
  }

  columns_to_transform <- data %>%
    select(matches("p1329|p1339|1349|1359|1369|1379|1389")) %>%
    names()


  for (column in columns_to_transform) {
    data <- data %>%
      mutate(!!sym(column) := convert_frequency(.data[[column]]))
  }

  return(data)
}
data <-transform_touchscreen(data)

data <- data %>% mutate(
  habitual_meat = rowSums(pick(matches("p1349|p1369|p1379|p1389")), na.rm = TRUE),
  habitual_meat = as.numeric(habitual_meat),
  habitual_poultry = rowSums(pick(matches("p1359")), na.rm = TRUE),
  habitual_poultry = as.numeric(habitual_poultry),
  habitual_fish = rowSums(pick(matches("p1329|p1339")), na.rm = TRUE),
  habitual_fish = as.numeric(habitual_fish)
)


# # Drop p-variables for diet ------------------------------------------------------
remove_diet <- c(
"p26113", "p26079", "p26071", "p26072", "p26073", "p26075",
"p26068", "p26083", "p26074", "p26076", "p26077", "p26078",
"p26105", "p26114", "p26128", "p26097", "p26116", "p26135",
"p26139", "p26154", "p26087", "p26096", "p26102", "p26103",
"p26099", "p26131", "p26133", "p26150", "p26112", "p26062",
"p26063", "p26155", "p26110", "p26111", "p26089", "p26090",
"p26091", "p26092", "p26093", "p26094", "p26107", "p26108",
"p26065", "p26098", "p26115", "p26123", "p26125", "p26143",
"p26146", "p26147", "p26144", "p26118", "p26119", "p26120",
"p26088", "p26145", "p26124", "p26141", "p26142", "p26148",
"p26081", "p26082", "p26095", "p26126", "p26127", "p26151",
"p26152", "p26153", "p26067", "p26138", "p26106", "p26140",
"p26134", "p26084", "p26085", "p26064", "p26080", "p26129",
"p26130", "p26086", "p26101", "p26136", "p26137", "p26066",
"p26100", "p26104", "p26117", "p26122", "p26121", "p26069",
"p26070", "p26109", "p26132", "p26149", "p26000", "p104280",
"p1329", "p1339", "p1349", "p1359", "p1369", "p1379", "p1389"
)
data <- data %>%
  select(-matches(remove_diet))
#
# # Save data ---------------------------------------------------------------
# arrow::write_parquet(data, here("data/data.parquet"))
# ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")
