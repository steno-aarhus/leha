# Diet data
# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)


# Average dietary intake of food groups -----------------------------------
calculate_total <- function(columns) {
  sum(dplyr::c_cross(columns), na.rm = TRUE)
}

calculate_daily <- function(data) {
  data |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_total"), list(daily = ~ .x / p20077))) |>
    dplyr::rename_with(~ stringr::str_replace("_total_daily", "_daily"))
}

calculate_weekly <- function(data) {
  data |>
    dplyr::mutate(dplyr::across(dplyr::ends_with("_total"), list(daily = ~ (.x / p20077) * 7) |>
    dplyr::rename_with(~ stringr::str_replace("_total_weekly", "_weekyl"))
}

calculate_food_intake <- function(diet_data) {
# estimating average daily and weekly intakes of food groups in g
data <- data %>%
  # creating food groups from UKB Aurora Perez
  rowwise() |>
  mutate(
    # refined cereals
    cereal_refined_total = calculate_total(matches("p26113|p26079|p26071|p26072|p26073|p26075|p26068|p26083")),

    # whole-grain cereals
    whole_grain_total = calculate_total(matches("p26074|p26076|p26077|p26078|p26105|p26114")),

    # mixed dishes
    mixed_dish_total = calculate_total(matches("p26128|p26097|p26116|p26135|p26139")),

    # dairy
    dairy_total = calculate_total(matches("p26154|p26087|p26096|p26102|p26103|p26099|p26131|p26133|p26150")),

    # fats and spread
    fats_total = calculate_total(matches("p26112|p26062|p26063|p26155|p26110|p26111")),

    # fruit
    fruit_total = calculate_total(matches("p26089|p26090|p26091|p26092|p26093|p26094")),

    # nuts and seeds
    nut_total = calculate_total(matches("p26107|p26108")),

    # vegetables
    veggie_total = calculate_total(matches("p26065|p26098|p26115|p26123|p26125|p26143|p26146|p26147|p26144")),

    # potatoes
    potato_total = calculate_total(matches("p26118|p26119|p26120")),

    # eggs
    egg_total = calculate_total(matches("p26088"))),

    # meat substitutes
    meat_sub_total = calculate_total(matches("p26145"))),

    # non-alcoholic beverages
    non_alc_beverage_total = calculate_total(matches("p26124|p26141|p26142|p26148|p26081|p26082|p26095|p26126|p26127")),

    # alcoholic beverages
    alc_beverage_total = calculate_total(matches("p26151|p26152|p26153|p26067|p26138")),

    # sugar, preserves, cakes & confectionery, snacks
    snack_total = calculate_total(matches("p26106|p26140|p26134|p26084|p26085|p26064|p26080")),

    # Sauces & condiments
    sauce_total = calculate_total(matches("p26129|p26130")),

    # legumes
    legume_total = calculate_total(matches("p26086|p26101|p26136|p26137")),

    # red meats
    red_meat_total = calculate_total(matches("p26066|p26100|p26104|p26117")),

    # processed meat
    proc_meat_total = calculate_total(matches("p26122"))),

    # red and processed
    meats_total = proc_meat_total + red_meat_total,

    # poultry
    poultry_total = calculate_total(matches("p26121|p26069")),

    # fish
    fish_total = calculate_total(matches("p26070|p26109|p26132|p26149")),

    # total weight of all foods
    food_weight_total = calculate_total(matches("p26000")),

    # for secondary analysis

    # legumes
    legume_pea_total = calculate_total(matches("p26086|p26101|p26136|p26137|peas")), # 1 serving = 80 g

    # vegetables
    veggie_pea_total = calculate_total(matches("p26065|p26098|p26147|p26123|p26125|p26143|p26146")) - peas
  ) |>
  calculate_daily() |>
  calculate_weekly() |>
  ungroup()
  return(diet_data)
}

# Drop p-variables for diet ------------------------------------------------------
remove_diet <- c("p26113", "p26079", "p26071","p26072", "p26073", "p26075",
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
                 "p26070", "p26109", "p26132", "p26149", "p26000")

data <- data %>%
  select(-matches(remove_diet))

# Save data ---------------------------------------------------------------
arrow::write_parquet(data, here("data/data.parquet"))
ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")
