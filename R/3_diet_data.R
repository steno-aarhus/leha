# Diet data
# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)
library(dplyr)




# Average dietary intake of food groups -----------------------------------
calculate_total <- function(columns) {
  sum(dplyr::c_cross(columns))

calculate_food_intake <- function(diet_data) {
# estimating average daily and weekly intakes of food groups in g
data <- data %>%
  # creating food groups from UKB Aurora Perez
  rowwise() |>
  mutate(
    # refined cereals
    cereal_refined_total = calculate_total(matches("p26113|p26079|p26071|p26072|p26073|p26075|p26068|p26083")),
    cereal_refined_daily = cereal_refined_total/p20077,
    cereal_refined_weekly = cereal_refined_daily * 7,

    # whole-grain cereals
    whole_grain_total = calculate_total(matches("p26074|p26076|p26077|p26078|p26105|p26114")),
    whole_grain_daily = whole_grain_total/p20077,
    whole_grain_weekly = whole_grain_daily * 7,

    # mixed dishes
    mixed_dish_total = calculate_total(matches("p26128|p26097|p26116|p26135|p26139")),
    mixed_dish_daily = mixed_dish_total/p20077,
    mixed_dish_weekly = mixed_dish_daily * 7,

    # dairy
    dairy_total = calculate_total(matches("p26154|p26087|p26096|p26102|p26103|p26099|p26131|p26133|p26150")),
    dairy_daily = dairy_total/p20077,
    dairy_weekly = dairy_daily * 7,

    # fats and spread
    fats_total = calculate_total(matches("p26112") | p26062") |
                                  p26063") | p26155") |
                                  p26110") | p26111"))),
    fats_daily = fats_total/p20077,
    fats_weekly = fats_daily * 7,
    # fruit
    fruit_total = calculate_total(matches("p26089") | p26090") |
                                   p26091") | p26092") |
                                   p26093") | p26094"))),
    fruit_daily = fruit_total/p20077,
    fruit_weekly = fruit_daily * 7,
        # nuts and seeds
    nut_total = calculate_total(matches("p26107") | p26108"))),
    nut_daily = nut_total/p20077,
    nut_weekly = nut_daily*7,
    # vegetables
    veggie_total = calculate_total(matches("p26065") | p26098") |
                                    p26115") | p26123") |
                                    p26125") | p26143") |
                                    p26146") | p26147") |
                                    p26144"))),
    veggie_daily = veggie_total/p20077,
    veggie_weekly = veggie_daily * 7,
    # potatoes
    potato_total = calculate_total(matches("p26118") | p26119") |
                                    p26120"))),
    potato_daily = potato_total/p20077,
    potato_weekly = potato_daily * 7,
    # eggs
    egg_total = calculate_total(matches("p26088"))),
    egg_daily = egg_total/p20077,
    egg_weekly = egg_daily * 7,
    # meat substitutes
    meat_sub_total = calculate_total(matches("p26145"))),
    meat_sub_daily = meat_sub_total/p20077,
    meat_sub_weekly = meat_sub_daily * 7,
    # non-alcoholic beverages
    non_alc_beverage_total = calculate_total(matches("p26124") | p26141") |
                                              p26142") | p26148") |
                                              p26081") | p26082") |
                                              p26095") | p26126") |
                                              p26127"))),
    non_alc_beverage_daily = non_alc_beverage_total/p20077,
    non_alc_beverages_weekly = non_alc_beverage_daily * 7,
    # alcoholic beverages
    alc_beverage_total = calculate_total(matches("p26151") | p26152") |
                                          p26153") | p26067") |
                                          p26138"))),
    alc_beverage_daily = alc_beverage_total/p20077,
    alc_beverage_weekly = alc_beverage_daily * 7,
    # sugar, preserves, cakes & confectionery, snacks
    snack_total = calculate_total(matches("p26106") | p26140") |
                                   p26134") | p26084") |
                                   p26085") | p26064") |
                                   p26080"))),
    snack_daily = snack_total/p20077,
    snack_weekly = snack_daily * 7,
    # Sauces & condiments
    sauce_total = calculate_total(matches("p26129") | p26130"))),
    sauce_daily = sauce_total/p20077,
    sauce_weekly = sauce_daily * 7,
    # legumes
    legume_total = calculate_total(matches("p26086") | p26101") |
                                    p26136") | p26137"),
    legume_daily = legume_total/p20077,
    legume_weekly = legume_daily * 7,
    # red meats
    red_meat_total = calculate_total(matches("p26066") | p26100") |
                                      p26104") | p26117"))),
    red_meat_daily = red_meat_total/p20077,
    red_meat_weekly = red_meat_daily * 7,
    # processed meat
    proc_meat_total = calculate_total(matches("p26122"))),
    proc_meat_daily = proc_meat_total/p20077,
    proc_meat_weekly = proc_meat_daily * 7,
    # poultry
    poultry_total = calculate_total(matches("p26121") | p26069"))),
    poultry_daily = poultry_total/p20077,
    poultry_weekly = poultry_daily * 7,
    # fish
    fish_total = calculate_total(matches("p26070") | p26109") |
                                  p26132") | p26149"))),
    fish_daily = fish_total/p20077,
    fish_weekly = fish_daily * 7,
    # total weight of all foods
    total_weight_food = rowSums(select(., starts_with("p26000"))),
    #for secondary analysis
    # legumes
    legume_pea_total = calculate_total(matches("p26086") | p26101") |
                                        p26136") | p26137" "peas"), # 1 serving = 80 g
    legume_pea_daily = legume_pea_total/p20077,
    legume_pea_weekly = legume_pea_daily * 7,
    # vegetables
    veggie_pea_total = calculate_total(matches("p26065") | p26098") |
                                p26147") | p26123") |
                                p26125") | p26143") |
                                p26146")-peas
    veggie_pea_daily = veggie_pea_total/p20077,
    veggie_pea_weekly = veggie_pea_daily * 7
  ) |>
    ungroup()
  return(diet_data)
}

diet_data <- calculate_food_intake(diet_data)


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
