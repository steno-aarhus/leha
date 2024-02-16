# estimating average daily and weekly intakes of food groups in g
diet_data <- sorted_data %>%
  mutate(
    cereal_refined_total = rowSums(select(., starts_with("p26113") | starts_with("p26079") |
                                            starts_with("p26071") | starts_with("p26072") |
                                            starts_with("p26073") | starts_with("p26075") |
                                            starts_with("p26068") | starts_with("p26083"))),
    cereal_refined_daily = cereal_refined_total/p20077,
    cereal_refined_weekly = cereal_refined_daily * 7,
    whole_grain_total = rowSums(select(., starts_with("p26074") | starts_with("p26076") |
                                         starts_with("p26077") | starts_with("p26078") |
                                         starts_with("p26105") | starts_with("p26114"))),
    whole_grain_daily = whole_grain_total/p20077,
    whole_grain_weekly = whole_grain_daily * 7,
    mixed_dish_total = rowSums(select(., starts_with("p26128") | starts_with("p26097") |
                                        starts_with("p26116") | starts_with("p26135") |
                                        starts_with("p26139"))),
    mixed_dish_daily = mixed_dish_total/p20077,
    mixed_dish_weekly = mixed_dish_daily * 7,
    dairy_total = rowSums(select(., starts_with("p26154") | starts_with("p26087") |
                                   starts_with("p26096") | starts_with("p26102") |
                                   starts_with("p26103") | starts_with("p26099") |
                                   starts_with("p26131") | starts_with("p26133") |
                                   starts_with("p26150"))),
    dairy_daily = dairy_total/p20077,
    dairy_weekly = dairy_daily * 7,
    fats_total = rowSums(select(., starts_with("p26112") | starts_with("p26062") |
                                  starts_with("p26063") | starts_with("p26155") |
                                  starts_with("p26110") | starts_with("p26111"))),
    fats_daily = fats_total/p20077,
    fats_weekly = fats_daily * 7,
    fruit_total = rowSums(select(., starts_with("p26089") | starts_with("p26090") |
                                   starts_with("p26091") | starts_with("p26092") |
                                   starts_with("p26093") | starts_with("p26094"))),
    fruit_daily = fruit_total/p20077,
    fruit_weekly = fruit_daily * 7,
    nut_total = rowSums(select(., starts_with("p26107") | starts_with("p26108"))),
    nut_daily = nut_total/p20077,
    nut_weekly = nut_daily*7,
    veggie_total = rowSums(select(., starts_with("p26065") | starts_with("p26098") |
                                    starts_with("p26115") | starts_with("p26123") |
                                    starts_with("p26125") | starts_with("p26143") |
                                    starts_with("p26146") | starts_with("p26147") |
                                    starts_with("p26144"))),
    veggie_daily = veggie_total/p20077,
    veggie_weekly = veggie_daily * 7,
    potato_total = rowSums(select(., starts_with("p26118") | starts_with("p26119") |
                                    starts_with("p26120"))),
    potato_daily = potato_total/p20077,
    potato_weekly = potato_daily * 7,
    egg_total = rowSums(select(., starts_with("p26088"))),
    egg_daily = egg_total/p20077,
    egg_weekly = egg_daily * 7,
    meat_sub_total = rowSums(select(., starts_with("p26145"))),
    meat_sub_daily = meat_sub_total/p20077,
    meat_sub_weekly = meat_sub_daily * 7,
    non_alc_beverage_total = rowSums(select(., starts_with("p26124") | starts_with("p26141") |
                                              starts_with("p26142") | starts_with("p26148") |
                                              starts_with("p26081") | starts_with("p26082") |
                                              starts_with("p26095") | starts_with("p26126") |
                                              starts_with("p26127"))),
    non_alc_beverage_daily = non_alc_beverage_total/p20077,
    non_alc_beverages_weekly = non_alc_beverage_daily * 7,
    alc_beverage_total = rowSums(select(., starts_with("p26151") | starts_with("p26152") |
                                          starts_with("p26153") | starts_with("p26067") |
                                          starts_with("p26138"))),
    alc_beverage_daily = alc_beverage_total/p20077,
    alc_beverage_weekly = alc_beverage_daily * 7,
    snack_total = rowSums(select(., starts_with("p26106") | starts_with("p26140") |
                                   starts_with("p26134") | starts_with("p26084") |
                                   starts_with("p26085") | starts_with("p26064") |
                                   starts_with("p26080"))),
    snack_daily = snack_total/p20077,
    Snack_weekly = snack_daily * 7,
    sauce_total = rowSums(select(., starts_with("p26129") | starts_with("p26130"))),
    sauce_daily = sauce_total/p20077,
    sauce_weekly = sauce_daily * 7,
    legume_total = rowSums(select(., starts_with("p26086") | starts_with("p26101") |
                                    starts_with("p26136") | starts_with("p26137"))),
    legume_daily = legume_total/p20077,
    legume_weekly = legume_daily * 7,
    red_meat_total = rowSums(select(., starts_with("p26066") | starts_with("p26100") |
                                      starts_with("p26104") | starts_with("p26117"))),
    red_meat_daily = red_meat_total/p20077,
    red_meat_weekly = red_meat_daily * 7,
    proc_meat_total = rowSums(select(., starts_with("p26122"))),
    proc_meat_daily = proc_meat_total/p20077,
    proc_meat_weekly = proc_meat_daily * 7,
    poultry_total = rowSums(select(., starts_with("p26121") | starts_with("p26069"))),
    poultry_daily = poultry_total/p20077,
    poultry_weekly = poultry_daily * 7,
    fish_total = rowSums(select(., starts_with("p26070") | starts_with("p26109") |
                                  starts_with("p26132") | starts_with("p26149"))),
    fish_daily = fish_total/p20077,
    fish_weekly = fish_daily * 7,
    total_weight_food = rowSums(select(., starts_with("p26000"))),
    legume_pea_total = rowSums(select(., starts_with("p26086") | starts_with("p26101") |
                                        starts_with("p26136") | starts_with("p26137") |
                                        "peas")), #1 serving = 80g
    legume_pea_daily = legume_pea_total/p20077,
    legume_pea_weekly = legume_pea_daily * 7,
    veggie_pea_total = rowSums(select(., starts_with("p26065") | starts_with("p26098") |
                                        starts_with("p26147") | starts_with("p26123") |
                                        starts_with("p26125") | starts_with("p26143") |
                                        starts_with("p26146")))-peas,
    veggie_pea_daily = veggie_pea_total/p20077,
    veggie_pea_weekly = veggie_pea_daily * 7
  )
