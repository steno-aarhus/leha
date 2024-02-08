# Diet data


# Load packages -----------------------------------------------------------
library(tidyverse)
library(magrittr)

# Average dietary intake of food groups -----------------------------------


#energy intake
energy = rowMeans(dplyr::across(dplyr::starts_with("p26002")), na.rm = TRUE) * 0.239,
p26002 = energy


# estimating average daily and weekly intakes of food groups in g
diet_data <- data %>%
  #include foods from 24h recalls, number of recalls, and id's
  select(starts_with("p26"), "p20077") %>%
  # creating food groups from UKB Aurora Perez
  mutate(
    # refined cereals


    cereal_refined_total = rowSums(select(., starts_with("p26113") | starts_with("p26079") |
                                            starts_with("p26071") | starts_with("p26072") |
                                            starts_with("p26073") | starts_with("p26075") |
                                            starts_with("p26068") | starts_with("p26083")
                                            ))


    cereal_refined_daily = cereal_refined_total/p20077,
    cereal_refined_weekly = cereal_refined_daily * 7,
    # whole-grain cereals
    cereal_whole_total = rowSums(select(., starts_with("p26074") | starts_with("p26076") |
                                          starts_with("p26077") | starts_with("p26078") |
                                          starts_with("p26105") | starts_with("p26114"))),

    # mixed dishes

    p26128,"Samosa, pakora",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26128
    p26097,Grain dishes - added fat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26097
    p26116,Pizza,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26116
    p26135,Soups,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26135
    p26139,Sushi,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26139


Pizza	Pizza (including gluten free crust)
Grain dishes - added fat	Double and single crust pies, crumble pies, Yorkshire pudding, snackpot noodles
Samosa, pakora	Indian samosa, pakora snacks
Soups	Soups, homemade, powdered and canned
Sushi	Sushi
  )
    # dairy
p26154,Cream,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26154
p26087,Milk-based and powdered drinks,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26087
p26096,Full fat yogurt,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26096
p26102,Low fat yogurt,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26102
p26103,Medium and low fat cheese,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26103
p26099,High fat cheese,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26099
p26131,Semi skimmed milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26131
p26133,Skimmed milk and cholesterol-lowering milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26133
p26150,Whole milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26150


    # fats and spread
p26032
p26062,Animal fat spread lower fat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26062
p26063,Animal fat spread normal,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26063
p26155,Trans fatty acids,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26155
p26110,Olive oil (drizzling/dunking),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26110
p26111,Plant-based spread lower fat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26111
p26112,Plant-based spread normal,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26112

    # fruits
p26089,Apples and pears,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26089
p26090,Berries,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26090
p26091,Citrus,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26091
p26092,Dried fruit,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26092
p26093,Other fruit,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26093
p26094,Stewed fruit,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26094



    # nuts and seeds
Salted nuts & seeds	Salted peanuts and nuts
Unsalted nuts & seeds	Unsalted peanuts and nuts
p26107,Unsalted nuts and seeds,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26107
p26108,Salted nuts and seeds,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26108

    # vegetables
p26065,Allium vegetables,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26065
p26098,Green leafy/cabbages,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26098
p26115,Peas and sweetcorn,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26115
p26123,Raw salad,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26123
p26125,Root vegetables,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26125
p26143,Tomatoes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26143
p26146,"Other vegetables, including mushrooms, fruiting and mixed vegetables",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26146
p26147,Vegetable side dishes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26147
p26144,Vegetable dips,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26144 # half hummus half guaca

Raw salad 	Mixed side salad, lettuce, watercress
Green leafy/cabbages	Broccoli, cabbage, kale, cauliflower, spinach, sprouts
Root vegetables	Beetroot, carrots, celery, parsnip, turnip
Tomatoes	Fresh and tinned tomatoes
Allium vegetables	Garlic, leek, onion
Other vegetables (mushrooms, fruiting, mixed)	Mushrooms, mixed vegetables, avocado, broad beans, green beans, butternut squash, courgettes, , peppers, other
Peas/sweetcorn	Peas, sweetcorn
Vegetable side dishes	Coleslaw, salad with added fat/mayonnaise
Vegetable dips	Hummus, guacamole (half is hummus other half is guaca)
    # potatoes
p26118,Potatoes and sweet potatoes (baked/boiled),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26118
p26119,Fried/roast potatoes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26119
p26120,Mashed potatoes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26120

Potatoes/sweet potatoes (baked/boiled)	Potatoes, sweet potatoes, boiled or baked
Mashed potatoes	Potatoes, mashed
Fried/roast potatoes 	Potatoes and chips, fried or roasted with fat

    # legumes
Soy drink	Soya drinks (including calcium fortified)
Legumes/pulses	Baked beans, pulses
Vegetable dips	Hummus, guacamole (half is hummus other half is guaca)
Soya-based desserts & yogurt	Soya-based desserts
p26086,Soy desserts and yogurt,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26086
p26101,Legumes and pulses,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26101
p26136,Soy milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26136
p26137,Meat substitutes - soy,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26137
p26144,Vegetable dips,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26144 # half hummus half guaca

    # red meats
p26066,Beef,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26066
p26100,Lamb,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26100
p26104,"Other meat, offal",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26104
p26117,Pork,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26117

Pork	Pork
Beef	Beef
Lamb	Lamb
Other meat, offal	Other meat including offal

    # processed meat
p26122,Processed meat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26122

Processed meat	Sausages, bacon (with and without fat), ham, liver pate
    # poultry
p26121,Poultry,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26121
p26069,Breaded/battered chicken,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26069

Poultry	Poultry (with/without skin)
Breaded/battered Chicken	Fried poultry with batter/breadcrumbs
    # fish
p26070,Breaded/battered fish,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26070
p26109,Oily fish,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26109
p26132,Shellfish,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26132
White fish & tinned tuna	Tinned tuna, white fish, other fish
Shellfish	Prawns, lobster, crab, shellfish
Oily fish	Oily fish, including salmon,
Breaded/battered Fish	Fried fish with batter/breadcrumbs
p26149,White fish and tinned tuna,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26149

    # eggs
Egg and egg dishes	Whole eggs and processed (omelette, scotch eggs, other)
p26088,Egg and egg dishes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26088

    # meat substitutes
p26145,Meat substitutes - vegetarian,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26145
Vegetarian meals	Quorn-based and vegetarian burgers and products
Soy-based meals 	Tofu-based products
    # non-alcoholic beverages
p26124,Rice/oat milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26124
p26141,Tea,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26141
p26142,"Tea, decaffeinated",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26142
p26148,Water (still and sparkling),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26148

p26081,"Coffee, caffeinated",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26081
p26082,"Coffee, decaffeinated",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26082
p26095,Fruit juice,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26095
p26126,Low/non sugar sugar-sweetened beverages,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26126
p26127,Sugar-sweetened beverages and other sugary drinks,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26127

Rice/oat drink	Rice and oat vegetable drinks
Fruit juice 	Orange, grapefruit drink and 100% fruit juice
Coffee, caffeinated	Normal instant, filter, cappuccino, espresso coffee
Coffee, decaffeinated	Decaffeinated instant, filter, cappuccino, espresso coffee
Tea	Black, green and other tea
Tea, decaffeinated	Decaffeinated black, herbal tea, rooibos
SSBs & other sugary drinks	Fizzy sugary drinks, squash, fruit smoothies
Low/non sugar SSBs	Low calorie fizzy drinks and squash
Water/sparkling water	Plain water, sparkling water
Milk-based & powdered drinks	Dairy-based smoothies, milk-based drinks, hot chocolate

    # alcoholic beverages
p26151,Fortified wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26151
p26152,Red wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26152
p26153,White wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26153
p26067,Beer and cider,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26067
p26138,Spirits,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26138

# sugar, preserves, cakes & confectionery, snacks
p26106,Nut-based spreads,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26106
p26140,Other sweets,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26140
p26134,Savoury snacks,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26134
Added sugars & preserves	Table sugar, honey, jam and preserves
Chocolate confectionery	Chocolate bar (including white, milk and dark chocolate), chocolate-covered raisins, chocolate-covered sweets
Other sweets	Hard and soft sweets (including sugar free)
Savoury snacks	Crisps, savoury biscuits, cheese snacks, other savoury biscuits
Biscuits	Chocolate biscuits, plain biscuits, sweet biscuits and cookies
Milk-dairy desserts	Ice cream, milk puddings, milk-based desserts, cheesecake
Desserts & cakes & pastries	Pancakes, croissant, Danish pastries, scones, fruitcakes, cakes, doughnuts, sponge puddings, other desserts, cereal bars, sweet snacks
Nut-based spreads	Peanut-butter and chocolate-based spread
p26084,Milk-dairy desserts,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26084
p26085,Other desserts and cakes and pastries,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26085
p26064,Added sugars and preserves,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26064
p26080,Chocolate confectionery,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26080

# Sauces & condiments
Sauces (higher fat)	Mayonnaise, salad dressing, pesto, cheese sauce, white sauce, gravy
Sauces (lower fat)	Yeast, chutney, olives, ketchup, brown sauce, tomato sauce

p26129,Sauces and condiments (high fat),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26129
p26130,Sauces and condiments (low fat),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26130

    # total weight of all foods
    total_weight_food = p26000

    legumes_total = rowSums(select(., starts_with("p26086") | starts_with("p26101") | starts_with("p26136") | starts_with("p26137"))),
    meats_total = rowSums(select(., starts_with("p26010") | starts_with("p26011") | starts_with("p26012") | starts_with("p26013") | starts_with("p26014") | starts_with("p26015"))),
    legumes_daily = legumes_total / p20077,
    meats_daily = meats_total / p20077,
    legumes_weekly = legumes_daily * 7,
    meats_weekly = meats_daily * 7


# for sensitivity analyses
vegetables includes 0.5*peas and sweetcorn - the other half goes into legumes as peas










