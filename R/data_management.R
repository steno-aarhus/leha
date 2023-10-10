# Data management
library(dplyr)
library(magrittr)
library(tidyr)


# Summary overview --------------------------------------------------------
str(data)

# Remove variables and columns --------------------------------------------
# Delete variables that were not needed after all -> go to project-
variables_to_remove <- c("p20160", "p22506", "p2887", "p3436", "p3446", "p6152",
                         "p20116", "p20117", "p20162", "p22200", "p131668", "p131670",
                         "p131674", "p131676")
data1 <- data %>%
  dplyr::select(-(starts_with(variables_to_remove)| ends_with("_i[0-4]")))

p20161 (pack years of smoking) # is this relevant?

# Delete follow-up instances for confounder variables
variables_to_edit <- c("p738", "p1239", "p1249", "p1538", "p1548", "p3456", "p6150",
                       "p20002","p20107", "p20110", "p20111", "p20161", "p20162",
                       "p21000", "p22040", "p22506", "p22508", "p23104", "p2443",
                       "p2453", "p40000")
data1 <- data1 %>%
  select(-matches(paste0(variables_to_edit, "_i[1-4]")))

# Recoding variables ------------------------------------------------------

# Self-reported non-cancer illness. No diseases coded as 0, liver disease coded as 1, CVD coded as 2, other coded as 3
Combination of p1239 (smoking status) and p3456 (number of cigarettes currently smoked) to make categories: never smoker; previous smoker; current 1-15; current 15-25; current 25+
  Maybe p22506 (tobacco smoking) + p22508 (amount of tobacco) instead of cigarettes?



  # New column names --------------------------------------------------------
# Change column names to understandable variables using dplyr::rename
# Can this be done with a call to the project-variables.csv file? It has ID and
# UKB description, which are the ones I need. I could then snake-case the
# variable names?
names(data)
rap_names <- readr::read_csv(here::here("data-raw/rap-variables.csv"))
nrow(rap_names)
names(data) <- rap_names$rap_variable_name


dplyr::pull(field_id)
field_id,rap_variable_name,id


# Step 2: Load the mapping from 'rap-variables.csv'
variable_mapping <- readr::read_csv(here::here("data-raw/rap-variables.csv"))

# Step 3: Rename variables based on 'rap_variable_name'
data2 <- data %>%
  rename_with(function(id) {
    match_desc <- variable_mapping$rap_variable_name[variable_mapping$id == id]
    if (!is.na(match_desc)) {
      return(match_desc)
    } else {
      return(id)
    }
  }, starts_with("p"))

# Print the updated dataset
view(data2)

data2 <- data %>%
  rename_with(function(id) {
    match_desc <- variable_mapping$rap_variable_name[variable_mapping$id == id]
    if (!is.na(match_desc) && length(match_desc) > 0) {
      return(match_desc)
    } else {
      return(id)
    }
  }, starts_with("p"))

data2 <- data1 %>%
  readr::write_csv("data1.csv")

# Step 1: Load your dataset (replace 'your_dataset.csv' with your actual dataset)
your_dataset <- read.csv("your_dataset.csv")

# Step 2: Load the mapping from 'project_variables.csv'
variable_mapping <- readr::read_csv(here::here("data-raw/rap-variables.csv"))

# Step 3: Merge the dataset with the mapping
merged_dataset <- inner_join(data1, variable_mapping, by = "id")

# Step 4: Rename the variables based on 'ukb_variable_description'
renamed_dataset <- merged_dataset %>%
  select(-id) %>%
  rename_with(~ukb_variable_description, starts_with("p"))

# Print the updated dataset
print(renamed_dataset)


data1 <- data1 %>%
  dplyr::rename(sex = p31,
                birth_year = p34,
                birth_month = p52,
                reason_lost_to_followup = p190, #interpretation in ~doc/data management.docx
                date_lost_to_followup = p191,
                household_income = starts_with("p738"),
                current_tobacco_smoking = starts_with("p1239"),
                past_tobacco_smoking = starts_with("p1249"),
                major_diet_change = starts_with("p1538"),
                variation_in_diet = starts_with("p1548"),
                heart_problems_diagnosed_by_doctor = starts_with("p6150"),
                non_cancer_illness = starts_with("p20002"),
                number_recalls_completed = p20077,
                illness_father = starts_with("p20107"),
                illness_mother = starts_with("20110"),
                illness_sibling = starts_with("20111"),
                ethnicity = p21000,
                age_at_recruitment = p21022,
                met_minutes_week = p22040,
                bmi = p23104,
                meat_substitutes_vegetarian = starts_with("p26145"),
  )

p3456,Number of cigarettes currently smoked daily (current cigarette smokers),36747,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=3456
p20161,Pack years of smoking,152789,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20161
p20403,Amount of alcohol drunk on a typical drinking day,143636,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20403
p22506,Tobacco smoking,121256,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=22506
p22508,Amount of tobacco currently smoked,2870,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=22508
p26000,Total weight of all foods and beverages,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26000
p26002,Energy,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26002
p26032,Monounsaturated fatty acids,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26032
p26062,Animal fat spread lower fat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26062
p26063,Animal fat spread normal,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26063
p26064,Added sugars and preserves,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26064
p26065,Allium vegetables,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26065
p26066,Beef,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26066
p26067,Beer and cider,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26067
p26068,Biscuits,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26068
p26069,Breaded/battered chicken,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26069
p26070,Breaded/battered fish,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26070
p26071,"Mixed bread (50/50), brown and seeded",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26071
p26072,Other bread,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26072
p26073,White bread,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26073
p26074,Wholemeal bread,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26074
p26075,Biscuit cereal,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26075
p26076,Bran cereal,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26076
p26077,Oat cereal (non sugar),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26077
p26078,Oat cereal (sugar),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26078
p26079,Other cereal (sugar),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26079
p26080,Chocolate confectionery,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26080
p26081,"Coffee, caffeinated",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26081
p26082,"Coffee, decaffeinated",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26082
p26083,Savoury crackers,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26083
p26084,Milk-dairy desserts,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26084
p26085,Other desserts and cakes and pastries,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26085
p26086,Soy desserts and yogurt,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26086
p26087,Milk-based and powdered drinks,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26087
p26088,Egg and egg dishes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26088
p26089,Apples and pears,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26089
p26090,Berries,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26090
p26091,Citrus,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26091
p26092,Dried fruit,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26092
p26093,Other fruit,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26093
p26094,Stewed fruit,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26094
p26095,Fruit juice,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26095
p26096,Full fat yogurt,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26096
p26097,Grain dishes - added fat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26097
p26098,Green leafy/cabbages,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26098
p26099,High fat cheese,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26099
p26100,Lamb,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26100
p26101,Legumes and pulses,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26101
p26102,Low fat yogurt,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26102
p26103,Medium and low fat cheese,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26103
p26104,"Other meat, offal",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26104
p26105,Muesli,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26105
p26106,Nut-based spreads,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26106
p26107,Unsalted nuts and seeds,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26107
p26108,Salted nuts and seeds,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26108
p26109,Oily fish,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26109
p26110,Olive oil (drizzling/dunking),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26110
p26111,Plant-based spread lower fat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26111
p26112,Plant-based spread normal,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26112
p26113,White pasta and rice,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26113
p26114,"Wholemeal pasta, brown rice and other wholegrains",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26114
p26115,Peas and sweetcorn,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26115
p26116,Pizza,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26116
p26117,Pork,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26117
p26118,Potatoes and sweet potatoes (baked/boiled),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26118
p26119,Fried/roast potatoes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26119
p26120,Mashed potatoes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26120
p26121,Poultry,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26121
p26122,Processed meat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26122
p26123,Raw salad,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26123
p26124,Rice/oat milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26124
p26125,Root vegetables,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26125
p26126,Low/non sugar sugar-sweetened beverages,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26126
p26127,Sugar-sweetened beverages and other sugary drinks,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26127
p26128,"Samosa, pakora",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26128
p26129,Sauces and condiments (high fat),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26129
p26130,Sauces and condiments (low fat),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26130
p26131,Semi skimmed milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26131
p26132,Shellfish,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26132
p26133,Skimmed milk and cholesterol-lowering milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26133
p26134,Savoury snacks,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26134
p26135,Soups,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26135
p26136,Soy milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26136
p26137,Meat substitutes - soy,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26137
p26138,Spirits,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26138
p26139,Sushi,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26139
p26140,Other sweets,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26140
p26141,Tea,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26141
p26142,"Tea, decaffeinated",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26142
p26143,Tomatoes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26143
p26144,Vegetable dips,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26144
p26145,Meat substitutes - vegetarian,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26145
p26146,"Other vegetables, including mushrooms, fruiting and mixed vegetables",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26146
p26147,Vegetable side dishes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26147
p26148,Water (still and sparkling),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26148
p26149,White fish and tinned tuna,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26149
p26150,Whole milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26150
p26151,Fortified wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26151
p26152,Red wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26152
p26153,White wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26153
p26154,Cream,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26154
p26155,Trans fatty acids,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26155
p26410,Index of Multiple Deprivation (England),432645,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26410
p26426,Index of Multiple Deprivation (Wales),21184,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26426
p26427,Index of Multiple Deprivation (Scotland),35846,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26427
p40000,Date of death,37897,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=40000
p41270,Diagnoses - ICD10,440017,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270
p41271,Diagnoses - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41271
p41272,Operative procedures - OPCS4,440159,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41272
p41280,Date of first in-patient diagnosis - ICD10,440014,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41280
p41281,Date of first in-patient diagnosis - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41281
p41282,Date of first operative procedure - OPCS4,440153,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41282
p100001,Food weight,210977,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=100001
p100002,Energy,210977,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=100002
p104280,Pea intake,51147,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=104280
p131668,Date K75 first reported (other inflammatory liver diseases),3171,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=131668
p131670,Date K76 first reported (other diseases of liver),14708,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=131670
p131674,Date K80 first reported (cholelithiasis),33836,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=131674
p131676,Date K81 first reported (cholecystitis),6509,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=131676


p26000,Total weight of all foods and beverages,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26000
p26002,Energy,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26002
p26032,Monounsaturated fatty acids,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26032
p26062,Animal fat spread lower fat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26062
p26063,Animal fat spread normal,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26063
p26064,Added sugars and preserves,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26064
p26065,Allium vegetables,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26065
p26066,Beef,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26066
p26067,Beer and cider,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26067
p26068,Biscuits,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26068
p26069,Breaded/battered chicken,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26069
p26070,Breaded/battered fish,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26070
p26071,"Mixed bread (50/50), brown and seeded",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26071
p26072,Other bread,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26072
p26073,White bread,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26073
p26074,Wholemeal bread,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26074
p26075,Biscuit cereal,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26075
p26076,Bran cereal,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26076
p26077,Oat cereal (non sugar),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26077
p26078,Oat cereal (sugar),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26078
p26079,Other cereal (sugar),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26079
p26080,Chocolate confectionery,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26080
p26081,"Coffee, caffeinated",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26081
p26082,"Coffee, decaffeinated",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26082
p26083,Savoury crackers,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26083
p26084,Milk-dairy desserts,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26084
p26085,Other desserts and cakes and pastries,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26085
p26086,Soy desserts and yogurt,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26086
p26087,Milk-based and powdered drinks,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26087
p26088,Egg and egg dishes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26088
p26089,Apples and pears,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26089
p26090,Berries,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26090
p26091,Citrus,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26091
p26092,Dried fruit,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26092
p26093,Other fruit,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26093
p26094,Stewed fruit,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26094
p26095,Fruit juice,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26095
p26096,Full fat yogurt,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26096
p26097,Grain dishes - added fat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26097
p26098,Green leafy/cabbages,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26098
p26099,High fat cheese,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26099
p26100,Lamb,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26100
p26101,Legumes and pulses,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26101
p26102,Low fat yogurt,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26102
p26103,Medium and low fat cheese,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26103
p26104,"Other meat, offal",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26104
p26105,Muesli,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26105
p26106,Nut-based spreads,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26106
p26107,Unsalted nuts and seeds,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26107
p26108,Salted nuts and seeds,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26108
p26109,Oily fish,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26109
p26110,Olive oil (drizzling/dunking),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26110
p26111,Plant-based spread lower fat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26111
p26112,Plant-based spread normal,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26112
p26113,White pasta and rice,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26113
p26114,"Wholemeal pasta, brown rice and other wholegrains",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26114
p26115,Peas and sweetcorn,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26115
p26116,Pizza,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26116
p26117,Pork,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26117
p26118,Potatoes and sweet potatoes (baked/boiled),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26118
p26119,Fried/roast potatoes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26119
p26120,Mashed potatoes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26120
p26121,Poultry,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26121
p26122,Processed meat,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26122
p26123,Raw salad,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26123
p26124,Rice/oat milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26124
p26125,Root vegetables,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26125
p26126,Low/non sugar sugar-sweetened beverages,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26126
p26127,Sugar-sweetened beverages and other sugary drinks,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26127
p26128,"Samosa, pakora",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26128
p26129,Sauces and condiments (high fat),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26129
p26130,Sauces and condiments (low fat),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26130
p26131,Semi skimmed milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26131
p26132,Shellfish,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26132
p26133,Skimmed milk and cholesterol-lowering milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26133
p26134,Savoury snacks,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26134
p26135,Soups,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26135
p26136,Soy milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26136
p26137,Meat substitutes - soy,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26137
p26138,Spirits,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26138
p26139,Sushi,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26139
p26140,Other sweets,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26140
p26141,Tea,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26141
p26142,"Tea, decaffeinated",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26142
p26143,Tomatoes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26143
p26144,Vegetable dips,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26144
p26145,Meat substitutes - vegetarian,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26145
p26146,"Other vegetables, including mushrooms, fruiting and mixed vegetables",210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26146
p26147,Vegetable side dishes,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26147
p26148,Water (still and sparkling),210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26148
p26149,White fish and tinned tuna,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26149
p26150,Whole milk,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26150
p26151,Fortified wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26151
p26152,Red wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26152
p26153,White wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26153
p26154,Cream,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26154
p26155,Trans fatty acids,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26155


# Remove participants with less than 2 24h recalls
data <- data %>%
  dplyr::filter(number_recalls >= 2)



# Convert data to numerics
# TO check if a variable is character, run this code
table(grepl(" ", data$varname)) #table of all variables with spaces in the cell content;

# to see the actual character content
data <- mutate(data, temp = as.numeric(varname))
filter(data, is.na(temp) & !is.na(varname)) %>%
  select(varname, temp)







