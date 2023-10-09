# Data management
install.packages("dplyr")
library(dplyr)


# Summary overview --------------------------------------------------------
str(data)



# Remove variables and columns --------------------------------------------
# Delete variables that were not needed after all
variables_to_remove <- c("p20160", "p22506", "p2887", "p3436", "p3446", "p6152", "p20116", "p20117", "p20162", "p22200")
data1 <- data %>%
  dplyr::select(-(starts_with(variables_to_remove)| ends_with("_i[1-4]")))


p20161 (pack years of smoking) # is this relevant?
p41270 # does not contain date of diagnoses, maybe not relevant?
p41271 # does not contain date of diagnoses, maybe not relevant?
p41272 # does not contain date of diagnoses, maybe not relevant?

# Delete follow-up instances for confounder variables
variables_to_edit <- c("p738", "p1239", "p1249", "p1538", "p1548", "p3456",
                       "p20107", "p20110", "p20111", "p20116", "p20162",
                       "p21000", "p22040", "p22506", "p22508", "p23104")
data1 <- data %>%
  select(-matches(paste0(variables_to_edit, "_i[1-4]")))



# Recoding variables ------------------------------------------------------
# Recode of variables across instances
# If any instance of the following variables is "yes", they should be recoded as 1, otherwise as 0
data1$diabetes_diagnosed_by_doctor <- ifelse(rowSums(df == "yes") > 0, 1, 0)
data1 <- data1 %>%
  mutate(diabetes_diagnosed_by_doctor = as.numeric(substring(starts_with("p2443"), 1, 3)== "yes"),
         cancer_diagnosed_by_doctor = ifelse(rowSums(data1$p2443 == "yes") > 0, 1, 0))

data1 <- data1 %>%
  mutate(diabetes_diagnosed_by_doctor = if_any(starts_with("p2443"), ~grepl("yes", x = .x)))


p2443 (diabetes)
p2453 (cancer)


# Other recoding

p20002 (non-cancer illness code, self-reported) -> across any instance, CVD should be coded as 1, liver disease as 2, the rest as 0
Combination of p1239 (smoking status) and p3456 (number of cigarettes currently smoked) to make categories: never smoker; previous smoker; current 1-15; current 15-25; current 25+
  Maybe p22506 (tobacco smoking) + p22508 (amount of tobacco) instead of cigarettes?




  # Long to wide to long data for at få gennemsnit af fødevareindtag
  # Using pivot_longer() from tidyr (Recommended for modern R workflows)
  install.packages("tidyr")
library(tidyr)


  # Try to do this for a subset of data first
  test_data <- data %>%
  (slice_sample(data, prop = 0.1) # gives 10% of rows

   test_long <- teat_data %>%
     pivot_longer(cols = matches("_i[0-4]$"),
                  names_to = c(".value", "i"),
                  names_sep = "_")


   # Then do it for the full data
   long_data <- data %>%
     pivot_longer(cols = matches("_i[0-4]$"),
                  names_to = c(".value", "i"),
                  names_sep = "_")


   # Eksempel med vinindtag
   # NAs should be recoded to 0 in all diet data to calculate average daily intakes
   # p26151,Fortified wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26151
   # p26152,Red wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26152
   # p26153,White wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26153

   long_data <- long_data %>%
     mutate(across(weight_wine = "p26151" + "p26152" + "p26153")

            # To group the weight_wine (=the sum) I will group by participant ID so that the 5 rows for each individual is collated into 1 row.
            group_by(id)
            summarise

            # Average daily intake.
            mutate(avg_wine = weight_wine/number_recalls)

            # Would it be relevant to look at average weekly intake instead? Or are the 24h recalls not suited for such assumptions, e.g., that we could divide by number of recalls and then multiply by 7 to day, that this is the weekly average intake which is relevant "always"?


            Create food groups --> go to sort_variables.R scripts



  # Diet data - all instances included; should be converted from wide to long? Or maybe just an average across columns?
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


# Outcomes
p40000 date of death - two instances?

  # How can I combine these?
  p41270,Diagnoses - ICD10,440017,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270 # delete?
p41271,Diagnoses - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41271 # delete?
p41272,Operative procedures - OPCS4,440159,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41272 # delete?

p41280,Date of first in-patient diagnosis - ICD10,440014,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41280
p41281,Date of first in-patient diagnosis - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41281
p41282,Date of first operative procedure - OPCS4,440153,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41282
# Or should I just keep these:
p131668,Date K75 first reported (other inflammatory liver diseases),3171,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=131668
p131670,Date K76 first reported (other diseases of liver),14708,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=131670
p131674,Date K80 first reported (cholelithiasis),33836,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=131674
p131676,Date K81 first reported (cholecystitis),6509,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=131676
# or should I make a combination of p42180 and p131668 etc to create a date of diagnosis?




# Convert data to numerics
# TO check if a variable is character, run this code
table(grepl(" ", data$varname)) #table of all variables with spaces in the cell content;

# to see the actual character content
data <- mutate(data, temp = as.numeric(varname))
filter(data, is.na(temp) & !is.na(varname)) %>%
  select(varname, temp)






# Change column names to understandable variables using dplyr::rename
# Can this be done with a call to the project-variables.csv file? It has ID and
# UKB description, which are the ones I need. I could then snake-case the
# variable names?

data <- data %>%
  dplyr::rename(sex = p31,
                birth_year = p34,
                birth_month = p52,
                number_recalls = p20077)

# Remove participants with less than 2 24h recalls
data <- data %>%
  dplyr::filter(number_recalls >= 2)
