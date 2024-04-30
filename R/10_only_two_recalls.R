# Only using 2 recalls to maximize number of events

# Load packages
library(tidyverse)
library(magrittr)
library(dplyr)
library(here)
library(broom)
library(survival)
library(ggsurvfit)
library(ggplot2)

# 1. change /targets.R to "csv", run targets::tar_make(), and convert to parquet (without saving data on RAP)
# 2. run 1_data_start.R
# 3. run 2_data_management.R (without saving data on RAP)


# 4. run 3_diet_data.R with some moderation to only include information from
# first two 24h recalls
# Only estimate average intake from the first two 24h recalls
data <- data %>%
  mutate(ques_comp_t0 = p105010_i0,
         ques_comp_t1 = p105010_i1,
         ques_comp_t2 = p105010_i2,
         ques_comp_t3 = p105010_i3,
         ques_comp_t4 = p105010_i4,
         # Removing specific time stamp
         ques_comp_t0 = substr(ques_comp_t0, 1, 10),
         ques_comp_t1 = substr(ques_comp_t1, 1, 10),
         ques_comp_t2 = substr(ques_comp_t2, 1, 10),
         ques_comp_t3 = substr(ques_comp_t3, 1, 10),
         ques_comp_t4 = substr(ques_comp_t4, 1, 10)
  ) %>%
  # Convert ques_0 to ques_4 to date format
  mutate(across(starts_with("ques_"), as.Date)) %>%
  # Gather all columns into key-value pairs
  pivot_longer(cols = starts_with("ques_"), names_to = "questionnaire", values_to = "date_filled1") %>%
  # Group by participant ID and select the first two entries for each participant
  group_by(id) %>%
  arrange(id, date_filled1) %>%  # Sort by date within each participant in ascending order
  slice(1:2) %>%  # Select the first two entries (first two questionnaires) for each participant
  ungroup() %>%
  # Rename the remaining column to indicate the first and second filled questionnaires
  mutate(questionnaire_order = if_else(row_number() == 1, "first_filled", "second_filled")) %>%
  rename(first_filled_questionnaire = questionnaire) %>%
  select(-questionnaire_order) %>%
  # Calculate row sums for selected food items
  mutate(
    # refined cereal
    cereal_refined_total = rowSums(select(., starts_with("p26113") | starts_with("p26079") |
                                            starts_with("p26071") | starts_with("p26072") |
                                            starts_with("p26073") | starts_with("p26075") |
                                            starts_with("p26068") | starts_with("p26083")), na.rm = TRUE),
    cereal_refined_daily = cereal_refined_total / 2,  # Assuming two questionnaires represent two days
    cereal_refined_weekly = cereal_refined_daily * 7,
    # whole-grain cereals
    whole_grain_total = rowSums(select(., starts_with("p26074") | starts_with("p26076") |
                                          starts_with("p26077") | starts_with("p26078") |
                                          starts_with("p26105") | starts_with("p26114")), na.rm = TRUE),
    whole_grain_daily = whole_grain_total/2,
    whole_grain_weekly = whole_grain_daily * 7,
    # mixed dishes
    mixed_dish_total = rowSums(select(., starts_with("p26128") | starts_with("p26097") |
                                        starts_with("p26116") | starts_with("p26135") |
                                        starts_with("p26139") | starts_with("p26145")), na.rm = TRUE),
    mixed_dish_daily = mixed_dish_total/2,
    mixed_dish_weekly = mixed_dish_daily * 7,
    # dairy
    dairy_total = rowSums(select(., starts_with("p26154") | starts_with("p26087") |
                                   starts_with("p26096") | starts_with("p26102") |
                                   starts_with("p26103") | starts_with("p26099") |
                                   starts_with("p26131") | starts_with("p26133") |
                                   starts_with("p26150")), na.rm = TRUE),
    dairy_daily = dairy_total/2,
    dairy_weekly = dairy_daily * 7,
    # fats and spread
    fats_total = rowSums(select(., starts_with("p26112") | starts_with("p26062") |
                                  starts_with("p26063") | starts_with("p26155") |
                                  starts_with("p26110") | starts_with("p26111")), na.rm = TRUE),
    fats_daily = fats_total/2,
    fats_weekly = fats_daily * 7,
    # fruit
    fruit_total = rowSums(select(., starts_with("p26089") | starts_with("p26090") |
                                   starts_with("p26091") | starts_with("p26092") |
                                   starts_with("p26093") | starts_with("p26094")), na.rm = TRUE),
    fruit_daily = fruit_total/2,
    fruit_weekly = fruit_daily * 7,
    # nuts and seeds
    nut_total = rowSums(select(., starts_with("p26107") | starts_with("p26108")), na.rm = TRUE),
    nut_daily = nut_total/2,
    nut_weekly = nut_daily*7,
    # vegetables
    veggie_total = rowSums(select(., starts_with("p26065") | starts_with("p26098") |
                                    starts_with("p26115") | starts_with("p26123") |
                                    starts_with("p26125") | starts_with("p26143") |
                                    starts_with("p26146") | starts_with("p26147") |
                                    starts_with("p26144")), na.rm = TRUE),
    veggie_daily = veggie_total/2,
    veggie_weekly = veggie_daily * 7,
    # potatoes
    potato_total = rowSums(select(., starts_with("p26118") | starts_with("p26119") |
                                    starts_with("p26120")), na.rm = TRUE),
    potato_daily = potato_total/2,
    potato_weekly = potato_daily * 7,
    # eggs
    egg_total = rowSums(select(., starts_with("p26088")), na.rm = TRUE),
    egg_daily = egg_total/2,
    egg_weekly = egg_daily * 7,
    # non-alcoholic beverages
    non_alc_beverage_total = rowSums(select(., starts_with("p26124") | starts_with("p26141") |
                                              starts_with("p26142") | starts_with("p26148") |
                                              starts_with("p26081") | starts_with("p26082") |
                                              starts_with("p26095") | starts_with("p26126") |
                                              starts_with("p26127")), na.rm = TRUE),
    non_alc_beverage_daily = non_alc_beverage_total/2,
    non_alc_beverage_weekly = non_alc_beverage_daily * 7,
    # alcoholic beverages
    alc_beverage_total = rowSums(select(., starts_with("p26151") | starts_with("p26152") |
                                          starts_with("p26153") | starts_with("p26067") |
                                          starts_with("p26138")), na.rm = TRUE),
    alc_beverage_daily = alc_beverage_total/2,
    alc_beverage_weekly = alc_beverage_daily * 7,
    # sugar, preserves, cakes & confectionery, snacks
    snack_total = rowSums(select(., starts_with("p26106") | starts_with("p26140") |
                                   starts_with("p26134") | starts_with("p26084") |
                                   starts_with("p26085") | starts_with("p26064") |
                                   starts_with("p26080")), na.rm = TRUE),
    snack_daily = snack_total/2,
    snack_weekly = snack_daily * 7,
    # Sauces & condiments
    sauce_total = rowSums(select(., starts_with("p26129") | starts_with("p26130")), na.rm = TRUE),
    sauce_daily = sauce_total/2,
    sauce_weekly = sauce_daily * 7,
    # legumes
    legume_total = rowSums(select(., starts_with("p26086") | starts_with("p26101") |
                                    starts_with("p26136") | starts_with("p26137")), na.rm = TRUE),
    legume_daily = legume_total/2,
    legume_weekly = legume_daily * 7,
    # red meats
    red_meat_total = rowSums(select(., starts_with("p26066") | starts_with("p26100") |
                                      starts_with("p26104") | starts_with("p26117")), na.rm = TRUE),
    red_meat_daily = red_meat_total/2,
    red_meat_weekly = red_meat_daily * 7,
    # processed meat
    proc_meat_total = rowSums(select(., starts_with("p26122")), na.rm = TRUE),
    proc_meat_daily = proc_meat_total/2,
    proc_meat_weekly = proc_meat_daily * 7,
    # red and processed
    meats_daily = proc_meat_daily + red_meat_daily,
    meats_weekly = proc_meat_weekly + red_meat_weekly,
    # poultry
    poultry_total = rowSums(select(., starts_with("p26121") | starts_with("p26069")), na.rm = TRUE),
    poultry_daily = poultry_total/2,
    poultry_weekly = poultry_daily * 7,
    # fish
    fish_total = rowSums(select(., starts_with("p26070") | starts_with("p26109") |
                                  starts_with("p26132") | starts_with("p26149")), na.rm = TRUE),
    fish_daily = fish_total/2,
    fish_weekly = fish_daily * 7,
    # total weights of foods
    weight_daily = cereal_refined_daily + whole_grain_daily + mixed_dish_daily +
      dairy_daily + fats_daily + fruit_daily + nut_daily + veggie_daily + potato_daily +
      egg_daily + non_alc_beverage_daily + alc_beverage_daily +
      snack_daily + sauce_daily + legume_daily + meats_daily + poultry_daily + fish_daily,
    weight_weekly = cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
      dairy_weekly + fats_weekly + fruit_weekly + nut_weekly + veggie_weekly + potato_weekly +
      egg_weekly + non_alc_beverage_weekly + alc_beverage_weekly +
      snack_weekly + sauce_weekly + legume_weekly + meats_weekly + poultry_weekly + fish_weekly
  )


# Drop p-variables for diet ------------------------------------------------------
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
  "p26070", "p26109", "p26132", "p26149", "p26000"
)

data <- data %>%
  select(-matches(paste0(remove_diet)))


# 5. run 4_icd10_outcomes_tte.R code with some moderations (below)

# time of last completed 24h recall
data <- data %>%
  mutate(ques_comp_t0 = p105010_i0,
         ques_comp_t1 = p105010_i1,
         ques_comp_t2 = p105010_i2,
         ques_comp_t3 = p105010_i3,
         ques_comp_t4 = p105010_i4,
         # Removing specific time stamp
         ques_comp_t0 = substr(ques_comp_t0, 1, 10),
         ques_comp_t1 = substr(ques_comp_t1, 1, 10),
         ques_comp_t2 = substr(ques_comp_t2, 1, 10),
         ques_comp_t3 = substr(ques_comp_t3, 1, 10),
         ques_comp_t4 = substr(ques_comp_t4, 1, 10)
  )

data <- data %>%
  # Convert ques_0 to ques_4 to date format
  mutate(across(starts_with("ques_"), as.Date)) %>%
  # Gather all columns into key-value pairs
  pivot_longer(cols = starts_with("ques_"), names_to = "questionnaire", values_to = "date_filled") %>%
  # Group by participant ID and select the row with the second latest date_filled for each participant
  group_by(id) %>%
  arrange(id, desc(date_filled)) %>%  # Sort by date within each participant in descending order
  slice(2) %>%  # Select the second entry (second 24h recall) for each participant
  ungroup() %>%
  # Rename the remaining column to indicate the second filled questionnaire
  rename(second_filled_questionnaire = questionnaire)

data <- data %>%
  mutate(date_filled = as.Date(date_filled))

remove <- c("p105010_i0", "p105010_i1", "p105010_i2", "p105010_i3","p105010_i4")
data <- data %>%
  select(-matches(remove))

# ICD10 codes ---------------------------------------------------------
# Split the diagnosis-variable into separate columns based on delimiter "|"
icd <- data %>%
  select(starts_with("p41270"), starts_with("p41280"), "id") %>%
  separate_wider_delim(p41270,
                       delim = "|",
                       names = paste0("p41270var_a", 0:258), too_few = "debug")

# Transform from wide to long format to match ICD-codes with date of diagnosis
icd10_subset <- icd %>%
  select(matches("p41270|p41280|id")) %>%
  pivot_longer(cols = matches("_a[0-9]*$"),
               names_to = c(".value", "a"),
               names_sep = "_")

# Defining dates of NAFLD
icd10_nafld <- icd10_subset %>%
  mutate(icd10_nafld_date = ifelse(str_detect(p41270var, "K76.0"),
                                   as.character(c_across(starts_with("p41280"))),
                                   NA),
         icd10_nafld_date = as.Date(icd10_nafld_date, format = "%Y-%m-%d"))

first_non_na_nafld <- icd10_nafld %>%
  filter(!is.na(icd10_nafld_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_nafld %>% select(id, icd10_nafld_date), by = "id")

# Defining dates of NASH
icd10_nash <- icd10_subset %>%
  mutate(icd10_nash_date = ifelse(str_detect(p41270var, "K75.8"),
                                  as.character(c_across(starts_with("p41280"))),
                                  NA),
         icd10_nash_date = as.Date(icd10_nash_date, format = "%Y-%m-%d"))

first_non_na_nash <- icd10_nash %>%
  filter(!is.na(icd10_nash_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_nash %>% select(id, icd10_nash_date), by = "id")

# Delete old ICD10 diagnosis and dates ------------------------------------
delete <- c("p41280", "p41270")
data <- data %>%
  select(-matches(paste0(delete)))


# ICD9 codes ---------------------------------------------------------
# Split the diagnosis-variable into separate columns based on delimiter "|"
icd9 <- data %>%
  select(starts_with("p41271"), starts_with("p41281"), "id") %>%
  separate_wider_delim(p41271,
                       delim = "|",
                       names = paste0("p41271var_a", 0:46), too_few = "debug")

# Transform from wide to long format to match ICD-codes with date of diagnosis
icd9_subset <- icd9 %>%
  select(matches("p41271|p41281|id")) %>%
  pivot_longer(cols = matches("_a[0-9]*$"),
               names_to = c(".value", "a"),
               names_sep = "_")

# Defining dates of NAFLD
icd9_nafld <- icd9_subset %>%
  mutate(icd9_nafld_date = ifelse(str_detect(p41271var, "5718"),
                                  as.character(c_across(starts_with("p41281"))),
                                  NA),
         icd9_nafld_date = as.Date(icd9_nafld_date, format = "%Y-%m-%d"))

first_non_na_nafld <- icd9_nafld %>%
  filter(!is.na(icd9_nafld_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_nafld %>% select(id, icd9_nafld_date), by = "id")

# Defining dates of NASH
icd9_nash <- icd9_subset %>%
  mutate(icd9_nash_date = ifelse(str_detect(p41271var, "5715"),
                                 as.character(c_across(starts_with("p41281"))),
                                 NA),
         icd9_nash_date = as.Date(icd9_nash_date, format = "%Y-%m-%d"))

first_non_na_nash <- icd9_nash %>%
  filter(!is.na(icd9_nash_date)) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup()

data <- data %>%
  left_join(first_non_na_nash %>% select(id, icd9_nash_date), by = "id")

# Delete old ICD9 diagnosis and dates ------------------------------------
delete <- c("p41281", "p41271")
data <- data %>%
  select(-matches(paste0(delete)))


# Define variables for survival analysis ----------------------------------
# Date of death and loss to follow up
data <- data %>%
  mutate(date_of_death = if_else(!is.na(p40000_i0), p40000_i0, p40000_i1),
         date_of_death = as.Date(date_of_death),
         loss_to_follow_up = p191,
         loss_to_follow_up = as.Date(loss_to_follow_up))

# remove p-values
remove <- c("p191", "p40000_i0", "p40000_i1")
data <- data %>%
  select(-matches(remove))

# Defining birth date as origin for survival analysis
# Merging birth year and month of birth into one column:

month_names <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")

data <- data %>%
  mutate(month_of_birth_num = sprintf("%02d", match(p52, month_names)))

data <- data %>%
  unite(date_birth, p34, month_of_birth_num, sep = "-")

remove(month_names)


# adding 15 as DD for all participants:
data$date_birth <- as.Date(paste0(data$date_birth, "-15"))

# Administrative censoring at October 31st, 2022
data <- data %>%
  mutate(censoring = as.Date("2022-10-31"))

# estimate survival time
data <- data %>%
  mutate(
    survival_time_tmp = case_when(
      !is.na(icd10_nafld_date) ~ as.numeric(difftime(icd10_nafld_date, date_birth, units = "days")),
      !is.na(icd10_nash_date) ~ as.numeric(difftime(icd10_nash_date, date_birth, units = "days")),
      !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_birth, units = "days")),
      !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_birth, units = "days")),
      TRUE ~ as.numeric(difftime(censoring, date_birth, units = "days"))
    ),
    # Use min to get the minimum survival time across columns
    survival_time = pmin(survival_time_tmp, na.rm = TRUE),
    survival_time = survival_time/365.25,
    # Remove temporary variable
    survival_time_tmp = NULL
  )

# binary variable to indicate if nafld happened
data <- data %>%
  mutate(nafld = case_when(
    !is.na(icd10_nafld_date) | !is.na(icd10_nash_date) |
      !is.na(icd9_nafld_date) | !is.na(icd9_nash_date) ~ 1,
    TRUE ~ 0))

# counting and removing those with event before baseline
# defining time in study
data <- data %>%
  mutate(
    survival_time_nafld = case_when(
      !is.na(icd10_nafld_date) ~ as.numeric(difftime(icd10_nafld_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_nash = case_when(
      !is.na(icd10_nash_date) ~ as.numeric(difftime(icd10_nash_date, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_ltfu = case_when(
      !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_death = case_when(
      !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, date_filled, units = "days")),
      TRUE ~ NA),
    survival_time_cenc = difftime(censoring, date_filled, units = "days"),
    time = pmin(survival_time_death, survival_time_cenc, survival_time_ltfu,
                survival_time_nash, survival_time_nafld, na.rm = TRUE),
    time = time/365.25
  )

# Keep only those with a positive event time (removes those with event before baseline)
data <- data %>%
  subset(data$time>0)

# Run analyses
# model 2 -----------------------------------------------------------------
data <- data %>%
  mutate(legumes80 = legume_weekly/80,
         meats80 = meats_weekly/80,
         poultry80 = poultry_weekly/80,
         fish80 = fish_weekly/80)
# meats
meat_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + poultry80 + fish80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly +
                       #other variables
                       age + region + sex +
                       alcohol_weekly + ethnicity + deprivation + education +
                       cohabitation + physical_activity + smoking +
                       related_disease + disease_family + yearly_income,
                     data = data, ties='breslow')

meat_model2 <- tidy(meat_model2, exponentiate = TRUE, conf.int = TRUE, digits = 2) # 2 digits doesn't work

# poultry
poultry_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                          # removing meat
                          legumes80 + meats80 + fish80+
                          #other food components
                          cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                          dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                          veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                          non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                          sauce_weekly + weight_weekly +
                          #other variables
                          age + region + sex +
                          alcohol_weekly + ethnicity + deprivation + education +
                          cohabitation + physical_activity + smoking +
                          related_disease + disease_family + yearly_income,
                        data = data, ties='breslow')

poultry_model2 <- tidy(poultry_model2, exponentiate = TRUE, conf.int = TRUE, digits = 2)


# fish
fish_model2 <- coxph(Surv(survival_time, nafld == 1) ~
                       # removing meat
                       legumes80 + meats80 + poultry80+
                       #other food components
                       cereal_refined_weekly + whole_grain_weekly + mixed_dish_weekly +
                       dairy_weekly + fats_weekly + fruit_weekly + nut_weekly +
                       veggie_weekly + potato_weekly + egg_weekly + meat_sub_weekly +
                       non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly +
                       sauce_weekly + weight_weekly +
                       #other variables
                       age + region + sex +
                       alcohol_weekly + ethnicity + deprivation + education +
                       cohabitation + physical_activity + smoking +
                       related_disease + disease_family + yearly_income,
                     data = data, ties='breslow')

fish_model2 <- tidy(fish_model2, exponentiate = TRUE, conf.int = TRUE, digits = 2)


