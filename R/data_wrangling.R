# Data wrangling functions

# Data management ---------------------------------------------------------

## Remove participants with less than 2 diet assessments -------------------------------------
two_recalls <- function(data) {
  data <- data %>%
    subset(p20077 >= 2) %>%
    mutate(p20077 = as.numeric(p20077))
  return(data)
}

## Add id number -----------------------------------------------------------
data_id <- function(data) {
  data <- data %>%
    dplyr::mutate(id = 1:n(), .before = everything())
  return(data)
}


## Wrangle covariates (not food) -------------------------------------------
# sociodemographic factors
sociodemographics <- function(data) {
  data <- data %>% mutate(
    sex = as.factor(p31),
    age = as.numeric(p21022),
    age_strata = case_when(
      age < 45 ~ 0,
      age >= 45 & age <= 49 ~ 1,
      age >= 50 & age <= 54 ~ 2,
      age >= 55 & age <= 59 ~ 3,
      age >= 60 & age <= 64 ~ 4,
      age >= 65 ~ 5
    ),
    age_strata = as.factor(age_strata),
    ethnicity = case_when(
      p21000_i0 == 1|p21000_i0 == 1001|p21000_i0 == 1002|p21000_i0 == 1003 ~ "White",
      TRUE ~ "other"
    ),
    deprivation = p22189,
    yearly_income = case_when(
      p738_i0 == "1" ~ "<18,000",
      p738_i0 == "2" ~ "18,000-30,999",
      p738_i0 == "3" ~ "31,000-51,999",
      p738_i0 == "4" ~ "52,000-100,000",
      p738_i0 == "5" ~ ">100,000",
      TRUE ~ "unknown"
    ),
    yearly_income = as.factor(yearly_income),
    education = case_when(
      stringr::str_detect(p6138_i0, "1") ~ "High",
      stringr::str_detect(p6138_i0, "2") | stringr::str_detect(p6138_i0, "3")  ~ "Intermediate",
      stringr::str_detect(p6138_i0, "4") | stringr::str_detect(p6138_i0, "5") |
        stringr::str_detect(p6138_i0, "6") | stringr::str_detect(p6138_i0, "-7")  ~ "Low",
      stringr::str_detect(p6138_i0, "-3") ~ "Unknown"
    ),
    education = as.factor(education),
    education = as.factor(education),
    physical_activity = case_when(
      p22040_i0 <= 918 ~ "low",
      p22040_i0 > 918 & p22040_i0 <= 3706 ~ "moderate",
      p22040_i0 > 3706 ~ "high",
      TRUE ~ "unknown"
    ),
    cohabitation = case_when(
      p709_i0 == 1 ~ "alone",
      stringr::str_detect(p6141_i0, "1") ~ "with spouse/partner",
      p6141_i0 == -3 ~ "unknown",
      TRUE ~ "with other non-partner"
    ),
    # 10 UK recruitment regions
    region = case_when(
      p54_i0 == 11012 | p54_i0 == 11020 | p54_i0 == 11018 ~ "London",
      p54_i0 == 11003 | p54_i0 == 11022 | p54_i0 == 11023 ~ "Wales",
      p54_i0 == 11008 | p54_i0 == 10003 | p54_i0 == 11016 | p54_i0 == 11001  ~ "North-West",
      p54_i0 == 11009 | p54_i0 == 11017 ~ "North-East",
      p54_i0 == 11010 | p54_i0 == 11014 ~ "Yorkshire-Humber",
      p54_i0 == 11021 | p54_i0 == 11024 ~ "West-Midlands",
      p54_i0 == 11013 | p54_i0 == 11006 ~ "East-Midlands",
      p54_i0 == 11002 | p54_i0 == 11007 ~ "South-East",
      p54_i0 == 11011 ~ "South-West",
      p54_i0 == 11005 | p54_i0 == 11004 ~ "Scotland"
    ),
    region = as.factor(region)
  )
  return(data)
}


# lifestyle variables
lifestyle <- function(data) {
  data <- data %>% mutate(
    p3456_i0 = case_when(
      p3456_i0 == -10 | p3456_i0 == -1 | p3456_i0 == -3 ~ NA,
      TRUE ~ p3456_i0
    ),
    smoking = case_when(
      p20116_i0 == 0 ~ "never",
      p20116_i0 == 1 ~ "former",
      p20116_i0 == 2 & as.numeric(p3456_i0) > 0 & as.numeric(p3456_i0) <= 15 ~ "current <15",
      p20116_i0 == 2 & as.numeric(p3456_i0) > 15 ~ "current > 15",
      TRUE ~ "unknown" # Handling cases not covered by the conditions
    ),
    # bmi
    bmi = p23104_i0,
    bmi = as.numeric(bmi),
    bmi30 = ifelse(p23104_i0 >= 30, 1, 0),
    bmi30 = as.numeric(bmi30)
  )
  return(data)
}

alcohol <- function(data) {
  data <- data %>% mutate(
    p26030_i0 = ifelse(is.na(p26030_i0), 0, p26030_i0),
    p26030_i1 = ifelse(is.na(p26030_i1), 0, p26030_i1),
    p26030_i2 = ifelse(is.na(p26030_i2), 0, p26030_i2),
    p26030_i3 = ifelse(is.na(p26030_i3), 0, p26030_i3),
    p26030_i4 = ifelse(is.na(p26030_i4), 0, p26030_i4)
  )
  return(data)
}

alcohol_intake <- function(data) {
  data <- data %>% mutate(
    alcohol_intake = rowSums(pick(matches("p26030")), na.rm = TRUE),
    alcohol_daily = alcohol_intake / p20077,
    alcohol_weekly = alcohol_daily * 7,
    alc_spline = splines::bs(alcohol_weekly, knots = 4, degree = 3)
  )
  return(data)
}


# related diseases or family history of related diseases
illness <- function(data) {
  data <- data %>% mutate(
    # Self-reported and doctor diagnosed related illness
    related_disease = case_when(
      p20002_i0 == 1065 | p20002_i0 == 1075 | p20002_i0 == 1081 |
        p20002_i0 == 1082 | p20002_i0 == 1583 | p20002_i0 == 1083 |
        p20002_i0 == 1086 | p20002_i0 == 1491 | p20002_i0 == 1473 |
        p20002_i0 == 1160 | p20002_i0 == 1162 | p20002_i0 == 1163 |
        p20002_i0 == 1506 | p20002_i0 == 1074 | p20002_i0 == 1220 |
        p20002_i0 == 1222 | p20002_i0 == 1223 | p6150_i0 == 1 |
        p6150_i0 == 2 | p6150_i0 == 3 | p6150_i0 == 4 | p2443_i0 == 1 |
        p2453_i0 == 1 ~ "yes",
      TRUE ~ "no"
    ),
    related_disease = as.factor(related_disease),
    # illness in closest family
    disease_family = case_when(
        stringr::str_detect(p20107_i0, "\\b(1|2|8|9)\\b") |
          stringr::str_detect(p20110_i0, "\\b(1|2|8|9)\\b") |
          stringr::str_detect(p20111_i0, "\\b(1|2|8|9)\\b") ~ "yes",
        p20107_i0 == -21 |
        p20110_i0 == -21 |
        p20111_i0 == -21 ~ "unknown",
        TRUE ~ "no"
      ),
    disease_family = as.factor(disease_family)
  )
  return(data)
}

# alanine aminotransferase
aminotransferase <- function(data) {
  data <- data %>%
    rename(alt = p30620_i0)
  return(data)
}

# Removing individuals with missing information on covariates
remove_missings <- function(data) {
  data <- data %>%
    filter(
      !is.na(age),
      !is.na(region),
      !is.na(sex),
      !is.na(ethnicity),
      !is.na(deprivation),
      !is.na(education),
      !is.na(cohabitation),
      !is.na(physical_activity),
      !is.na(smoking),
      !is.na(related_disease),
      !is.na(disease_family),
      !is.na(yearly_income),
      !is.na(bmi30)
    )
  return(data)
}

# remove recoded variables before modelling diet variables
remove_p_vars <- function(data) {
  data <- data %>%
    select(-matches(c(
      "p20111", "p20110", "p20107", "p23104",
      "p6150", "p20002", "p2453", "p2443", "p31",
      "p20116", "p26030", "p3456", "p21022",
      "p22040", "p6141", "p6138", "p22189",
      "p21000", "p54", "p738", "p30650",
      "p30620", "p41272", "p20165", "p100002",
      "p100001", "p41282", "p709"
    )))
  return(data)
}


## Create diet variables ---------------------------------------------------

# estimate intake of peas based on pea servings
pea_servings <- function(data) {
  data <- data %>% mutate(
    pea_servings = case_when(
      str_detect(p104280_i0, "1") | str_detect(p104280_i1, "1") | str_detect(p104280_i2, "1") |
        str_detect(p104280_i3, "1") | str_detect(p104280_i4, "1") ~ 1,
      str_detect(p104280_i0, "2") | str_detect(p104280_i1, "2") | str_detect(p104280_i2, "2") |
        str_detect(p104280_i3, "2") | str_detect(p104280_i4, "2") ~ 2,
      str_detect(p104280_i0, "3+") | str_detect(p104280_i1, "3+") | str_detect(p104280_i2, "3+") |
        str_detect(p104280_i3, "3+") | str_detect(p104280_i4, "3+") ~ 3,
      str_detect(p104280_i0, "half") | str_detect(p104280_i1, "half") | str_detect(p104280_i2, "half") |
        str_detect(p104280_i3, "half") | str_detect(p104280_i4, "half") ~ 0.5,
      str_detect(p104280_i0, "quarter") | str_detect(p104280_i1, "quarter") | str_detect(p104280_i2, "quarter") |
        str_detect(p104280_i3, "quarter") | str_detect(p104280_i4, "quarter") ~ 0.25
    ),
    pea_servings = as.numeric(pea_servings),
    peas = pea_servings * 80
  ) # assuming 1 serving 80g
  return(data)
}

# calculate weekly intake of food groups
calculate_weekly_diet <- function(variables, number_recalls) {
  rowSums(dplyr::pick(tidyselect::matches(variables)), na.rm = TRUE) / number_recalls * 7
}

# creating food groups from UKB Aurora Perez
food_groups <- function(data) {
  data <- data %>%
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
      fruit_weekly = calculate_weekly_diet("p26089|p26090|p26091|p26092|p26093|p26094", p20077),
      nut_weekly = calculate_weekly_diet("p26107|p26108", p20077),
      veggie_weekly = calculate_weekly_diet("p26065|p26098|p26115|p26123|p26125|p26143|p26146|p26147|p26144", p20077),
      potato_weekly = calculate_weekly_diet("p26118|p26119|p26120", p20077),
      egg_weekly = calculate_weekly_diet("p26088", p20077),
      non_alc_beverage_weekly = calculate_weekly_diet("p26124|p26141|p26142|p26148|p26081|p26082|p26095|p26126|p26127", p20077),
      alc_beverage_weekly = calculate_weekly_diet("p26151|p26152|p26153|p26067|p26138", p20077),
      snack_weekly = calculate_weekly_diet("p26106|p26140|p26134|p26084|p26085|p26064|p26080", p20077),
      sauce_weekly = calculate_weekly_diet("p26129|p26130", p20077),
      legume_pea_weekly = calculate_weekly_diet("p26086|p26101|p26136|p26137|peas", p20077),
      veggie_pea_weekly = ((rowSums(pick(matches("p26065|p26098|p26147|p26123|p26125|p26143|p26146")), na.rm = TRUE) - peas) / p20077) * 7,
      legume_no_soymilk = calculate_weekly_diet("p26086|p26101|p26137", p20077), # removing soy milk from legumes
      non_alc_beverage_soymilk_weekly = calculate_weekly_diet("p26136|p26124|p26141|p26142|p26148|p26081|p26082|p26095|p26126|p26127", p20077),
      legume_soy_meat = calculate_weekly_diet("p26086|p26101|p26137", p20077), # removing soy milk and soy desert from legumes
      food_weight_weekly = legume_weekly + meats_weekly + poultry_weekly + fish_weekly + cereal_refined_weekly + whole_grain_weekly +
        mixed_dish_weekly + dairy_weekly + fats_weekly + fruit_weekly + nut_weekly + veggie_weekly + potato_weekly + egg_weekly +
        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly + sauce_weekly
    )
  return(data)
}

total_diet <- function(data) {
  data <- data %>% mutate(
    total_meat = rowSums(pick(matches("p26066|p26100|p26104|p26117|p26122")), na.rm = TRUE),
    total_poultry = rowSums(pick(matches("p26121|p26069")), na.rm = TRUE),
    total_fish = rowSums(pick(matches("p26070|p26109|p26132|p26149")), na.rm = TRUE)
  )
  return(data)
}

transform_touchscreen <- function(data) {
  convert_frequency <- function(column) {
    case_when(
      str_detect(column, "Never") | str_detect(column, "know") ~ 0,
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


habitual_diet <- function(data) {
  data <- data %>% mutate(
    habitual_meat = rowSums(pick(matches("p1349|p1369|p1379|p1389")), na.rm = TRUE),
    habitual_meat = as.numeric(habitual_meat),
    habitual_poultry = rowSums(pick(matches("p1359")), na.rm = TRUE),
    habitual_poultry = as.numeric(habitual_poultry),
    habitual_fish = rowSums(pick(matches("p1329|p1339")), na.rm = TRUE),
    habitual_fish = as.numeric(habitual_fish)
  )
  return(data)
}


# remove recoded diet p-variables before modelling outcomes
remove_diet_p_vars <- function(data) {
  data <- data %>% select(-matches(c(
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
  )))
  return(data)
}


## Create outcome variables for time to event ------------------------------

# ICD10 diagnoses codes
icd10_diagnoses <- function(data) {
  data <- data %>%
    mutate(across(starts_with("p41280"), ~ as.Date(.)))

  icd10_subset <- data %>%
    select(starts_with("p41270"), starts_with("p41280"), "id") %>%
    # splitting diagnoses string-variable each time a | is in the string
    separate_wider_delim(p41270, delim = "|", names = paste0("p41270var_a", 0:258), too_few = "debug") %>%
    select(matches("p41270|p41280|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%
    # creating outcome variables with date info from p41280
    mutate(
      # NAFLD
      icd10_nafld_date = ifelse(str_detect(p41270var, "K760"), as.character(c_across(starts_with("p41280"))), NA),
      icd10_nafld_date = as.Date(icd10_nafld_date, format = "%Y-%m-%d"),
      # NASH
      icd10_nash_date = ifelse(str_detect(p41270var, "K758"), as.character(c_across(starts_with("p41280"))), NA),
      icd10_nash_date = as.Date(icd10_nash_date, format = "%Y-%m-%d")
    ) %>%
    # retrieve first diagnosis date
    select(id, icd10_nafld_date, icd10_nash_date) %>%
    pivot_longer(cols = starts_with("icd10_"), names_to = "condition", values_to = "date") %>%
    filter(!is.na(date)) %>%
    group_by(id, condition) %>%
    slice(1) %>%
    pivot_wider(names_from = condition, values_from = date) %>%
    ungroup()

  data <- data %>%
    left_join(icd10_subset, by = "id")

  return(data)
}


# ICD9 diagnoses codes
icd9_diagnoses <- function(data) {
  data <- data %>%
    mutate(across(starts_with("p41281_"), ~ as.Date(.)))

  icd9_subset <- data %>%
    select(starts_with("p41271"), starts_with("p41281"), "id") %>%
    # splitting diagnoses string-variable each time a | is in the string
    separate_wider_delim(p41271, delim = "|", names = paste0("p41271var_a", 0:46), too_few = "debug") %>%
    select(matches("p41271|p41281|id")) %>%
    pivot_longer(cols = matches("_a[0-9]*$"), names_to = c(".value", "a"), names_sep = "_") %>%
    # creating outcome variables with date info from p41281
    mutate(
      # NAFLD
      icd9_nafld_date = ifelse(str_detect(p41271var, "\\b(5718)\\b"), as.character(c_across(starts_with("p41281"))), NA),
      icd9_nafld_date = as.Date(icd9_nafld_date, format = "%Y-%m-%d"),
      # NASH
      icd9_nash_date = ifelse(str_detect(p41271var, "\\b(5715)\\b"), as.character(c_across(starts_with("p41281"))), NA),
      icd9_nash_date = as.Date(icd9_nash_date, format = "%Y-%m-%d")
    ) %>%
    # retrieve first diagnosis date
    select(id, icd9_nafld_date, icd9_nash_date) %>%
    pivot_longer(cols = starts_with("icd9_"), names_to = "condition", values_to = "date") %>%
    filter(!is.na(date)) %>%
    group_by(id, condition) %>%
    slice(1) %>%
    pivot_wider(names_from = condition, values_from = date) %>%
    ungroup()

  data <- data %>%
    left_join(icd9_subset, by = "id")

  return(data)
}

# Defining birth date as origin for survival analysis
date_birth <- function(data) {
  # Convert month names in p52 to their numeric equivalents and create date strings
  data <- data %>%
    mutate(
      # Combine year, month, and day (always set to "15") to form a complete date string
      date_birth = paste(p34, p52, "15", sep = "-")
    ) %>%
    # Convert the date_birth column to Date format
    mutate(date_birth = as.Date(date_birth, format = "%Y-%m-%d"))

  return(data)
}

# Estimate last follow-up date for ICD10 codes (stagnation of diagnoses)
censoring_date <- function(data) {
  # Estimate last date of diagnoses
  dates <- data %>%
    subset(!is.na(icd10_nafld_date))

  # Find the last date of diagnosis
  last_date <- max(dates$icd10_nafld_date) %>% print()

  data <- data %>%
    mutate(censoring = as.Date(last_date))
  return(data)
}

# define variables for survival analyses
outcome_variables <- function(data) {
  data <- data %>%
    mutate(
      date_of_death = if_else(!is.na(p40000_i0), p40000_i0, p40000_i1),
      date_of_death = as.Date(date_of_death),
      loss_to_follow_up = p191,
      loss_to_follow_up = as.Date(loss_to_follow_up),
      # binary variable to indicate if nafld happened
      nafld = case_when(
        !is.na(icd10_nafld_date) | !is.na(icd10_nash_date) ~ 1,
        # no icd9 diagnoses were found and they are therefore not included
        # in outcome variable
        TRUE ~ 0
      ),
      nafld_alc = case_when(
      (!is.na(icd10_nafld_date) | !is.na(icd10_nash_date)) &
      ((sex == 0 & alcohol_intake < 20) |
       (sex == 1 & alcohol_intake < 30)) ~ 1,
      TRUE ~ 0
    )
  )
  return(data)
}

# Eligibility criteria based on outcomes ----------------------------------
# time of last completed 24h recall as baseline date
last_completed_recall <- function(data) {
  data <- data %>%
    mutate(
      ques_comp_t0 = p105010_i0,
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
  return(data)
}


# setting baseline start date as last completed questionnaire
# New column with baseline start date set as last completed questionnaire
baseline_date <- function(data) {
  baseline_start_date <- data %>%
    select(p20077, starts_with("ques_comp_t"), id) %>%
    pivot_longer(
      cols = starts_with("ques_comp_t"),
      names_to = "instance",
      values_to = "completion_date"
    ) %>%
    filter(!is.na(completion_date)) %>%
    group_by(id) %>%
    arrange(completion_date, .by_group = TRUE) %>%
    slice_tail() %>%
    rename(baseline_start_date = completion_date) %>%
    ungroup() %>%
    select(id, baseline_start_date)
  data <- data %>%
    left_join(baseline_start_date, by = "id")

  data <- data %>%
    filter(!is.na(baseline_start_date))
  return(data)
}

# defining time in study
time_in_study <- function(data) {
  data <- data %>% mutate(
    survival_time_nafld = case_when(
      !is.na(icd10_nafld_date) ~ as.numeric(difftime(icd10_nafld_date, baseline_start_date, units = "days")),
      TRUE ~ NA
    ),
    survival_time_nash = case_when(
      !is.na(icd10_nash_date) ~ as.numeric(difftime(icd10_nash_date, baseline_start_date, units = "days")),
      TRUE ~ NA
    ),
    survival_time_ltfu = case_when(
      !is.na(loss_to_follow_up) ~ as.numeric(difftime(loss_to_follow_up, baseline_start_date, units = "days")),
      TRUE ~ NA
    ),
    survival_time_death = case_when(
      !is.na(date_of_death) ~ as.numeric(difftime(date_of_death, baseline_start_date, units = "days")),
      TRUE ~ NA
    ),
    survival_time_cenc = difftime(censoring, baseline_start_date, units = "days"),
    time = pmin(survival_time_death, survival_time_cenc, survival_time_ltfu,
      survival_time_nash, survival_time_nafld,
      na.rm = TRUE
    ),
    time = time / 365.25
  )
  return(data)
}

# count and remove those with event before baseline (time < 0)
event_before_base <- function(data) {
  data_time <- data %>%
    filter(data$time <= 0)
  # counting those with event before baseline
  nafld_nash <- sum(!is.na(data_time$survival_time_nafld) |
    !is.na(data_time$survival_time_nash)) %>%
    print()
  # counting those lost to follow-up or dead before baseline
  ltfu_or_dead <- sum(!is.na(data_time$survival_time_ltfu) |
    !is.na(data_time$survival_time_death) &
      is.na(data_time$survival_time_nafld) &
      is.na(data_time$survival_time_nash) &
      is.na(data_time$survival_time_death)) %>%
    print()

  # removing those with no time in study
  data <- data %>%
    subset(data$time > 0)

  return(data)
}

# delete recoded outcome variables
remove_outcome_p_vars <- function(data) {
  data <- data %>% select(-matches(c(
    "p41280", "p41270", "p41281", "p41271", "p105010_i0",
    "p105010_i1", "p105010_i2", "p105010_i3", "p105010_i4",
    "p191", "p40000_i0", "p40000_i1", "p34", "p52"
  )))
  return(data)
}
# Define survival time ----------------------------------------------------
survival_time <- function(data) {
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
      survival_time = survival_time / 365.25,
      # Remove temporary variable
      survival_time_tmp = NULL
    )
  return(data)
}

# number of events --------------------------------------------------------
number_events <- function(data) {
  table(data$nafld) %>% print()
}
