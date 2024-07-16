# Functions


# Data management ---------------------------------------------------------

## Remove participants with less than 2 diet assessments -------------------------------------
two_recalls <- function(data) {
  data <- data %>%
  subset(p20077>=2) %>%
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
    sex = p31,
    sex = as.factor(sex),
    age = p21022,
    age = as.numeric(age),
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
      p21000_i0 == "White" | p21000_i0 == "British" | p21000_i0 == "Irish" | p21000_i0 == "Any other white background" ~ "white",
      p21000_i0 == "Chinese" | p21000_i0 == "Asian or Asian British" | p21000_i0 =="Indian" | p21000_i0 == "Pakistani" | p21000_i0 == "Bangladeshi" | p21000_i0 == "Any other Asian background" ~ "asian",
      p21000_i0 == "Black or Black British" | p21000_i0 == "Caribbean" | p21000_i0 == "African" | p21000_i0 == "Any other Black background" ~ "black",
      p21000_i0 == "Mixed" | p21000_i0 == "White and Black Caribbean" |p21000_i0 == "White and Black African" | p21000_i0 == "White and Asian" | p21000_i0 == "Any other mixed background" |
        p21000_i0 == "Other ethnic group" | p21000_i0 == "Do not know" | p21000_i0 == "Prefer not to answer" | str_detect(p21000_i0, "NA") ~ "mixed or other"
    ),
    deprivation = p22189,
    deprivation_quint = ntile(deprivation, 5),
    deprivation_quint = as.factor(deprivation_quint),
    yearly_income = case_when(
      str_detect(p738_i0, "18,000 to") ~ "18,000-30,999",
      str_detect(p738_i0, "31,000") ~ "31,000-51,999",
      str_detect(p738_i0, "52,000") ~ "52,000-100,000",
      str_detect(p738_i0, "know") ~ "don't know",
      str_detect(p738_i0, "Greater") ~ ">100,000",
      str_detect(p738_i0, "Less") ~ "<18,000",
      str_detect(p738_i0, "answer") ~ "no answer",
      TRUE ~ "no answer"
    ),
    yearly_income = as.factor(yearly_income),
    education_short = as.character(str_sub(p6138_i0, start = 1, end = 28)),
    education = case_when(
      education_short == "College or University degree" ~ "High",
      education_short == "A levels/AS levels or equiva" ~ "Intermediate",
      education_short == "O levels/GCSEs or equivalent" ~ "Intermediate",
      education_short == "CSEs or equivalent|NVQ or HN" ~ "Low",
      education_short == "CSEs or equivalent|Other pro" ~ "Low",
      education_short == "CSEs or equivalent" ~ "Low",
      education_short == "NVQ or HND or HNC or equival" ~ "Low",
      education_short == "Other professional qualifica" ~ "Low",
      education_short == "None of the above" ~ "Low"
    ),
    education = as.factor(education),
    physical_activity = case_when(
      p22040_i0 >0 & p22040_i0 <=918 ~ "low",
      p22040_i0 >918 & p22040_i0 <=3706 ~ "moderate",
      p22040_i0 >3706 ~ "high",
      TRUE ~ "unknown"
      ),
    cohabitation = case_when(
      p709_i0 == 1 ~ "alone",
      str_detect(p6141_i0, "Husband, wife or partner") ~ "with spouse/partner",
      p6141_i0 == "Prefer not to answer" ~ "no answer",
      TRUE ~ "other non-partner"
      ),
    # 10 UK recruitment regions
    region = case_when(
      str_detect(p54_i0, "Barts") | str_detect(p54_i0, "Croydon") | str_detect(p54_i0, "Hounslow")  ~ "London",
      str_detect(p54_i0, "Cardiff") | str_detect(p54_i0, "Swansea") | str_detect(p54_i0, "Wrexham") ~ "Wales",
      str_detect(p54_i0, "Bury") | str_detect(p54_i0, "Stockport") | str_detect(p54_i0, "Liverpool") | str_detect(p54_i0, "Manchester") ~ "North_West",
      str_detect(p54_i0, "Newcastle") | str_detect(p54_i0, "Middlesborough") ~ "North_East",
      str_detect(p54_i0, "Leeds") | str_detect(p54_i0, "Sheffield") ~ "Yorkshire_Humber",
      str_detect(p54_i0, "Birmingham") | str_detect(p54_i0, "Cheadle")  ~ "West_Midlands",
      str_detect(p54_i0, "Nottingham") | str_detect(p54_i0, "Stoke") ~ "East_Midlands",
      str_detect(p54_i0, "Oxford") | str_detect(p54_i0, "Reading") ~ "South_East",
      str_detect(p54_i0, "Bristol") ~ "South_West",
      str_detect(p54_i0, "Edinburgh") | str_detect(p54_i0, "Glasgow") ~ "Scotland"
      ),
      region = as.factor(region))
  return(data)
}

# lifestyle variables
lifestyle <- function(data) {
  data <- data %>% mutate(
    smoking = case_when(
      str_detect(p20116_i0, "Never") ~ "never",
      str_detect(p20116_i0, "Previous") ~ "former",
      str_detect(p20116_i0, "Current") & as.numeric(p3456_i0) > 0 & as.numeric(p3456_i0) <= 15 ~ "current <15",
      str_detect(p20116_i0, "Current") & as.numeric(p3456_i0) > 15 ~ "current > 15",
      str_detect(p20116_i0, "answer") ~ "no answer",
      TRUE ~ "no answer"  # Handling cases not covered by the conditions
    ),
    # bmi
    bmi = p23104_i0,
    bmi = as.numeric(bmi),
    bmi30 = ifelse(p23104_i0 >= 30, 1, 0),
    bmi30 = as.numeric(bmi30),
    # alcohol
    p26030_i0 = ifelse(is.na(p26030_i0), 0, p26030_i0),
    p26030_i1 = ifelse(is.na(p26030_i1), 0, p26030_i1),
    p26030_i2 = ifelse(is.na(p26030_i2), 0, p26030_i2),
    p26030_i3 = ifelse(is.na(p26030_i3), 0, p26030_i3),
    p26030_i4 = ifelse(is.na(p26030_i4), 0, p26030_i4),
    alcohol_intake = rowSums(select(., starts_with("p26030"))),
    alcohol_daily = alcohol_intake/p20077,
    alcohol_weekly = alcohol_daily * 7)
  return(data)
}

# related diseases or family history of related diseases
illness <- function(data) {
  data <- data %>% mutate(
    p6150_i0 = ifelse(is.na(p6150_i0), "None", p6150_i0),
    p6150_i0 = as.character(p6150_i0),
    p20002_i0 = ifelse(is.na(p20002_i0), "None", p20002_i0),
    p20002_i0 = as.character(p20002_i0),
    # Self-reported and doctor diagnosed related illness
    related_disease = case_when(
      str_detect(p20002_i0, "hypert") | str_detect(p6150_i0, "High") |
        str_detect(p20002_i0, "myocardial") | str_detect(p6150_i0, "Heart") |
        str_detect(p20002_i0, "stroke") | str_detect(p20002_i0, "ischaemic") |
        str_detect(p20002_i0, "haemorrhage") | str_detect(p6150_i0, "Stroke") |
        str_detect(p20002_i0, "cholesterol") | str_detect(p20002_i0, "cholangitis") |
        str_detect(p20002_i0, "cholelithiasis") | str_detect(p20002_i0, "cholecyst") |
        str_detect(p20002_i0, "primary biliary cirrhosis") | str_detect(p6150_i0, "Angina") |
        str_detect(p20002_i0, "alcoholic cirrhosis") | p2443_i0 == "Yes" |
        str_detect(p2453_i0, "Yes") ~ "yes",
      TRUE ~ "no"),
    related_disease = as.factor(related_disease),
    # illness in closest family
    disease_family = case_when(
      str_detect(p20107_i0, "Diabetes") | str_detect(p20110_i0, "Diabetes") |
        str_detect(p20111_i0, "Diabetes") | str_detect(p20107_i0, "High blood pressure") |
        str_detect(p20110_i0, "High blood pressure") | str_detect(p20111_i0, "High blood pressure") |
        str_detect(p20107_i0, "Stroke") | str_detect(p20110_i0, "Stroke") |
        str_detect(p20111_i0, "Stroke") | str_detect(p20107_i0, "Heart disease") |
        str_detect(p20110_i0, "Heart disease") | str_detect(p20111_i0, "Heart disease") ~ "yes",
        TRUE ~ "no" # If none of the conditions match
      ),
    disease_family = as.factor(disease_family))
  return(data)
}

# alanine aminotransferase
aminotransferase <- function(data) {
  data <- data %>%
    rename(alt = p30620_i0)
  return(data)
}

# Removing individuals with missing information on covariates
## should result in 123822 individuals in df
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
      !is.na(bmi30))
  return(data)
}

# remove recoded variables before modelling diet variables
remove_p_vars <- function(data) {
  data <- data %>%
  select(-matches(c("p20111", "p20110", "p20107", "p23104",
                    "p6150", "p20002", "p2453", "p2443", "p31",
                    "p20116", "p26030", "p3456", "p21022",
                    "p22040", "p6141", "p6138", "p22189",
                    "p21000", "p54", "p738", "p30650",
                    "p30620", "p41272", "p20165", "p100002",
                    "p100001", "p41282", "p709")))
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
        str_detect(p104280_i3, "quarter") | str_detect(p104280_i4, "quarter") ~ 0.25),
    pea_servings = as.numeric(pea_servings),
    peas = pea_servings * 80) #assuming 1 serving 80g
  return(data)
}

# calculate weekly intake of food groups
calculate_weekly_diet <- function(variables, number_recalls) {
  rowSums(dplyr::pick(tidyselect::matches(variables)), na.rm = TRUE) / number_recalls * 7
}

# creating food groups from UKB Aurora Perez
diet_data <- function(data) {
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
      fruit_weekly= calculate_weekly_diet("p26089|p26090|p26091|p26092|p26093|p26094", p20077),
      nut_weekly = calculate_weekly_diet("p26107|p26108", p20077),
      veggie_weekly = calculate_weekly_diet("p26065|p26098|p26115|p26123|p26125|p26143|p26146|p26147|p26144", p20077),
      potato_weekly = calculate_weekly_diet("p26118|p26119|p26120", p20077),
      egg_weekly = calculate_weekly_diet("p26088", p20077),
      non_alc_beverage_weekly = calculate_weekly_diet("p26124|p26141|p26142|p26148|p26081|p26082|p26095|p26126|p26127", p20077),
      alc_beverage_weekly = calculate_weekly_diet("p26151|p26152|p26153|p26067|p26138", p20077),
      snack_weekly = calculate_weekly_diet("p26106|p26140|p26134|p26084|p26085|p26064|p26080", p20077),
      sauce_weekly = calculate_weekly_diet("p26129|p26130", p20077),
      legume_pea_weekly = calculate_weekly_diet("p26086|p26101|p26136|p26137|peas", p20077),
      veggie_pea = ((rowSums(pick(matches("p26065|p26098|p26147|p26123|p26125|p26143|p26146")), na.rm = TRUE) - peas) / p20077) * 7,
      food_weight_weekly = legume_weekly + meats_weekly + poultry_weekly + fish_weekly + cereal_refined_weekly + whole_grain_weekly +
        mixed_dish_weekly + dairy_weekly + fats_weekly + fruit_weekly + nut_weekly + veggie_weekly + potato_weekly + egg_weekly +
        non_alc_beverage_weekly + alc_beverage_weekly + snack_weekly + sauce_weekly)
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
    "p26070", "p26109", "p26132", "p26149", "p26000", "p104280")))
  return(data)
}


## Create outcome variables for time to event ------------------------------
<- function(data) {


}






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


# time of last completed 24h recall as baseline date
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


# New column with baseline start date as last completed questionnaire
data <- data %>%
  # Convert ques_0 to ques_4 to date format
  mutate(across(starts_with("ques_"), as.Date)) %>%
  # Gather all columns into key-value pairs
  pivot_longer(cols = starts_with("ques_"), names_to = "questionnaire", values_to = "date_filled") %>%
  # Group by participant ID and select the row with the latest date_filled for each participant
  group_by(id) %>%
  slice(which.max(date_filled)) %>%
  ungroup() %>%
  # Rename the remaining column to indicate the last filled questionnaire
  rename(last_filled_questionnaire = questionnaire)


data <- data %>%
  mutate(date_filled = as.Date(date_filled))

remove <- c("p105010_i0", "p105010_i1", "p105010_i2", "p105010_i3","p105010_i4")
data <- data %>%
  select(-matches(remove))


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


# Set cut-off date for follow-up ------------------------------------------
# Estimated last follow-up date for ICD10 codes (stagnation of diagnoses)
# Create the plot
ggplot(data, aes(x = icd10_nafld_date, y = id)) +
  geom_point() + # Use points to show individual data points
  geom_smooth(method = "lm", se = FALSE) + # Add linear regression line
  annotate("text", x = max(data$icd10_nafld_date), y = min(data$id),
           label = paste("Last observed date:", max(data$icd10_nafld_date)),
           hjust = 1, vjust = -0.5, size = 4) +  # Add annotation for the last observed date
  labs(title = "Dates of Disease Over Time", x = "Date of Disease", y = "Participant ID")

# The density is very high in the right of the plot - I will estimate the last
# diagnosis date in data:

dates <- data %>%
  subset(!is.na(icd10_nafld_date))

# Find the last date of diagnosis
last_date <- max(dates$icd10_nafld_date)

# Print or use the last date as needed
print(last_date)

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

# counting and removing those with event before baseline
data_time <- data %>%
  subset(data$time<0)

nafld_nash <-sum(!is.na(data_time$survival_time_nafld)
                 | !is.na(data_time$survival_time_nash))

ltfu <- sum(!is.na(data_time$survival_time_ltfu)
            & is.na(data_time$survival_time_nafld)
            & is.na(data_time$survival_time_nash)
            & is.na(data_time$survival_time_death))

death <- sum(!is.na(data_time$survival_time_death)
             & is.na(data_time$survival_time_nafld)
             & is.na(data_time$survival_time_nash)
             & is.na(data_time$survival_time_ltfu))


# remove those with event before baseline
data <- data %>%
  subset(data$time>=0)


## remove old p-variables --------------------------------------------------
<- function(data) {


}




