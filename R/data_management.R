# Data management
library(dplyr)
library(magrittr)
library(tidyr)

# Remove ineligible number of recalls ------------
data <- data %>%
subset(p20077>=2)
data <- data %>%
  mutate(p20077 = as.numeric(p20077))

# Add ID ------------------------------------------------------------------
data <- data %>%
  mutate(id = 1:n(), .before = everything())

# Remove variables and columns --------------------------------------------
# Delete follow-up instances for confounder variables
variables_to_edit <- c("p738", "p2443", "p2453", "p3456", "p6150",
                       "p20002","p20107", "p20110", "p20111", "p20161", "p20162",
                       "p21000", "p22040", "p22506", "p23104", "p2443",
                       "p2453", "p40000", "p6141")
data <- data %>%
  select(-matches(paste0(variables_to_edit, "_i[1-4]")))


# Recoding covariables (not foods) ------------------------------------------------------

sorted_data <- data %>% mutate(
  sex = p31,
  sex = as.factor(sex),
  age = p21022,
  age = as.numeric(age),
  age_strata = case_when(
    p21022 < 45 ~ 0,
    p21022 >= 45 & p21022 <= 49 ~ 1,
    p21022 >= 50 & p21022 <= 54 ~ 2,
    p21022 >= 55 & p21022 <= 59 ~ 3,
    p21022 >= 60 & p21022 <= 64 ~ 4,
    p21022 >= 65 ~ 5
    ),
  age_strata = as.factor(age),
  ethnicity = case_when(
    p21000_i0 == "White" | p21000_i0 == "British" | p21000_i0 == "Irish" | p21000_i0 == "Any other white background" ~ "white",
    p21000_i0 == "Mixed" | p21000_i0 == "White and Black Caribbean" |p21000_i0 == "White and Black African" | p21000_i0 == "White and Asian" | p21000_i0 == "Any other mixed background" ~ "mixed",
    p21000_i0 == "Chinese" | p21000_i0 == "Asian or Asian British" | p21000_i0 =="Indian" | p21000_i0 == "Pakistani" | p21000_i0 == "Bangladeshi" | p21000_i0 == "Any other Asian background" ~ "asian",
    p21000_i0 == "Black or Black British" | p21000_i0 == "Caribbean" | p21000_i0 == "African" | p21000_i0 == "Any other Black background" ~ "black",
    p21000_i0 == "Other ethnic group" | p21000_i0 == "Do not know" | p21000_i0 == "Prefer not to answer" | str_detect(p21000_i0, "NA") ~ "other"
    ),
  deprivation = p22189,
  deprivation = as.numeric(deprivation),
  # categories from this paper: https://doi.org/10.1016/j.eclinm.2020.100658
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
  cohabitation = p6141_i0,
  physical_activity = p22040_i0,
  smoking = case_when(
    str_detect(p20116_i0, "Never") ~ 0,
    str_detect(p20116_i0, "Previous") ~ 1,
    str_detect(p20116_i0, "Current") & p3456_i0 < 15 ~ 2,
    str_detect(p20116_i0, "Current") & p3456_i0 >= 15 & p3456_i0 <= 25 ~ 3,
    str_detect(p20116_i0, "Current") & p3456_i0 > 25 ~ 4,
    str_detect(p20116_i0, "answer") ~ 5,
    TRUE ~ NA_real_  # Handling cases not covered by the conditions
    ),
  alcohol_intake = rowSums(select(., starts_with("p26030"))),
  alcohol_intake = as.numeric(alcohol_intake),
  # Self-reported and doctor diagnosed non-cancer illness.
  p6150_i0 = as.character(p6150_i0),
  p20002_i0 = as.character(p20002_i0),
  non_cancer_illness = case_when(
    str_detect(p20002_i0, "hypert") | str_detect(p6150_i0, "High") ~ "hypertension",
    str_detect(p20002_i0, "myocardial") | str_detect(p6150_i0, "Heart") ~ "mi",
    str_detect(p20002_i0, "stroke") | str_detect(p20002_i0, "ischaemic") | str_detect(p20002_i0, "haemorrhage") | str_detect(p6150_i0, "Stroke")~ "stroke",
    str_detect(p20002_i0, "cholesterol") ~ "cholesterolemia",
    str_detect(p20002_i0, "cholangitis") | str_detect(p20002_i0, "cholelithiasis") | str_detect(p20002_i0, "cholecyst") | str_detect(p20002_i0, "primary biliary cirrhosis") ~ "gbd",
    str_detect(p20002_i0, "alcoholic cirrhosis") ~ "alcoholic liver disease",
    str_detect(p6150_i0, "Angina") ~ "angina",
    TRUE ~ NA_character_  # If none of the conditions match
    ),
  non_cancer_illness = as.factor(non_cancer_illness),
  diabetes = p2443_i0,
  diabetes = as.factor(diabetes),
  # illness in closest family
  p20107_i0 = as.character(p20107_i0),
  p20110_i0 = as.character(p20110_i0),
  p20111_i0 = as.character(p20111_i0),
  family_illness= case_when(
    str_detect(p20107_i0, "Diabetes") | str_detect(p20110_i0, "Diabetes") | str_detect(p20111_i0, "Diabetes")~ "diabetes",
    str_detect(p20107_i0, "High blood pressure") | str_detect(p20110_i0, "High blood pressure") | str_detect(p20111_i0, "High blood pressure") ~ "hypertension",
    str_detect(p20107_i0, "Stroke") | str_detect(p20110_i0, "Stroke") | str_detect(p20111_i0, "Stroke")~ "stroke",
    str_detect(p20107_i0, "Heart disease") | str_detect(p20110_i0, "Heart disease") | str_detect(p20111_i0, "Heart disease") ~ "heart disease",
    TRUE ~ NA_character_ # If none of the conditions match
    ),
  family_illness = as.factor(family_illness),
    cancer = p2453_i0,
  cancer = as.factor(cancer),
    bmi = p23104_i0,
  bmi = as.numeric(bmi),
    bmi30 = ifelse(p23104_i0 >= 30, 1, 0),
  bmi30 = as.numeric(bmi30)
  )


# Removing all participants who have had liver cancer before baseline :

data <- data %>%
  filter(!(cancer_diag0 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date0) < as.Date(baseline_start_date)) &
           !(cancer_diag1 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date1) < as.Date(baseline_start_date)) &
           !(cancer_diag2 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date2) < as.Date(baseline_start_date)) &
           !(cancer_diag3 %in% c("C22.0 Liver cell carcinoma", "C22.1 Intrahepatic bile duct carcinoma") & as.Date(cancer_date3) < as.Date(baseline_start_date)))
