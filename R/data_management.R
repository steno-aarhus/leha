# Data management
library(dplyr)
library(magrittr)
library(tidyr)

# Remove ineligible number of recalls ------------
data <- data %>%
subset(p20077>=2)

# Add ID ------------------------------------------------------------------
data <- data %>%
  mutate(id = 1:n(), .before = everything())

# Remove variables and columns --------------------------------------------
# Delete follow-up instances for confounder variables
variables_to_edit <- c("p738", "p2443", "p2453", "p3456", "p6150",
                       "p20002","p20107", "p20110", "p20111", "p20161", "p20162",
                       "p21000", "p22040", "p22506", "p23104", "p2443",
                       "p2453", "p40000")
data <- data %>%
  select(-matches(paste0(variables_to_edit, "_i[1-4]")))


# Recoding covariables (not foods) ------------------------------------------------------

# sex
data <- data %>%
  mutate(sex = case_when(
    str_detect(p31, "Fem") ~ 0, #female
    str_detect(p31, "Male") ~ 1, #male
    TRUE ~ NA_integer_  # If none of the conditions match
  )
)

# strata of age at recruitment
data <- data %>%
  mutate(age_strata = case_when(
    p21022 < 45 ~ 0,
    p21022 >= 45 & p21022 <= 49 ~ 1,
    p21022 >= 50 & p21022 <= 54 ~ 2,
    p21022 >= 55 & p21022 <= 59 ~ 3,
    p21022 >= 60 & p21022 <= 64 ~ 4,
    p21022 >= 65 ~ 5
  )
)

# ethnic background
data <- data %>%
  mutate(ethnicity = case_when(
    p21000_i0 == "White" | p21000_i0 == "British" | p21000_i0 == "Irish" | p21000_i0 == "Any other white background" ~ 0,
    p21000_i0 == "Mixed" | p21000_i0 == "White and Black Caribbean" |p21000_i0 == "White and Black African" | p21000_i0 == "White and Asian" | p21000_i0 == "Any other mixed background" ~ 1,
    p21000_i0 == "Asian or Asian British" | p21000_i0 =="Indian" | p21000_i0 == "Pakistani" | p21000_i0 == "Bangladeshi" | p21000_i0 == "Any other Asian background" ~ 2,
    p21000_i0 == "Black or Black British" | p21000_i0 == "Caribbean" | p21000_i0 == "African" | p21000_i0 == "Any other Black background" ~ 3,
    p21000_i0 == "Chinese" ~ 4,
    p21000_i0 == "Other ethnic group" ~ 5,
    p21000_i0 == "Do not know" | p21000_i0 == "Prefer not to answer" | str_detect(p21000_i0, "NA") ~ 6
  )
)

# townsend deprivation index
data <- data %>%
  mutate(townsend_deprivation_index = p22189)

# educational level
data <- data %>%
  mutate(education_short = as.character(str_sub(p6138_i0, start = 1, end = 28)),
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
  )
)

# cohabitation (living with a wife or partner (yes, no))
p6141

# physical activity
# Check for normal distribution:
library(ggplot2)
data %>% ggplot(aes(x=p22040_i0))+
  geom_histogram(aes(y=..density..),bins=10)+
  stat_function(fun=dnorm,
                args=list(mean=mean(data$p22040_i0),sd=sd(data$p22040_i0)))

# not normally distributed, categories?
data <- data %>%
  mutate()
p22040_i0,Summed MET minutes per week for all activity,402303,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=22040

# Smoking status categorized as never, former, current 1-15, current 15-25, current 25+, and no answer
data <- data %>%
  mutate(
    smoking = case_when(
      str_detect(p20116_i0, "Never") ~ 0,
      str_detect(p20116_i0, "Previous") ~ 1,
      str_detect(p20116_i0, "Current") & p3456_i0 < 15 ~ 2,
      str_detect(p20116_i0, "Current") & p3456_i0 >= 15 & p3456_i0 <= 25 ~ 3,
      str_detect(p20116_i0, "Current") & p3456_i0 > 25 ~ 4,
      str_detect(p20116_i0, "answer") ~ 5,
      TRUE ~ NA_real_  # Handling cases not covered by the conditions
    )
  )

# Self-reported non-cancer illness. No relevant diseases coded as "NA", other diseases of
# relevance for liver disease combined.
data <- data %>%
  mutate(
    non_cancer_illness= case_when(
      str_detect(p20002_i0, "hypert") | (p6150_i0, "High") ~ 0, # hypertension
      str_detect(p20002_i0, "myocardial") | (p6150_i0, "Heart") ~ 1, # MI
      str_detect(p20002_i0, "stroke") | (p20002_i0, "ischaemic") | (p20002_i0, "haemorrhage") | (p6150_i0, "Stroke")~ 2, # Stroke
      str_detect(p20002_i0, "cholesterol") ~ 3, # high cholesterol
      str_detect(p20002_i0, "cholangitis") | (p20002_i0, "cholelithiasis") | (p20002_i0, "cholecyst") | (p20002_i0, "primary biliary cirrhosis") ~ 4, #gallbladder problems
      str_detect(p20002_i0, "alcoholic cirrhosis") ~ 5, #alcohol liver disease
      str_detect(p6150_i0, "Angina") ~ 6, # angina pectoris
      TRUE ~ NA_integer_  # If none of the conditions match
    )
  )

diabetes doctor str_detect(p2443_i0, )

p2453,Cancer diagnosed by doctor,501498,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=2453

# Illness in closest family - skal den her være med?
# Father
data <- data %>%
  mutate(
    father_illness= case_when(
      str_detect(p20107_i0, "Diabetes") ~ 0,
      str_detect(p20107_i0, "High blood pressure") ~ 1,
      str_detect(p20107_i0, "Stroke") ~ 2,
      str_detect(p20107_i0, "Heart disease") ~ 3,
      TRUE ~ NA_integer_  # If none of the conditions match
    )
  )

# Mother
data <- data %>%
  mutate(
    mother_illness= case_when(
      str_detect(p20110_i0, "Diabetes") ~ 0,
      str_detect(p20110_i0, "High blood pressure") ~ 1,
      str_detect(p20110_i0, "Stroke") ~ 2,
      str_detect(p20110_i0, "Heart disease") ~ 3,
      TRUE ~ NA_integer_  # If none of the conditions match
    )
  )

# Siblings
data <- data %>%
  mutate(
    sibling_illness= case_when(
      str_detect(p20111_i0, "divert") ~ 0,
      str_detect(p20111_i0, "heart") ~ 1,
      str_detect(p20111_i0, "Stroke") ~ 2,
      str_detect(p20111_i0, "Heart disease") ~ 3,
      TRUE ~ NA_integer_  # If none of the conditions match
    )
  )


# BMI cut-off 30


#energy intake
energy = rowMeans(dplyr::across(dplyr::starts_with("p26002")), na.rm = TRUE) * 0.239,

# Convert data to numerics
# TO check if a variable is character, run this code
table(grepl(" ", data$varname)) #table of all variables with spaces in the cell content;

# to see the actual character content
data <- mutate(data, temp = as.numeric(varname))
filter(data, is.na(temp) & !is.na(varname)) %>%
  select(varname, temp)


# Now run diet-data-scripts (probably the sort variables.R scripts)






# Remove old variables used to make new -----------------------------------

variables_to_remove <- c("p3456", "p20107", "p20110", "p20111", "p31",
                         "p21000_i0", "p22189", "p20002". "p6138")

# data <- data %>%
#   select(-any_of(variables_to_remove))
# the above does not work?


# New column names --------------------------------------------------------
# Change column names to understandable variables using dplyr::rename
# Can this be done with a call to the project-variables.csv file? It has ID and
# UKB description, which are the ones I need. I could then snake-case the
# variable names?
names(data)
rap_names <- readr::read_csv(here::here("data-raw/rap-variables.csv"))
nrow(rap_names)
names(data) <- rap_names$rap_variable_name

