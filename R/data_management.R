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
                       "p21000", "p22040", "p22506", "p22508", "p23104", "p2443",
                       "p2453", "p40000")
data <- data %>%
  select(-matches(paste0(variables_to_edit, "_i[1-4]")))


# Recoding covariables (not foods) ------------------------------------------------------

sex


age
strata of age at recruitment
(\<45, 45-49, 50-54, 55-59, 60-64, ≤65 years)
ethnic group

townsend

educational level

Uk regions

cohabitation

physical activity


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
      str_detect(p20002_i0, "hypert") ~ 0, # hypertension
      str_detect(p20002_i0, "myocardial") ~ 1, # MI
      str_detect(p20002_i0, "stroke") | (p20002_i0, "ischaemic") | (p20002_i0, "haemorrhage")  ~ 2, # Stroke
      str_detect(p20002_i0, "cholesterol") ~ 3, # high cholesterol
      str_detect(p20002_i0, "cholangitis") | (p20002_i0, "cholelithiasis") | (p20002_i0, "cholecyst") | (p20002_i0, "primary biliary cirrhosis") ~ 4, #gallbladder problems
      str_detect(p20002_i0, "alcoholic cirrhosis") ~ 5, #alcohol liver disease
      TRUE ~ NA_integer_  # If none of the conditions match
    )
  )


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


# Convert data to numerics
# TO check if a variable is character, run this code
table(grepl(" ", data$varname)) #table of all variables with spaces in the cell content;

# to see the actual character content
data <- mutate(data, temp = as.numeric(varname))
filter(data, is.na(temp) & !is.na(varname)) %>%
  select(varname, temp)


# Now run diet-data-scripts (probably the sort variables.R scripts)






# Remove old variables used to make new -----------------------------------

variables_to_remove <- c("p3456", "p20107", "p20110", "p20111")

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

