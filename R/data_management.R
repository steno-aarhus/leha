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


# Recoding variables ------------------------------------------------------

# Self-reported non-cancer illness. No diseases coded as 0, liver disease coded as 1, CVD coded as 2, other coded as 3
# Self-reported diseases should be coded as 0 for those who have not had any relevant
# diseases, 1 for those who have had CVD but no liver disease, 2 for those who have had
# liver disease but no CVD and 3 for those who had both liver and CVD.
data <- data %>%
  mutate(
    non_cancer_illness= case_when(
      str_detect(p20002_i0, "divert") ~ 0,
      str_detect(p20002_i0, "heart") ~ 1,
      TRUE ~ NA_integer_  # If none of the conditions match
    )
  )

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

p20107,Illnesses of father,488041,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20107
p20110,Illnesses of mother,492917,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=20110
p20111

# Illness in closest family
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

