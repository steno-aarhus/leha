# Data management
library(dplyr)
library(magrittr)
library(tidyr)

# Remove uneligible number of recalls ------------
data <- data %>% 
subset(p20077>=2)

# Add ID ------------------------------------------------------------------
data <- data %>%
  mutate(id = 1:n(), .before = everything())

# Remove variables and columns --------------------------------------------
# Delete follow-up instances for confounder variables
variables_to_edit <- c("p738", "p1239", "p1538", "p1548", "p3456", "p6150",
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


Combination of p1239 (smoking status) and p3456 (number of cigarettes currently smoked) to make categories: never smoker; previous smoker; current 1-15; current 15-25; current 25+
  Maybe p22506 (tobacco smoking) + p22508 (amount of tobacco) instead of cigarettes?
p20161 (pack years of smoking) # is this relevant? Has information on all types
# of smoking.

Smoking: never, former, current (pack years)


# Convert data to numerics
# TO check if a variable is character, run this code
table(grepl(" ", data$varname)) #table of all variables with spaces in the cell content;

# to see the actual character content
data <- mutate(data, temp = as.numeric(varname))
filter(data, is.na(temp) & !is.na(varname)) %>%
  select(varname, temp)


# Now run diet-data-scripts (probably the sort variables.R scripts)


# New column names --------------------------------------------------------
# Change column names to understandable variables using dplyr::rename
# Can this be done with a call to the project-variables.csv file? It has ID and
# UKB description, which are the ones I need. I could then snake-case the
# variable names?
names(data)
rap_names <- readr::read_csv(here::here("data-raw/rap-variables.csv"))
nrow(rap_names)
names(data) <- rap_names$rap_variable_name

