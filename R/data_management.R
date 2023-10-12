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

