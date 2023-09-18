# Data management

# Change column names to understandable variables using dplyr::rename
library(dplyr)

data <- data %>%
  dplyr::rename(sex = p31,
         birth_year = p34,
         birth_month = p52)

