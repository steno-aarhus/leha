# Data management

# Change column names to understandable variables using dplyr::rename
library(dplyr)

data <- data %>%
  dplyr::rename(sex = p31,
         birth_year = p34,
         birth_month = p52,
         number_recall = p20077)


# Remove participants with less than 2 24h recalls
number_recalls >= 2


# Long to wide to long data for at få gennemsnit af fødevareindtag
# Using pivot_longer() from tidyr (Recommended for modern R workflows)
install.packages("tidyr")
library(tidyr)
long_data <- wide_data %>%
  pivot_longer(cols = -ID, names_to = "Variable", values_to = "Value")

# Using melt() from reshape2
install.packages("reshape2")
library(reshape2)
long_data <- melt(wide_data, id.vars = "ID")
across_all(^"_i")
