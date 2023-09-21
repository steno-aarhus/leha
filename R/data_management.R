# Data management

# Convert data to numerics
# TO check if a variable is character, run this code
table(grepl(" ", data$p2887_i0)) #table of all variables with spaces in the cell content; p2887 is a varible for smoking

# to see the actual character content
data <- mutate(data, temp = as.numeric(p2887_i0))
filter(data, is.na(temp) & !is.na(p2887_i0)) %>%
  select(p2887_i0, temp)

data <- data %>%
  select(p2887_i0) %>% as.numeric()
as.numeric(data$p2887_i0) # reads some numbers but not all and may convert some numbers into NAs - should be checked, but not manually, how?
# p2887 has the answer option "don't know" And "less than 1 daily" which doesn't converge. Should be changed to a number, but which?
# After deciding on the fate of p2887, mutate the answer options to a relevant number and continue.


!!!!# WARNING. This was just a test to try to run the wide to long transformation! DO NOT INCLUDE IN REAL CODING
  data <- data %>%
  mutate_if(is.character, as.numeric)



# Long to wide to long data for at få gennemsnit af fødevareindtag
# Using pivot_longer() from tidyr (Recommended for modern R workflows)
install.packages("tidyr")
library(tidyr)

# Try to do this for a subset of data first
test_data <- data %>%
  (slice_sample(data, prop = 0.1) # gives 10% of rows

test_long <- teat_data %>%
  pivot_longer(cols = matches("_i[0-4]$"),
                names_to = c(".value", "i"),
                names_sep = "_")


# Then do it for the full data
long_data <- data %>%
  pivot_longer(cols = matches("_i[0-4]$"),
                names_to = c(".value", "i"),
                names_sep = "_")


# Eksempel med vinindtag
# NAs should be recoded to 0 in all diet data to calculate average daily intakes
# p26151,Fortified wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26151
# p26152,Red wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26152
# p26153,White wine,210965,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=26153

long_data <- long_data %>%
  mutate(across(weight_wine = "p26151" + "p26152" + "p26153")

# To group the weight_wine (=the sum) I will group by participant ID so that the 5 rows for each individual is collated into 1 row.
group_by(id)
summarise

# Average daily intake.
mutate(avg_wine = weight_wine/number_recalls)

# Would it be relevant to look at average weekly intake instead? Or are the 24h recalls not suited for such assumptions, e.g., that we could divide by number of recalls and then multiply by 7 to day, that this is the weekly average intake which is relevant "always"?


Create food groups --> go to sort_variables.R scripts



# Change column names to understandable variables using dplyr::rename
# Can this be done with a call to the project-variables.csv file? It has ID and
# UKB description, which are the ones I need. I could then snake-case the
# variable names?
library(dplyr)

data <- data %>%
  dplyr::rename(sex = p31,
                birth_year = p34,
                birth_month = p52,
                number_recalls = p20077)

# Remove participants with less than 2 24h recalls
filter(number_recalls >= 2)
