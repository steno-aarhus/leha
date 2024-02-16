# Processing
# Load packages -----------------------------------------------------------
library(tidyverse)

# Load saved data:
# source(here::here("R/1_data_start.R")) #loads data before data management
# source(here::here("R/2_data_management.R")) #loads data and runs data management
# source(here::here("R/3_diet_data.R")) #loads sorted_data and runs diet code
# source(here::here("R/4_icd10_outcomes_tte.R") #loads sorted_data and runs outcome code

# Save the changes as parquet and upload to the RAP folder for easy download
# next time you sign in
arrow::write_parquet(sorted_data, here("data/sorted_data.parquet"))

# Upload to the project RAP folder.
ukbAid::upload_data(here("data/sorted_data.parquet"), username = "FieLangmann")

# Go to UKB RAP and manually change name of uploaded .parquet to sorted_data.parquet
# Now you can run your analyses with sorted_data as data frame
