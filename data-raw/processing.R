# Processing
# Load packages -----------------------------------------------------------
library(tidyverse)

# Load saved data:
# source(here::here("R/1_data_start.R")) #loads data before data management
source(here::here("R/2_data_management.R")) #runs data management and saves sorted_data.parquet
source(here::here("R/3_diet_data.R")) #runs diet code and saves sorted_data.parquet
# source(here::here("R/4_icd10_outcomes_tte.R") #loads data and runs outcome code

# Save the changes as parquet and upload to the RAP folder for easy download
# next time you sign in
arrow::write_parquet(sorted_data, here("data/sorted_data.parquet"))

# Upload to the project RAP folder.
ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")

# Go to UKB RAP and manually change name of uploaded .parquet to sorted_data.parquet
# Now you can run your analyses with sorted_data as data frame
