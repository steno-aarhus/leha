library(tidyverse)

# Processing

# Download saved data
source(here::here("R/1_data_start.R"))

# Run all data management
# source(here::here("R/data_management.R"))
# source diet_data
# source outcome variables

# Save the changes as parquet and upload to the RAP folder for easy download
# next time you sign in
arrow::write_parquet(sorted_data, here("data/sorted_data.parquet"))

# Upload to the project RAP folder.
ukbAid::upload_data(here("data/sorted_data.parquet"), username = "FieLangmann")

# Go to UKB RAP and manually change name of uploaded .parquet to sorted_data.parquet
# Now you can run your analyses with sorted_data as data frame
