library(tidyverse)

# Processing

# Download saved data
source(here::here("R/data_start.R"))

# Run all data management
# source(here::here("R/data_management.R"))
# source diet_data
# source outcome variables

# Save the changes as parquet and upload to the RAP folder for easy download
# next time you sign in
# arrow::write_parquet(data_csv, here("data/data.parquet"))

# Upload to the project RAP folder.
# ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")


