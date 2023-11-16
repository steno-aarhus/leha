library(tidyverse)

# Processing

# download data

ukb_data <- ukbAid::read_parquet(here::here("data/data.parquet"))

# DATA MANAGEMENT
# source data_management.r script

# Save the changes as parquet and upload to the RAP folder for easy download
# next time you sign in
arrow::write_parquet(data_csv, here("data/data.parquet"))

# Upload to the project RAP folder.
ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")


