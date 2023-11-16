library(tidyverse)

# Processing

# Download saved data
data <- ukbAid::read_parquet(here::here("data/data.parquet"))
data <-tibble::as_tibble(data)

# DATA MANAGEMENT
# source data_management.r script
# source(here::here("R/data_management.R"))
# source diet_data
# source outcome variables

# Save the changes as parquet and upload to the RAP folder for easy download
# next time you sign in
# arrow::write_parquet(data_csv, here("data/ukb_data.parquet"))

# Upload to the project RAP folder.
# ukbAid::upload_data(here("data/ukb_data.parquet"), username = "FieLangmann")


