library(tidyverse)

# Processing

# download data

ukb_data <- ukbAid::read_parquet(here::here("data/data.parquet"))

# DATA MANAGEMENT
# source data_management.r script

# Convert to the Parquet format. Why? Parquet is faster and a smaller
# file size.
arrow::write_parquet(data_csv, here("data/data.parquet"))

# Upload to the project RAP folder.
ukbAid::upload_data(here("data/data.parquet"), username = "FieLangmann")


