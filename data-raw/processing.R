# Processing
# Load packages -----------------------------------------------------------
library(tidyverse)
library(ukbAid)
# Save data to RAP --------------------------------------------------------

# save data locally to be able to upload to RAP
readr::write_csv(data, here::here("data/data_leha.csv"))

# save to RAP
rap_copy_to("data/data_lega.csv", "/users/FieLangmann/data_leha.csv")


# Load data from RAP ------------------------------------------------------

# load data from RAP
rap_data <- rap_copy_from("/users/FieLangmann/data_leha.csv", "data/data_leha.csv")
# load data into RStudio
data <- readr::read_csv("data/data_leha.csv")
