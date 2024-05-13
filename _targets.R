# Load packages required to define the pipeline:
library(targets)
library(magrittr)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = ukbAid::get_proj_dependencies(),
  format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # This likely isn't necessary for most UK Biobank users at SDCA/AU.
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
)

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
# Or just some files:
source(here::here("R/functions.R"))

# Constants

username <- "FieLangmann"
parquet_path <- here::here("data/data.parquet")

# Things to run in order to work.
list(
  tar_target(
    name = project_data_csv_path,
    command = ukbAid::download_data(username = username, file_ext = "csv"),
    format = "file"
  ),
  tar_target(
    name = import_csv_parquet_prep,
    command = project_data_csv_path |>
      csv_to_parquet_prep()
  ),
  tar_target(
    name = cleaned_data,
    command = import_csv_parquet_prep |>
      # Need to add ID before converting to duckdb
      add_id() |>
      arrow::to_duckdb() |>
      remove_ineligible_recalls() |>
      drop_followup_confounders() |>
      rename_variables() |>
      clean_data() |>
      add_age_strata() |>
      add_ethnicity() |>
      # Need to end with converting to tibble
      dplyr::as_tibble()
  ),
  tar_target(
    name = saved_as_parquet_path,
    command = cleaned_data |>
      save_to_parquet(parquet_path),
    format = "file"
  )
  # tar_target(
  #   name = uploaded_rap_path,
  #   command = saved_as_parquet_path |>
  #     ukbAid::upload_data(username = username),
  #   format = "file"
  # ),
  # tar_target(
  #   name = project_data_path,
  #   command = ukbAid::download_data(username = username, file_ext = "parquet"),
  #   format = "file"
  # )
)
