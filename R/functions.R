import_parquet <- function(path) {
  data <- ukbAid::read_parquet(path)
  tibble::as_tibble(data)
}

csv_to_parquet_prep <- function(path) {
  # uncomment to debug this function
  # browser()
  # Split dataset into core and those with dates because
  # there were issues loading them.
  data_csv_core <- readr::read_csv(
    here::here("data/data.csv"),
    col_select = c(-tidyselect::matches("^p4128[0-3]_.*$"))
  )
  # readr::problems(data_csv_core)

  # Create a separate dataset of variables that are only dates
  # to fix the importing issue.
  data_csv_dates <- readr::read_csv(
    here::here("data/data.csv"),
    col_select = tidyselect::matches("^p4128[0-3]_.*$"),
    # All dates
    col_types = "D"
  )
  # readr::problems(data_csv_dates)

  # Combine them together.
  purrr::list_cbind(list(
    data_csv_core,
    data_csv_dates
  ))
}
