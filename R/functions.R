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

remove_ineligible_recalls <- function(data) {
  dplyr::filter(data, p20077 >= 2)
}

add_id <- function(data) {
  dplyr::mutate(data, id = 1:dplyr::n(), .before = dplyr::everything())
}

clean_data <- function(data) {
  dplyr::mutate(data,
    p20077 = as.numeric(p20077),
    sex = as.factor(sex),
    age = as.numeric(age)
  )
}

# Delete follow-up instances for confounder variables
drop_followup_confounders <- function(data) {
  variables_to_edit <- c(
    "p738", "p2443", "p2453", "p3456", "p6150",
    "p20002", "p20107", "p20110", "p20111", "p20161", "p20162",
    "p21000", "p22040", "p22506", "p23104", "p2443",
    "p2453", "p6141", "p709"
  )
  dplyr::select(data, -tidyselect::matches(paste0(variables_to_edit, "_i[1-4]")))
}

rename_variables <- function(data) {
  data |>
    dplyr::rename(
      sex = p31,
      age = p21022
    )
}

save_to_parquet <- function(data, path) {
  arrow::write_parquet(data, path)
  path
}

add_age_strata <- function(data) {
  data |>
    dplyr::mutate(
      age_strata = dplyr::case_when(
        age < 45 ~ 0,
        age >= 45 & age <= 49 ~ 1,
        age >= 50 & age <= 54 ~ 2,
        age >= 55 & age <= 59 ~ 3,
        age >= 60 & age <= 64 ~ 4,
        age >= 65 ~ 5
      )
    )
}

add_ethnicity <- function(data) {
  data |>
    dplyr::mutate(
      ethnicity = dplyr::case_when(
        stringr::str_detect(p21000_i0, "White|British|Irish|Any other white background") ~ "white",
        stringr::str_detect(p21000_i0, "Chinese|Asian or Asian British|Indian|Pakistani|Bangladeshi|Any other Asian background") ~ "asian",
        stringr::str_detect(p21000_i0, "Black or Black British|Caribbean|African|Any other Black background") ~ "black",
        stringr::str_detect(p21000_i0, "White and Black Caribbean|Mixed|White and Black African|White and Asian|Any other mixed background|Other ethnic group|Do not know|Prefer not to answer|NA") ~ "mixed or other"
      )
    )
}
