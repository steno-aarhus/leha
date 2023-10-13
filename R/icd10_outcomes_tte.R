# This script will split p41270 (ICD10-codes) into columns and match the date of
# diagnosis with the diagnosis code for that specific data. This is useful when
# using ICD10 diagnoses as outcomes in time-to-event analyses


# On a subset of data -----------------------------------------------------
# subset of data for overview
test <- data %>%
  slice(55001:60000)

# Split the diagnosis-variable into separate column based on delimiter "|"
test <- test %>%
  separate_wider_delim(p41270,
                       delim = "|",
                       names = paste0("p41270var_a", 0:258), too_few = "debug")

# Transform from wide to long format to match ICD-codes with date of diagnosis
test %>%  select(matches("_a[0-9]*$"))

icd10_subset <- test %>%
  select(matches("p41270|p41280|id")) %>%
  pivot_longer(cols = matches("_a[0-9]*$"),
               names_to = c(".value", "a"),
               names_sep = "_")

# Remove columns not needed
remove <- c("p41270_ok", "p41270_pieces", "p41270_remainder")

icd10_subset <- icd10_subset %>%
  dplyr::select(-(remove))

# Match variable content across p41270 and p41280 and remove irrelevant dates
icd10_k80 <- icd10_subset %>%
  mutate(p41280 = ifelse(str_detect(p41270var, "K80"), as.Date(p41280), "NA")) # This should be coded as.Date but does not?

# When only the relevant dates are left in the data frame, you can transform
# back to wide and remove irrelevant columns.

# Create a new column for column names in the wide format
icd10_k80 <- icd10_k80 %>%
  mutate(new_column = paste("p41280", a, sep = "_"))

# Pivot the data back to wide format
wide_data <- icd10_k80 %>%
  pivot_wider(names_from = new_column, values_from = p41280)



Delete all diagnosis code and only keep the dates? Or can I make a new variable to
merge the diagnosis code and date to create a variable called date_icd80, date_icd81, etc.?

icd10_subset <- icd10_subset %>%
  rowwise() %>%
  mutate(icd10_k80_date = as.Date(unlist(c_across(starts_with("p41280_a")), use.names = FALSE)))


# In lack of better description, I want to do this:
# if any starts_with("p41270") and ends with "_a[0-259]" includes "K80" or "K81",
# include any p41280 than ends with the same "_a[0-259]". Variable content not matching
# these criteria should be changed to "NA"

# Define a vector of suffixes to match
suffixes_to_match <- paste0("_a", 0:259)

# Apply the logic to update the variables
# df <- test %>%
#   rowwise() %>%
#   mutate(
#     across(starts_with("p41270"), .fns = ~ {
#       case_when(
#         grepl("K80|K81", .) & grepl(paste0("(", paste(suffixes_to_match, collapse = "|"), ")$"), cur_column()) ~ .,
#         TRUE ~ "NA"
#       )
#     }),
#     across(starts_with("p41280"), .fns = ~ {
#       case_when(
#         grepl(paste0("(", paste(suffixes_to_match, collapse = "|"), ")$"), cur_column()) ~ .,
#         TRUE ~ "NA"
#       )
#     })
#   ) %>%
#   ungroup()

> update_variable <- function(test, var_prefix, suffixes_to_match) {
  +     var_names <- names(test)
  +
    +     for (suffix in suffixes_to_match) {
      +         p41270_var_name <- paste0(var_prefix, suffix)
      +         matching_var_names <- var_names[startsWith(var_names, p41270_var_name)]
      +         matching_var_names <- matching_var_names[grepl("K80|K81", matching_var_names)]
      +         test[, matching_var_names] <- ifelse(is.na(test[, matching_var_names]), "NA", test[, matching_var_names])
      +     }
  +
    +     return(test)
  + }

df <- update_variable(test, "p41270", suffixes_to_match)
df <- update_variable(test, "p41280", suffixes_to_match)
