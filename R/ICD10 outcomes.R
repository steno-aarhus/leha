# ICD10-codes
# subset of data for overview
test <- data %>%
  slice(1:100)

# Split the 'variable into separate variables based on delimiter "|"
test1 <- test %>%
  separate_wider_delim(p41270, delim = "|",
                       names_sep = "var_a", too_few = "debug")

test2 <- test |>
  separate_wider_delim(p41270,
                       delim = "|",
                       names = paste0("p41270var_a", 1:259), too_few = "debug")

# To check if test and test2 matches:
all.equal(test, test2)

final <- test %>%
  separate_wider_delim(p41270,
                       delim = "|",
                       names = paste0("p41270var_a", 0:258), too_few = "debug")

final %>%  select(matches("_a[0-9]*$"))




test %>%
  pivot_longer(cols = matches("_a[0-9]*$"),
               names_to = c(".value", "a"),
               names_sep = "_")


# When code works, uncomment below and split variable for all observations
# data <- data %>%
#   separate_wider_delim(p41270, delim = "|",
#                        names_sep = "var_a", too_few = "debug")

# Pivot_longer code
# long_test <- test %>%
#   pivot_longer(cols = matches("_a[0-259]$"),
#                names_to = c(".value", "a"),
#                names_sep = "_")

# When code works, uncomment below and pivot_longer
# long_data <- data %>%
#   pivot_longer(cols = matches("_a[0-259]$"),
#                names_to = c(".value", "a"),
#                names_sep = "_")


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
