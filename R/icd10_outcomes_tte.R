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
  mutate(p41280 = ifelse(str_detect(p41270var, "K80"), as.character(p41280), NA),
         p41280 = as.Date(p41280, format = "%Y-%m-%d"))

# When only the relevant dates are left in the data frame, you can transform
# back to wide and remove irrelevant columns.

# Create a new column for column names in the wide format
icd10_k80 <- icd10_k80 %>%
  mutate(new_column = paste("p41280", a, sep = "_"))

# Pivot the data back to wide format
wide_data <- icd10_k80 %>%
  pivot_wider(names_from = new_column, values_from = p41280)

# Now I can remove rows where column p41270var does not contain any info or "NA"
wide_data <- wide_data %>%
  filter(!is.na(p41270var) & p41270var != "")

# Delete all diagnosis code and only keep the dates? Or can I make a new variable to
# merge the diagnosis code and date to create a variable called date_icd80, date_icd81, etc.?
wide_data <- wide_data %>%
  mutate(icd10_k80_date = ifelse(str_detect(p41270var, "K80"), as.character(starts_with(p41280), NA),
         icd10_k80_date = as.Date(icd10_k80_date, format = "%Y-%m-%d"))


First time a date appears across p41280_a[0-258], the date should be added to a new
column (icd10_k80_date). If there is no date across any of the columns for each observation,
then "NA" should be added to the new variable instead.



icd10_subset <- icd10_subset %>%
  rowwise() %>%
  mutate(icd10_k80_date = as.Date(unlist(c_across(starts_with("p41280_a")), use.names = FALSE)))


# In lack of better description, I want to do this:
# if any starts_with("p41270") and ends with "_a[0-258]" includes "K80" or "K81",
# include any p41280 than ends with the same "_a[0-258]". Variable content not matching
# these criteria should be changed to "NA"
