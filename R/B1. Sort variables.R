#1. Sort variables
# Packages
library(tidyr)


# 1.1. Exposures
#Grouping foods
#by weight
data_weight<-data %>%
  mutate(Wlegumes=(beans+),
         Wleg_pea= (Wlegumes + peas)
         Wmeats=(beef+lamb+...),
         Wpoultry=(),
         Wfish=(),
         Wmixdish=(),
         Wruits= (),
         Wveggies=()
         Wfats=(),
         Wdrinks= (),
         Wcereal = (),
         Wdairy= (),
         Wsauces=(),
         Wsweets=(),
         Walcohol=(),

  )

)


#by energy
#Energy provided per food <-- I will need to mutate as above with a multiplication for energy contributions from each foods, e.g. weight_beef/(100*E*0.239kcal)
data_kcal<-data %>%
  mutate(energy_foods=(p26002)*0.293,
         Elegumes=(),
         Emeats=((beef*E)+(lamb*E)+...)*0.239kcal,
  )


#1.2. outcomes
gallstone
cholecystect
cholecystitis
end of follow-up
death
loss to follow-up

#1.3. Covariables


#For liver/biliary/pancreas problems:
p41271,Diagnoses - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41271
p41281,Date of first in-patient diagnosis - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41281
#ICD-9 codes:574(cholelithiasis); 575.0-2 (Cholecystitis+obstruction of gallbladder)
p41270,Diagnoses - ICD10,440017,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270
p41280,Date of first in-patient diagnosis - ICD10,440014,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41280
#ICD-10 code before entry?








# Outcomes
# death
data1 <- data1 %>%
  mutate(date_of_death = if_else(is.na(p40000_i0), p40000_i1, p40000_i0))

# emigration
# loss to follow-up

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

# When code works it should be rerun for ICD9 and OPSC4 codes below:
# if p41271 includes starts_with"574" or "5750" or "5751", include any p41281 arrays that matches this
# if p41272 includes "J18", "J21.1 ", "J24.2", J24.3 J26.1, include any p41282 arrays that matches this


# How can I combine these?
p41270,Diagnoses - ICD10,440017,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41270 # delete?
p41271,Diagnoses - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41271 # delete?
p41272,Operative procedures - OPCS4,440159,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41272 # delete?

p41280,Date of first in-patient diagnosis - ICD10,440014,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41280
p41281,Date of first in-patient diagnosis - ICD9,20299,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41281
p41282,Date of first operative procedure - OPCS4,440153,https://biobank.ndph.ox.ac.uk/ukb/field.cgi?id=41282



