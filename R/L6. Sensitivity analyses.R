
# Load packages -----------------------------------------------------------
library(dplyr)
library(magrittr)


#6. Sensitivity analyses
Legumes+peas
Multiple imputations / permutations?


  # removing high ALT and AST
sorted_data <- sorted_data %>% mutate(
  alt = case_when(
    p30620 <= 45 & sex == "Female" | p30620 <= 70 & sex == "Male" ~ 0,
    p30620 > 45 & sex == "Female" | p30620 > 70 & sex == "Male" ~ 1
  ),
  ast = case_when(
    p30650 <= 35 & sex == "Female" | p30650 <= 45 & sex == "Male" ~ 0,
    p30650 > 35 & sex == "Female" | p30650 > 45 & sex == "Male" ~ 1
  )
)
