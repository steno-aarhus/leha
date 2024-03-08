data <- data %>% mutate(non_cancer_illness = case_when(
  str_detect(p20002_i0, "hypert") | str_detect(p6150_i0, "High") ~ "hypertension",
  str_detect(p20002_i0, "myocardial") | str_detect(p6150_i0, "Heart") ~ "mi",
  str_detect(p20002_i0, "stroke") | str_detect(p20002_i0, "ischaemic") | str_detect(p20002_i0, "haemorrhage") | str_detect(p6150_i0, "Stroke")~ "stroke",
  str_detect(p20002_i0, "cholesterol") ~ "cholesterolemia",
  str_detect(p20002_i0, "cholangitis") | str_detect(p20002_i0, "cholelithiasis") | str_detect(p20002_i0, "cholecyst") | str_detect(p20002_i0, "primary biliary cirrhosis") ~ "gbd",
  str_detect(p20002_i0, "alcoholic cirrhosis") ~ "alcoholic liver disease",
  str_detect(p6150_i0, "Angina") ~ "angina",
  TRUE ~ ifelse(p6150_i0 == "None" | p20002_i0 == "None", "none of the above", NA_character_))
  )
