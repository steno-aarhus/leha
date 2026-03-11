# Additional calculations

# Time between completed diet questionnaires ------------------------------



# Printing knots of alc_spline --------------------------------------------
library(dplyr)
library(magrittr)
library(tidyverse)

data <- targets::tar_read(id_data)

data <- data %>% mutate(
  alcohol_intake = rowSums(pick(matches("p26030")), na.rm = TRUE),
  alcohol_daily = alcohol_intake / p20077,
  alcohol_weekly = alcohol_daily * 7,
  alc_spline = splines::bs(alcohol_weekly, knots = 4, degree = 3)
)

# Calculate internal knots based on quantiles
nknots <- 4  # Number of knots (adjust as needed)
internal_knots <- quantile(data$alcohol_weekly, probs = seq(0.25, 0.75, length.out = nknots), na.rm = TRUE)

# Generate the spline basis using specified knots
spline_basis <- splines::bs(data$alcohol_weekly, knots = internal_knots, degree = 3)

# Extract and print the knot positions
knots <- attr(spline_basis, "knots")

cat("knots are placed at:", knots, "\n")



