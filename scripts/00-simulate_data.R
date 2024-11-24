#### Preamble ####
# Purpose: Simulates a dataset of healthcare statistics similar to the described dataset.
# Author: Chenika Bukes
# Date: 24 November 2024
# Contact: chenika.bukes@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed.

#### Workspace setup ####
library(tidyverse)
set.seed(66)

#### Simulate data ####
# Years for the data
years <- 2015:2019

# Function to ensure non-negative random values
generate_non_negative <- function(n, mean, sd, min_value = 0) {
  abs(rnorm(n, mean, sd))  # Ensure all values are non-negative
}

# Generate data for each year
simulated_data <- tibble(
  Year = rep(years, each = 5),  # 5 observations per year
  beds_per_1000 = generate_non_negative(n = 25, mean = 2.5, sd = 0.2),  
  physicians_per_1000 = generate_non_negative(n = 25, mean = 2.07, sd = 0.1),  
  attendance = generate_non_negative(n = 25, mean = 100, sd = 15),  
  
  # Simulated wait times for procedures (in months)
  Wait_CorGraft = generate_non_negative(n = 25, mean = 6, sd = 1.5),
  Wait_Angioplasty = generate_non_negative(n = 25, mean = 5, sd = 1.2),
  Wait_Knee = generate_non_negative(n = 25, mean = 8, sd = 2),
  Wait_Hip = generate_non_negative(n = 25, mean = 7, sd = 1.8),
  Wait_Cataract = generate_non_negative(n = 25, mean = 3, sd = 0.8),
  Wait_Prostatectomy = generate_non_negative(n = 25, mean = 4, sd = 1.1),
  Wait_Hysterectomy = generate_non_negative(n = 25, mean = 5.5, sd = 1.3)
)

# Calculate percentage increases relative to 2015 for each procedure
base_year <- simulated_data %>%
  filter(Year == 2015) %>%
  select(-Year, -beds_per_1000, -physicians_per_1000, -attendance)

# Ensure all base_year values are strictly positive to avoid division by zero
base_year <- base_year %>%
  mutate(across(everything(), ~ if_else(. <= 0, 0.01, .)))  # Replace 0s with a small positive value

# Calculate percentage increase for each procedure
simulated_data <- simulated_data %>%
  mutate(across(
    starts_with("Wait_"),
    ~ round((. / base_year[[cur_column()]][1] - 1) * 100, 2),
    .names = "Pct_{.col}"
  ))

# Ensure all percentage columns are non-negative
simulated_data <- simulated_data %>%
  mutate(across(starts_with("Pct_"), ~ if_else(. < 0, 0, .)))

# Calculate average percentage increase across procedures
simulated_data <- simulated_data %>%
  rowwise() %>%
  mutate(
    Pct_Wait_Avg = round(mean(c_across(starts_with("Pct_")), na.rm = TRUE), 2)
  ) %>%
  ungroup()

#### Save data ####
# Save the simulated dataset
write_csv(simulated_data, "data/00-simulated_data/simulated_healthcare_data.csv")
write_parquet(simulated_data, "data/00-simulated_data/simulated_healthcare_data.parquet")
