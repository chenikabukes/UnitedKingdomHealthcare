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

# Generate data for each year
simulated_data <- tibble(
  Year = rep(years, each = 5),  # 5 observations per year
  beds_per_1000 = rnorm(n = 25, mean = 2.5, sd = 0.2),  
  physicians_per_1000 = rnorm(n = 25, mean = 2.07, sd = 0.1),  
  attendance = rnorm(n = 25, mean = 100, sd = 15),  
  
  # Simulated wait times for procedures (in months)
  Wait_CorGraft = rnorm(n = 25, mean = 6, sd = 1.5),
  Wait_Angioplasty = rnorm(n = 25, mean = 5, sd = 1.2),
  Wait_Knee = rnorm(n = 25, mean = 8, sd = 2),
  Wait_Hip = rnorm(n = 25, mean = 7, sd = 1.8),
  Wait_Cataract = rnorm(n = 25, mean = 3, sd = 0.8),
  Wait_Prostatectomy = rnorm(n = 25, mean = 4, sd = 1.1),
  Wait_Hysterectomy = rnorm(n = 25, mean = 5.5, sd = 1.3)
)

# Calculate percentage increases relative to 2015 for each procedure
base_year <- simulated_data %>%
  filter(Year == 2015) %>%
  select(-Year, -beds_per_1000, -physicians_per_1000, -attendance)

# Calculate percentage increase for each procedure
simulated_data <- simulated_data %>%
  mutate(across(
    starts_with("Wait_"),
    ~ round((. / base_year[[cur_column()]][1] - 1) * 100, 2),
    .names = "Pct_{.col}"
  ))

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
