#### Preamble ####
# Purpose: Tests the structure and validity of the simulated healthcare dataset.
# Author: Chenika Bukes
# Date: 24 November 2024
# Contact: chenika.bukes@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded
# - The simulated healthcare dataset must exist
# Any other information needed? Ensure that the dataset is generated and saved in the appropriate directory.


#### Workspace setup ####
library(tidyverse)

simulated_data <- read_csv("data/00-simulated_data/simulated_healthcare_data.csv")

# Test if the data was successfully loaded
if (exists("simulated_data")) {
  message("Test Passed: The dataset was successfully loaded.")
} else {
  stop("Test Failed: The dataset could not be loaded.")
}


#### Test data ####

# Check if the dataset has 25 rows (5 years, 5 observations per year)
if (nrow(simulated_data) == 25) {
  message("Test Passed: The dataset has 25 rows.")
} else {
  stop("Test Failed: The dataset does not have 25 rows.")
}

# Check if the dataset has the correct number of columns
expected_columns <- c(
  "Year", "beds_per_1000", "physicians_per_1000", "attendance",
  "Wait_CorGraft", "Wait_Angioplasty", "Wait_Knee", "Wait_Hip",
  "Wait_Cataract", "Wait_Prostatectomy", "Wait_Hysterectomy",
  "Pct_Wait_CorGraft", "Pct_Wait_Angioplasty", "Pct_Wait_Knee", 
  "Pct_Wait_Hip", "Pct_Wait_Cataract", "Pct_Wait_Prostatectomy", 
  "Pct_Wait_Hysterectomy", "Pct_Wait_Avg"
)

if (all(colnames(simulated_data) == expected_columns)) {
  message("Test Passed: The dataset contains the correct columns.")
} else {
  stop("Test Failed: The dataset does not contain the correct columns.")
}

# Check if the 'Year' column contains values between 2015 and 2019
if (all(simulated_data$Year >= 2015 & simulated_data$Year <= 2019)) {
  message("Test Passed: The 'Year' column contains only values between 2015 and 2019.")
} else {
  stop("Test Failed: The 'Year' column contains invalid values.")
}

# Check if numerical columns are non-negative
numerical_columns <- simulated_data %>%
  select(
    beds_per_1000, physicians_per_1000, attendance,
    starts_with("Wait_"), starts_with("Pct_")
  )

if (all(sapply(numerical_columns, function(col) all(col >= 0, na.rm = TRUE)))) {
  message("Test Passed: All numerical columns contain non-negative values.")
} else {
  stop("Test Failed: Some numerical columns contain negative values.")
}

# Check if percentage columns are valid percentages (-100 to Inf)
percentage_columns <- simulated_data %>%
  select(starts_with("Pct_"))

if (all(sapply(percentage_columns, function(col) all(col >= -100, na.rm = TRUE)))) {
  message("Test Passed: All percentage columns contain valid values.")
} else {
  stop("Test Failed: Some percentage columns contain invalid values.")
}

# Check if there are any missing values in the dataset
if (all(!is.na(simulated_data))) {
  message("Test Passed: The dataset contains no missing values.")
} else {
  stop("Test Failed: The dataset contains missing values.")
}

# Check if 'Pct_Wait_Avg' is correctly calculated as the mean of other percentages
calculated_avg <- simulated_data %>%
  rowwise() %>%
  mutate(
    Avg_Check = round(mean(c_across(starts_with("Pct_Wait_")), na.rm = TRUE), 2)
  ) %>%
  ungroup()

if (all(calculated_avg$Pct_Wait_Avg == calculated_avg$Avg_Check)) {
  message("Test Passed: 'Pct_Wait_Avg' is correctly calculated.")
} else {
  stop("Test Failed: 'Pct_Wait_Avg' is not correctly calculated.")
}

# Check if 'beds_per_1000' and 'physicians_per_1000' are within realistic ranges
if (all(simulated_data$beds_per_1000 >= 1 & simulated_data$beds_per_1000 <= 3) &&
    all(simulated_data$physicians_per_1000 >= 1.5 & simulated_data$physicians_per_1000 <= 2.5)) {
  message("Test Passed: 'beds_per_1000' and 'physicians_per_1000' are within realistic ranges.")
} else {
  stop("Test Failed: 'beds_per_1000' or 'physicians_per_1000' contain unrealistic values.")
}
