#### Preamble ####
# Purpose: Tests the structure and validity of the simulated healthcare dataset.
# Author: Chenika Bukes
# Date: 24 November 2024
# Contact: chenika.bukes@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
#   - The `tidyverse` and `testthat` packages must be installed and loaded
#   - The simulated healthcare dataset must exist
# Any other information needed? Ensure that the dataset is generated and saved in the appropriate directory.


#### Workspace setup ####
library(tidyverse)
library(testthat)

simulated_data <- read_csv("../data/00-simulated_data/simulated_healthcare_data.csv")

#### Test data ####

# Test that the dataset was successfully loaded
test_that("Dataset is loaded", {
  expect_true(exists("simulated_data"))
})

# Test that the dataset has 25 rows (5 years, 5 observations per year)
test_that("Dataset has 25 rows", {
  expect_equal(nrow(simulated_data), 25)
})

# Test that the dataset has the expected columns
expected_columns <- c(
  "Year", "beds_per_1000", "physicians_per_1000", "attendance",
  "Wait_CorGraft", "Wait_Angioplasty", "Wait_Knee", "Wait_Hip",
  "Wait_Cataract", "Wait_Prostatectomy", "Wait_Hysterectomy",
  "Pct_Wait_CorGraft", "Pct_Wait_Angioplasty", "Pct_Wait_Knee", 
  "Pct_Wait_Hip", "Pct_Wait_Cataract", "Pct_Wait_Prostatectomy", 
  "Pct_Wait_Hysterectomy", "Pct_Wait_Avg"
)
test_that("Dataset has correct columns", {
  expect_equal(colnames(simulated_data), expected_columns)
})

# Test that the 'Year' column contains values between 2015 and 2019
test_that("'Year' column contains valid values", {
  expect_true(all(simulated_data$Year %in% 2015:2019))
})

# Test that numerical columns are non-negative
numerical_columns <- simulated_data %>%
  select(
    beds_per_1000, physicians_per_1000, attendance,
    starts_with("Wait_"), starts_with("Pct_")
  )
test_that("Numerical columns are non-negative", {
  expect_true(all(sapply(numerical_columns, function(col) all(col >= 0, na.rm = TRUE))))
})

# Test that percentage columns contain valid values (-100 to Inf)
percentage_columns <- simulated_data %>%
  select(starts_with("Pct_"))
test_that("Percentage columns contain valid values", {
  expect_true(all(sapply(percentage_columns, function(col) all(col >= -100, na.rm = TRUE))))
})

# Test that there are no missing values in the dataset
test_that("No missing values in dataset", {
  expect_true(all(!is.na(simulated_data)))
})

# Test that 'Pct_Wait_Avg' is correctly calculated as the mean of other percentages
calculated_avg <- simulated_data %>%
  rowwise() %>%
  mutate(
    Avg_Check = round(mean(c_across(starts_with("Pct_Wait_")), na.rm = TRUE), 2)
  ) %>%
  ungroup()
test_that("'Pct_Wait_Avg' is correctly calculated", {
  expect_equal(simulated_data$Pct_Wait_Avg, calculated_avg$Avg_Check)
})

# Test that 'beds_per_1000' and 'physicians_per_1000' are within realistic ranges
test_that("'beds_per_1000' and 'physicians_per_1000' are realistic", {
  expect_true(all(simulated_data$beds_per_1000 >= 1 & simulated_data$beds_per_1000 <= 3))
  expect_true(all(simulated_data$physicians_per_1000 >= 1.5 & simulated_data$physicians_per_1000 <= 2.5))
})
                  