#### Preamble ####
# Purpose: Tests the structure and validity of the cleaned healthcare dataset.
# Author: Chenika Bukes
# Date: 24 November 2024
# Contact: chenika.bukes@mail.utoronto.ca
# License: MIT
# Pre-requisites:
#   - The `tidyverse` and `testthat` packages must be installed and loaded
#   - The cleaned dataset must exist at "data/02-analysis_data/final_healthcare_data.csv"

#### Workspace setup ####
library(tidyverse)
library(testthat)

cleaned_data <- read_csv("../data/02-analysis_data/final_healthcare_data.csv")

#### Test data ####

# Test that the dataset has 5 rows (one per year, 2015–2019)
test_that("dataset has 5 rows", {
  expect_equal(nrow(cleaned_data), 5)
})

# Test that all values in 'Year' are integers between 2015 and 2019
test_that("'Year' column contains valid years", {
  expect_true(all(cleaned_data$Year %in% 2015:2019))
})

# Test that 'beds_per_1000' and 'physicians_per_1000' are within realistic ranges
test_that("'beds_per_1000' and 'physicians_per_1000' are within realistic ranges", {
  expect_true(all(cleaned_data$beds_per_1000 >= 1 & cleaned_data$beds_per_1000 <= 3))
  expect_true(all(cleaned_data$physicians_per_1000 >= 1.5 & cleaned_data$physicians_per_1000 <= 2.5))
})

# Test that 'attendance' is non-negative
test_that("'attendance' column is non-negative", {
  expect_true(all(cleaned_data$attendance >= 0))
})

# Test that all waiting times are non-negative
test_that("waiting time columns are non-negative", {
  waiting_time_columns <- grep("^Wait_", names(cleaned_data), value = TRUE)
  expect_true(all(sapply(cleaned_data[waiting_time_columns], function(col) all(col >= 0, na.rm = TRUE))))
})

# Test that 'Pct_Wait_Avg' is non-negative
test_that("'Pct_Wait_Avg' is non-negative", {
  expect_true(all(cleaned_data$Pct_Wait_Avg >= 0))
})

# Test that there are no missing values in the dataset
test_that("no missing values in dataset", {
  expect_true(all(!is.na(cleaned_data)))
})

# Test that 'Wait_CorGraft' column contains numeric data
test_that("'Wait_CorGraft' is numeric", {
  expect_type(cleaned_data$Wait_CorGraft, "double")
})

# Test that 'Pct_Wait_Avg' is correctly calculated (mean of percentage columns)
test_that("'Pct_Wait_Avg' matches mean of percentage columns", {
  percentage_columns <- grep("^Pct_Wait_", names(cleaned_data), value = TRUE)
  calculated_avg <- rowMeans(cleaned_data[percentage_columns], na.rm = TRUE)
  expect_equal(round(cleaned_data$Pct_Wait_Avg, 2), round(calculated_avg, 2))
})
