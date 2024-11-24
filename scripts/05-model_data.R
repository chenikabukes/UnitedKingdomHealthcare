#### Preamble ####
# Purpose: Fits and saves a frequentist linear model for hospital wait times.
# Author: Chenika Bukes
# Date: 24 November 2024
# Contact: chenika.bukes@mail.utoronto.ca
# License: MIT
# Pre-requisites: Cleaned dataset is available at "./data/02-analysis_data/final_healthcare_data.csv".

#### Workspace setup ####
library(tidyverse)

#### Read data ####
# Load the cleaned data
analysis_data <- read_csv("./data/02-analysis_data/final_healthcare_data.csv")

#### Fit the linear model ####
# Model: Percentage change in wait times as a function of healthcare predictors
wait_times_model <- lm(
  Pct_Wait_Avg ~ beds_per_1000 + physicians_per_1000 + attendance,
  data = analysis_data
)

#### Save the model ####
# Save the fitted model for reuse
saveRDS(wait_times_model, file = "./models/wait_times_model.rds")
