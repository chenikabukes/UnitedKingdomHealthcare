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
analysis_data <- read_parquet("./data/02-analysis_data/final_healthcare_data.parquet")

#### Fit the linear model ####
# Model: Percentage change in wait times as a function of healthcare predictors
wait_times_model <- lm(
  Total ~ beds_per_1000 + physicians_per_1000 + attendance,
  data = analysis_data
)

#### Save the model ####
# Save the fitted model for reuse
saveRDS(wait_times_model, file = "./models/wait_times_model_2015_to_2022.rds")

#### Filter data for second model ####
# Include only data from 2015 to 2019
filtered_data <- analysis_data %>% 
  filter(Year >= 2015 & Year <= 2019)

#### Fit the linear model for 2015-2019####
# Model: Percentage change in wait times as a function of healthcare predictors
wait_times_model_adj <- lm(
  Total ~ beds_per_1000 + physicians_per_1000 + attendance,
  data = filtered_data
)

#### Save the model for 2015-2019####
# Save the fitted model for reuse
saveRDS(wait_times_model_adj, file = "./models/wait_times_model_pre_2020.rds")

