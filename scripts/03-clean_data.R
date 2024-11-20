#### Preamble ####
# Purpose: Cleans the raw healthcare data from OECD.
# Author: Chenika Bukes
# Date: 20 November 2024
# Contact: chenika.bukes@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None


# Load the library
# library(arrow)

# Read the CSV file
df <- read.csv("./data/01-raw_data/hospital_beds.csv")

# Save the data frame as a Parquet file
parquet_file_path <- "./data/01-raw_data/hospital_beds.parquet"
write_parquet(df, parquet_file_path)


#### Workspace setup ####
library(arrow)
library(tidyverse)

#### Clean data ####
# Load the raw Parquet file
raw_data <- read_parquet("./data/01-raw_data/waiting_times.parquet")

# Filter for required conditions and select relevant columns
cleaned_wait_times <- raw_data |>
  filter(Reference.area == "United Kingdom" & 
           TIME_PERIOD >= 2015 & 
           TIME_PERIOD <= 2019) |>
  select(TIME_PERIOD, OBS_VALUE, Medical.procedure) |>
  rename(
    WaitTime = OBS_VALUE
  ) |>
  arrange(Year)

# Load the raw Parquet file
raw_data <- read_parquet("./data/01-raw_data/number_hospitals.parquet")

# Filter for required conditions and select relevant columns
cleaned_hospital_data <- raw_data |>
  filter(Reference.area == "United Kingdom" & 
           TIME_PERIOD >= 2015 & 
           TIME_PERIOD <= 2019 &
           Hospital.type == "Total") |>
  select(TIME_PERIOD, OBS_VALUE) |>
  rename(
    Year = TIME_PERIOD,
    HospitalsPerMillionInhabitants = OBS_VALUE
  ) |>
  arrange(Year)

raw_data <- read_parquet("./data/01-raw_data/number_physicians.parquet")

# Filter for required conditions and select relevant columns
cleaned_physician_data <- raw_data |>
  filter(Reference.area == "United Kingdom" & 
           TIME_PERIOD >= 2015 & 
           TIME_PERIOD <= 2019) |>
  select(TIME_PERIOD, OBS_VALUE) |>
  rename(
    Year = TIME_PERIOD,
    NumberPhysicians = OBS_VALUE
  ) |>
  arrange(Year)

# Load the raw Parquet file
raw_data <- read_parquet("./data/01-raw_data/hospital_beds.parquet")

# Filter for required conditions and select relevant columns
cleaned_beds_data <- raw_data |>
  filter(Reference.area == "United Kingdom" & 
           TIME_PERIOD >= 2015 & 
           TIME_PERIOD <= 2019) |>
  select(TIME_PERIOD, OBS_VALUE) |>
  rename(
    Year = TIME_PERIOD,
    HospitalBedsPer1000Inhabitants = OBS_VALUE
  ) |>
  arrange(Year)


#### Save data ####
# Save the cleaned data as a CSV file
write_csv(cleaned_wait_times, "./data/02-analysis_data/cleaned_waiting_times.csv")

# Save the cleaned data as a Parquet file 
write_parquet(cleaned_wait_times, "./data/02-analysis_data/cleaned_waiting_times.parquet")

# Save the cleaned data as a CSV file
write_csv(cleaned_hospital_data, "./data/02-analysis_data/cleaned_hospital_data.csv")

# Save the cleaned data as a Parquet file 
write_parquet(cleaned_hospital_data, "./data/02-analysis_data/cleaned_hospital_data.parquet")

# Save the cleaned data as a CSV file
write_csv(cleaned_physician_data, "./data/02-analysis_data/cleaned_physician_data.csv")

# Save the cleaned data as a Parquet file 
write_parquet(cleaned_physician_data, "./data/02-analysis_data/cleaned_physician_data.parquet")

# Save the cleaned data as a CSV file
write_csv(cleaned_beds_data, "./data/02-analysis_data/cleaned_beds_data.csv")

# Save the cleaned data as a Parquet file 
write_parquet(cleaned_beds_data, "./data/02-analysis_data/cleaned_beds_data.parquet")

