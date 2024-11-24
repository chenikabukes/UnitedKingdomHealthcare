#### Preamble ####
# Purpose: Cleans and merges raw healthcare data from OECD into one dataset with physicians normalized per 1000 population.
# Author: Chenika Bukes
# Date: 20 November 2024
# Contact: chenika.bukes@mail.utoronto.ca
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(arrow)
library(tidyverse)


#### Process and merge datasets ####

# 1. Load and process the waiting times data
waiting_times_data <- read_parquet("./data/01-raw_data/waiting_times.parquet") |>
  filter(Reference.area == "United Kingdom" & 
           TIME_PERIOD >= 2015 & 
           TIME_PERIOD <= 2019) |>
  select(TIME_PERIOD, OBS_VALUE, Medical.procedure) |>
  rename(
    Year = TIME_PERIOD,
    WaitTime = OBS_VALUE
  ) |>
  pivot_wider(
    names_from = Medical.procedure,
    values_from = WaitTime,
    names_prefix = "WaitTime_"
  ) |>
  arrange(Year)

# Calculate percentage increase for each operation relative to 2015
wait_times_percentage <- waiting_times_data |>
  mutate(across(
    starts_with("WaitTime_"),
    ~ round((. / .[Year == 2015] - 1) * 100, 2),  # Calculate percentage increase and round to 2 decimals
    .names = "Pct_{.col}"               
  ))

# Add percentage increase columns for each operation relative to 2015
waiting_times_percentage <- waiting_times_data |>
  mutate(across(
    starts_with("WaitTime_"),
    ~ round((. / .[Year == 2015] - 1) * 100, 2),  # Calculate percentage increase and round to 2 decimals
    .names = "Pct_{.col}"  # Add prefix "Pct_" to new columns
  ))

# Calculate average percentage increase across all operations
wait_times_percentage <- wait_times_percentage |>
  rowwise() |>
  mutate(
    Pct_Wait_Avg = round(mean(c_across(starts_with("Pct_")), na.rm = TRUE), 2)  # Av
  ) |>
  ungroup()

# Merge only the average percentage increase with the predictors
wait_times_data <- wait_times_percentage |>
  select(Year, starts_with("WaitTime_"), Pct_Wait_Avg)

# 2. Load and process the physician data
physician_data <- read_parquet("./data/01-raw_data/number_physicians.parquet") |>
  filter(Reference.area == "United Kingdom" & 
           TIME_PERIOD >= 2015 & 
           TIME_PERIOD <= 2019) |>
  select(TIME_PERIOD, OBS_VALUE) |>
  rename(
    Year = TIME_PERIOD,
    number_physicians = OBS_VALUE
  ) |>
  arrange(Year)

# 3. Load and process the hospital beds data
beds_data <- read_parquet("./data/01-raw_data/hospital_beds.parquet") |>
  filter(Reference.area == "United Kingdom" & 
           TIME_PERIOD >= 2015 & 
           TIME_PERIOD <= 2019) |>
  select(TIME_PERIOD, OBS_VALUE) |>
  rename(
    Year = TIME_PERIOD,
    beds_per_1000 = OBS_VALUE
  ) |>
  arrange(Year)

# 4. Load and process the population data
population_data <- read_parquet("./data/01-raw_data/population_estimate.parquet") |>
  filter(str_detect(Title, "^\\d{4}$")) |>
  mutate(Year = as.numeric(Title)) |>
  mutate(Population = as.numeric(gsub(",", "", `United Kingdom population mid-year estimate`))) |>
  filter(Year >= 2015 & Year <= 2019) |>
  select(Year, Population)

# 5. Load and process the A&E data
a_and_e_data <- read_parquet("./data/01-raw_data/a_and_e_activity_data_full.parquet") |>
  filter(Year >= 2015 & Year <= 2019) |>
  pivot_wider(
    names_from = Type,
    values_from = Percentage
  ) |>
  # Adjust percentages relative to 2015 baseline
  mutate(
    baseline_attendance = `Type 1 Attendances`[Year == 2015],
    baseline_admissions = `Type 1 Emergency Admissions`[Year == 2015],
    attendance = `Type 1 Attendances` - baseline_attendance,
    emergency_admit = `Type 1 Emergency Admissions` - baseline_admissions
  ) |>
  select(Year, emergency_admit, attendance)

# 6. Merge all predictors with wait times and population
final_data <- hospital_data |>
  left_join(physician_data, by = "Year") |>
  left_join(beds_data, by = "Year") |>
  left_join(population_data, by = "Year") |>
  left_join(a_and_e_data, by = "Year") |>
  left_join(wait_times_data, by = "Year") |>
  mutate(
    physicians_per_1000 = number_physicians / (Population / 1000)
  ) |>
  select(-number_physicians, -Population) |>
  select(
    Year, 
    beds_per_1000, 
    physicians_per_1000, 
    attendance,
    starts_with("WaitTime_"),
    Pct_Wait_Avg
  )

# Rename waiting times columns for clarity
final_data <- final_data |>
  rename(
    Wait_CorGraft = `WaitTime_Coronary artery bypass graft`,
    Wait_Angioplasty = `WaitTime_Transluminal coronary angioplasty`,
    Wait_Knee = `WaitTime_Knee replacement (including the revision of knee replacement)`,
    Wait_Hip = `WaitTime_Hip replacement (total and partial, including the revision of hip replacement)`,
    Wait_Cataract = `WaitTime_Cataract surgery`,
    Wait_Prostatectomy = `WaitTime_Prostatectomy`,
    Wait_Hysterectomy = `WaitTime_Hysterectomy`
  ) 


#### Save final dataset ####

# Save the merged dataset
write_csv(final_data, "./data/02-analysis_data/final_healthcare_data.csv")
write_parquet(final_data, "./data/02-analysis_data/final_healthcare_data.parquet")

