#### Preamble ####
# Purpose: Cleans and merges raw healthcare data from OECD into one dataset with physicians normalized per 1000 population.
# Author: Chenika Bukes
# Date: 20 November 2024
# Contact: chenika.bukes@mail.utoronto.ca
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
library(arrow)
library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)

#### Process and merge datasets ####

# 1. Load and process the waiting times data
# Define the path to the folder containing the Excel files for waiting times
file_path <- "./data/01-raw_data"  
file_list <- list.files(file_path, pattern = "*.xls", full.names = TRUE)

# Define treatment function names (before and after April 2021)
treatment_names_before_2021 <- c(
  "General Surgery", "Urology", "Trauma & Orthopaedics", "ENT", "Ophthalmology", 
  "Oral Surgery", "Neurosurgery", "Plastic Surgery", "Cardiothoracic Surgery", 
  "General Medicine", "Gastroenterology", "Cardiology", "Dermatology", 
  "Thoracic Medicine", "Neurology", "Rheumatology", "Geriatric Medicine", 
  "Gynaecology", "Other", "Total"
)

treatment_names_after_2021 <- c(
  "General Surgery Service", "Urology Service", "Trauma and Orthopaedic Service", 
  "Ear Nose and Throat Service", "Ophthalmology Service", "Oral Surgery Service", 
  "Neurosurgical Service", "Plastic Surgery Service", "Cardiothoracic Surgery Service", 
  "General Internal Medicine Service", "Gastroenterology Service", "Cardiology Service", 
  "Dermatology Service", "Respiratory Medicine Service", "Neurology Service", 
  "Rheumatology Service", "Elderly Medicine Service", "Gynaecology Service", 
  "Other - Medical Services", "Other - Mental Health Services", "Other - Paediatric Services", 
  "Other - Surgical Services", "Other - Other Services", "Total"
)

# Create a mapping for renaming columns
rename_mapping <- c(
  "General Surgery Service" = "General Surgery",
  "Urology Service" = "Urology",
  "Trauma and Orthopaedic Service" = "Trauma & Orthopaedics",
  "Ear Nose and Throat Service" = "ENT",
  "Ophthalmology Service" = "Ophthalmology",
  "Oral Surgery Service" = "Oral Surgery",
  "Neurosurgical Service" = "Neurosurgery",
  "Plastic Surgery Service" = "Plastic Surgery",
  "Cardiothoracic Surgery Service" = "Cardiothoracic Surgery",
  "General Internal Medicine Service" = "General Medicine",
  "Gastroenterology Service" = "Gastroenterology",
  "Cardiology Service" = "Cardiology",
  "Dermatology Service" = "Dermatology",
  "Respiratory Medicine Service" = "Thoracic Medicine",
  "Neurology Service" = "Neurology",
  "Rheumatology Service" = "Rheumatology",
  "Elderly Medicine Service" = "Geriatric Medicine",
  "Gynaecology Service" = "Gynaecology",
  "Other - Medical Services" = "Other",
  "Other - Mental Health Services" = "Other",
  "Other - Paediatric Services" = "Other",
  "Other - Surgical Services" = "Other",
  "Other - Other Services" = "Other",
  "Total" = "Total"
)

# Function to process and extract relevant columns from each file
process_file <- function(file) {
  # Read the Excel file
  data <- read_excel(file, skip = 13) 
  
  # Extract month and year from file name
  month_year <- str_extract(basename(file), "[A-Za-z]{3}[0-9]{2}")
  year <- as.numeric(paste0("20", substr(month_year, 4, 5)))  # Extract year
  
  # Standardize column names
  colnames(data) <- tolower(colnames(data))
  
  # Check if necessary columns exist
  if (!("treatment function" %in% colnames(data)) || 
      !"average (median) waiting time (in weeks)" %in% colnames(data)) {
    warning(paste("File skipped (missing columns):", file))
    return(NULL)
  }
  
  # Extract relevant columns and add year/month
  data <- data %>%
    select(
      treatment_function = `treatment function`, 
      average_waiting_time = `average (median) waiting time (in weeks)`
    ) %>%
    mutate(Year = year, Month_Year = month_year)
  
  return(data)
}

# Process all files and combine the data
all_data <- bind_rows(lapply(file_list, process_file), .id = "File_ID")

# Standardize treatment function names
all_data <- all_data %>%
  mutate(treatment_function = case_when(
    Year < 2021 & treatment_function %in% treatment_names_before_2021 ~ treatment_function,
    Year >= 2021 & treatment_function %in% names(rename_mapping) ~ rename_mapping[treatment_function],
    TRUE ~ NA_character_  
  )) %>%
  filter(!is.na(treatment_function)) %>%
  filter(!is.na(treatment_function) & treatment_function != "Other")

# Calculate yearly averages for each treatment function
yearly_averages <- all_data %>%
  group_by(Year, treatment_function) %>%
  summarize(Average_Waiting_Time = mean(average_waiting_time, na.rm = TRUE)) %>%
  ungroup()

# Pivot the data so that years are rows and treatment functions are columns
wide_yearly_averages <- yearly_averages %>%
  pivot_wider(names_from = treatment_function, values_from = Average_Waiting_Time)

# Calculate percent change from 2015 levels with robust handling of NA values
wide_yearly_averages_percent_change <- wide_yearly_averages %>%
  mutate(across(
    -Year,  # Exclude the 'Year' column from the calculation
    ~ ifelse(!is.na(.) & !is.na(first(.)), (.) / first(.) * 100 - 100, NA),  # Calculate percent change, handle NA
    .names = "{.col}_percent_change"  # Rename columns to indicate percent change
  ))

# Drop the original columns and retain only the percent change columns
wide_yearly_averages_percent_change <- wide_yearly_averages_percent_change %>%
  select(Year, ends_with("_percent_change")) %>%
  rename_with(~ gsub("_percent_change", "", .), ends_with("_percent_change"))  # Rename back to original names

# Rename columns to keep only the first word for treatment names and resolve duplicates
wide_yearly_averages_percent_change <- wide_yearly_averages_percent_change %>%
  rename_with(~ {
    # Extract the first word
    renamed <- sapply(strsplit(., " "), `[`, 1)
    # Handle duplicates
    duplicated_names <- duplicated(renamed) | duplicated(renamed, fromLast = TRUE)
    renamed[duplicated_names] <- ifelse(
      renamed[duplicated_names] == "General",
      paste0(renamed[duplicated_names], c("_S", "_M")),  # Assign suffix for Surgery and Medicine
      renamed[duplicated_names]
    )
    renamed
  }, -Year)


# 2. Load and process the physician data
physician_data <- read.csv("./data/01-raw_data/number_physicians.csv") |>
  filter(Reference.area == "United Kingdom" & 
           TIME_PERIOD >= 2015 & 
           TIME_PERIOD <= 2022) |>
  select(TIME_PERIOD, OBS_VALUE) |>
  rename(
    Year = TIME_PERIOD,
    number_physicians = OBS_VALUE
  ) |>
  arrange(Year)

# Assuming `physician_data` is your dataframe
physician_data <- physician_data %>%
  group_by(Year) %>%
  filter(abs(number_physicians - 3) == min(abs(number_physicians - 3))) %>%
  ungroup() |>
  rename(
    physicians_per_1000 = number_physicians
  )

# 3. Load and process the hospital beds data
beds_data <- read.csv("./data/01-raw_data/hospital_beds.csv") |>
  filter(Reference.area == "United Kingdom" & 
           TIME_PERIOD >= 2015 & 
           TIME_PERIOD <= 2022) |>
  select(TIME_PERIOD, OBS_VALUE) |>
  rename(
    Year = TIME_PERIOD,
    beds_per_1000 = OBS_VALUE
  ) |>
  arrange(Year)

# 4. Load and process the A&E data
a_and_e_data <- read.csv("./data/01-raw_data/a_and_e_activity_data_full.csv") |>
  filter(Year >= 2015 & Year <= 2022) |>
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

# 5. Merge all predictors with wait times and population
final_data <- physician_data |>
  left_join(beds_data, by = "Year") |>
  left_join(a_and_e_data, by = "Year") |>
  left_join(wide_yearly_averages_percent_change, by = "Year") |>
  select(
    Year, 
    beds_per_1000, 
    physicians_per_1000, 
    attendance,
    Cardiology,
    Cardiothoracic,
    Dermatology,
    ENT,
    Gastroenterology,
    General_S,
    General_M,
    Geriatric,
    Gynaecology,
    Neurology,
    Neurosurgery,
    Ophthalmology,
    Oral,
    Plastic,
    Rheumatology,
    Thoracic, 
    Trauma,
    Urology,
    Total
  ) 
  )


#### Save final dataset ####
write_parquet(final_data, "./data/02-analysis_data/final_healthcare_data.parquet")
write.csv(final_data, "./data/02-analysis_data/final_healthcare_data.csv")

