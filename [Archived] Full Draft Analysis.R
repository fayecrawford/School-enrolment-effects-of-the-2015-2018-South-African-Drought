# Libraries 
library(tidyverse)
library(fixest) # for fixed effects regression 
library(modelsummary) # for regression tables
library(kableExtra)


# Load data and functions
spei <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei12_districts.csv")
enrollment <- readRDS("enrollment_panel.rds")
source("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/10 Helper functions drought variables.R")

# Step 1: Pre-process spei data - aggregate grid cells to districts (unweighted)
spei_monthly <- spei %>%
  # Drop rows with missing district information
  filter(!is.na(DistrictMunicipalityCode), 
         !is.na(DistrictMunicipalityName)) %>%
  # Then aggregate
  group_by(DistrictMunicipalityCode, DistrictMunicipalityName,
           ProvinceCode, year, month) %>%
  summarise(
    spei = mean(spei, na.rm = TRUE),
    n_grid_cells = n(),
    .groups = "drop"
  ) %>%
  rename(
    district = DistrictMunicipalityCode,
    district_name = DistrictMunicipalityName,
    province = ProvinceCode
  )

# Quick verification checks
# Check 1: One row per district-month-year?
rows_per_combo <- spei_monthly %>%
  group_by(district, year, month) %>%
  summarise(n_rows = n(), .groups = "drop") %>%
  pull(n_rows) %>%
  table()
rows_per_combo

# Check 2: No missing districts?
n_missing_district <- sum(is.na(spei_monthly$district))
n_missing_name <- sum(is.na(spei_monthly$district_name))
cat("Missing district codes:", n_missing_district, "\n")
cat("Missing district names:", n_missing_name, "\n")

# Step 2: Create main specification dataset
data_main <- create_main_spec(enrollment, spei_monthly)

# Keep the 2-letter code as 'province', drop the number coding 
# from NIDS 
data_main <- data_main %>%
  mutate(province = province.y) %>%
  select(-province.x, -province.y)

# Verify it worked
cat("Provinces:", unique(data_main$province), "\n")
cat("Number of provinces:", n_distinct(data_main$province), "\n")

# Verify dataset 
# Check 1: Dimensions and Merge Success
cat("Original enrollment obs:", nrow(enrollment), "\n")
cat("After merging:", nrow(data_main), "\n")
cat("Obs with SPEI data:", sum(!is.na(data_main$spei)), 
    sprintf("(%.1f%%)\n", 100 * mean(!is.na(data_main$spei))))

if (nrow(data_main) == nrow(enrollment)) {
  cat("✓ PASS: No rows lost in merge\n\n")
} else {
  cat("✗ WARNING: Row count changed!\n\n")
}

# Check 2: Core Variables Exist
required_vars <- c(
  # Original enrollment vars
  "pid", "year", "district", "enrolled",
  # SPEI variables
  "spei", "lag1", "lag2", "lag3", "lag4", "lag5",
  # Drought indicators
  "drought", "drought_extreme",
  "drought_lag1", "drought_lag2", "drought_lag3", "drought_lag4", "drought_lag5",
  # Cumulative
  "spei_cumul",
  # Counts
  "drought_count5", "drought_extreme_count5",
  # Consecutive
  "drought_consecutive"
)

missing_vars <- setdiff(required_vars, names(data_main))
if (length(missing_vars) == 0) {
  cat("✓ PASS: All required variables present\n\n")
} else {
  cat("✗ FAIL: Missing variables:", paste(missing_vars, collapse = ", "), "\n\n")
}

# Check 3: Lagged Variables Work Correctly

# Check what districts have non-missing SPEI data
districts_with_data <- data_main %>%
  filter(!is.na(spei)) %>%
  pull(district) %>%
  unique()

# Then explicitly pick one
sample_check <- data_main %>%
  filter(district == districts_with_data[1]) %>%
  arrange(year) %>%
  select(district, year, spei, lag1, lag2, lag3) %>%
  head(10)

# Check: lag1 in year t should equal spei in year t-1 (same month)
lag_check <- data_main %>%
  filter(!is.na(spei), !is.na(lag1), year != 2007) %>%
  # Take one row per district-year for the check (or whatever uniquely identifies time periods)
  distinct(district, year, .keep_all = TRUE) %>%
  group_by(district) %>%
  arrange(year) %>%
  mutate(
    spei_prev = lag(spei, 1),
    lag_matches = abs(lag1 - spei_prev) < 0.001
  ) %>%
  ungroup()

pct_correct <- mean(lag_check$lag_matches, na.rm = TRUE)
cat("\nLag verification (excluding 2007):", sprintf("%.1f%%", 100 * pct_correct), "of lags match correctly\n")

# Check 4: Cumulative SPEI

# Manually calculate cumulative for a sample
cumul_check <- data_main %>%
  filter(district == first(district), !is.na(spei_cumul)) %>%
  arrange(year) %>%
  mutate(
    manual_cumul = lag1 + lag2 + lag3 + lag4 + lag5,
    diff = abs(spei_cumul - manual_cumul)
  ) %>%
  select(year, spei, lag1, lag2, lag3, lag4, lag5, spei_cumul, manual_cumul, diff) %>%
  head(10)

cat("Sample cumulative SPEI calculation:\n")
print(cumul_check)

max_diff <- max(cumul_check$diff, na.rm = TRUE)
if (max_diff < 0.001) {
  cat("✓ PASS: Cumulative SPEI calculated correctly\n\n")
} else {
  cat("✗ WARNING: Cumulative SPEI doesn't match manual calculation!\n\n")
}

# Check 5: Drought Indicators

# Check that drought = 1 when -1.5 <= spei < -1 (moderate drought)
# and drought_extreme = 1 when spei < -1.5
drought_check <- data_main %>%
  filter(!is.na(spei)) %>%
  mutate(
    should_be_drought = as.integer(spei < -1 & spei >= -1.5),
    should_be_extreme = as.integer(spei < -1.5),
    drought_correct = (drought == should_be_drought),
    extreme_correct = (drought_extreme == should_be_extreme)
  )

pct_correct_drought <- mean(drought_check$drought_correct, na.rm = TRUE)
pct_correct_extreme <- mean(drought_check$extreme_correct, na.rm = TRUE)

cat("Moderate drought indicator accuracy:", sprintf("%.1f%%\n", 100 * pct_correct_drought))
cat("Extreme drought indicator accuracy:", sprintf("%.1f%%\n", 100 * pct_correct_extreme))

# Check lagged drought indicators (moderate and extreme)
drought_lag_check <- data_main %>%
  filter(!is.na(lag1)) %>%
  mutate(
    should_be_drought_lag1 = as.integer(lag1 < -1 & lag1 >= -1.5),
    should_be_extreme_lag1 = as.integer(lag1 < -1.5),
    drought_correct = (drought_lag1 == should_be_drought_lag1),
    extreme_correct = (drought_extreme_lag1 == should_be_extreme_lag1)
  )

pct_correct_lag <- mean(drought_lag_check$drought_correct, na.rm = TRUE)
pct_correct_extreme_lag <- mean(drought_lag_check$extreme_correct, na.rm = TRUE)

cat("Lagged moderate drought indicator accuracy:", sprintf("%.1f%%\n", 100 * pct_correct_lag))
cat("Lagged extreme drought indicator accuracy:", sprintf("%.1f%%\n", 100 * pct_correct_extreme_lag))

# Check 6: Drought Counts
count_check <- data_main %>%
  filter(!is.na(drought_count5)) %>%
  mutate(
    manual_count = drought_lag1 + drought_lag2 + drought_lag3 + 
      drought_lag4 + drought_lag5,
    matches = (drought_count5 == manual_count)
  )

pct_match <- mean(count_check$matches, na.rm = TRUE)
cat("Count accuracy:", sprintf("%.1f%%\n", 100 * pct_match))

# Check 8: Missing Data Patterns
missing_summary <- data_main %>%
  summarise(
    pct_missing_spei = 100 * mean(is.na(spei)),
    pct_missing_lag1 = 100 * mean(is.na(lag1)),
    pct_missing_cumul = 100 * mean(is.na(spei_cumul)),
    pct_missing_count = 100 * mean(is.na(drought_count5))
  )

print(missing_summary)
missing_by_year <- data_main %>%
  group_by(year) %>%
  summarise(
    n = n(),
    pct_missing_spei = 100 * mean(is.na(spei)),
    pct_missing_cumul5 = 100 * mean(is.na(spei_cumul))
  )
print(missing_by_year)

# See where the NAs come from
colSums(is.na(data_main[, c("enrolled", "drought", "age", "gender", "race", 
                            "wave", "hhid", "geotype", "questionnaire", 
                            "spei_cumul", "province", "drought_lag1", "drought_lag2")]))

# Clean the data before regression
data_main <- data_main %>%
  filter(district != "", district != "-5", !is.na(district))

# Verify NA district codes are gone
cat("Remaining districts:", n_distinct(data_main$district), "\n")

# Step 3 - Run regressions! Main specification 
# Contemp model with 2-way fixed effects
m1_2w <- feols(enrolled ~ drought + drought_extreme + age + gender + race | 
                 district + year,
            data = data_main, cluster = ~district)

# Lag model w/2-way fixed effects 
m2_2w <- feols(enrolled ~ drought + drought_extreme + age + gender + race + 
                 drought_lag1  + drought_lag2 + drought_extreme_lag1 + 
                 drought_extreme_lag2 | district + year,
               data = data_main, cluster = ~district)

# Cumulative model w/2-way fixed effects 
m3_2w <- feols(enrolled ~ drought + drought_extreme + age + gender + race + 
                 spei_cumul + drought:spei_cumul | 
                 district + year,
               data = data_main, cluster = ~district)

# View results 
etable(m1_2w, m2_2w, m3_2w)

# Export main results 
etable(m1_2w, m2_2w, m3_2w,
       tex = TRUE,
       file = "main spec results table.tex")

# Step 4 - Look for Heterogenous effects 
# Contemp models with heterogenous effects 
m1_race <- feols(enrolled ~ drought + drought_extreme + age + gender + race + 
                   drought:race + drought_extreme:race | district + year,
               data = data_main, cluster = ~district)

m1_gender <- feols(enrolled ~ drought + drought_extreme + age + gender + race + 
                     drought:gender + drought_extreme:gender | district + year,
                 data = data_main, cluster = ~district)

# Lag model heterogeneous 
m2_race <- feols(enrolled ~ drought + drought_extreme + age + gender + race + 
                   drought_lag1 + drought_lag2 + drought_extreme_lag1 + 
                   drought_extreme_lag2 + drought_lag1:race + 
                   drought_lag2:race + drought_extreme_lag1:race + 
                   drought_extreme_lag2:race | district + year,
               data = data_main, cluster = ~district)

m2_gender <- feols(enrolled ~ drought + drought_extreme + age + gender + race + 
                     drought_lag1 + drought_lag2 + drought_extreme_lag1 + 
                     drought_extreme_lag2 + drought_lag1:gender + 
                     drought_lag2:gender + drought_extreme_lag1:gender + 
                     drought_extreme_lag2:gender | district + year,
                 data = data_main, cluster = ~district)

# Cumulative model heterogeneous  
m3_race <- feols(enrolled ~ drought + drought_extreme + age + gender + race + 
                   spei_cumul + spei_cumul:race | 
                 district + year,
               data = data_main, cluster = ~district)

m3_gender <- feols(enrolled ~ drought + age + gender + race + 
                     spei_cumul + spei_cumul:gender | 
                  district + year,
                data = data_main, cluster = ~district)

# View results 
etable(m1_race, m1_gender, m2_race, m2_gender, m3_race, m3_gender)

# Export heterogeneity results 
etable(m1_race, m1_gender,
       tex = TRUE,
       file = "het results m1 table.tex")

etable(m2_race,
       tex = TRUE,
       file = "het results m2 race table.tex")

etable(m2_gender,
       tex = TRUE,
       file = "het results m2 gender table.tex")

etable(m3_race, m3_gender,
       tex = TRUE,
       file = "het results m3 table.tex")

# Step 5 - Robustness models

# Contemp model with 3-way fixed effects
m1_3w <- feols(enrolled ~ drought + drought_extreme + age + gender | 
                 district + year + hhid,
               data = data_main, cluster = ~district)

# Lag model w/3-way fixed effects 
m2_3w <- feols(enrolled ~ drought + drought_extreme + age + gender +  
                 drought_lag1 + drought_lag2 + drought_extreme_lag1 + 
                 drought_extreme_lag2| district + year + hhid,
               data = data_main, cluster = ~district)

# Cumulative model w/3-way fixed effects 
m3_3w <- feols(enrolled ~ drought + drought_extreme + age + gender +  
                 spei_cumul + drought:spei_cumul | 
                 district + year + hhid,
               data = data_main, cluster = ~district)

# View results
etable(m1_3w, m2_3w, m3_3w)

# Export to CSV
etable(m1_3w, m2_3w, m3_3w,
       tex = TRUE,
       file = "3w results table.tex")
