# ==============================================================================
# Helper Functions for Creating Analysis Variables
# ==============================================================================
#
# This script contains functions to create drought indicators, lagged variables,
# and cumulative measures by merging enrollment panel with monthly SPEI data.
#
# Usage:
#   source("8 Helper functions drought variables.R")
#
#   # Aggregate raw SPEI grid-cell data to district level
#   spei_monthly <- aggregate_spei_to_districts(spei_raw)
#
#   # Create analysis-ready dataset
#   data_main <- create_main_spec(enrollment, spei_monthly)
#
#   # Optional: verify data integrity
#   verify_analysis_data(data_main, enrollment)
#
# ==============================================================================

library(tidyverse)
library(zoo)  # For rolling sums

# ------------------------------------------------------------------------------
# SPEI Aggregation: Grid Cells to Districts
# ------------------------------------------------------------------------------

aggregate_spei_to_districts <- function(spei_raw) {
  #' Aggregate raw grid-cell SPEI data to district-level monthly means
  #'
  #' @param spei_raw Raw SPEI data with grid-cell observations
  #' @return District-level monthly SPEI data frame

  spei_raw %>%
    filter(!is.na(DistrictMunicipalityCode),
           !is.na(DistrictMunicipalityName)) %>%
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
}

# ------------------------------------------------------------------------------
# Main Function: Create All Analysis Variables
# ------------------------------------------------------------------------------

create_analysis_vars <- function(enrollment_data,
                                 spei_monthly_data,
                                 drought_type = "absolute",
                                 drought_threshold = -1,
                                 extreme_threshold = -1.5,
                                 sd_threshold = NULL,
                                 max_lag = 5,
                                 cumul_window = 5,
                                 create_counts = TRUE) {
  #' Create complete analysis dataset with drought variables
  #' 
  #' @param enrollment_data NIDS enrollment panel (no drought variables)
  #' @param spei_monthly_data Monthly district SPEI data (1901-2018+)
  #' @param drought_type "absolute" (fixed threshold) or "stddev" (SD below mean)
  #' @param drought_threshold SPEI threshold for drought (if absolute)
  #' @param extreme_threshold SPEI threshold for extreme drought (if absolute)
  #' @param sd_threshold Number of SDs below mean for drought (if stddev)
  #' @param max_lag Maximum number of lags to create
  #' @param cumul_window Window for cumulative SPEI (z years)
  #' @param create_counts Whether to create drought count variables
  #' @return Data frame with analysis variables added
  
  # ------------------------------------------------------------------------------
  # Step 1: Prepare January SPEI data
  # ------------------------------------------------------------------------------
  
  spei_to_merge <- spei_monthly_data %>%
    filter(month == 1) %>%
    select(-month)  # Remove month since it's always January
  
  # ------------------------------------------------------------------------------
  # Step 2: Create lagged and cumulative variables in SPEI data
  # ------------------------------------------------------------------------------
  
  # Lag within district (using January of each year)
  spei_prepared <- spei_to_merge %>%
    group_by(district) %>%
    arrange(year) %>%
    mutate(
      # Create individual lags
      across(spei, 
             list(lag1 = ~lag(.x, 1),
                  lag2 = ~lag(.x, 2),
                  lag3 = ~lag(.x, 3),
                  lag4 = ~lag(.x, 4),
                  lag5 = ~lag(.x, 5)),
             .names = "{.fn}"),
      
      # Create cumulative SPEI
      spei_cumul = rollsumr(lag(spei, 1), k = cumul_window, fill = NA, align = "right")
    ) %>%
    ungroup()
  
  # ------------------------------------------------------------------------------
  # Step 3: Merge with enrollment panel
  # ------------------------------------------------------------------------------
  
  # Merge on district and year only (using January SPEI)
  data <- enrollment_data %>%
    left_join(
      spei_prepared,
      by = c("district", "year")
    )
  
  n_matched <- sum(!is.na(data$spei))
  cat("Observations matched to SPEI:", n_matched, 
      sprintf("(%.1f%%)", 100 * n_matched / nrow(data)), "\n")
  
  # ------------------------------------------------------------------------------
  # Step 4: Create drought indicators based on threshold
  # ------------------------------------------------------------------------------
  
  if (drought_type == "absolute") {
    data <- data %>%
      mutate(
        # Contemporaneous drought severity (single categorical, reference = "no drought")
        drought = factor(
          case_when(
            is.na(spei)              ~ NA_character_,
            spei < extreme_threshold ~ "extreme drought",
            spei < drought_threshold ~ "moderate drought",
            TRUE                     ~ "no drought"
          ),
          levels = c("no drought", "moderate drought", "extreme drought")
        ),
        
        # Lagged drought indicators
        drought_lag1 = as.integer(lag1 < drought_threshold & lag1 >= extreme_threshold),
        drought_lag2 = as.integer(lag2 < drought_threshold & lag2 >= extreme_threshold),
        drought_lag3 = as.integer(lag3 < drought_threshold & lag3 >= extreme_threshold),
        drought_lag4 = as.integer(lag4 < drought_threshold & lag4 >= extreme_threshold),
        drought_lag5 = as.integer(lag5 < drought_threshold & lag5 >= extreme_threshold),
        
        # Lagged extreme drought indicators
        drought_extreme_lag1 = as.integer(lag1 < extreme_threshold),
        drought_extreme_lag2 = as.integer(lag2 < extreme_threshold),
        drought_extreme_lag3 = as.integer(lag3 < extreme_threshold),
        drought_extreme_lag4 = as.integer(lag4 < extreme_threshold),
        drought_extreme_lag5 = as.integer(lag5 < extreme_threshold),

        # Lagged any-drought indicators (SPEI < threshold, moderate + extreme combined)
        # Used for Di Falco et al. counts
        drought_any_lag1 = as.integer(lag1 < drought_threshold),
        drought_any_lag2 = as.integer(lag2 < drought_threshold),
        drought_any_lag3 = as.integer(lag3 < drought_threshold),
        drought_any_lag4 = as.integer(lag4 < drought_threshold),
        drought_any_lag5 = as.integer(lag5 < drought_threshold)
      )

  } else if (drought_type == "stddev") {
    # Calculate cutoffs based on sample distribution
    spei_mean <- mean(data$spei, na.rm = TRUE)
    spei_sd <- sd(data$spei, na.rm = TRUE)
    
    drought_cutoff <- spei_mean - sd_threshold * spei_sd
    extreme_cutoff <- spei_mean - (sd_threshold + 0.5) * spei_sd
    
    cat("Calculated cutoffs:\n")
    cat("  Mean SPEI:", round(spei_mean, 3), "\n")
    cat("  SD SPEI:", round(spei_sd, 3), "\n")
    cat("  Drought cutoff (", sd_threshold, "SD):", round(drought_cutoff, 3), "\n")
    cat("  Extreme cutoff (", sd_threshold + 0.5, "SD):", round(extreme_cutoff, 3), "\n")
    
    data <- data %>%
      mutate(
        # Contemporaneous drought indicators
        drought = as.integer(spei < drought_cutoff),
        drought_extreme = as.integer(spei < extreme_cutoff),
        drought_mild = as.integer(spei < drought_cutoff & spei >= extreme_cutoff),
        
        # Lagged drought indicators
        drought_lag1 = as.integer(lag1 < drought_cutoff),
        drought_lag2 = as.integer(lag2 < drought_cutoff),
        drought_lag3 = as.integer(lag3 < drought_cutoff),
        drought_lag4 = as.integer(lag4 < drought_cutoff),
        drought_lag5 = as.integer(lag5 < drought_cutoff),
        
        # Lagged extreme drought indicators
        drought_extreme_lag1 = as.integer(lag1 < extreme_cutoff),
        drought_extreme_lag2 = as.integer(lag2 < extreme_cutoff),
        drought_extreme_lag3 = as.integer(lag3 < extreme_cutoff),
        drought_extreme_lag4 = as.integer(lag4 < extreme_cutoff),
        drought_extreme_lag5 = as.integer(lag5 < extreme_cutoff)
      )
  }
  
  # ------------------------------------------------------------------------------
  # Step 5: Create drought counts (Di Falco et al. approach) & consecutive
  # drought indicator (von Uexkull approach)
  # ------------------------------------------------------------------------------

  if (create_counts) {

    # Di Falco et al.: count of any-drought years (SPEI < threshold) in each
    # rolling window of 1 to cumul_window years preceding the survey year
    data <- data %>%
      mutate(
        drought_any_count1 = drought_any_lag1,
        drought_any_count2 = drought_any_lag1 + drought_any_lag2,
        drought_any_count3 = drought_any_lag1 + drought_any_lag2 + drought_any_lag3,
        drought_any_count4 = drought_any_lag1 + drought_any_lag2 + drought_any_lag3 +
                             drought_any_lag4,
        drought_any_count5 = drought_any_lag1 + drought_any_lag2 + drought_any_lag3 +
                             drought_any_lag4 + drought_any_lag5
      )

    # Retain legacy extreme-drought count for backward compatibility
    extreme_lag_cols <- grep("^drought_extreme_lag", names(data), value = TRUE)
    data[[paste0("drought_extreme_count", cumul_window)]] <-
      rowSums(data[, extreme_lag_cols[1:cumul_window]], na.rm = FALSE)

  }

  # Von Uexkull et al.: number of consecutive preceding years with SPEI < -1
  data <- data %>%
    mutate(
      drought_consecutive = case_when(
        is.na(drought_any_lag1)                                        ~ NA_integer_,
        drought_any_lag1 == 0                                          ~ 0L,
        drought_any_lag1 == 1 &
          (is.na(drought_any_lag2) | drought_any_lag2 == 0)           ~ 1L,
        drought_any_lag1 == 1 & drought_any_lag2 == 1 &
          (is.na(drought_any_lag3) | drought_any_lag3 == 0)           ~ 2L,
        drought_any_lag1 == 1 & drought_any_lag2 == 1 &
          drought_any_lag3 == 1 &
          (is.na(drought_any_lag4) | drought_any_lag4 == 0)           ~ 3L,
        drought_any_lag1 == 1 & drought_any_lag2 == 1 &
          drought_any_lag3 == 1 & drought_any_lag4 == 1 &
          (is.na(drought_any_lag5) | drought_any_lag5 == 0)           ~ 4L,
        drought_any_lag1 == 1 & drought_any_lag2 == 1 &
          drought_any_lag3 == 1 & drought_any_lag4 == 1 &
          drought_any_lag5 == 1                                        ~ 5L,
        TRUE                                                           ~ NA_integer_
      )
    )
  
  # ------------------------------------------------------------------------------
  # Step 6: Resolve province column after merge
  # ------------------------------------------------------------------------------

  # The merge creates province.x (from NIDS) and province.y (from SPEI, 2-letter code)
  # Keep the 2-letter province code from the SPEI data
  if ("province.x" %in% names(data) & "province.y" %in% names(data)) {
    data <- data %>%
      mutate(province = province.y) %>%
      select(-province.x, -province.y)
  }

  # ------------------------------------------------------------------------------
  # Step 7: Recode school_phase
  # ------------------------------------------------------------------------------

  # Impute school phase from age for enrolled with missing phase,
  # and assign phase by age for non-enrolled
  if ("school_phase" %in% names(data) & "age" %in% names(data)) {
    data <- data %>%
      mutate(school_phase = case_when(
        # Enrolled but missing phase: impute from age
        enrolled == 1 & is.na(school_phase) & age <= 5 ~ "Pre-primary",
        enrolled == 1 & is.na(school_phase) & age >= 6 & age <= 12 ~ "Primary",
        enrolled == 1 & is.na(school_phase) & age >= 13 & age <= 18 ~ "Secondary",
        enrolled == 1 & is.na(school_phase) & age >= 19 ~ "Post-secondary",

        # Not enrolled: assign phase by age
        enrolled == 0 & age <= 5 ~ "Pre-primary",
        enrolled == 0 & age >= 6 & age <= 12 ~ "Primary",
        enrolled == 0 & age >= 13 & age <= 18 ~ "Secondary",
        enrolled == 0 & age >= 19 ~ "Post-secondary",

        # Keep existing values otherwise
        TRUE ~ school_phase
      )) %>%
      # Set factor levels with Primary as reference category
      mutate(school_phase = factor(school_phase,
                                   levels = c("Primary", "Pre-primary", "Secondary", "Post-secondary")))
  }

  # ------------------------------------------------------------------------------
  # Step 8: Clean invalid observations
  # ------------------------------------------------------------------------------

  data <- data %>%
    filter(district != "", district != "-5", !is.na(district)) %>%
    filter(age >= 0 | is.na(age)) %>%
    filter(!is.na(race), !is.na(gender))

  # ------------------------------------------------------------------------------
  # Step 9: Summary statistics
  # ------------------------------------------------------------------------------
  
  cat("\n=== Variable Creation Summary ===\n")
  cat("Total observations:", nrow(data), "\n")
  cat("Observations with SPEI data:", sum(!is.na(data$spei)), "\n")
  
  cat("\nCumulative SPEI summary (", cumul_window, " years):\n", sep = "")
  print(summary(data$spei_cumul))  # Use fixed name instead of dynamic
  
  if (create_counts) {
    cat("\nDrought count summary (past", cumul_window, "years):\n")
    print(table(data[[paste0("drought_count", cumul_window)]], useNA = "always"))
  }
  
  cat("\nConsecutive drought years:\n")
  print(table(data$drought_consecutive, useNA = "always"))
  
  return(data)
}

# ------------------------------------------------------------------------------
# Convenience Functions for Common Specifications
# ------------------------------------------------------------------------------

create_main_spec <- function(enrollment_data, 
                             spei_monthly_data) {
  #' Main specification: SPEI < -1, 5-year cumulative window, January timing
  create_analysis_vars(
    enrollment_data = enrollment_data,
    spei_monthly_data = spei_monthly_data,
    drought_type = "absolute",
    drought_threshold = -1,
    extreme_threshold = -1.5,
    max_lag = 5,
    cumul_window = 5,
    create_counts = TRUE
  )
}

create_robustness_spec <- function(enrollment_data,
                                   spei_monthly_data,
                                   threshold = NULL,
                                   window = NULL,
                                   sd_based = FALSE,
                                   sd_threshold = 1) {
  #' Robustness specifications with different thresholds or windows
  
  if (sd_based) {
    # Standard deviation-based definition
    create_analysis_vars(
      enrollment_data = enrollment_data,
      spei_monthly_data = spei_monthly_data,
      drought_type = "stddev",
      sd_threshold = sd_threshold,
      max_lag = 5,
      cumul_window = window %||% 5,
      create_counts = TRUE
    )
  } else {
    # Threshold-based with custom values
    create_analysis_vars(
      enrollment_data = enrollment_data,
      spei_monthly_data = spei_monthly_data,
      drought_type = "absolute",
      drought_threshold = threshold %||% -1,
      extreme_threshold = -1.5,
      max_lag = 5,
      cumul_window = window %||% 5,
      create_counts = TRUE
    )
  }
}

# ------------------------------------------------------------------------------
# Verification Function (Optional)
# ------------------------------------------------------------------------------

verify_analysis_data <- function(data, enrollment_data) {
  #' Run integrity checks on the analysis dataset
  #'
  #' @param data Output of create_analysis_vars() or create_main_spec()
  #' @param enrollment_data Original enrollment panel for row-count comparison
  #' @return Invisible NULL; prints check results

  cat("=== Data Verification ===\n\n")

  # Check 1: Dimensions and Merge Success
  cat("Original enrollment obs:", nrow(enrollment_data), "\n")
  cat("After merging:", nrow(data), "\n")
  cat("Obs with SPEI data:", sum(!is.na(data$spei)),
      sprintf("(%.1f%%)\n", 100 * mean(!is.na(data$spei))))

  if (nrow(data) <= nrow(enrollment_data)) {
    cat("PASS: No rows gained in merge\n\n")
  } else {
    cat("WARNING: Row count increased!\n\n")
  }

  # Check 2: Core Variables Exist
  required_vars <- c(
    "pid", "year", "district", "enrolled", "school_phase", "hh_income",
    "spei", "lag1", "lag2", "lag3", "lag4", "lag5",
    "drought",
    "drought_lag1", "drought_lag2", "drought_lag3", "drought_lag4", "drought_lag5",
    "spei_cumul",
    "drought_count5", "drought_extreme_count5",
    "drought_consecutive"
  )

  missing_vars <- setdiff(required_vars, names(data))
  if (length(missing_vars) == 0) {
    cat("PASS: All required variables present\n\n")
  } else {
    cat("FAIL: Missing variables:", paste(missing_vars, collapse = ", "), "\n\n")
  }

  # Check 3: Lagged Variables Work Correctly
  districts_with_data <- data %>%
    filter(!is.na(spei)) %>%
    pull(district) %>%
    unique()

  lag_check <- data %>%
    filter(!is.na(spei), !is.na(lag1), year != 2007) %>%
    distinct(district, year, .keep_all = TRUE) %>%
    group_by(district) %>%
    arrange(year) %>%
    mutate(
      spei_prev = lag(spei, 1),
      lag_matches = abs(lag1 - spei_prev) < 0.001
    ) %>%
    ungroup()

  pct_correct <- mean(lag_check$lag_matches, na.rm = TRUE)
  cat("Lag verification (excluding 2007):", sprintf("%.1f%%", 100 * pct_correct), "of lags match correctly\n")

  # Check 4: Cumulative SPEI
  cumul_check <- data %>%
    filter(district == first(district), !is.na(spei_cumul)) %>%
    arrange(year) %>%
    mutate(
      manual_cumul = lag1 + lag2 + lag3 + lag4 + lag5,
      diff = abs(spei_cumul - manual_cumul)
    ) %>%
    select(year, spei, lag1, lag2, lag3, lag4, lag5, spei_cumul, manual_cumul, diff) %>%
    head(10)

  cat("\nSample cumulative SPEI calculation:\n")
  print(cumul_check)

  max_diff <- max(cumul_check$diff, na.rm = TRUE)
  if (max_diff < 0.001) {
    cat("PASS: Cumulative SPEI calculated correctly\n\n")
  } else {
    cat("WARNING: Cumulative SPEI doesn't match manual calculation!\n\n")
  }

  # Check 5: Drought Indicators
  drought_check <- data %>%
    filter(!is.na(spei)) %>%
    mutate(
      should_be = case_when(
        spei < -1.5 ~ "extreme drought",
        spei < -1   ~ "moderate drought",
        TRUE        ~ "no drought"
      ),
      drought_correct = (as.character(drought) == should_be)
    )

  cat("Drought category accuracy:", sprintf("%.1f%%\n", 100 * mean(drought_check$drought_correct, na.rm = TRUE)))
  cat("Drought category distribution:\n")
  print(table(drought_check$drought, useNA = "always"))

  # Check 6: Drought Counts
  count_check <- data %>%
    filter(!is.na(drought_count5)) %>%
    mutate(
      manual_count = drought_lag1 + drought_lag2 + drought_lag3 +
        drought_lag4 + drought_lag5,
      matches = (drought_count5 == manual_count)
    )

  cat("Count accuracy:", sprintf("%.1f%%\n", 100 * mean(count_check$matches, na.rm = TRUE)))

  # Check 7: Missing Data Patterns
  cat("\nMissing data summary:\n")
  missing_summary <- data %>%
    summarise(
      pct_missing_spei = 100 * mean(is.na(spei)),
      pct_missing_lag1 = 100 * mean(is.na(lag1)),
      pct_missing_cumul = 100 * mean(is.na(spei_cumul)),
      pct_missing_count = 100 * mean(is.na(drought_count5))
    )
  print(missing_summary)

  missing_by_year <- data %>%
    group_by(year) %>%
    summarise(
      n = n(),
      pct_missing_spei = 100 * mean(is.na(spei)),
      pct_missing_cumul5 = 100 * mean(is.na(spei_cumul))
    )
  print(missing_by_year)

  cat("\nColumn-level NA counts:\n")
  na_cols <- c("enrolled", "drought", "age", "gender", "race", "school_phase", "hh_income",
               "wave", "hhid", "geotype", "questionnaire",
               "spei_cumul", "province", "drought_lag1", "drought_lag2")
  na_cols <- intersect(na_cols, names(data))
  print(colSums(is.na(data[, na_cols])))

  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Example Usage (commented out)
# ------------------------------------------------------------------------------

# # Load raw data
# spei_raw <- read.csv("spei12_districts.csv")
# enrollment <- readRDS("enrollment_panel.rds")
#
# # Aggregate SPEI to districts (one-time step)
# spei_monthly <- aggregate_spei_to_districts(spei_raw)
#
# # Main specification
# data_main <- create_main_spec(enrollment, spei_monthly)
#
# # Optional: verify data integrity
# verify_analysis_data(data_main, enrollment)
#
# # Robustness: Different drought threshold
# data_r1 <- create_robustness_spec(enrollment, spei_monthly, threshold = -0.8)
# data_r2 <- create_robustness_spec(enrollment, spei_monthly, threshold = -1.5)
#
# # Robustness: Different cumulative window
# data_r3 <- create_robustness_spec(enrollment, spei_monthly, window = 3)
# data_r4 <- create_robustness_spec(enrollment, spei_monthly, window = 7)
#
# # Robustness: Standard deviation-based definition
# data_r5 <- create_robustness_spec(enrollment, spei_monthly, sd_based = TRUE, sd_threshold = 1)