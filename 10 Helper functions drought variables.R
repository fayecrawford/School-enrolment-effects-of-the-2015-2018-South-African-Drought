# ==============================================================================
# Helper Functions for Creating Analysis Variables
# ==============================================================================
# 
# This script contains functions to create drought indicators, lagged variables,
# and cumulative measures by merging enrollment panel with monthly SPEI data.
#
# Usage:
#   source("Helper functions drought variables.R")
#   
#   # Create analysis dataset
#   data_main <- create_analysis_vars(enrollment, spei_monthly)
#
# ==============================================================================

library(tidyverse)
library(zoo)  # For rolling sums

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
        # Contemporaneous drought indicators
        drought = as.integer(spei < drought_threshold & spei >= extreme_threshold),
        drought_extreme = as.integer(spei < extreme_threshold),
        
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
        drought_extreme_lag5 = as.integer(lag5 < extreme_threshold)
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
    
    # Create count variables based on lagged drought indicators
    
    # Count of drought years in past cumul_window years
    drought_lag_cols <- grep("^drought_lag", names(data), value = TRUE)
    data[[paste0("drought_count", cumul_window)]] <- 
      rowSums(data[, drought_lag_cols[1:cumul_window]], na.rm = FALSE)
    
    # Count of extreme drought years
    extreme_lag_cols <- grep("^drought_extreme_lag", names(data), value = TRUE)
    data[[paste0("drought_extreme_count", cumul_window)]] <- 
      rowSums(data[, extreme_lag_cols[1:cumul_window]], na.rm = FALSE)
    
  }
  
  data <- data %>%
    mutate(
      drought_consecutive = case_when(
        is.na(drought_lag1) ~ NA_integer_,
        drought_lag1 == 0 ~ 0L,
        drought_lag1 == 1 & (is.na(drought_lag2) | drought_lag2 == 0) ~ 1L,
        drought_lag1 == 1 & drought_lag2 == 1 & (is.na(drought_lag3) | drought_lag3 == 0) ~ 2L,
        drought_lag1 == 1 & drought_lag2 == 1 & drought_lag3 == 1 & 
          (is.na(drought_lag4) | drought_lag4 == 0) ~ 3L,
        drought_lag1 == 1 & drought_lag2 == 1 & drought_lag3 == 1 & drought_lag4 == 1 &
          (is.na(drought_lag5) | drought_lag5 == 0) ~ 4L,
        drought_lag1 == 1 & drought_lag2 == 1 & drought_lag3 == 1 & 
          drought_lag4 == 1 & drought_lag5 == 1 ~ 5L,
        TRUE ~ NA_integer_
      )
    )
  
  # ------------------------------------------------------------------------------
  # Step 6: Summary statistics
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
# Example Usage (commented out)
# ------------------------------------------------------------------------------

# # Load base data (no drought variables)
# enrollment <- readRDS("enrollment_panel.rds")
# spei_monthly <- readRDS("spei_district_monthly.rds")
# 
# # Main specification: January timing, SPEI < -1, 5-year window
# data_main <- create_main_spec(enrollment, spei_monthly)
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
# 
# # Run regressions
# library(fixest)
# 
# # Basic model with contemporaneous drought
# m1 <- feols(
#   enrolled ~ drought | district + year,
#   data = data_main,
#   cluster = ~district
# )
# 
# # Model with lags
# m2 <- feols(
#   enrolled ~ drought + drought_lag1 + drought_lag2 | district + year,
#   data = data_main,
#   cluster = ~district
# )
# 
# # Full model with cumulative effects and interaction (created in formula)
# m3 <- feols(
#   enrolled ~ drought + drought_lag1 + spei_cumul5 + drought:spei_cumul5 | 
#              district + year + province^year,
#   data = data_main,
#   cluster = ~district
# )
# 
# etable(m1, m2, m3)