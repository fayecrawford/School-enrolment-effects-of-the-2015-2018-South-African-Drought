# Install packages, load libraries 
install.packages("haven")
install.packages("dplyr")
install.packages("forcats")

library(haven)   # to read .dta (Stata) files
library(dplyr)
library(forcats)

# First, child data! 

# Read in child data, all waves  
child_w1 <- read_dta("Child_W1_Anon_V7.0.0.dta")
child_w2 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 2/Child_W2_Anon_V4.0.0.dta")
child_w3 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 3/Child_W3_Anon_V3.0.0.dta")
child_w4 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 4/Child_W4_Anon_V2.0.0.dta")
child_w5 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 5/Child_W5_Anon_V1.0.0.dta")

# Read in adult data, all waves  
adult_w1 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 1/Adult_W1_Anon_V7.0.0.dta")
adult_w2 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 2/Adult_W2_Anon_V4.0.0.dta")
adult_w3 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 3/Adult_W3_Anon_V3.0.0.dta")
adult_w4 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 4/Adult_W4_Anon_V2.0.0.dta")
adult_w5 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 5/Adult_W5_Anon_V1.0.0.dta")

# Read in household data, all waves 
hh_w1 <- read_dta("hhderived_W1_Anon_V7.0.0.dta")
hh_w2 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 2/hhderived_W2_Anon_V4.0.0.dta")
hh_w3 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 3/hhderived_W3_Anon_V3.0.0.dta")
hh_w4 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 4/hhderived_W4_Anon_V2.0.0.dta")
hh_w5 <- read_dta("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/NIDS data/Wave 5/hhderived_W5_Anon_V1.0.0.dta")

# Join child/household data by household identifier 
child_hh_w1 <- left_join(child_w1, hh_w1, by = "w1_hhid")
child_hh_w2 <- left_join(child_w2, hh_w2, by = "w2_hhid")
child_hh_w3 <- left_join(child_w3, hh_w3, by = "w3_hhid")
child_hh_w4 <- left_join(child_w4, hh_w4, by = "w4_hhid")
child_hh_w5 <- left_join(child_w5, hh_w5, by = "w5_hhid")

# Join adult/household data by household identifier 
adult_hh_w1 <- left_join(adult_w1, hh_w1, by = "w1_hhid")
adult_hh_w2 <- left_join(adult_w2, hh_w2, by = "w2_hhid")
adult_hh_w3 <- left_join(adult_w3, hh_w3, by = "w3_hhid")
adult_hh_w4 <- left_join(adult_w4, hh_w4, by = "w4_hhid")
adult_hh_w5 <- left_join(adult_w5, hh_w5, by = "w5_hhid")


# Create a list of wave numbers and corresponding data frames
waves <- 1:5
wave_dfs_c <- list(child_hh_w1, child_hh_w2, child_hh_w3, child_hh_w4, child_hh_w5)

# Loop through each wave
for (i in waves) {
  cat("\n========== Wave", i, "Checks ==========\n")
  
  # Get the current wave's data frame
  current_df <- wave_dfs_c[[i]]
  
  # Construct variable names for this wave
  dc_var <- paste0("w", i, "_dc2011")
  prov_var <- paste0("w", i, "_prov2011")
  
  # Check 1: NA values in district council variable
  cat("\nNAs in", dc_var, ":\n")
  print(table(is.na(current_df[[dc_var]])))
  
  # Check 2: Number of unique district councils
  cat("\nNumber of unique values in", dc_var, ":", 
      length(unique(current_df[[dc_var]])), "\n")
  
  # Check 3: Province distribution
  cat("\nDistribution of", prov_var, ":\n")
  print(table(current_df[[prov_var]]))
  
  # Write CSV file
  output_file <- paste0("Child_Household_W", i, ".csv")
  write.csv(current_df, output_file, row.names = FALSE)
  cat("\nSaved:", output_file, "\n")
}

# Next, adults 
wave_dfs_a <- list(adult_hh_w1, adult_hh_w2, adult_hh_w3, adult_hh_w4, adult_hh_w5)

# Loop through each wave
for (i in waves) {
  cat("\n========== Adult Wave", i, "Checks ==========\n")
  
  # Get the current wave's data frame
  current_df <- wave_dfs_a[[i]]
  
  # Construct variable names for this wave
  dc_var <- paste0("w", i, "_dc2011")
  prov_var <- paste0("w", i, "_prov2011")
  
  # Check 1: NA values in district council variable
  cat("\nNAs in", dc_var, ":\n")
  print(table(is.na(current_df[[dc_var]])))
  
  # Check 2: Number of unique district councils
  cat("\nNumber of unique values in", dc_var, ":", 
      length(unique(current_df[[dc_var]])), "\n")
  
  # Check 3: Province distribution
  cat("\nDistribution of", prov_var, ":\n")
  print(table(current_df[[prov_var]]))
  
  # Write CSV file
  output_file <- paste0("Adult_Household_W", i, ".csv")
  write.csv(current_df, output_file, row.names = FALSE)
  cat("\nSaved:", output_file, "\n")
}

