# ========================================
# Extract South Africa SPEI Data
# ========================================

install.packages("ncdf4")
library(ncdf4)


# ========================================
# Define South Africa boundaries
# ========================================

# South Africa approximate boundaries
lon_min <- 16.5  # Western boundary
lon_max <- 32.9  # Eastern boundary
lat_min <- -34.8 # Southern boundary
lat_max <- -22.1 # Northern boundary

# ========================================
# Function to extract SA data from a SPEI file
# ========================================

extract_sa_spei <- function(spei_file, time_scale) {
  
  cat(paste("\nProcessing", time_scale, "month SPEI...\n"))
  
  # Open the netCDF file
  nc <- nc_open(spei_file)
  
  # Get coordinates
  lon <- ncvar_get(nc, "lon")
  lat <- ncvar_get(nc, "lat")
  time <- ncvar_get(nc, "time")
  
  # Convert time to dates (assuming origin is 1900-01-01)
  dates <- as.Date(time, origin = "1900-01-01")
  
  # Find indices for South Africa
  lon_idx <- which(lon >= lon_min & lon <= lon_max)
  lat_idx <- which(lat >= lat_min & lat <= lat_max)
  
  cat(paste("  Longitude range:", min(lon[lon_idx]), "to", max(lon[lon_idx]), "\n"))
  cat(paste("  Latitude range:", min(lat[lat_idx]), "to", max(lat[lat_idx]), "\n"))
  cat(paste("  Grid cells:", length(lon_idx), "x", length(lat_idx), "=", 
            length(lon_idx) * length(lat_idx), "cells\n"))
  cat(paste("  Time period:", min(dates), "to", max(dates), "\n"))
  
  # Extract SPEI data for South Africa
  spei_data <- ncvar_get(nc, "spei",
                         start = c(min(lon_idx), min(lat_idx), 1),
                         count = c(length(lon_idx), length(lat_idx), -1))
  
  # Close the file
  nc_close(nc)
  
  # Create a data frame with all the data
  # This creates one row per grid cell per time point
  df_list <- list()
  
  for (i in seq_along(lon_idx)) {
    for (j in seq_along(lat_idx)) {
      lon_val <- lon[lon_idx[i]]
      lat_val <- lat[lat_idx[j]]
      spei_vals <- spei_data[i, j, ]
      
      df_list[[length(df_list) + 1]] <- data.frame(
        longitude = lon_val,
        latitude = lat_val,
        date = dates,
        spei = spei_vals,
        time_scale = time_scale
      )
    }
  }
  
  # Combine all data frames
  df <- do.call(rbind, df_list)
  
  cat(paste("  Created data frame with", nrow(df), "rows\n"))
  
  return(df)
}

# ========================================
# Extract data from all SPEI files
# ========================================

cat("Starting extraction of South Africa SPEI data...\n")

# Process each time scale
spei_01 <- extract_sa_spei("./outputNcdf/spei01.nc", 1)
spei_03 <- extract_sa_spei("./outputNcdf/spei03.nc", 3)
spei_06 <- extract_sa_spei("./outputNcdf/spei06.nc", 6)
spei_12 <- extract_sa_spei("./outputNcdf/spei12.nc", 12)

# ========================================
# Save to CSV files
# ========================================

cat("\nSaving data to CSV files...\n")

# Create output directory if it doesn't exist
if (!dir.exists("./south_africa_data")) {
  dir.create("./south_africa_data")
}

# Save each time scale separately
write.csv(spei_01, "./south_africa_data/south_africa_spei_01month.csv", row.names = FALSE)
write.csv(spei_03, "./south_africa_data/south_africa_spei_03month.csv", row.names = FALSE)
write.csv(spei_06, "./south_africa_data/south_africa_spei_06month.csv", row.names = FALSE)
write.csv(spei_12, "./south_africa_data/south_africa_spei_12month.csv", row.names = FALSE)

cat("\nCSV files saved to ./south_africa_data/\n")

# ========================================
# Quick summary statistics
# ========================================

cat("\n========================================\n")
cat("Summary Statistics\n")
cat("========================================\n")

for (ts in c(1, 3, 6, 12)) {
  df <- get(paste0("spei_", formatC(ts, width = 2, flag = "0")))
  cat(paste("\n", ts, "-month SPEI:\n", sep = ""))
  cat(paste("  Total observations:", nrow(df), "\n"))
  cat(paste("  Number of grid cells:", length(unique(paste(df$longitude, df$latitude))), "\n"))
  cat(paste("  Date range:", min(df$date), "to", max(df$date), "\n"))
  cat(paste("  SPEI range:", round(min(df$spei, na.rm = TRUE), 2), "to", 
            round(max(df$spei, na.rm = TRUE), 2), "\n"))
  cat(paste("  Mean SPEI:", round(mean(df$spei, na.rm = TRUE), 2), "\n"))
  cat(paste("  Missing values:", sum(is.na(df$spei)), "\n"))
}

cat("\n========================================\n")
cat("Extraction complete!\n")
cat("========================================\n")

# Preview the 12-month SPEI data
cat("\nPreview of 12-month SPEI data (first 10 rows):\n")
print(head(spei_12, 10))

# Check what objects were created
ls()

# Check if the CSV files were created
list.files("./south_africa_data/")
