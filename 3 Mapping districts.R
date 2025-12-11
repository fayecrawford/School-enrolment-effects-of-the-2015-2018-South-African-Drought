# ============================================================================
# SPATIAL JOIN PROCEDURE 
# ============================================================================

# Install packages, load libraries 
install.packages("sf")

library(sf)
library(dplyr)
library(ggplot2)

# Set path to geodatabase
gdb_path <- "/Users/fayecrawford/Documents/Capstone!/MDBDistrictMunicipalBoundary2011.gdb" # noting 2011 boundaries
districts <- st_read(gdb_path, layer = "MDBDistrictMunicipalBoundary2011")

# Examine the structure
# print(districts)
# names(districts)  # See column names
# head(districts)   # Look at first few rows

# Visualize the districts
plot(st_geometry(districts), main = "South African District Municipalities 2011")

# Load SPEI data 
spei_1month <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/south_africa_data/south_africa_spei_01month.csv")
spei_3month <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/south_africa_data/south_africa_spei_03month.csv")
spei_6month <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/south_africa_data/south_africa_spei_06month.csv")
spei_12month <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/south_africa_data/south_africa_spei_12month.csv")

# Checking for NAs -> around 15% 
head(spei_12month)
unique(spei_12month$spei)
sum(is.na(spei_12month$spei))
mean(is.na(spei_12month$spei))

# Convert date columns to Date format 
spei_1month$date <- as.Date(spei_1month$date)
spei_3month$date <- as.Date(spei_3month$date)
spei_6month$date <- as.Date(spei_6month$date)
spei_12month$date <- as.Date(spei_12month$date)

# Function to convert SPEI data to spatial points and join with districts
map_spei_to_districts <- function(spei_data, spei_column = "spei") {
  
  # Convert to sf object
  spei_sf <- st_as_sf(spei_data, 
                      coords = c("longitude", "latitude"), 
                      crs = 4326)  # WGS84
  
  # Spatial join: assign each SPEI point to a district
  spei_with_districts <- st_join(spei_sf, districts)
  
  return(spei_with_districts)
}

# Apply to each SPEI dataset
spei_1m_districts <- map_spei_to_districts(spei_1month)
spei_3m_districts <- map_spei_to_districts(spei_3month)
spei_6m_districts <- map_spei_to_districts(spei_6month)
spei_12m_districts <- map_spei_to_districts(spei_12month)

# Convert sf objects back to regular data frames with coordinates
# Extract coordinates and combine with data
spei_1m_df <- spei_1m_districts %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2],
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  st_drop_geometry()

spei_3m_df <- spei_3m_districts %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2],
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  st_drop_geometry()

spei_6m_df <- spei_6m_districts %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2],
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  st_drop_geometry()

spei_12m_df <- spei_12m_districts %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude = st_coordinates(.)[,2],
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  st_drop_geometry()

# Save as CSV files
write.csv(spei_1m_df, 
          "/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei01_districts.csv",
          row.names = FALSE)

write.csv(spei_3m_df, 
          "/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei03_districts.csv",
          row.names = FALSE)

write.csv(spei_6m_df, 
          "/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei06_districts.csv",
          row.names = FALSE)

write.csv(spei_12m_df, 
          "/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei12_districts.csv",
          row.names = FALSE)

# Verify the save worked
cat("Files saved successfully!\n")
cat("Number of rows in each file:\n")
cat("1-month:", nrow(spei_1m_df), "\n")
cat("3-month:", nrow(spei_3m_df), "\n")
cat("6-month:", nrow(spei_6m_df), "\n")
cat("12-month:", nrow(spei_12m_df), "\n")

# Check that year and month were added correctly
cat("\nVerifying year and month extraction:\n")
cat("Year range:", min(spei_12m_df$year), "-", max(spei_12m_df$year), "\n")
cat("Months present:", paste(sort(unique(spei_12m_df$month)), collapse = ", "), "\n")
cat("Sample rows:\n")
print(head(spei_12m_df %>% select(date, year, month, spei, DistrictMunicipalityName)))
