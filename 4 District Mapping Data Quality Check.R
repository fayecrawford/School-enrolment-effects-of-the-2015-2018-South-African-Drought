library(sf)
library(dplyr)
library(ggplot2)

# ============================================================
#                  LOAD DATA (ONCE)
# ============================================================

# Read district boundaries
gdb_path <- "/Users/fayecrawford/Documents/Capstone!/MDBDistrictMunicipalBoundary2011.gdb"
districts <- st_read(gdb_path, layer = "MDBDistrictMunicipalBoundary2011")

# Read SPEI data with district assignments
spei12_districts <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei12_districts.csv")

# Read original SPEI data (for comparison)
spei_12month_original <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/south_africa_data/south_africa_spei_12month.csv")
spei_12month_original$date <- as.Date(spei_12month_original$date)

# ============================================================
#      DATA QUALITY ASSESSMENT: UNMATCHED OBSERVATIONS  
# ============================================================

# 1. Check basic stats
cat(sprintf("Total SPEI observations in matched file: %s\n", 
            format(nrow(spei12_districts), big.mark = ",")))
cat(sprintf("Total SPEI observations in original file (2006-2018): %s\n", 
            format(nrow(spei_12month_original), big.mark = ",")))
cat(sprintf("Difference: %s observations\n\n", 
            format(nrow(spei_12month_original) - nrow(spei12_districts), big.mark = ",")))

cat(sprintf("Observations with NA district: %s (%.1f%%)\n", 
            format(sum(is.na(spei12_districts$DistrictMunicipalityCode)), big.mark = ","),
            sum(is.na(spei12_districts$DistrictMunicipalityCode))/nrow(spei12_districts)*100))
cat(sprintf("Observations with valid district: %s (%.1f%%)\n\n", 
            format(sum(!is.na(spei12_districts$DistrictMunicipalityCode)), big.mark = ","),
            sum(!is.na(spei12_districts$DistrictMunicipalityCode))/nrow(spei12_districts)*100))

# 2. Check unique coordinate pairs
unique_coords_original <- spei_12month_original %>%
  distinct(longitude, latitude)
cat(sprintf("Unique SPEI grid cells (original): %d\n", nrow(unique_coords_original)))

unique_coords_matched <- spei12_districts %>%
  filter(!is.na(DistrictMunicipalityCode)) %>%
  distinct(longitude, latitude)
cat(sprintf("Unique SPEI grid cells matched to districts: %d\n", nrow(unique_coords_matched)))

unique_coords_unmatched <- spei12_districts %>%
  filter(is.na(DistrictMunicipalityCode)) %>%
  distinct(longitude, latitude)
cat(sprintf("Unique SPEI grid cells NOT matched: %d\n", nrow(unique_coords_unmatched)))
cat(sprintf("Percentage of unique cells matched: %.1f%%\n\n", 
            nrow(unique_coords_matched) / nrow(unique_coords_original) * 100))

# 3. Compare coordinate ranges
cat("ORIGINAL SPEI DATA:\n")
cat(sprintf("  Longitude: %.2f to %.2f\n", 
            min(spei_12month_original$longitude, na.rm=TRUE), 
            max(spei_12month_original$longitude, na.rm=TRUE)))
cat(sprintf("  Latitude: %.2f to %.2f\n\n", 
            min(spei_12month_original$latitude, na.rm=TRUE), 
            max(spei_12month_original$latitude, na.rm=TRUE)))

cat("DISTRICT BOUNDARIES:\n")
bbox <- st_bbox(districts)
cat(sprintf("  Longitude: %.2f to %.2f\n", bbox["xmin"], bbox["xmax"]))
cat(sprintf("  Latitude: %.2f to %.2f\n\n", bbox["ymin"], bbox["ymax"]))

matched <- spei12_districts %>% filter(!is.na(DistrictMunicipalityCode))
cat("MATCHED SPEI POINTS:\n")
cat(sprintf("  Longitude: %.2f to %.2f\n", 
            min(matched$longitude, na.rm=TRUE), 
            max(matched$longitude, na.rm=TRUE)))
cat(sprintf("  Latitude: %.2f to %.2f\n\n", 
            min(matched$latitude, na.rm=TRUE), 
            max(matched$latitude, na.rm=TRUE)))

unmatched <- spei12_districts %>% filter(is.na(DistrictMunicipalityCode))
cat("UNMATCHED SPEI POINTS:\n")
cat(sprintf("  Longitude: %.2f to %.2f\n", 
            min(unmatched$longitude, na.rm=TRUE), 
            max(unmatched$longitude, na.rm=TRUE)))
cat(sprintf("  Latitude: %.2f to %.2f\n\n", 
            min(unmatched$latitude, na.rm=TRUE), 
            max(unmatched$latitude, na.rm=TRUE)))

# 4. Create diagnostic plots

# Get unique coordinates for plotting
spei_coords <- spei12_districts %>%
  distinct(longitude, latitude, .keep_all = TRUE) %>%
  mutate(matched = !is.na(DistrictMunicipalityCode))

# Plot: Overview with districts and SPEI points
p_diagnostic <- ggplot() +
  geom_sf(data = districts, fill = NA, color = "black", size = 0.3) +
  geom_point(data = spei_coords, 
             aes(x = longitude, y = latitude, color = matched), 
             alpha = 0.6, size = 1.5) +
  scale_color_manual(values = c("TRUE" = "blue", "FALSE" = "red"),
                     labels = c("TRUE" = "Matched", "FALSE" = "Unmatched"),
                     name = "Match Status") +
  theme_minimal() +
  labs(title = "SPEI Grid Points: Matched vs Unmatched to Districts",
       subtitle = sprintf("Red = unmatched (%d cells), Blue = matched (%d cells)", 
                          nrow(unique_coords_unmatched), nrow(unique_coords_matched)),
       x = "Longitude", y = "Latitude")
print(p_diagnostic)

# ============================================================
#                   CHECK FOR MISSING DISTRICTS
# ============================================================

all_districts <- districts$DistrictMunicipalityCode
districts_in_data <- unique(matched$DistrictMunicipalityCode)
missing_districts <- setdiff(all_districts, districts_in_data)

if(length(missing_districts) > 0) {
  cat(sprintf("WARNING: %d district(s) have NO SPEI data:\n", length(missing_districts)))
  missing_info <- districts %>%
    filter(DistrictMunicipalityCode %in% missing_districts) %>%
    st_drop_geometry() %>%
    select(DistrictMunicipalityCode, DistrictMunicipalityName, ProvinceName)
  print(missing_info)
  cat("\nReason: These districts are too small for the 0.5-degree SPEI grid.\n")
  cat("No grid cell centers fall within their boundaries.\n")
} else {
  cat("✓ All 52 districts have SPEI data\n")
}

# ============================================================
#        FILTER TO MATCHED DATA FOR ANALYSIS
# ============================================================

spei12_districts <- spei12_districts %>% 
  filter(!is.na(DistrictMunicipalityCode))
cat(sprintf("Final dataset: %s observations across %d districts\n", 
            format(nrow(spei12_districts), big.mark = ","),
            n_distinct(spei12_districts$DistrictMunicipalityCode)))

# ============================================================
#              FINAL DATASET CHARACTERISTICS 
# ============================================================

cat("1. OVERALL DATASET SIZE\n")
cat(sprintf("Total observations: %s\n", 
            format(nrow(spei12_districts), big.mark = ",")))
cat(sprintf("Number of districts: %d\n", 
            n_distinct(spei12_districts$DistrictMunicipalityCode)))
cat(sprintf("Time period: %s to %s\n", 
            min(spei12_districts$date), max(spei12_districts$date)))
cat(sprintf("Number of time points: %d months\n", 
            n_distinct(spei12_districts$date)))

# Grid cells per district
cells_per_district <- spei12_districts %>%
  group_by(DistrictMunicipalityCode, DistrictMunicipalityName) %>%
  summarise(n_cells = n_distinct(longitude, latitude), .groups = "drop") %>%
  arrange(n_cells)

cat(sprintf("Minimum cells per district: %d\n", min(cells_per_district$n_cells)))
cat(sprintf("Maximum cells per district: %d\n", max(cells_per_district$n_cells)))
cat(sprintf("Median cells per district: %d\n", median(cells_per_district$n_cells)))
cat(sprintf("Mean cells per district: %.1f\n", mean(cells_per_district$n_cells)))

# Show extremes
min_cells <- min(cells_per_district$n_cells)
cat(sprintf("\nDistricts with %d cell(s):\n", min_cells))
print(cells_per_district %>% filter(n_cells == min_cells), n = Inf)

max_cells <- max(cells_per_district$n_cells)
cat(sprintf("\nDistricts with %d cells (maximum):\n", max_cells))
print(cells_per_district %>% filter(n_cells == max_cells), n = Inf)

cat("\nTop 10 districts by grid cell count:\n")
print(tail(cells_per_district, 10), n = 10)

# Quantiles
cat("\nDistribution of grid cells per district:\n")
quantiles <- quantile(cells_per_district$n_cells, probs = c(0.25, 0.5, 0.75))
cat(sprintf("25th percentile: %g cells\n", quantiles[1]))
cat(sprintf("50th percentile (median): %g cells\n", quantiles[2]))
cat(sprintf("75th percentile: %g cells\n", quantiles[3]))

# Observations per district
cat("\n\n3. OBSERVATIONS PER DISTRICT\n")
cat("----------------------------\n")

obs_per_district <- spei12_districts %>%
  group_by(DistrictMunicipalityCode, DistrictMunicipalityName) %>%
  summarise(
    n_obs = n(),
    n_cells = n_distinct(longitude, latitude),
    n_timepoints = n_distinct(date),
    .groups = "drop"
  ) %>%
  arrange(desc(n_obs))

cat(sprintf("Minimum: %s observations\n", format(min(obs_per_district$n_obs), big.mark = ",")))
cat(sprintf("Maximum: %s observations\n", format(max(obs_per_district$n_obs), big.mark = ",")))
cat(sprintf("Median: %s observations\n", format(median(obs_per_district$n_obs), big.mark = ",")))
cat(sprintf("Mean: %s observations\n", format(round(mean(obs_per_district$n_obs)), big.mark = ",")))

# Temporal coverage
temporal_coverage <- spei12_districts %>%
  group_by(DistrictMunicipalityCode) %>%
  summarise(n_timepoints = n_distinct(date), .groups = "drop")

expected_timepoints <- n_distinct(spei12_districts$date)
cat(sprintf("Expected time points: %d months\n", expected_timepoints))
cat(sprintf("All districts have complete coverage: %s\n", 
            ifelse(all(temporal_coverage$n_timepoints == expected_timepoints), "YES", "NO")))

# SPEI distribution
spei_stats <- spei12_districts %>%
  summarise(
    mean_spei = mean(spei, na.rm = TRUE),
    median_spei = median(spei, na.rm = TRUE),
    sd_spei = sd(spei, na.rm = TRUE),
    min_spei = min(spei, na.rm = TRUE),
    max_spei = max(spei, na.rm = TRUE),
    n_missing = sum(is.na(spei)),
    pct_missing = sum(is.na(spei)) / n() * 100
  )

cat(sprintf("Mean: %.3f\n", spei_stats$mean_spei))
cat(sprintf("Median: %.3f\n", spei_stats$median_spei))
cat(sprintf("Std Dev: %.3f\n", spei_stats$sd_spei))
cat(sprintf("Range: %.3f to %.3f\n", spei_stats$min_spei, spei_stats$max_spei))
cat(sprintf("Missing: %s (%.2f%%)\n", 
            format(spei_stats$n_missing, big.mark = ","), spei_stats$pct_missing))

# ============================================================
#                      VISUALIZATIONS
# ============================================================

# Plot 1: Distribution of cells per district
p1 <- ggplot(cells_per_district, aes(x = n_cells)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Grid Cells per District",
       x = "Number of SPEI Grid Cells", y = "Number of Districts")
print(p1)
