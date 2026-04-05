# ============================================================================
# 3 Mapping Districts, Data Quality, and Spatial Variation
# Assigns SPEI grid cells to South African districts (spatial join), performs
# data quality checks on the match, characterises the final dataset, and
# measures within-district spatial variation.
# Only the 12-month SPEI time scale is used.
# (Incorporates former scripts 4 and 5.)
# ============================================================================

library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)

project_dir <- "/Users/fayecrawford/Documents/Capstone!/SPEI_Project"
gdb_path    <- "/Users/fayecrawford/Documents/Capstone!/MDBDistrictMunicipalBoundary2011.gdb"

# ============================================================================
# LOAD DATA
# ============================================================================

districts    <- st_read(gdb_path, layer = "MDBDistrictMunicipalBoundary2011")
spei_12month <- read.csv(file.path(project_dir, "south_africa_data/south_africa_spei_12month.csv"))
spei_12month$date <- as.Date(spei_12month$date)

# ============================================================================
# SPATIAL JOIN: Assign SPEI grid cells to districts
# ============================================================================

map_spei_to_districts <- function(spei_data) {
  spei_sf <- st_as_sf(spei_data, coords = c("longitude", "latitude"), crs = 4326)
  st_join(spei_sf, districts)
}

spei_12m_districts <- map_spei_to_districts(spei_12month)

spei_12m_df <- spei_12m_districts %>%
  mutate(
    longitude = st_coordinates(.)[,1],
    latitude  = st_coordinates(.)[,2],
    year      = year(date),
    month     = month(date)
  ) %>%
  st_drop_geometry()

write.csv(spei_12m_df,
          file.path(project_dir, "spei12_districts.csv"),
          row.names = FALSE)

# ============================================================================
# DATA QUALITY ASSESSMENT: Unmatched observations
# ============================================================================

unique_coords_original  <- spei_12month %>% distinct(longitude, latitude)
unique_coords_matched   <- spei_12m_df %>% filter(!is.na(DistrictMunicipalityCode)) %>% distinct(longitude, latitude)
unique_coords_unmatched <- spei_12m_df %>% filter( is.na(DistrictMunicipalityCode)) %>% distinct(longitude, latitude)

cat(sprintf("Total observations (matched file):  %s\n", format(nrow(spei_12m_df), big.mark = ",")))
cat(sprintf("Total observations (original file): %s\n", format(nrow(spei_12month), big.mark = ",")))
cat(sprintf("NA district: %s (%.1f%%)  |  Valid district: %s (%.1f%%)\n",
            format(sum( is.na(spei_12m_df$DistrictMunicipalityCode)), big.mark = ","),
            sum( is.na(spei_12m_df$DistrictMunicipalityCode)) / nrow(spei_12m_df) * 100,
            format(sum(!is.na(spei_12m_df$DistrictMunicipalityCode)), big.mark = ","),
            sum(!is.na(spei_12m_df$DistrictMunicipalityCode)) / nrow(spei_12m_df) * 100))
cat(sprintf("Grid cells — original: %d  |  matched: %d  |  unmatched: %d  (%.1f%% matched)\n\n",
            nrow(unique_coords_original), nrow(unique_coords_matched), nrow(unique_coords_unmatched),
            nrow(unique_coords_matched) / nrow(unique_coords_original) * 100))

matched   <- spei_12m_df %>% filter(!is.na(DistrictMunicipalityCode))
unmatched <- spei_12m_df %>% filter( is.na(DistrictMunicipalityCode))
bbox      <- st_bbox(districts)

cat(sprintf("Original  — lon: %.2f to %.2f, lat: %.2f to %.2f\n",
            min(spei_12month$longitude), max(spei_12month$longitude),
            min(spei_12month$latitude),  max(spei_12month$latitude)))
cat(sprintf("Districts — lon: %.2f to %.2f, lat: %.2f to %.2f\n",
            bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"]))
cat(sprintf("Matched   — lon: %.2f to %.2f, lat: %.2f to %.2f\n",
            min(matched$longitude),   max(matched$longitude),
            min(matched$latitude),    max(matched$latitude)))
cat(sprintf("Unmatched — lon: %.2f to %.2f, lat: %.2f to %.2f\n",
            min(unmatched$longitude), max(unmatched$longitude),
            min(unmatched$latitude),  max(unmatched$latitude)))

# Diagnostic map: matched vs unmatched grid points
spei_coords <- spei_12m_df %>%
  distinct(longitude, latitude, .keep_all = TRUE) %>%
  mutate(matched = !is.na(DistrictMunicipalityCode))

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

# Check for districts with no SPEI data
missing_districts <- setdiff(districts$DistrictMunicipalityCode,
                             unique(matched$DistrictMunicipalityCode))
if (length(missing_districts) > 0) {
  cat(sprintf("WARNING: %d district(s) have NO SPEI data:\n", length(missing_districts)))
  districts %>%
    filter(DistrictMunicipalityCode %in% missing_districts) %>%
    st_drop_geometry() %>%
    select(DistrictMunicipalityCode, DistrictMunicipalityName, ProvinceName) %>%
    print()
  cat("Reason: these districts are too small for the 0.5-degree SPEI grid.\n")
} else {
  cat("All 52 districts have SPEI data.\n")
}

# ============================================================================
# FILTER TO MATCHED DATA
# ============================================================================

spei12_matched <- spei_12m_df %>%
  filter(!is.na(DistrictMunicipalityCode))

cat(sprintf("\nFinal dataset: %s observations across %d districts\n",
            format(nrow(spei12_matched), big.mark = ","),
            n_distinct(spei12_matched$DistrictMunicipalityCode)))

# ============================================================================
# FINAL DATASET CHARACTERISTICS
# ============================================================================

cat(sprintf("Time period: %s to %s (%d months)\n",
            min(spei12_matched$date), max(spei12_matched$date),
            n_distinct(spei12_matched$date)))

cells_per_district <- spei12_matched %>%
  group_by(DistrictMunicipalityCode, DistrictMunicipalityName) %>%
  summarise(n_cells = n_distinct(longitude, latitude), .groups = "drop") %>%
  arrange(n_cells)

cat(sprintf("Grid cells per district — min: %d, median: %d, mean: %.1f, max: %d\n",
            min(cells_per_district$n_cells),
            as.integer(median(cells_per_district$n_cells)),
            mean(cells_per_district$n_cells),
            max(cells_per_district$n_cells)))

obs_per_district <- spei12_matched %>%
  group_by(DistrictMunicipalityCode, DistrictMunicipalityName) %>%
  summarise(
    n_obs        = n(),
    n_cells      = n_distinct(longitude, latitude),
    n_timepoints = n_distinct(date),
    .groups = "drop"
  ) %>%
  arrange(desc(n_obs))

cat(sprintf("Observations per district — min: %s, median: %s, mean: %s, max: %s\n",
            format(min(obs_per_district$n_obs),            big.mark = ","),
            format(as.integer(median(obs_per_district$n_obs)), big.mark = ","),
            format(round(mean(obs_per_district$n_obs)),    big.mark = ","),
            format(max(obs_per_district$n_obs),            big.mark = ",")))

temporal_coverage <- spei12_matched %>%
  group_by(DistrictMunicipalityCode) %>%
  summarise(n_timepoints = n_distinct(date), .groups = "drop")
cat(sprintf("All districts have complete temporal coverage: %s\n",
            ifelse(all(temporal_coverage$n_timepoints == n_distinct(spei12_matched$date)),
                   "YES", "NO")))

spei_stats <- spei12_matched %>%
  summarise(
    mean_spei   = mean(spei, na.rm = TRUE),
    sd_spei     = sd(spei,   na.rm = TRUE),
    min_spei    = min(spei,  na.rm = TRUE),
    max_spei    = max(spei,  na.rm = TRUE),
    pct_missing = sum(is.na(spei)) / n() * 100
  )
cat(sprintf("SPEI — mean: %.3f, SD: %.3f, range: %.3f to %.3f, missing: %.2f%%\n",
            spei_stats$mean_spei, spei_stats$sd_spei,
            spei_stats$min_spei,  spei_stats$max_spei, spei_stats$pct_missing))

p_cells <- ggplot(cells_per_district, aes(x = n_cells)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Grid Cells per District",
       x = "Number of SPEI Grid Cells", y = "Number of Districts")
print(p_cells)

# ============================================================================
# WITHIN-DISTRICT SPATIAL VARIATION
# ============================================================================

calculate_spatial_variation_by_time <- function(spei_districts, spei_col = "spei") {

  by_time <- spei_districts %>%
    group_by(DistrictMunicipalityCode, DistrictMunicipalityName, ProvinceName, date) %>%
    summarise(
      mean_spei  = mean(.data[[spei_col]], na.rm = TRUE),
      sd_spei    = if_else(n() > 1, sd(.data[[spei_col]], na.rm = TRUE), 0),
      min_spei   = min(.data[[spei_col]], na.rm = TRUE),
      max_spei   = max(.data[[spei_col]], na.rm = TRUE),
      range_spei = max_spei - min_spei,
      n_points   = n(),
      .groups = "drop"
    )

  summary <- by_time %>%
    group_by(DistrictMunicipalityCode, DistrictMunicipalityName, ProvinceName) %>%
    summarise(
      avg_spatial_sd    = mean(sd_spei,    na.rm = TRUE),
      max_spatial_sd    = max(sd_spei,     na.rm = TRUE),
      median_spatial_sd = median(sd_spei,  na.rm = TRUE),
      min_spatial_sd    = min(sd_spei,     na.rm = TRUE),
      avg_spatial_range = mean(range_spei, na.rm = TRUE),
      max_spatial_range = max(range_spei,  na.rm = TRUE),
      n_timepoints      = n(),
      avg_n_points      = mean(n_points),
      pct_single_cell   = sum(n_points == 1) / n() * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(avg_spatial_sd))

  list(by_time = by_time, summary = summary)
}

spatial_var_12m <- calculate_spatial_variation_by_time(spei12_matched, "spei")

cat("\nWithin-district spatial variation — 12-month SPEI:\n")
print(spatial_var_12m$summary, n = 52)

# Map: average and maximum within-district spatial variation
plot_spatial_sd <- function(variation_summary, districts, title, metric = "avg_spatial_sd") {
  districts %>%
    left_join(variation_summary, by = "DistrictMunicipalityCode") %>%
    ggplot() +
    geom_sf(aes(fill = .data[[metric]]), color = "white", size = 0.2) +
    scale_fill_viridis_c(option = "plasma", name = "Spatial SD") +
    theme_minimal() +
    labs(title = title) +
    theme(legend.position = "right", plot.title = element_text(face = "bold"))
}

print(plot_spatial_sd(spatial_var_12m$summary, districts,
                      "12-Month SPEI: Average Within-District Spatial Variation"))
print(plot_spatial_sd(spatial_var_12m$summary, districts,
                      "12-Month SPEI: Maximum Within-District Spatial Variation",
                      "max_spatial_sd"))

# Distribution of spatial variation across districts
dist_plot_av <- ggplot(spatial_var_12m$summary, aes(x = avg_spatial_sd)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0.3, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 0.3, y = Inf, label = "Threshold: 0.3",
           vjust = 2, hjust = -0.1, color = "red") +
  theme_minimal() +
  labs(title = "Distribution of Within-District Spatial Variation (Average)",
       subtitle = "12-Month SPEI", x = "Average Spatial SD", y = "Number of Districts")
print(dist_plot_av)

dist_plot_max <- ggplot(spatial_var_12m$summary, aes(x = max_spatial_sd)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Within-District Spatial Variation (Maximum)",
       subtitle = "12-Month SPEI", x = "Maximum Spatial SD", y = "Number of Districts")
print(dist_plot_max)

# Time series: spatial variation over time for highest-variation district
worst_district      <- spatial_var_12m$summary$DistrictMunicipalityCode[1]
worst_district_name <- spatial_var_12m$summary$DistrictMunicipalityName[1]

ts_plot <- spatial_var_12m$by_time %>%
  filter(DistrictMunicipalityCode == worst_district) %>%
  mutate(date = as.Date(date)) %>%
  ggplot(aes(x = date, y = sd_spei)) +
  geom_line(color = "steelblue") +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0.3, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = paste("Spatial Variation Over Time:", worst_district_name),
       subtitle = "SD of SPEI within district at each month",
       x = "Date", y = "Spatial SD")
print(ts_plot)
