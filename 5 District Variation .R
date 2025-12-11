library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

# Read in South Africa SPEI with districts, 2006-2018
spei01_districts <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei01_districts.csv")
spei03_districts <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei03_districts.csv")
spei06_districts <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei06_districts.csv")
spei12_districts <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei12_districts.csv")

table(is.na(spei12_districts$spei))
# Read district boundaries for mapping
gdb_path <- "/Users/fayecrawford/Documents/Capstone!/MDBDistrictMunicipalBoundary2011.gdb"
districts <- st_read(gdb_path, layer = "MDBDistrictMunicipalBoundary2011")

# Filter out NA cells 
spei12_districts <- spei12_districts %>% 
  filter(!is.na(DistrictMunicipalityCode))

# ============================================================================
# FUNCTION: Calculate within-district SPATIAL variation at each time point
# ============================================================================
calculate_spatial_variation_by_time <- function(spei_districts, spei_col = "spei") {
  
  # Calculate variation within each district AT EACH TIME POINT
  by_time <- spei_districts %>%
    group_by(DistrictMunicipalityCode, DistrictMunicipalityName, ProvinceName, date) %>%
    summarise(
      mean_spei = mean(.data[[spei_col]], na.rm = TRUE),
      sd_spei = if_else(n() > 1, sd(.data[[spei_col]], na.rm = TRUE), 0),  # SD=0 if only 1 point
      min_spei = min(.data[[spei_col]], na.rm = TRUE),
      max_spei = max(.data[[spei_col]], na.rm = TRUE),
      range_spei = max_spei - min_spei,
      n_points = n(),
      .groups = "drop"
    )
  
  # Summarize: average spatial variation across all time points for each district
  summary <- by_time %>%
    group_by(DistrictMunicipalityCode, DistrictMunicipalityName, ProvinceName) %>%
    summarise(
      avg_spatial_sd = mean(sd_spei, na.rm = TRUE),        # Average SD across months
      max_spatial_sd = max(sd_spei, na.rm = TRUE),         # Worst month
      median_spatial_sd = median(sd_spei, na.rm = TRUE),   # Typical month
      min_spatial_sd = min(sd_spei, na.rm = TRUE),         # Best month
      avg_spatial_range = mean(range_spei, na.rm = TRUE),  # Average range
      max_spatial_range = max(range_spei, na.rm = TRUE),   # Max range
      n_timepoints = n(),
      avg_n_points = mean(n_points),
      pct_single_cell = sum(n_points == 1) / n() * 100,   # % of months with only 1 cell
      .groups = "drop"
    ) %>%
    arrange(desc(avg_spatial_sd))
  
  return(list(by_time = by_time, summary = summary))
}

# Calculate spatial variation
spatial_var_01m <- calculate_spatial_variation_by_time(spei01_districts, "spei")
spatial_var_03m <- calculate_spatial_variation_by_time(spei03_districts, "spei")
spatial_var_06m <- calculate_spatial_variation_by_time(spei06_districts, "spei")
spatial_var_12m <- calculate_spatial_variation_by_time(spei12_districts, "spei")

# ============================================================================
# Display results
# ============================================================================
cat("WITHIN-DISTRICT SPATIAL VARIATION (Same month, different locations)\n")
cat("12-Month SPEI - Spatial Variation Summary:\n")
print(spatial_var_12m$summary, n = 52)

# ============================================================================
# Summary statistics across all districts
# ============================================================================
summary_stats <- data.frame(
  timescale = c("1-month", "3-month", "6-month", "12-month"),
  mean_spatial_sd = c(
    mean(spatial_var_01m$summary$avg_spatial_sd, na.rm = TRUE),
    mean(spatial_var_03m$summary$avg_spatial_sd, na.rm = TRUE),
    mean(spatial_var_06m$summary$avg_spatial_sd, na.rm = TRUE),
    mean(spatial_var_12m$summary$avg_spatial_sd, na.rm = TRUE)
  ),
  median_spatial_sd = c(
    median(spatial_var_01m$summary$avg_spatial_sd, na.rm = TRUE),
    median(spatial_var_03m$summary$avg_spatial_sd, na.rm = TRUE),
    median(spatial_var_06m$summary$avg_spatial_sd, na.rm = TRUE),
    median(spatial_var_12m$summary$avg_spatial_sd, na.rm = TRUE)
  ),
  max_spatial_sd = c(
    max(spatial_var_01m$summary$max_spatial_sd, na.rm = TRUE),
    max(spatial_var_03m$summary$max_spatial_sd, na.rm = TRUE),
    max(spatial_var_06m$summary$max_spatial_sd, na.rm = TRUE),
    max(spatial_var_12m$summary$max_spatial_sd, na.rm = TRUE)
  )
)

cat("\n\nSummary: Within-district spatial variation by timescale\n")
cat("(How different are locations within districts in the same month?)\n\n")
print(summary_stats)

# ============================================================================
# Visualizations
# ============================================================================

# Map: Average spatial variation
plot_spatial_sd <- function(variation_summary, districts, title, metric = "avg_spatial_sd") {
  
  districts_with_var <- districts %>%
    left_join(variation_summary, by = "DistrictMunicipalityCode")
  
  ggplot(districts_with_var) +
    geom_sf(aes(fill = .data[[metric]]), color = "white", size = 0.2) +
    scale_fill_viridis_c(option = "plasma", name = "Spatial SD") +
    theme_minimal() +
    labs(title = title) +
    theme(legend.position = "right",
          plot.title = element_text(face = "bold"))
}

p12_avg <- plot_spatial_sd(spatial_var_12m$summary, districts, 
                           "12-Month SPEI: Average Within-District Spatial Variation",
                           "avg_spatial_sd")
print(p12_avg)

p12_max <- plot_spatial_sd(spatial_var_12m$summary, districts, 
                           "12-Month SPEI: Maximum Within-District Spatial Variation",
                           "max_spatial_sd")
print(p12_max)

# Distribution plot: How many districts have what level of variation?
dist_plot_av <- ggplot(spatial_var_12m$summary, aes(x = avg_spatial_sd)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0.3, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 0.3, y = Inf, label = "Threshold: 0.3", 
           vjust = 2, hjust = -0.1, color = "red") +
  theme_minimal() +
  labs(title = "Distribution of Within-District Spatial Variation",
       subtitle = "12-Month SPEI (1901-2018)",
       x = "Average Spatial SD",
       y = "Number of Districts")

# Distribution plot: How many districts have what level of variation?
dist_plot_max <- ggplot(spatial_var_12m$summary, aes(x = max_spatial_sd)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Maximum Within-District Spatial Variation",
       subtitle = "12-Month SPEI",
       x = "Maximum Spatial SD",
       y = "Number of Districts")

print(dist_plot_av)
print(dist_plot_max)

# Time series example: Show variation over time for highest-variation district
worst_district <- spatial_var_12m$summary$DistrictMunicipalityCode[1]
worst_district_name <- spatial_var_12m$summary$DistrictMunicipalityName[1]

time_series_worst <- spatial_var_12m$by_time %>%
  filter(DistrictMunicipalityCode == worst_district) %>%
  mutate(date = as.Date(date))

ts_plot <- ggplot(time_series_worst, aes(x = date, y = sd_spei)) +
  geom_line(color = "steelblue") +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0.3, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = paste("Spatial Variation Over Time:", worst_district_name),
       subtitle = "SD of SPEI within district at each month",
       x = "Date",
       y = "Spatial SD")

print(ts_plot)
