library(tidyverse)
library(modelsummary)

# ============================================================================
# NIDS ENROLLMENT PANEL - DESCRIPTIVE STATISTICS
# Produces tables and visualizations for enrollment patterns
# Focus on school-age population (ages 6-18) for enrollment rates
# ============================================================================

# LOAD DATA -----------------------------------------------------------------
enrollment_panel <- readRDS("enrollment_panel.rds")

# DEFINE SCHOOL-AGE POPULATION ----------------------------------------------
# Primary analysis focuses on ages 5-25 
SCHOOL_AGE_MIN <- 5
SCHOOL_AGE_MAX <- 25

enrollment_panel <- enrollment_panel %>%
  mutate(
    school_age = !is.na(age) & age >= SCHOOL_AGE_MIN & age <= SCHOOL_AGE_MAX
  )

cat("=== ENROLLMENT PANEL DESCRIPTIVE STATISTICS ===\n")
cat("Note: Enrollment rates calculated for school-age population (5-25) unless noted\n\n")

# 1. OVERALL SAMPLE CHARACTERISTICS -----------------------------------------
cat("1. OVERALL SAMPLE CHARACTERISTICS\n")
cat("----------------------------------\n")

overall_stats <- list(
  "Total person-years (all ages)" = nrow(enrollment_panel),
  "School-age person-years (5-25)" = sum(enrollment_panel$school_age),
  "Unique individuals (all ages)" = n_distinct(enrollment_panel$pid),
  "Unique school-age individuals" = n_distinct(enrollment_panel$pid[enrollment_panel$school_age]),
  "Years covered" = paste(min(enrollment_panel$year), "to", max(enrollment_panel$year)),
  "Mean observations per person" = round(nrow(enrollment_panel) / n_distinct(enrollment_panel$pid), 2)
)

for (stat_name in names(overall_stats)) {
  cat(paste0("  ", stat_name, ": ", overall_stats[[stat_name]], "\n"))
}

# Enrollment rates for school-age only
school_age_data <- enrollment_panel %>% filter(school_age)
cat(paste0("\n  Enrollment rate (school-age 5-25): ", 
           round(mean(school_age_data$enrolled, na.rm = TRUE) * 100, 1), "%\n"))
cat(paste0("  Enrollment rate (all ages): ", 
           round(mean(enrollment_panel$enrolled, na.rm = TRUE) * 100, 1), 
           "% [includes non-school-age population]\n"))
cat("\n")

# 2. ENROLLMENT BY DEMOGRAPHIC CHARACTERISTICS ------------------------------
cat("2. ENROLLMENT BY DEMOGRAPHIC CHARACTERISTICS (SCHOOL-AGE 5-25)\n")
cat("---------------------------------------------------------------\n")

school_age_data <- enrollment_panel %>% filter(school_age)

# By Gender
cat("\nA. Enrollment by Gender:\n")
gender_stats <- school_age_data %>%
  group_by(gender) %>%
  summarize(
    n_observations = n(),
    n_individuals = n_distinct(pid),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_observations = n_observations / sum(n_observations) * 100,
    enrollment_percent = enrollment_rate * 100
  )

print(gender_stats)
cat("\n")

# By Race
cat("B. Enrollment by Race/Population Group:\n")
race_stats <- school_age_data %>%
  group_by(race) %>%
  summarize(
    n_observations = n(),
    n_individuals = n_distinct(pid),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_observations = n_observations / sum(n_observations) * 100,
    enrollment_pct = enrollment_rate * 100
  ) %>%
  arrange(desc(n_observations))

print(race_stats)
cat("\n")

# By Gender x Race
cat("C. Enrollment by Gender x Race:\n")
gender_race_stats <- school_age_data %>%
  group_by(gender, race) %>%
  summarize(
    n_observations = n(),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(enrollment_pct = enrollment_rate * 100) %>%
  arrange(gender, desc(n_observations))

print(gender_race_stats)
cat("\n")

# 3. ENROLLMENT BY AGE ------------------------------------------------------
cat("3. ENROLLMENT BY AGE (ALL AGES - FOR DISTRIBUTION)\n")
cat("---------------------------------------------------\n")
cat("Note: Enrollment rates shown for all ages to illustrate age pattern.\n")
cat("      Main analysis focuses on school-age (6-18).\n\n")

age_stats <- enrollment_panel %>%
  filter(!is.na(age)) %>%  # Drop observations with missing age
  group_by(age) %>%
  summarize(
    n_observations = n(),
    n_individuals = n_distinct(pid),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(enrollment_pct = enrollment_rate * 100) %>%
  arrange(age)

# Print key age groups
cat("\nEnrollment rates by age group:\n")
age_group_stats <- enrollment_panel %>%
  filter(!is.na(age)) %>%
  mutate(
    age_group = case_when(
      age < 6 ~ "Under 6",
      age >= 6 & age <= 12 ~ "6-12 (Primary)",
      age >= 13 & age <= 18 ~ "13-18 (Secondary)",
      age >= 19 & age <= 24 ~ "19-24",
      age >= 25 ~ "25+"
    )
  ) %>%
  group_by(age_group) %>%
  summarize(
    n_observations = n(),
    n_individuals = n_distinct(pid),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(enrollment_pct = enrollment_rate * 100)

print(age_group_stats)
cat("\n")

# Print detailed single-year age stats
cat("\nDetailed enrollment by single year of age:\n")
print(age_stats)
cat("\n")

# 4. ENROLLMENT BY PROVINCE -------------------------------------------------
cat("4. ENROLLMENT BY PROVINCE (SCHOOL-AGE 6-18)\n")
cat("--------------------------------------------\n")

province_stats <- school_age_data %>%
  group_by(province) %>%
  summarize(
    n_observations = n(),
    n_individuals = n_distinct(pid),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_observations = n_observations / sum(n_observations) * 100,
    enrollment_pct = enrollment_rate * 100
  ) %>%
  arrange(desc(n_observations))

print(province_stats)
cat("\n")

# 5. ENROLLMENT BY GEOGRAPHIC TYPE ------------------------------------------
cat("5. ENROLLMENT BY GEOGRAPHIC TYPE (SCHOOL-AGE 6-18)\n")
cat("---------------------------------------------------\n")

geo_stats <- school_age_data %>%
  group_by(geotype) %>%
  summarize(
    n_observations = n(),
    n_individuals = n_distinct(pid),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_observations = n_observations / sum(n_observations) * 100,
    enrollment_pct = enrollment_rate * 100
  )

print(geo_stats)
cat("\n")

# 6. ENROLLMENT TRENDS OVER TIME --------------------------------------------
cat("6. ENROLLMENT TRENDS OVER TIME (SCHOOL-AGE 6-18)\n")
cat("-------------------------------------------------\n")

time_stats <- school_age_data %>%
  group_by(year) %>%
  summarize(
    n_observations = n(),
    n_individuals = n_distinct(pid),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(enrollment_pct = enrollment_rate * 100)

print(time_stats)
cat("\n")

# By year and age group
cat("\nEnrollment trends by age group over time:\n")
time_age_stats <- enrollment_panel %>%
  filter(!is.na(age)) %>%
  mutate(
    age_group = case_when(
      age >= 6 & age <= 12 ~ "Primary (6-12)",
      age >= 13 & age <= 18 ~ "Secondary (13-18)",
      TRUE ~ "Other"
    )
  ) %>%
  filter(age_group != "Other") %>%
  group_by(year, age_group) %>%
  summarize(
    n_observations = n(),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(enrollment_pct = enrollment_rate * 100)

print(time_age_stats)
cat("\n")

# 7. WAVE REPRESENTATION ----------------------------------------------------
cat("7. OBSERVATIONS BY WAVE (SCHOOL-AGE 6-18)\n")
cat("------------------------------------------\n")

wave_stats <- school_age_data %>%
  group_by(wave, questionnaire) %>%
  summarize(
    n_observations = n(),
    n_individuals = n_distinct(pid),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(enrollment_pct = enrollment_rate * 100) %>%
  arrange(wave, questionnaire)

print(wave_stats)
cat("\n")

# 8. ATTRITION PATTERNS -----------------------------------------------------
cat("8. ATTRITION PATTERNS\n")
cat("----------------------\n")

# How many waves does each person appear in?
attrition_stats <- enrollment_panel %>%
  group_by(pid) %>%
  summarize(
    n_waves = n_distinct(wave),
    n_years = n(),
    .groups = "drop"
  ) %>%
  group_by(n_waves) %>%
  summarize(
    n_individuals = n(),
    pct_individuals = n() / n_distinct(enrollment_panel$pid) * 100,
    .groups = "drop"
  )

cat("\nNumber of waves each individual appears in:\n")
print(attrition_stats)
cat("\n")

# 9. MISSING DATA PATTERNS --------------------------------------------------
cat("9. MISSING DATA PATTERNS\n")
cat("-------------------------\n")

missing_stats <- enrollment_panel %>%
  summarize(
    total_obs = n(),
    missing_age = sum(is.na(age)),
    missing_gender = sum(is.na(gender)),
    missing_race = sum(is.na(race)),
    missing_district = sum(is.na(district)),
    missing_province = sum(is.na(province)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("missing"),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    variable = str_remove(variable, "missing_"),
    pct_missing = n_missing / total_obs * 100
  )

print(missing_stats)
cat("\n")

# ==========================================================================
# VISUALIZATION: ENROLLMENT RATE BY AGE (ALL AGES)
# ==========================================================================

cat("Generating visualization: Enrollment by Age (All Ages)\n")

# Show full age range to illustrate the pattern
plot_age <- enrollment_panel %>%
  filter(!is.na(age), age >= 3, age <= 30) %>%
  group_by(age) %>%
  summarize(
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p1 <- ggplot(plot_age, aes(x = age, y = enrollment_rate)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  # Highlight school-age range
  geom_vline(xintercept = SCHOOL_AGE_MIN, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = SCHOOL_AGE_MAX, linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("rect", xmin = SCHOOL_AGE_MIN, xmax = SCHOOL_AGE_MAX, ymin = 0, ymax = 1,
           alpha = 0.1, fill = "red") +
  annotate("text", x = 12, y = 0.95, label = "School-age (6-18)", 
           color = "red", fontface = "bold") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "School Enrollment Rate by Age",
    subtitle = "NIDS Waves 1-5 (2007-2017) - Shaded area shows school-age focus",
    x = "Age",
    y = "Enrollment Rate",
    caption = paste0("Total n = ", sum(plot_age$n), " person-year observations")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

print(p1)
ggsave("enrollment_by_age.png", p1, width = 10, height = 6, dpi = 300)
cat("Saved: enrollment_by_age.png\n\n")

# ==========================================================================
# VISUALIZATION: ENROLLMENT TRENDS OVER TIME BY AGE GROUP
# ==========================================================================

cat("Generating visualization: Enrollment Trends by Age Group\n")

plot_time_age <- enrollment_panel %>%
  filter(!is.na(age)) %>%
  mutate(
    age_group = case_when(
      age >= 6 & age <= 12 ~ "Primary (6-12)",
      age >= 13 & age <= 18 ~ "Secondary (13-18)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(age_group)) %>%
  group_by(year, age_group) %>%
  summarize(
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  )

p2 <- ggplot(plot_time_age, aes(x = year, y = enrollment_rate, color = age_group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent, limits = c(0.7, 1)) +
  scale_color_manual(values = c("Primary (6-12)" = "#2E86AB", 
                                "Secondary (13-18)" = "#A23B72")) +
  labs(
    title = "School Enrollment Trends Over Time",
    subtitle = "By Age Group, NIDS Waves 1-5",
    x = "Year",
    y = "Enrollment Rate",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p2)
ggsave("enrollment_trends_by_age.png", p2, width = 10, height = 6, dpi = 300)
cat("Saved: enrollment_trends_by_age.png\n\n")

# ==========================================================================
# VISUALIZATION: ENROLLMENT BY PROVINCE (SCHOOL-AGE)
# ==========================================================================

cat("Generating visualization: Enrollment by Province (School-Age)\n")

plot_province <- school_age_data %>%
  group_by(province) %>%
  summarize(
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(enrollment_rate)

# Create factor with ordered levels for plotting
plot_province$province <- factor(plot_province$province, 
                                 levels = plot_province$province)

p3 <- ggplot(plot_province, aes(x = province, y = enrollment_rate)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = paste0(round(enrollment_rate * 100, 1), "%")), 
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "School Enrollment Rate by Province",
    subtitle = "School-age population (6-18), NIDS Waves 1-5 (2007-2017)",
    x = "Province",
    y = "Enrollment Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p3)
ggsave("enrollment_by_province.png", p3, width = 10, height = 6, dpi = 300)
cat("Saved: enrollment_by_province.png\n\n")

# ==========================================================================
# VISUALIZATION: ENROLLMENT BY RACE (SCHOOL-AGE)
# ==========================================================================

cat("Generating visualization: Enrollment by Race (School-Age)\n")

plot_race <- school_age_data %>%
  group_by(race) %>%
  summarize(
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(enrollment_rate)

plot_race$race <- factor(plot_race$race, levels = plot_race$race)

p4 <- ggplot(plot_race, aes(x = race, y = enrollment_rate)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = paste0(round(enrollment_rate * 100, 1), "%")), 
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "School Enrollment Rate by Race/Population Group",
    subtitle = "School-age population (6-18), NIDS Waves 1-5 (2007-2017)",
    x = "Race/Population Group",
    y = "Enrollment Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p4)
ggsave("enrollment_by_race.png", p4, width = 10, height = 6, dpi = 300)
cat("Saved: enrollment_by_race.png\n\n")
