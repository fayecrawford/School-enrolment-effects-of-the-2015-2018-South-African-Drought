# Libraries 
library(tidyverse)
library(fixest) # for fixed effects regression 
library(modelsummary) # for regression tables
library(kableExtra)
library(marginaleffects)
library(car)

# DATASET SET-UP

# Load data and functions
spei_raw <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei12_districts.csv")
enrollment <- readRDS("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/enrollment_panel.rds")
source("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/8 Helper functions drought variables.R")
setwd("/Users/fayecrawford/Documents/Capstone!/SPEI_Project")

# Aggregate SPEI grid cells to district level (one-time step)
spei_monthly <- aggregate_spei_to_districts(spei_raw)

# Create analysis-ready dataset (all school phases retained)
data <- create_main_spec(enrollment, spei_monthly)

# Create school-phase analysis dataset (Pre-primary, Primary, Secondary only)
data_school <- data %>%
  filter(school_phase %in% c("Pre-primary", "Primary", "Secondary")) %>%
  mutate(school_phase = droplevels(school_phase))

# Optional: verify data integrity
verify_analysis_data(data_school, enrollment)

# DESCRIPTIVE STATISTICS

# Common theme for vizualizations
theme_clean <- theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 12, face = "bold"))

# Overall stats
overall_stats <- list(
  "Total person-years (all ages)" = nrow(data),
  "School-age person-years (5-20)" = sum(data$age >= 5 & data$age <= 20, na.rm = TRUE),
  "Unique individuals (all ages)" = n_distinct(data$pid),
  "Unique school-age individuals" = n_distinct(data$pid[data$age >= 5 & data$age <= 20]),
  "Years covered" = paste(min(data$year), "to", max(data$year)),
  "Mean observations per person" = round(nrow(data) / n_distinct(data$pid), 2)
)

for (stat_name in names(overall_stats)) {
  cat(paste0("  ", stat_name, ": ", overall_stats[[stat_name]], "\n"))
}

# Enrollment at a glance
cat(paste0("\n  Enrollment rate (all under 24): ",
           round(mean(data$enrolled[data$age < 24], na.rm = TRUE) * 100, 1), "%\n"))

# By Gender (all races)
cat("D. Enrollment by Gender (all races):\n")
gender_stats <- data_school %>%
  group_by(gender) %>%
  summarize(
    n_observations = n(),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_sample     = n_observations / sum(n_observations) * 100,
    enrollment_pct = enrollment_rate * 100
  )
print(gender_stats)

# By age 
age_stats <- data_school %>%
  filter(!is.na(age)) %>%
  group_by(age) %>%
  summarize(
    n_observations = n(),
    n_individuals = n_distinct(pid),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(enrollment_pct = enrollment_rate * 100) %>%
  arrange(age)

# By school phase
phase_stats <- data_school %>%
  filter(!is.na(school_phase)) %>%
  group_by(school_phase) %>%
  summarize(
    n_observations = n(),
    n_individuals = n_distinct(pid),
    enrollment_rate = mean(enrolled, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pct_observations = n_observations / sum(n_observations) * 100)
print(phase_stats)

# Print detailed single-year age stats
cat("\nDetailed enrollment by single year of age:\n")
print(age_stats, n=100)

data %>%
  filter(age >= 19 & age <= 30) %>%
  mutate(status = case_when(
    enrolled == 0                          ~ "Not enrolled",
    enrolled == 1 & school_phase == "Secondary"     ~ "Secondary",
    enrolled == 1 & school_phase == "Post-secondary" ~ "Post-secondary",
    TRUE                                   ~ "Other/unknown"
  )) %>%
  count(status) %>%
  mutate(pct = n / sum(n) * 100)

enrollment_by_age <- data %>%
  filter(!is.na(age), age <= 30) %>%
  group_by(age) %>%
  summarize(enrollment_rate = mean(enrolled, na.rm = TRUE), .groups = "drop")

ggplot(enrollment_by_age, aes(x = age, y = enrollment_rate)) +
  annotate("rect", xmin = 5, xmax = 20, ymin = 0, ymax = 1, 
           fill = "pink", alpha = 0.3) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 2) +
  geom_vline(xintercept = c(5, 20), linetype = "dashed", color = "red", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 30, 10)) +
  labs(x = "Age", y = "Enrollment Rate") +
  theme_clean

# Race
data_school %>%
  count(race) %>%
  mutate(pct = n / sum(n) * 100)

# Location
data_school %>%
  count(province) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(desc(n))

data_school %>%
  count(district) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(desc(n)) %>%
  print(n = 55)

# Income
summary(data_school$hh_income)
mean(is.na(data_school$hh_income)) * 100

# Define percentiles
q1 <- quantile(data_school$hh_income, 0.25, na.rm = TRUE)
q3 <- quantile(data_school$hh_income, 0.75, na.rm = TRUE)
p10 <- quantile(data_school$hh_income, 0.10, na.rm = TRUE)
p90 <- quantile(data_school$hh_income, 0.90, na.rm = TRUE)
max_income <- max(data_school$hh_income, na.rm = TRUE)

# Inter-quartile distribution (25th-75th)
ggplot(data_school %>% filter(hh_income >= q1 & hh_income <= q3), aes(x = hh_income)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, q3, by = 1000)) +
  labs(x = "Household Income (Rand)", y = "Count", 
       title = "Household Income: Inter-quartile Range (25th-75th)") +
  theme_clean

# 10th-90th percentile distribution
ggplot(data_school %>% filter(hh_income >= p10 & hh_income <= p90), aes(x = hh_income)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, p90, by = 1000)) +
  labs(x = "Household Income (Rand)", y = "Count",
       title = "Household Income: 10th-90th Percentile") +
  theme_clean

# Complete distribution (excluding max outlier)
ggplot(data_school %>% filter(hh_income < max_income), aes(x = hh_income)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, max_income, by = 50000)) +
  labs(x = "Household Income (Rand)", y = "Count",
       title = "Household Income: Full Distribution (excl. max outlier)") +
  theme_clean

# Top 10% (excluding max outlier)
ggplot(data_school %>% filter(hh_income >= p90 & hh_income < max_income), aes(x = hh_income)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, max_income, by = 50000)) +
  labs(x = "Household Income (Rand)", y = "Count",
       title = "Household Income: Top 10% (excl. max outlier)") +
  theme_clean

# Step 3 - Run regressions!

# Average effects: both drought and cumulative drought count (5-year), no interactions
m1 <- feols(enrolled ~ drought + drought_any_count5 +
              school_phase + hh_income + gender + race |
              district + year,
            data = data_school, cluster = ~district)

# All interactions: both drought and cumulative drought count (5-year) with demographics
m2 <- feols(enrolled ~ drought*drought_any_count5 +
              drought*school_phase +
              drought*hh_income +
              drought*gender +
              drought*race +
              drought_any_count5*school_phase +
              drought_any_count5*hh_income +
              drought_any_count5*gender +
              drought_any_count5*race |
              district + year,
            data = data_school, cluster = ~district)

# View results
etable(m1, m2)

# MODELS ON FILTERED SUBGROUPS

# Race subsets: drop race as covariate since no variation within subset
run_race_models <- function(df) {
  m1 <- feols(enrolled ~ drought + drought_any_count5 +
                hh_income + gender + school_phase |
                district + year,
              data = df, cluster = ~district)
  m2 <- feols(enrolled ~ drought*drought_any_count5 +
                drought*hh_income +
                drought*gender +
                drought*school_phase +
                drought_any_count5*hh_income +
                drought_any_count5*gender +
                drought_any_count5*school_phase |
                district + year,
              data = df, cluster = ~district)
  list(m1 = m1, m2 = m2)
}

# Gender subsets: drop gender as covariate since no variation within subset
run_gender_models <- function(df) {
  m1 <- feols(enrolled ~ drought + drought_any_count5 +
                hh_income + race + school_phase |
                district + year,
              data = df, cluster = ~district)
  m2 <- feols(enrolled ~ drought*drought_any_count5 +
                drought*hh_income +
                drought*race +
                drought*school_phase +
                drought_any_count5*hh_income +
                drought_any_count5*race +
                drought_any_count5*school_phase |
                district + year,
              data = df, cluster = ~district)
  list(m1 = m1, m2 = m2)
}

data_black    <- data_school %>% filter(race == "Black")
data_white    <- data_school %>% filter(race == "White")
data_coloured <- data_school %>% filter(race == "Coloured")
data_asian    <- data_school %>% filter(race == "Asian/Indian")
data_female   <- data_school %>% filter(gender == "Female")
data_male     <- data_school %>% filter(gender == "Male")

models_black    <- run_race_models(data_black)
models_white    <- run_race_models(data_white)
models_asian    <- run_race_models(data_asian)
models_coloured <- run_race_models(data_coloured)
models_female   <- run_gender_models(data_female)
models_male     <- run_gender_models(data_male)

# View results
etable(models_black$m1,    models_black$m2)
etable(models_white$m1,    models_white$m2)
etable(models_asian$m1,    models_asian$m2)
etable(models_coloured$m1, models_coloured$m2)
etable(models_female$m1,   models_female$m2)
etable(models_male$m1,     models_male$m2)

# BY SCHOOL PHASE

# Filter into subsets
data_preprimary <- data_school %>% filter(school_phase == "Pre-primary")
data_primary    <- data_school %>% filter(school_phase == "Primary")
data_secondary  <- data_school %>% filter(school_phase == "Secondary")

# Helper to run both models on a given subset (school_phase dropped as covariate)
run_phase_models <- function(df) {
  m1 <- feols(enrolled ~ drought + drought_any_count5 +
                hh_income + gender + race |
                district + year,
              data = df, cluster = ~district)

  m2 <- feols(enrolled ~ drought*drought_any_count5 +
                drought*hh_income +
                drought*gender +
                drought*race +
                drought_any_count5*hh_income +
                drought_any_count5*gender +
                drought_any_count5*race |
                district + year,
              data = df, cluster = ~district)

  list(m1 = m1, m2 = m2)
}

models_preprimary <- run_phase_models(data_preprimary)
models_primary    <- run_phase_models(data_primary)
models_secondary  <- run_phase_models(data_secondary)

# View results by phase
etable(models_preprimary$m1, models_preprimary$m2)
etable(models_primary$m1,    models_primary$m2)
etable(models_secondary$m1,  models_secondary$m2)

# BLACK STUDENTS BY SCHOOL PHASE

data_black_preprimary <- data_school %>% filter(race == "Black", school_phase == "Pre-primary")
data_black_primary    <- data_school %>% filter(race == "Black", school_phase == "Primary")
data_black_secondary  <- data_school %>% filter(race == "Black", school_phase == "Secondary")

# Black-by-phase models: drop both race and school_phase (no variation within subset)
run_black_phase_models <- function(df) {
  m1 <- feols(enrolled ~ drought + drought_any_count5 + hh_income + gender |
                district + year, data = df, cluster = ~district)
  m2 <- feols(enrolled ~ drought*drought_any_count5 +
                drought*hh_income +
                drought*gender +
                drought_any_count5*hh_income +
                drought_any_count5*gender |
                district + year, data = df, cluster = ~district)
  list(m1 = m1, m2 = m2)
}

models_black_preprimary <- run_black_phase_models(data_black_preprimary)
models_black_primary    <- run_black_phase_models(data_black_primary)
models_black_secondary  <- run_black_phase_models(data_black_secondary)

# View results
etable(models_black_preprimary$m1, models_black_preprimary$m2)
etable(models_black_primary$m1,    models_black_primary$m2)
etable(models_black_secondary$m1,  models_black_secondary$m2)

# Export
etable(models_black_preprimary$m1, models_black_preprimary$m2,
       tex = TRUE, file = "black preprimary results table.tex")
etable(models_black_primary$m1, models_black_primary$m2,
       tex = TRUE, file = "black primary results table.tex")
etable(models_black_secondary$m1, models_black_secondary$m2,
       tex = TRUE, file = "black secondary results table.tex")

# COLOURED STUDENTS BY SCHOOL PHASE
# Reuses run_black_phase_models: drops race and school_phase, keeps gender and hh_income

data_coloured_preprimary <- data_school %>% filter(race == "Coloured", school_phase == "Pre-primary")
data_coloured_primary    <- data_school %>% filter(race == "Coloured", school_phase == "Primary")
data_coloured_secondary  <- data_school %>% filter(race == "Coloured", school_phase == "Secondary")

models_coloured_preprimary <- run_black_phase_models(data_coloured_preprimary)
models_coloured_primary    <- run_black_phase_models(data_coloured_primary)
models_coloured_secondary  <- run_black_phase_models(data_coloured_secondary)

etable(models_coloured_preprimary$m1, models_coloured_preprimary$m2)
etable(models_coloured_primary$m1,    models_coloured_primary$m2)
etable(models_coloured_secondary$m1,  models_coloured_secondary$m2)

etable(models_coloured_preprimary$m1, models_coloured_preprimary$m2,
       tex = TRUE, file = "coloured preprimary results table.tex")
etable(models_coloured_primary$m1, models_coloured_primary$m2,
       tex = TRUE, file = "coloured primary results table.tex")
etable(models_coloured_secondary$m1, models_coloured_secondary$m2,
       tex = TRUE, file = "coloured secondary results table.tex")

# FEMALE STUDENTS BY SCHOOL PHASE
# Drops gender and school_phase (no variation within subset), keeps race and hh_income

run_female_phase_models <- function(df) {
  m1 <- feols(enrolled ~ drought + drought_any_count5 + hh_income + race |
                district + year, data = df, cluster = ~district)
  m2 <- feols(enrolled ~ drought*drought_any_count5 +
                drought*hh_income +
                drought*race +
                drought_any_count5*hh_income +
                drought_any_count5*race |
                district + year, data = df, cluster = ~district)
  list(m1 = m1, m2 = m2)
}

data_female_preprimary <- data_school %>% filter(gender == "Female", school_phase == "Pre-primary")
data_female_primary    <- data_school %>% filter(gender == "Female", school_phase == "Primary")
data_female_secondary  <- data_school %>% filter(gender == "Female", school_phase == "Secondary")

models_female_preprimary <- run_female_phase_models(data_female_preprimary)
models_female_primary    <- run_female_phase_models(data_female_primary)
models_female_secondary  <- run_female_phase_models(data_female_secondary)

etable(models_female_preprimary$m1, models_female_preprimary$m2)
etable(models_female_primary$m1,    models_female_primary$m2)
etable(models_female_secondary$m1,  models_female_secondary$m2)

etable(models_female_preprimary$m1, models_female_preprimary$m2,
       tex = TRUE, file = "female preprimary results table.tex")
etable(models_female_primary$m1, models_female_primary$m2,
       tex = TRUE, file = "female primary results table.tex")
etable(models_female_secondary$m1, models_female_secondary$m2,
       tex = TRUE, file = "female secondary results table.tex")

