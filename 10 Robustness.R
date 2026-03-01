# ==============================================================================
# 10 Robustness.R
# Robustness checks: alternative drought specifications
#
# Tests implemented here:
#   [1] Single drought category (SPEI < -1, no moderate/extreme split)
#   [2] Threshold range: single drought indicator at thresholds 0 to -2.5
#       in steps of 0.1
# ==============================================================================

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

# ------------------------------------------------------------------------------
# DATA SETUP
# ------------------------------------------------------------------------------

spei_raw   <- read.csv("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/spei12_districts.csv")
enrollment <- readRDS("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/enrollment_panel.rds")
source("/Users/fayecrawford/Documents/Capstone!/SPEI_Project/8 Helper functions drought variables.R")
setwd("/Users/fayecrawford/Documents/Capstone!/SPEI_Project")

spei_monthly <- aggregate_spei_to_districts(spei_raw)
data         <- create_main_spec(enrollment, spei_monthly)
data_school  <- data %>%
  filter(school_phase != "Post-secondary") %>%
  mutate(school_phase = droplevels(school_phase))

# Common plot theme
theme_clean <- theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 12, face = "bold"))


# ==============================================================================
# ROBUSTNESS TEST 1: Single drought category (SPEI < -1)
#
# Main spec splits drought into:
#   drought         = SPEI < -1 AND >= -1.5  (moderate)
#   drought_extreme = SPEI < -1.5            (extreme)
#
# This test collapses both into a single indicator:
#   drought_any = SPEI < -1
# ==============================================================================

data_r1 <- data_school %>%
  mutate(drought_any = as.integer(spei < -1))

# M1: Average effects (both drought measures, no interactions)
r1_m1 <- feols(enrolled ~ drought_any + drought_any_count5 +
                 school_phase + hh_income + gender + race |
                 district + year,
               data = data_r1, cluster = ~district)

# M2: All interactions
r1_m2 <- feols(enrolled ~ drought_any*drought_any_count5 +
                 drought_any*school_phase +
                 drought_any*hh_income +
                 drought_any*gender +
                 drought_any*race +
                 drought_any_count5*school_phase +
                 drought_any_count5*hh_income +
                 drought_any_count5*gender +
                 drought_any_count5*race |
                 district + year,
               data = data_r1, cluster = ~district)

etable(r1_m1, r1_m2)

# ------------------------------------------------------------------------------
# Robustness Test 1: Subset models
# By school phase, race (Black, Coloured), and female
# school_phase / race / gender dropped as covariates within their own subsets
# ------------------------------------------------------------------------------

# --- By school phase ---

r1_phase_data <- list(
  preprimary = data_r1 %>% filter(school_phase == "Pre-primary"),
  primary    = data_r1 %>% filter(school_phase == "Primary"),
  secondary  = data_r1 %>% filter(school_phase == "Secondary")
)

r1_phase_models <- lapply(r1_phase_data, function(df) {
  m1 <- feols(enrolled ~ drought_any + drought_any_count5 +
                hh_income + gender + race |
                district + year,
              data = df, cluster = ~district)
  m2 <- feols(enrolled ~ drought_any*drought_any_count5 +
                drought_any*hh_income +
                drought_any*gender +
                drought_any*race +
                drought_any_count5*hh_income +
                drought_any_count5*gender +
                drought_any_count5*race |
                district + year,
              data = df, cluster = ~district)
  list(m1 = m1, m2 = m2)
})

etable(r1_phase_models$preprimary$m1, r1_phase_models$preprimary$m2)
etable(r1_phase_models$primary$m1,    r1_phase_models$primary$m2)
etable(r1_phase_models$secondary$m1,  r1_phase_models$secondary$m2)

# --- By race (Black, Coloured) ---

r1_race_data <- list(
  black    = data_r1 %>% filter(race == "Black"),
  coloured = data_r1 %>% filter(race == "Coloured")
)

r1_race_models <- lapply(r1_race_data, function(df) {
  m1 <- feols(enrolled ~ drought_any + drought_any_count5 +
                school_phase + hh_income + gender |
                district + year,
              data = df, cluster = ~district)
  m2 <- feols(enrolled ~ drought_any*drought_any_count5 +
                drought_any*school_phase +
                drought_any*hh_income +
                drought_any*gender +
                drought_any_count5*school_phase +
                drought_any_count5*hh_income +
                drought_any_count5*gender |
                district + year,
              data = df, cluster = ~district)
  list(m1 = m1, m2 = m2)
})

etable(r1_race_models$black$m1,    r1_race_models$black$m2)
etable(r1_race_models$coloured$m1, r1_race_models$coloured$m2)

# --- Female subset ---

r1_female_m1 <- feols(enrolled ~ drought_any + drought_any_count5 +
                        school_phase + hh_income + race |
                        district + year,
                      data = filter(data_r1, gender == "Female"),
                      cluster = ~district)

r1_female_m2 <- feols(enrolled ~ drought_any*drought_any_count5 +
                        drought_any*school_phase +
                        drought_any*hh_income +
                        drought_any*race +
                        drought_any_count5*school_phase +
                        drought_any_count5*hh_income +
                        drought_any_count5*race |
                        district + year,
                      data = filter(data_r1, gender == "Female"),
                      cluster = ~district)

etable(r1_female_m1, r1_female_m2)

# ==============================================================================
# ROBUSTNESS TEST 2: Threshold range (0 to -2.5, step 0.1)
#
# For each threshold t, define:
#   drought_t = as.integer(spei < t)
# and re-run all four models.
#
# NOTE: Both drought_t and drought_any_count5_t are recomputed at each threshold t.
# drought_any_count5_t = count of preceding 5 lag years with SPEI < t.
# ==============================================================================

thresholds <- seq(0, -2.5, by = -0.1)

# Storage
coef_rows <- vector("list", length(thresholds))

for (i in seq_along(thresholds)) {
  t <- round(thresholds[i], 1)

  data_t <- data_school %>%
    mutate(
      drought_t            = as.integer(spei < t),
      drought_any_count5_t = as.integer(lag1 < t) + as.integer(lag2 < t) +
                             as.integer(lag3 < t) + as.integer(lag4 < t) +
                             as.integer(lag5 < t)
    )

  # Skip if no variation in drought indicator
  if (var(data_t$drought_t, na.rm = TRUE) == 0) {
    message("Skipping threshold ", t, " — no variation in drought indicator")
    next
  }

  m1_t <- feols(enrolled ~ drought_t + drought_any_count5_t +
                  school_phase + hh_income + gender + race |
                  district + year,
                data = data_t, cluster = ~district)

  m2_t <- feols(enrolled ~ drought_t*drought_any_count5_t +
                  drought_t*school_phase +
                  drought_t*hh_income +
                  drought_t*gender +
                  drought_t*race +
                  drought_any_count5_t*school_phase +
                  drought_any_count5_t*hh_income +
                  drought_any_count5_t*gender +
                  drought_any_count5_t*race |
                  district + year,
                data = data_t, cluster = ~district)

  # Extract coefficients and SEs
  m1_coef       <- coef(m1_t)["drought_t"]
  m1_se         <- se(m1_t)["drought_t"]

  m1_count_coef <- coef(m1_t)["drought_any_count5_t"]
  m1_count_se   <- se(m1_t)["drought_any_count5_t"]

  m2_coef      <- coef(m2_t)["drought_t"]
  m2_se        <- se(m2_t)["drought_t"]

  m2_spei_coef <- coef(m2_t)["drought_t:drought_any_count5_t"]
  m2_spei_se   <- se(m2_t)["drought_t:drought_any_count5_t"]

  coef_rows[[i]] <- data.frame(
    threshold = t,
    m1_coef = m1_coef, m1_se = m1_se,
    m1_count_coef = m1_count_coef, m1_count_se = m1_count_se,
    m2_coef = m2_coef, m2_se = m2_se,
    m2_spei_coef = m2_spei_coef, m2_spei_se = m2_spei_se,
    row.names = NULL
  )
}

coef_table <- bind_rows(coef_rows) %>%
  mutate(
    m1_lo         = m1_coef       - 1.96 * m1_se,
    m1_hi         = m1_coef       + 1.96 * m1_se,
    m1_count_lo   = m1_count_coef - 1.96 * m1_count_se,
    m1_count_hi   = m1_count_coef + 1.96 * m1_count_se,
    m2_lo         = m2_coef       - 1.96 * m2_se,
    m2_hi         = m2_coef       + 1.96 * m2_se,
    m2_spei_lo    = m2_spei_coef  - 1.96 * m2_spei_se,
    m2_spei_hi    = m2_spei_coef  + 1.96 * m2_spei_se
  )

write.csv(coef_table, "robustness_threshold_range_coefs.csv", row.names = FALSE)

# --- Plots ---

# M1: drought_t coefficient across thresholds
ggplot(coef_table, aes(x = threshold, y = m1_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_lo, ymax = m1_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  annotate("text", x = -1.05, y = max(coef_table$m1_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, vjust = -0.6, size = 5, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on Drought (drought_t)",
       title = "M1: Drought Coefficient Across Thresholds") +
  theme_clean

# M1: drought_any_count5_t coefficient across thresholds
ggplot(coef_table, aes(x = threshold, y = m1_count_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_count_lo, ymax = m1_count_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "orange", linewidth = 1) +
  geom_point(color = "orange", size = 2) +
  annotate("text", x = -1.05, y = max(coef_table$m1_count_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 5, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)",
       y = "Coefficient on drought_any_count5",
       title = "M1: Cumulative Drought Count Coefficient Across Thresholds") +
  theme_clean

# M2: drought_t:drought_any_count5_t interaction across thresholds
ggplot(coef_table, aes(x = threshold, y = m2_spei_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m2_spei_lo, ymax = m2_spei_hi), alpha = 0.2, fill = "darkorchid") +
  geom_line(color = "darkorchid", linewidth = 1) +
  geom_point(color = "darkorchid", size = 2) +
  annotate("text", x = -1.05, y = max(coef_table$m2_spei_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2, by = -0.5)) +
  coord_cartesian(xlim = c(-2, 0), ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Interaction: drought_t × drought_any_count5_t",
       title = "M2: Drought × Cumulative Drought Count Interaction Across Thresholds") +
  theme_clean

# ------------------------------------------------------------------------------
# Robustness Test 2: Threshold range — Black subset
# ------------------------------------------------------------------------------

data_black_t2    <- data_school %>% filter(race == "Black")
coef_rows_black  <- vector("list", length(thresholds))

for (i in seq_along(thresholds)) {
  t <- round(thresholds[i], 1)

  data_t <- data_black_t2 %>%
    mutate(
      drought_t            = as.integer(spei < t),
      drought_any_count5_t = as.integer(lag1 < t) + as.integer(lag2 < t) +
                             as.integer(lag3 < t) + as.integer(lag4 < t) +
                             as.integer(lag5 < t)
    )

  if (var(data_t$drought_t, na.rm = TRUE) == 0) {
    message("Black: skipping threshold ", t, " — no variation in drought indicator")
    next
  }

  m1_t <- feols(enrolled ~ drought_t + drought_any_count5_t +
                  school_phase + hh_income + gender |
                  district + year,
                data = data_t, cluster = ~district)

  m2_t <- feols(enrolled ~ drought_t*drought_any_count5_t +
                  drought_t*school_phase +
                  drought_t*hh_income +
                  drought_t*gender +
                  drought_any_count5_t*school_phase +
                  drought_any_count5_t*hh_income +
                  drought_any_count5_t*gender |
                  district + year,
                data = data_t, cluster = ~district)

  m1_coef       <- coef(m1_t)["drought_t"]
  m1_se         <- se(m1_t)["drought_t"]

  m1_count_coef <- coef(m1_t)["drought_any_count5_t"]
  m1_count_se   <- se(m1_t)["drought_any_count5_t"]

  m2_coef      <- coef(m2_t)["drought_t"]
  m2_se        <- se(m2_t)["drought_t"]
  m2_spei_coef <- coef(m2_t)["drought_t:drought_any_count5_t"]
  m2_spei_se   <- se(m2_t)["drought_t:drought_any_count5_t"]

  coef_rows_black[[i]] <- data.frame(
    threshold = t,
    m1_coef = m1_coef, m1_se = m1_se,
    m1_count_coef = m1_count_coef, m1_count_se = m1_count_se,
    m2_coef = m2_coef, m2_se = m2_se,
    m2_spei_coef = m2_spei_coef, m2_spei_se = m2_spei_se,
    row.names = NULL
  )
}

coef_table_black <- bind_rows(coef_rows_black) %>%
  mutate(
    m1_lo         = m1_coef       - 1.96 * m1_se,
    m1_hi         = m1_coef       + 1.96 * m1_se,
    m1_count_lo   = m1_count_coef - 1.96 * m1_count_se,
    m1_count_hi   = m1_count_coef + 1.96 * m1_count_se,
    m2_lo         = m2_coef       - 1.96 * m2_se,
    m2_hi         = m2_coef       + 1.96 * m2_se,
    m2_spei_lo    = m2_spei_coef  - 1.96 * m2_spei_se,
    m2_spei_hi    = m2_spei_coef  + 1.96 * m2_spei_se
  )

write.csv(coef_table_black, "robustness_threshold_range_black_coefs.csv", row.names = FALSE)

ggplot(coef_table_black, aes(x = threshold, y = m1_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_lo, ymax = m1_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  annotate("text",
           x     = coef_table_black$threshold[which.max(coef_table_black$threshold)],
           y     = coef_table_black$m1_coef[which.max(coef_table_black$threshold)],
           label = "Black", hjust = 1, vjust = -0.6,
           size = 6, color = "steelblue", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_black$m1_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, vjust = -0.6, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on Drought (drought_t)",
       title = "M1: Drought Coefficient Across Thresholds — Black") +
  theme_clean

ggplot(coef_table_black, aes(x = threshold, y = m1_count_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_count_lo, ymax = m1_count_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "orange", linewidth = 1) +
  geom_point(color = "orange", size = 2) +
  annotate("text",
           x     = coef_table_black$threshold[which.max(coef_table_black$threshold)],
           y     = coef_table_black$m1_count_coef[which.max(coef_table_black$threshold)],
           label = "Black", hjust = 2, vjust = -1.5,
           size = 6, color = "orange", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_black$m1_count_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)",
       y = "Coefficient drought_any_count5_t",
       title = "M1: Cumulative Drought Count Coefficient Across Thresholds — Black") +
  theme_clean

ggplot(coef_table_black, aes(x = threshold, y = m2_spei_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m2_spei_lo, ymax = m2_spei_hi), alpha = 0.2, fill = "darkorchid") +
  geom_line(color = "darkorchid", linewidth = 1) +
  geom_point(color = "darkorchid", size = 2) +
  annotate("text",
           x     = coef_table_black$threshold[which.max(coef_table_black$threshold)],
           y     = coef_table_black$m2_spei_coef[which.max(coef_table_black$threshold)],
           label = "Black", hjust = 1, vjust = -0.6,
           size = 3.5, color = "darkorchid", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_black$m2_spei_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2, by = -0.5)) +
  coord_cartesian(xlim = c(-2, 0), ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Interaction: drought_t × drought_any_count5_t",
       title = "M2: Drought × Cumulative Drought Count Interaction Across Thresholds — Black") +
  theme_clean

# ------------------------------------------------------------------------------
# Robustness Test 2: Threshold range — Pre-primary subset
# ------------------------------------------------------------------------------

data_preprimary_t2   <- data_school %>% filter(school_phase == "Pre-primary")
coef_rows_preprimary <- vector("list", length(thresholds))

for (i in seq_along(thresholds)) {
  t <- round(thresholds[i], 1)

  data_t <- data_preprimary_t2 %>%
    mutate(
      drought_t            = as.integer(spei < t),
      drought_any_count5_t = as.integer(lag1 < t) + as.integer(lag2 < t) +
                             as.integer(lag3 < t) + as.integer(lag4 < t) +
                             as.integer(lag5 < t)
    )

  if (var(data_t$drought_t, na.rm = TRUE) == 0) {
    message("Pre-primary: skipping threshold ", t, " — no variation in drought indicator")
    next
  }

  m1_t <- feols(enrolled ~ drought_t + drought_any_count5_t +
                  hh_income + gender + race |
                  district + year,
                data = data_t, cluster = ~district)

  m2_t <- feols(enrolled ~ drought_t*drought_any_count5_t +
                  drought_t*hh_income +
                  drought_t*gender +
                  drought_t*race +
                  drought_any_count5_t*hh_income +
                  drought_any_count5_t*gender +
                  drought_any_count5_t*race |
                  district + year,
                data = data_t, cluster = ~district)

  m1_coef       <- coef(m1_t)["drought_t"]
  m1_se         <- se(m1_t)["drought_t"]

  m1_count_coef <- coef(m1_t)["drought_any_count5_t"]
  m1_count_se   <- se(m1_t)["drought_any_count5_t"]

  m2_coef      <- coef(m2_t)["drought_t"]
  m2_se        <- se(m2_t)["drought_t"]
  m2_spei_coef <- coef(m2_t)["drought_t:drought_any_count5_t"]
  m2_spei_se   <- se(m2_t)["drought_t:drought_any_count5_t"]

  coef_rows_preprimary[[i]] <- data.frame(
    threshold = t,
    m1_coef = m1_coef, m1_se = m1_se,
    m1_count_coef = m1_count_coef, m1_count_se = m1_count_se,
    m2_coef = m2_coef, m2_se = m2_se,
    m2_spei_coef = m2_spei_coef, m2_spei_se = m2_spei_se,
    row.names = NULL
  )
}

coef_table_preprimary <- bind_rows(coef_rows_preprimary) %>%
  mutate(
    m1_lo         = m1_coef       - 1.96 * m1_se,
    m1_hi         = m1_coef       + 1.96 * m1_se,
    m1_count_lo   = m1_count_coef - 1.96 * m1_count_se,
    m1_count_hi   = m1_count_coef + 1.96 * m1_count_se,
    m2_lo         = m2_coef       - 1.96 * m2_se,
    m2_hi         = m2_coef       + 1.96 * m2_se,
    m2_spei_lo    = m2_spei_coef  - 1.96 * m2_spei_se,
    m2_spei_hi    = m2_spei_coef  + 1.96 * m2_spei_se
  )

write.csv(coef_table_preprimary, "robustness_threshold_range_preprimary_coefs.csv", row.names = FALSE)

ggplot(coef_table_preprimary, aes(x = threshold, y = m1_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_lo, ymax = m1_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  annotate("text",
           x     = coef_table_preprimary$threshold[which.max(coef_table_preprimary$threshold)],
           y     = coef_table_preprimary$m1_coef[which.max(coef_table_preprimary$threshold)],
           label = "Pre-primary", hjust = 1, vjust = -3,
           size = 6, color = "steelblue", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_preprimary$m1_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on Drought (drought_t)",
       title = "M1: Drought Coefficient Across Thresholds — Pre-primary") +
  theme_clean

ggplot(coef_table_preprimary, aes(x = threshold, y = m1_count_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_count_lo, ymax = m1_count_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "orange", linewidth = 1) +
  geom_point(color = "orange", size = 2) +
  annotate("text",
           x     = coef_table_preprimary$threshold[which.max(coef_table_preprimary$threshold)],
           y     = coef_table_preprimary$m1_count_coef[which.max(coef_table_preprimary$threshold)],
           label = "Pre-primary", hjust = 1, vjust = -2.9,
           size = 6, color = "orange", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_preprimary$m1_count_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)",
       y = "Coefficient on drought_any_count5_t",
       title = "M1: Cumulative Drought Count Coefficient Across Thresholds — Pre-primary") +
  theme_clean

ggplot(coef_table_preprimary, aes(x = threshold, y = m2_spei_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m2_spei_lo, ymax = m2_spei_hi), alpha = 0.2, fill = "darkorchid") +
  geom_line(color = "darkorchid", linewidth = 1) +
  geom_point(color = "darkorchid", size = 2) +
  annotate("text",
           x     = coef_table_preprimary$threshold[which.max(coef_table_preprimary$threshold)],
           y     = coef_table_preprimary$m2_spei_coef[which.max(coef_table_preprimary$threshold)],
           label = "Pre-primary", hjust = 1, vjust = -0.6,
           size = 3.5, color = "darkorchid", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_preprimary$m2_spei_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2, by = -0.5)) +
  coord_cartesian(xlim = c(-2, 0), ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Interaction: drought_t × drought_any_count5_t",
       title = "M2: Drought × Cumulative Drought Count Interaction Across Thresholds — Pre-primary") +
  theme_clean

# ------------------------------------------------------------------------------
# Robustness Test 2: Threshold range — Coloured subset
# ------------------------------------------------------------------------------

data_coloured_t2    <- data_school %>% filter(race == "Coloured")
coef_rows_coloured  <- vector("list", length(thresholds))

for (i in seq_along(thresholds)) {
  t <- round(thresholds[i], 1)

  data_t <- data_coloured_t2 %>%
    mutate(
      drought_t            = as.integer(spei < t),
      drought_any_count5_t = as.integer(lag1 < t) + as.integer(lag2 < t) +
                             as.integer(lag3 < t) + as.integer(lag4 < t) +
                             as.integer(lag5 < t)
    )

  if (var(data_t$drought_t, na.rm = TRUE) == 0) {
    message("Coloured: skipping threshold ", t, " — no variation in drought indicator")
    next
  }

  m1_t <- feols(enrolled ~ drought_t + drought_any_count5_t +
                  school_phase + hh_income + gender |
                  district + year,
                data = data_t, cluster = ~district)

  m2_t <- feols(enrolled ~ drought_t*drought_any_count5_t +
                  drought_t*school_phase +
                  drought_t*hh_income +
                  drought_t*gender +
                  drought_any_count5_t*school_phase +
                  drought_any_count5_t*hh_income +
                  drought_any_count5_t*gender |
                  district + year,
                data = data_t, cluster = ~district)

  m1_coef       <- coef(m1_t)["drought_t"]
  m1_se         <- se(m1_t)["drought_t"]

  m1_count_coef <- coef(m1_t)["drought_any_count5_t"]
  m1_count_se   <- se(m1_t)["drought_any_count5_t"]

  m2_coef      <- coef(m2_t)["drought_t"]
  m2_se        <- se(m2_t)["drought_t"]
  m2_spei_coef <- coef(m2_t)["drought_t:drought_any_count5_t"]
  m2_spei_se   <- se(m2_t)["drought_t:drought_any_count5_t"]

  coef_rows_coloured[[i]] <- data.frame(
    threshold = t,
    m1_coef = m1_coef, m1_se = m1_se,
    m1_count_coef = m1_count_coef, m1_count_se = m1_count_se,
    m2_coef = m2_coef, m2_se = m2_se,
    m2_spei_coef = m2_spei_coef, m2_spei_se = m2_spei_se,
    row.names = NULL
  )
}

coef_table_coloured <- bind_rows(coef_rows_coloured) %>%
  mutate(
    m1_lo         = m1_coef       - 1.96 * m1_se,
    m1_hi         = m1_coef       + 1.96 * m1_se,
    m1_count_lo   = m1_count_coef - 1.96 * m1_count_se,
    m1_count_hi   = m1_count_coef + 1.96 * m1_count_se,
    m2_lo         = m2_coef       - 1.96 * m2_se,
    m2_hi         = m2_coef       + 1.96 * m2_se,
    m2_spei_lo    = m2_spei_coef  - 1.96 * m2_spei_se,
    m2_spei_hi    = m2_spei_coef  + 1.96 * m2_spei_se
  )

write.csv(coef_table_coloured, "robustness_threshold_range_coloured_coefs.csv", row.names = FALSE)

ggplot(coef_table_coloured, aes(x = threshold, y = m1_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_lo, ymax = m1_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  annotate("text",
           x     = coef_table_coloured$threshold[which.max(coef_table_coloured$threshold)],
           y     = coef_table_coloured$m1_coef[which.max(coef_table_coloured$threshold)],
           label = "Coloured", hjust = 1, vjust = -0.6,
           size = 3.5, color = "steelblue", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_coloured$m1_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on Drought (drought_t)",
       title = "M1: Drought Coefficient Across Thresholds — Coloured") +
  theme_clean

ggsave("robustness_threshold_m1_coloured_coefs.png", width = 8, height = 5, dpi = 150)

ggplot(coef_table_coloured, aes(x = threshold, y = m1_count_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_count_lo, ymax = m1_count_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "orange", linewidth = 1) +
  geom_point(color = "orange", size = 2) +
  annotate("text",
           x     = coef_table_coloured$threshold[which.max(coef_table_coloured$threshold)],
           y     = coef_table_coloured$m1_count_coef[which.max(coef_table_coloured$threshold)],
           label = "Coloured", hjust = 1, vjust = -0.6,
           size = 3.5, color = "orange", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_coloured$m1_count_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on drought_any_count5_t",
       title = "M1: Cumulative Drought Count Coefficient Across Thresholds — Coloured") +
  theme_clean

ggsave("robustness_threshold_m1_coloured_count_coefs.png", width = 8, height = 5, dpi = 150)

ggplot(coef_table_coloured, aes(x = threshold, y = m2_spei_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m2_spei_lo, ymax = m2_spei_hi), alpha = 0.2, fill = "darkorchid") +
  geom_line(color = "darkorchid", linewidth = 1) +
  geom_point(color = "darkorchid", size = 2) +
  annotate("text",
           x     = coef_table_coloured$threshold[which.max(coef_table_coloured$threshold)],
           y     = coef_table_coloured$m2_spei_coef[which.max(coef_table_coloured$threshold)],
           label = "Coloured", hjust = 1, vjust = -0.9,
           size = 6, color = "darkorchid", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_coloured$m2_spei_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2, by = -0.5)) +
  coord_cartesian(xlim = c(-2, 0), ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "drought_t × drought_any_count5_t",
       title = "M2: Drought × Cumulative Drought — Coloured") +
  theme_clean

# ------------------------------------------------------------------------------
# Robustness Test 2: Threshold range — Female subset
# ------------------------------------------------------------------------------

data_female_t2    <- data_school %>% filter(gender == "Female")
coef_rows_female  <- vector("list", length(thresholds))

for (i in seq_along(thresholds)) {
  t <- round(thresholds[i], 1)

  data_t <- data_female_t2 %>%
    mutate(
      drought_t            = as.integer(spei < t),
      drought_any_count5_t = as.integer(lag1 < t) + as.integer(lag2 < t) +
                             as.integer(lag3 < t) + as.integer(lag4 < t) +
                             as.integer(lag5 < t)
    )

  if (var(data_t$drought_t, na.rm = TRUE) == 0) {
    message("Female: skipping threshold ", t, " — no variation in drought indicator")
    next
  }

  m1_t <- feols(enrolled ~ drought_t + drought_any_count5_t +
                  school_phase + hh_income + race |
                  district + year,
                data = data_t, cluster = ~district)

  m2_t <- feols(enrolled ~ drought_t*drought_any_count5_t +
                  drought_t*school_phase +
                  drought_t*hh_income +
                  drought_t*race +
                  drought_any_count5_t*school_phase +
                  drought_any_count5_t*hh_income +
                  drought_any_count5_t*race |
                  district + year,
                data = data_t, cluster = ~district)

  m1_coef       <- coef(m1_t)["drought_t"]
  m1_se         <- se(m1_t)["drought_t"]

  m1_count_coef <- coef(m1_t)["drought_any_count5_t"]
  m1_count_se   <- se(m1_t)["drought_any_count5_t"]

  m2_coef      <- coef(m2_t)["drought_t"]
  m2_se        <- se(m2_t)["drought_t"]
  m2_spei_coef <- coef(m2_t)["drought_t:drought_any_count5_t"]
  m2_spei_se   <- se(m2_t)["drought_t:drought_any_count5_t"]

  coef_rows_female[[i]] <- data.frame(
    threshold = t,
    m1_coef = m1_coef, m1_se = m1_se,
    m1_count_coef = m1_count_coef, m1_count_se = m1_count_se,
    m2_coef = m2_coef, m2_se = m2_se,
    m2_spei_coef = m2_spei_coef, m2_spei_se = m2_spei_se,
    row.names = NULL
  )
}

coef_table_female <- bind_rows(coef_rows_female) %>%
  mutate(
    m1_lo         = m1_coef       - 1.96 * m1_se,
    m1_hi         = m1_coef       + 1.96 * m1_se,
    m1_count_lo   = m1_count_coef - 1.96 * m1_count_se,
    m1_count_hi   = m1_count_coef + 1.96 * m1_count_se,
    m2_lo         = m2_coef       - 1.96 * m2_se,
    m2_hi         = m2_coef       + 1.96 * m2_se,
    m2_spei_lo    = m2_spei_coef  - 1.96 * m2_spei_se,
    m2_spei_hi    = m2_spei_coef  + 1.96 * m2_spei_se
  )

write.csv(coef_table_female, "robustness_threshold_range_female_coefs.csv", row.names = FALSE)

ggplot(coef_table_female, aes(x = threshold, y = m1_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_lo, ymax = m1_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  annotate("text",
           x     = coef_table_female$threshold[which.max(coef_table_female$threshold)],
           y     = coef_table_female$m1_coef[which.max(coef_table_female$threshold)],
           label = "Female", hjust = 1, vjust = -0.9,
           size = 6, color = "steelblue", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_female$m1_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on Drought (drought_t)",
       title = "M1: Drought Coefficient Across Thresholds — Female") +
  theme_clean

ggplot(coef_table_female, aes(x = threshold, y = m1_count_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_count_lo, ymax = m1_count_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "orange", linewidth = 1) +
  geom_point(color = "orange", size = 2) +
  annotate("text",
           x     = coef_table_female$threshold[which.max(coef_table_female$threshold)],
           y     = coef_table_female$m1_count_coef[which.max(coef_table_female$threshold)],
           label = "Female", hjust = 1, vjust = -0.8,
           size = 6, color = "orange", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_female$m1_count_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on drought_any_count5_t",
       title = "M1: Cumulative Drought Count Coefficient Across Thresholds — Female") +
  theme_clean

ggplot(coef_table_female, aes(x = threshold, y = m2_spei_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m2_spei_lo, ymax = m2_spei_hi), alpha = 0.2, fill = "darkorchid") +
  geom_line(color = "darkorchid", linewidth = 1) +
  geom_point(color = "darkorchid", size = 2) +
  annotate("text",
           x     = coef_table_female$threshold[which.max(coef_table_female$threshold)],
           y     = coef_table_female$m2_spei_coef[which.max(coef_table_female$threshold)],
           label = "Female", hjust = 1, vjust = -0.6,
           size = 3.5, color = "darkorchid", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_female$m2_spei_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2, by = -0.5)) +
  coord_cartesian(xlim = c(-2, 0), ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Interaction: drought_t × drought_any_count5_t",
       title = "M2: Drought × Cumulative Drought Count Interaction Across Thresholds — Female") +
  theme_clean

# ------------------------------------------------------------------------------
# Robustness Test 2: Threshold range — Primary subset
# ------------------------------------------------------------------------------

data_primary_t2    <- data_school %>% filter(school_phase == "Primary")
coef_rows_primary  <- vector("list", length(thresholds))

for (i in seq_along(thresholds)) {
  t <- round(thresholds[i], 1)

  data_t <- data_primary_t2 %>%
    mutate(
      drought_t            = as.integer(spei < t),
      drought_any_count5_t = as.integer(lag1 < t) + as.integer(lag2 < t) +
                             as.integer(lag3 < t) + as.integer(lag4 < t) +
                             as.integer(lag5 < t)
    )

  if (var(data_t$drought_t, na.rm = TRUE) == 0) {
    message("Primary: skipping threshold ", t, " — no variation in drought indicator")
    next
  }

  m1_t <- feols(enrolled ~ drought_t + drought_any_count5_t +
                  hh_income + gender + race |
                  district + year,
                data = data_t, cluster = ~district)

  m2_t <- feols(enrolled ~ drought_t*drought_any_count5_t +
                  drought_t*hh_income +
                  drought_t*gender +
                  drought_t*race +
                  drought_any_count5_t*hh_income +
                  drought_any_count5_t*gender +
                  drought_any_count5_t*race |
                  district + year,
                data = data_t, cluster = ~district)

  m1_coef       <- coef(m1_t)["drought_t"]
  m1_se         <- se(m1_t)["drought_t"]

  m1_count_coef <- coef(m1_t)["drought_any_count5_t"]
  m1_count_se   <- se(m1_t)["drought_any_count5_t"]

  m2_coef      <- coef(m2_t)["drought_t"]
  m2_se        <- se(m2_t)["drought_t"]
  m2_spei_coef <- coef(m2_t)["drought_t:drought_any_count5_t"]
  m2_spei_se   <- se(m2_t)["drought_t:drought_any_count5_t"]

  coef_rows_primary[[i]] <- data.frame(
    threshold = t,
    m1_coef = m1_coef, m1_se = m1_se,
    m1_count_coef = m1_count_coef, m1_count_se = m1_count_se,
    m2_coef = m2_coef, m2_se = m2_se,
    m2_spei_coef = m2_spei_coef, m2_spei_se = m2_spei_se,
    row.names = NULL
  )
}

coef_table_primary <- bind_rows(coef_rows_primary) %>%
  mutate(
    m1_lo         = m1_coef       - 1.96 * m1_se,
    m1_hi         = m1_coef       + 1.96 * m1_se,
    m1_count_lo   = m1_count_coef - 1.96 * m1_count_se,
    m1_count_hi   = m1_count_coef + 1.96 * m1_count_se,
    m2_lo         = m2_coef       - 1.96 * m2_se,
    m2_hi         = m2_coef       + 1.96 * m2_se,
    m2_spei_lo    = m2_spei_coef  - 1.96 * m2_spei_se,
    m2_spei_hi    = m2_spei_coef  + 1.96 * m2_spei_se
  )

write.csv(coef_table_primary, "robustness_threshold_range_primary_coefs.csv", row.names = FALSE)

ggplot(coef_table_primary, aes(x = threshold, y = m1_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_lo, ymax = m1_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  annotate("text",
           x     = coef_table_primary$threshold[which.max(coef_table_primary$threshold)],
           y     = coef_table_primary$m1_coef[which.max(coef_table_primary$threshold)],
           label = "Primary", hjust = 1, vjust = -0.9,
           size = 6, color = "steelblue", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_primary$m1_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on Drought (drought_t)",
       title = "M1: Drought Coefficient Across Thresholds — Primary") +
  theme_clean

ggplot(coef_table_primary, aes(x = threshold, y = m1_count_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_count_lo, ymax = m1_count_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "orange", linewidth = 1) +
  geom_point(color = "orange", size = 2) +
  annotate("text",
           x     = coef_table_primary$threshold[which.max(coef_table_primary$threshold)],
           y     = coef_table_primary$m1_count_coef[which.max(coef_table_primary$threshold)],
           label = "Primary", hjust = 1, vjust = -0.9,
           size = 6, color = "orange", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_primary$m1_count_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on drought_any_count5_t",
       title = "M1: Cumulative Drought Count Coefficient Across Thresholds — Primary") +
  theme_clean

ggplot(coef_table_primary, aes(x = threshold, y = m2_spei_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m2_spei_lo, ymax = m2_spei_hi), alpha = 0.2, fill = "darkorchid") +
  geom_line(color = "darkorchid", linewidth = 1) +
  geom_point(color = "darkorchid", size = 2) +
  annotate("text",
           x     = coef_table_primary$threshold[which.max(coef_table_primary$threshold)],
           y     = coef_table_primary$m2_spei_coef[which.max(coef_table_primary$threshold)],
           label = "Primary", hjust = 1, vjust = -0.8,
           size = 6, color = "darkorchid", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_primary$m2_spei_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2, by = -0.5)) +
  coord_cartesian(xlim = c(-2, 0), ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "drought_t × drought_any_count5_t",
       title = "M2: Drought × Cumulative Drought Count Interaction Across Thresholds — Primary") +
  theme_clean

# ------------------------------------------------------------------------------
# Robustness Test 2: Threshold range — Secondary subset
# ------------------------------------------------------------------------------

data_secondary_t2    <- data_school %>% filter(school_phase == "Secondary")
coef_rows_secondary  <- vector("list", length(thresholds))

for (i in seq_along(thresholds)) {
  t <- round(thresholds[i], 1)

  data_t <- data_secondary_t2 %>%
    mutate(
      drought_t            = as.integer(spei < t),
      drought_any_count5_t = as.integer(lag1 < t) + as.integer(lag2 < t) +
                             as.integer(lag3 < t) + as.integer(lag4 < t) +
                             as.integer(lag5 < t)
    )

  if (var(data_t$drought_t, na.rm = TRUE) == 0) {
    message("Secondary: skipping threshold ", t, " — no variation in drought indicator")
    next
  }

  m1_t <- feols(enrolled ~ drought_t + drought_any_count5_t +
                  hh_income + gender + race |
                  district + year,
                data = data_t, cluster = ~district)

  m2_t <- feols(enrolled ~ drought_t*drought_any_count5_t +
                  drought_t*hh_income +
                  drought_t*gender +
                  drought_t*race +
                  drought_any_count5_t*hh_income +
                  drought_any_count5_t*gender +
                  drought_any_count5_t*race |
                  district + year,
                data = data_t, cluster = ~district)

  m1_coef       <- coef(m1_t)["drought_t"]
  m1_se         <- se(m1_t)["drought_t"]

  m1_count_coef <- coef(m1_t)["drought_any_count5_t"]
  m1_count_se   <- se(m1_t)["drought_any_count5_t"]

  m2_coef      <- coef(m2_t)["drought_t"]
  m2_se        <- se(m2_t)["drought_t"]
  m2_spei_coef <- coef(m2_t)["drought_t:drought_any_count5_t"]
  m2_spei_se   <- se(m2_t)["drought_t:drought_any_count5_t"]

  coef_rows_secondary[[i]] <- data.frame(
    threshold = t,
    m1_coef = m1_coef, m1_se = m1_se,
    m1_count_coef = m1_count_coef, m1_count_se = m1_count_se,
    m2_coef = m2_coef, m2_se = m2_se,
    m2_spei_coef = m2_spei_coef, m2_spei_se = m2_spei_se,
    row.names = NULL
  )
}

coef_table_secondary <- bind_rows(coef_rows_secondary) %>%
  mutate(
    m1_lo         = m1_coef       - 1.96 * m1_se,
    m1_hi         = m1_coef       + 1.96 * m1_se,
    m1_count_lo   = m1_count_coef - 1.96 * m1_count_se,
    m1_count_hi   = m1_count_coef + 1.96 * m1_count_se,
    m2_lo         = m2_coef       - 1.96 * m2_se,
    m2_hi         = m2_coef       + 1.96 * m2_se,
    m2_spei_lo    = m2_spei_coef  - 1.96 * m2_spei_se,
    m2_spei_hi    = m2_spei_coef  + 1.96 * m2_spei_se
  )

write.csv(coef_table_secondary, "robustness_threshold_range_secondary_coefs.csv", row.names = FALSE)

ggplot(coef_table_secondary, aes(x = threshold, y = m1_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_lo, ymax = m1_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  annotate("text",
           x     = coef_table_secondary$threshold[which.max(coef_table_secondary$threshold)],
           y     = coef_table_secondary$m1_coef[which.max(coef_table_secondary$threshold)],
           label = "Secondary", hjust = 1, vjust = -0.8,
           size = 6, color = "steelblue", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_secondary$m1_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on Drought (drought_t)",
       title = "M1: Drought Coefficient Across Thresholds — Secondary") +
  theme_clean

ggplot(coef_table_secondary, aes(x = threshold, y = m1_count_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m1_count_lo, ymax = m1_count_hi), alpha = 0.2, fill = "steelblue") +
  geom_line(color = "orange", linewidth = 1) +
  geom_point(color = "orange", size = 2) +
  annotate("text",
           x     = coef_table_secondary$threshold[which.max(coef_table_secondary$threshold)],
           y     = coef_table_secondary$m1_count_coef[which.max(coef_table_secondary$threshold)],
           label = "Secondary", hjust = 1, vjust = -0.9,
           size = 6, color = "orange", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_secondary$m1_count_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, vjust= -0.8, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2.5, by = -0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Coefficient on drought_any_count5_t",
       title = "M1: Cumulative Drought Count Coefficient Across Thresholds — Secondary") +
  theme_clean

ggplot(coef_table_secondary, aes(x = threshold, y = m2_spei_coef)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "red", alpha = 0.7) +
  geom_ribbon(aes(ymin = m2_spei_lo, ymax = m2_spei_hi), alpha = 0.2, fill = "darkorchid") +
  geom_line(color = "darkorchid", linewidth = 1) +
  geom_point(color = "darkorchid", size = 2) +
  annotate("text",
           x     = coef_table_secondary$threshold[which.max(coef_table_secondary$threshold)],
           y     = coef_table_secondary$m2_spei_coef[which.max(coef_table_secondary$threshold)],
           label = "Secondary", hjust = 1, vjust = -0.6,
           size = 3.5, color = "darkorchid", fontface = "bold") +
  annotate("text", x = -1.05, y = max(coef_table_secondary$m2_spei_hi, na.rm = TRUE),
           label = "Main spec\nthreshold", hjust = 1, size = 3, color = "red") +
  scale_x_continuous(breaks = seq(0, -2, by = -0.5)) +
  coord_cartesian(xlim = c(-2, 0), ylim = c(-0.1, 0.1)) +
  labs(x = "Drought Threshold (SPEI)", y = "Interaction: drought_t × drought_any_count5_t",
       title = "M2: Drought × Cumulative Drought Count Interaction Across Thresholds — Secondary") +
  theme_clean

# ==============================================================================
# ROBUSTNESS TEST 3: Alternative cumulative drought definitions
#
# Main spec uses spei_cumul: rolling 5-year sum of lagged SPEI values (continuous).
# This test substitutes two alternative cumulative measures into M2 and M4:
#
#   Di Falco et al. (2024): count of any-drought years (SPEI < -1) in the
#     preceding 1, 2, 3, 4, and 5 years (drought_any_count1 to _count5).
#     Tested separately for each window to show sensitivity to window choice.
#
#   Von Uexkull et al. (2016): number of consecutive preceding years with
#     SPEI < -1 (drought_consecutive, 0-5).
#
# M1 and M3 are unchanged (contemporaneous drought specification is the same).
# ==============================================================================

# ------------------------------------------------------------------------------
# Di Falco et al.: M2 and M4 for each count window (1 to 5 years)
# ------------------------------------------------------------------------------

difalco_windows <- 1:5
difalco_m1 <- vector("list", 5)
difalco_m2 <- vector("list", 5)

for (w in difalco_windows) {
  count_var <- paste0("drought_any_count", w)

  difalco_m1[[w]] <- feols(
    as.formula(paste0(
      "enrolled ~ drought + ", count_var,
      " + school_phase + hh_income + gender + race | district + year"
    )),
    data = data_school, cluster = ~district
  )

  difalco_m2[[w]] <- feols(
    as.formula(paste0(
      "enrolled ~ drought*", count_var,
      " + ", count_var, "*school_phase",
      " + ", count_var, "*hh_income",
      " + ", count_var, "*gender",
      " + ", count_var, "*race | district + year"
    )),
    data = data_school, cluster = ~district
  )
}

# View and export Di Falco M1 results (count windows 1-5 side by side)
etable(difalco_m1[[1]], difalco_m1[[2]], difalco_m1[[3]],
       difalco_m1[[4]], difalco_m1[[5]])

etable(difalco_m1[[1]], difalco_m1[[2]], difalco_m1[[3]],
       difalco_m1[[4]], difalco_m1[[5]],
       tex = TRUE, file = "robustness_difalco_m1_results.tex")

# View and export Di Falco M2 results (count windows 1-5 side by side)
etable(difalco_m2[[1]], difalco_m2[[2]], difalco_m2[[3]],
       difalco_m2[[4]], difalco_m2[[5]])

etable(difalco_m2[[1]], difalco_m2[[2]], difalco_m2[[3]],
       difalco_m2[[4]], difalco_m2[[5]],
       tex = TRUE, file = "robustness_difalco_m2_results.tex")

# ------------------------------------------------------------------------------
# Di Falco M4 by school phase
# Mirrors run_phase_models() in script 9: filter to each phase, drop school_phase
# as covariate, retain hh_income + gender + race and their interactions.
# ------------------------------------------------------------------------------

data_preprimary <- data_school %>% filter(school_phase == "Pre-primary")
data_primary    <- data_school %>% filter(school_phase == "Primary")
data_secondary  <- data_school %>% filter(school_phase == "Secondary")

phase_data <- list(
  preprimary = data_preprimary,
  primary    = data_primary,
  secondary  = data_secondary
)

# M1 by phase: drought + drought_any_countX as average effects, no interactions
difalco_phase_m1 <- lapply(phase_data, function(df) {
  models <- vector("list", 5)
  for (w in 1:5) {
    count_var <- paste0("drought_any_count", w)
    models[[w]] <- feols(
      as.formula(paste0(
        "enrolled ~ drought + ", count_var,
        " + hh_income + gender + race | district + year"
      )),
      data = df, cluster = ~district
    )
  }
  models
})

etable(difalco_phase_m1$preprimary[[1]], difalco_phase_m1$preprimary[[2]],
       difalco_phase_m1$preprimary[[3]], difalco_phase_m1$preprimary[[4]],
       difalco_phase_m1$preprimary[[5]])

etable(difalco_phase_m1$primary[[1]], difalco_phase_m1$primary[[2]],
       difalco_phase_m1$primary[[3]], difalco_phase_m1$primary[[4]],
       difalco_phase_m1$primary[[5]])

etable(difalco_phase_m1$secondary[[1]], difalco_phase_m1$secondary[[2]],
       difalco_phase_m1$secondary[[3]], difalco_phase_m1$secondary[[4]],
       difalco_phase_m1$secondary[[5]])

etable(difalco_phase_m1$preprimary[[1]], difalco_phase_m1$preprimary[[2]],
       difalco_phase_m1$preprimary[[3]], difalco_phase_m1$preprimary[[4]],
       difalco_phase_m1$preprimary[[5]],
       tex = TRUE, file = "robustness_difalco_m1_preprimary.tex")

etable(difalco_phase_m1$primary[[1]], difalco_phase_m1$primary[[2]],
       difalco_phase_m1$primary[[3]], difalco_phase_m1$primary[[4]],
       difalco_phase_m1$primary[[5]],
       tex = TRUE, file = "robustness_difalco_m1_primary.tex")

etable(difalco_phase_m1$secondary[[1]], difalco_phase_m1$secondary[[2]],
       difalco_phase_m1$secondary[[3]], difalco_phase_m1$secondary[[4]],
       difalco_phase_m1$secondary[[5]],
       tex = TRUE, file = "robustness_difalco_m1_secondary.tex")

# Storage: difalco_phase_m2[["primary"]][[3]] = M2, primary subset, 3-year window
difalco_phase_m2 <- lapply(phase_data, function(df) {
  models <- vector("list", 5)
  for (w in 1:5) {
    count_var <- paste0("drought_any_count", w)
    models[[w]] <- feols(
      as.formula(paste0(
        "enrolled ~ drought*", count_var,
        " + ", count_var, "*hh_income",
        " + ", count_var, "*gender",
        " + ", count_var, "*race | district + year"
      )),
      data = df, cluster = ~district
    )
  }
  models
})

# View results: one table per phase, all five windows side by side
etable(difalco_phase_m2$preprimary[[1]], difalco_phase_m2$preprimary[[2]],
       difalco_phase_m2$preprimary[[3]], difalco_phase_m2$preprimary[[4]],
       difalco_phase_m2$preprimary[[5]])

etable(difalco_phase_m2$primary[[1]], difalco_phase_m2$primary[[2]],
       difalco_phase_m2$primary[[3]], difalco_phase_m2$primary[[4]],
       difalco_phase_m2$primary[[5]])

etable(difalco_phase_m2$secondary[[1]], difalco_phase_m2$secondary[[2]],
       difalco_phase_m2$secondary[[3]], difalco_phase_m2$secondary[[4]],
       difalco_phase_m2$secondary[[5]])

# Export
etable(difalco_phase_m2$preprimary[[1]], difalco_phase_m2$preprimary[[2]],
       difalco_phase_m2$preprimary[[3]], difalco_phase_m2$preprimary[[4]],
       difalco_phase_m2$preprimary[[5]],
       tex = TRUE, file = "robustness_difalco_m2_preprimary.tex")

etable(difalco_phase_m2$primary[[1]], difalco_phase_m2$primary[[2]],
       difalco_phase_m2$primary[[3]], difalco_phase_m2$primary[[4]],
       difalco_phase_m2$primary[[5]],
       tex = TRUE, file = "robustness_difalco_m2_primary.tex")

etable(difalco_phase_m2$secondary[[1]], difalco_phase_m2$secondary[[2]],
       difalco_phase_m2$secondary[[3]], difalco_phase_m2$secondary[[4]],
       difalco_phase_m2$secondary[[5]],
       tex = TRUE, file = "robustness_difalco_m2_secondary.tex")

# ------------------------------------------------------------------------------
# Von Uexkull et al.: M2 and M4 with drought_consecutive
# ------------------------------------------------------------------------------

r3_vonuexkull_m1 <- feols(enrolled ~ drought + drought_consecutive +
                             school_phase + hh_income + gender + race |
                             district + year,
                           data = data_school, cluster = ~district)

r3_vonuexkull_m2 <- feols(enrolled ~ drought*drought_consecutive +
                             drought_consecutive*school_phase +
                             drought_consecutive*hh_income +
                             drought_consecutive*gender +
                             drought_consecutive*race |
                             district + year,
                           data = data_school, cluster = ~district)

etable(r3_vonuexkull_m1, r3_vonuexkull_m2)

etable(r3_vonuexkull_m1, r3_vonuexkull_m2,
       tex = TRUE, file = "robustness_vonuexkull_results.tex")

# ------------------------------------------------------------------------------
# Descriptive statistics: Di Falco counts and Von Uexkull consecutive
# ------------------------------------------------------------------------------

# Summary table: min, mean, median, max for each variable
desc_vars <- c("drought_any_count1", "drought_any_count2", "drought_any_count3",
               "drought_any_count4", "drought_any_count5", "drought_consecutive")

desc_stats <- data_school %>%
  summarise(across(
    all_of(desc_vars),
    list(
      min    = ~min(.x, na.rm = TRUE),
      mean   = ~round(mean(.x, na.rm = TRUE), 3),
      median = ~median(.x, na.rm = TRUE),
      max    = ~max(.x, na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("variable", "stat"),
               names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

print(desc_stats)
write.csv(desc_stats, "robustness_cumulative_desc_stats.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# Figure: average Di Falco count by year (one line per window)
# ------------------------------------------------------------------------------

counts_by_year <- data_school %>%
  group_by(year) %>%
  summarise(
    across(all_of(paste0("drought_any_count", 1:5)),
           ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("drought_any_count"),
    names_to  = "window",
    values_to = "mean_count"
  ) %>%
  mutate(window = dplyr::recode(window,
    "drought_any_count1" = "1-year",
    "drought_any_count2" = "2-year",
    "drought_any_count3" = "3-year",
    "drought_any_count4" = "4-year",
    "drought_any_count5" = "5-year"
  ))

ggplot(counts_by_year, aes(x = year, y = mean_count, colour = window)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_colour_brewer(palette = "Blues", direction = 1,
                      name = "Window") +
  scale_x_continuous(breaks = sort(unique(counts_by_year$year))) +
  labs(x = "Year", y = "Mean drought year count",
       title = "Di Falco: average cumulative drought count by year and window") +
  theme_clean +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("robustness_difalco_counts_by_year.png", width = 8, height = 5, dpi = 150)


# ==============================================================================
# ROBUSTNESS TEST 4: Continuous cumulative SPEI (spei_cumul)
#
# Main spec uses drought_any_count5: count of drought years in preceding 5 years.
# This test substitutes the continuous rolling 5-year sum of lagged SPEI values
# (spei_cumul) as the cumulative drought measure, mirroring the main spec
# structure exactly.
# ==============================================================================

# M1: Average effects
r4_m1 <- feols(enrolled ~ drought + spei_cumul +
                 school_phase + hh_income + gender + race |
                 district + year,
               data = data_school, cluster = ~district)

# M2: All interactions
r4_m2 <- feols(enrolled ~ drought*spei_cumul +
                 drought*school_phase +
                 drought*hh_income +
                 drought*gender +
                 drought*race +
                 spei_cumul*school_phase +
                 spei_cumul*hh_income +
                 spei_cumul*gender +
                 spei_cumul*race |
                 district + year,
               data = data_school, cluster = ~district)

etable(r4_m1, r4_m2)

etable(r4_m1, r4_m2,
       tex = TRUE, file = "robustness_spei_cumul_results.tex")

# By school phase
r4_phase_models <- lapply(phase_data, function(df) {
  m1 <- feols(enrolled ~ drought + spei_cumul +
                hh_income + gender + race |
                district + year,
              data = df, cluster = ~district)
  m2 <- feols(enrolled ~ drought*spei_cumul +
                drought*hh_income +
                drought*gender +
                drought*race +
                spei_cumul*hh_income +
                spei_cumul*gender +
                spei_cumul*race |
                district + year,
              data = df, cluster = ~district)
  list(m1 = m1, m2 = m2)
})

etable(r4_phase_models$preprimary$m1, r4_phase_models$preprimary$m2)
etable(r4_phase_models$primary$m1,    r4_phase_models$primary$m2)
etable(r4_phase_models$secondary$m1,  r4_phase_models$secondary$m2)

etable(r4_phase_models$preprimary$m1, r4_phase_models$preprimary$m2,
       tex = TRUE, file = "robustness_spei_cumul_preprimary.tex")
etable(r4_phase_models$primary$m1, r4_phase_models$primary$m2,
       tex = TRUE, file = "robustness_spei_cumul_primary.tex")
etable(r4_phase_models$secondary$m1, r4_phase_models$secondary$m2,
       tex = TRUE, file = "robustness_spei_cumul_secondary.tex")
