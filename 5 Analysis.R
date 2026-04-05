# 1. Libraries
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
  scale_y_continuous(labels = scales::percent, limits = c(0.5, 1)) +
  scale_x_continuous(breaks = seq(0, 30, 10)) +
  labs(x = "Age", y = "Enrollment Rate") +
  theme_clean

# Race
data_school %>%
  count(race) %>%
  mutate(pct = n / sum(n) * 100)

# Average annual enrollment rate by race
# For each race × year, compute enrollment rate; then average across years per race
enrollment_by_race_year <- data_school %>%
  filter(!is.na(race), !is.na(enrolled)) %>%
  group_by(race, year) %>%
  summarize(annual_rate = mean(enrolled, na.rm = TRUE), .groups = "drop") %>%
  group_by(race) %>%
  summarize(avg_annual_rate = mean(annual_rate), .groups = "drop") %>%
  mutate(race = factor(race, levels = c("Black", "Coloured", "White", "Asian/Indian")))

ggplot(enrollment_by_race_year, aes(x = race, y = avg_annual_rate, fill = race)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(avg_annual_rate, accuracy = 0.1)),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Race Group", y = "Average Annual Enrollment Rate") +
  theme_clean +
  theme(legend.position = "none")

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

# ATTRITION ANALYSIS
# Investigates whether sample attrition is systematic with respect to
# key control variables (race, age, income, gender) and drought exposure.
# Uses the enrollment panel (pre-merge with SPEI) to track NIDS wave presence.

# ---- 1. Build wave-presence flags from the merged analysis dataset ----
# Use `data` (not raw `enrollment`) since age is computed post-merge
wave_presence <- data %>%
  filter(age >= 5, age <= 20) %>%
  distinct(pid, wave) %>%
  mutate(present = 1) %>%
  pivot_wider(names_from = wave, values_from = present,
              names_prefix = "wave", values_fill = 0)

# Ensure all wave columns exist even if no obs in that wave
for (w in 1:5) {
  col <- paste0("wave", w)
  if (!col %in% names(wave_presence)) wave_presence[[col]] <- 0
}

# ---- 2. Derive attrition indicators ----
# "Attritor" = present in wave 1 but missing from wave 5
# "Ever attritor" = present in at least one wave but not all waves they could attend
wave_presence <- wave_presence %>%
  mutate(
    n_waves      = wave1 + wave2 + wave3 + wave4 + wave5,
    attrited_w5  = if_else(wave1 == 1 & wave5 == 0, 1L, 0L),  # wave1 present, wave5 absent
    ever_absent  = if_else(n_waves < 5, 1L, 0L)                # missed at least one wave
  )

# ---- 3. Merge baseline characteristics (wave 1 values) ----
baseline_chars <- data %>%
  filter(wave == 1, age >= 5, age <= 20) %>%
  group_by(pid) %>%
  slice(1) %>%  # one row per individual
  ungroup() %>%
  select(pid, race, gender, age, hh_income, district,
         drought, drought_moderate_count5, drought_extreme_count5)

attrition_df <- wave_presence %>%
  inner_join(baseline_chars, by = "pid") %>%
  mutate(
    race   = factor(race,   levels = c("Black", "Coloured", "White", "Asian/Indian")),
    gender = factor(gender, levels = c("Male", "Female"))
  )

# ---- 4. Descriptive comparison: wave 1 stayers vs. attritors (by wave 5) ----
cat("\n=== ATTRITION ANALYSIS: Wave 1 baseline characteristics ===\n")
cat("(Attritor = present in wave 1, absent from wave 5)\n\n")

attrition_summary <- attrition_df %>%
  filter(wave1 == 1) %>%
  group_by(attrited_w5) %>%
  summarise(
    n               = n(),
    pct_female      = mean(gender == "Female",   na.rm = TRUE) * 100,
    mean_age        = mean(age,                  na.rm = TRUE),
    pct_black       = mean(race == "Black",       na.rm = TRUE) * 100,
    pct_coloured    = mean(race == "Coloured",    na.rm = TRUE) * 100,
    pct_white       = mean(race == "White",       na.rm = TRUE) * 100,
    mean_hh_income  = mean(hh_income,             na.rm = TRUE),
    pct_missing_inc = mean(is.na(hh_income))      * 100,
    pct_no_drought  = mean(drought == "no drought",       na.rm = TRUE) * 100,
    pct_mod_drought = mean(drought == "moderate drought", na.rm = TRUE) * 100,
    pct_ext_drought = mean(drought == "extreme drought",  na.rm = TRUE) * 100,
    mean_mod_count  = mean(drought_moderate_count5, na.rm = TRUE),
    mean_ext_count  = mean(drought_extreme_count5,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(group = if_else(attrited_w5 == 1, "Attritor (W1 only)", "Stayer (W1 & W5)")) %>%
  select(group, everything(), -attrited_w5)

print(attrition_summary, width = 120)

# ---- 5. t-tests / chi-sq for key variables ----
cat("\n--- Significance tests (stayers vs. attritors) ---\n")

att_df_w1 <- attrition_df %>% filter(wave1 == 1)

# Continuous vars
for (v in c("age", "hh_income", "drought_moderate_count5", "drought_extreme_count5")) {
  tst <- t.test(att_df_w1[[v]] ~ att_df_w1$attrited_w5, na.action = na.omit)
  cat(sprintf("  %-35s  t = %6.3f,  p = %.4f\n", v, tst$statistic, tst$p.value))
}

# Categorical vars (including drought, which is a 3-level factor)
for (v in c("gender", "race", "drought")) {
  keep <- !is.na(att_df_w1[[v]]) & !is.na(att_df_w1$attrited_w5)
  tbl  <- table(droplevels(att_df_w1[[v]][keep]), att_df_w1$attrited_w5[keep])
  cst  <- chisq.test(tbl, simulate.p.value = TRUE)
  cat(sprintf("  %-35s  chi2 = %6.3f,  p = %.4f\n", v, cst$statistic, cst$p.value))
}

# ---- 6. Logit: predict attrition from baseline chars ----
cat("\n--- Logistic regression: P(attrition by wave 5 | wave 1 characteristics) ---\n")
attrition_logit <- glm(
  attrited_w5 ~ gender + age + race + log1p(hh_income) +
    drought + drought_moderate_count5 + drought_extreme_count5,
  data   = att_df_w1,
  family = binomial(link = "logit")
)
summary(attrition_logit)

# ---- 6b. Wave-by-wave dropout logits ----
# For each consecutive transition (Wn -> Wn+1), flag individuals present in
# wave n who are absent from wave n+1, using their characteristics at wave n
# as predictors. This tests whether the moderate drought effect from the
# W1->W5 logit is consistent across transitions or a one-off.

wave_pairs <- list(c(1, 2), c(2, 3), c(3, 4), c(4, 5))
wave_years <- c(1, 2, 3, 4, 5)  # wave numbers map to survey rounds

cat("\n=== WAVE-BY-WAVE DROPOUT LOGITS ===\n")
cat("Outcome: dropped out in next wave | present in current wave\n\n")

for (pair in wave_pairs) {
  w_from <- pair[1]
  w_to   <- pair[2]

  # Individuals present in wave w_from
  present_now <- data %>%
    filter(age >= 5, age <= 20, wave == w_from) %>%
    group_by(pid) %>%
    slice(1) %>%
    ungroup() %>%
    select(pid, gender, age, race, hh_income,
           drought, drought_moderate_count5, drought_extreme_count5)

  # Individuals present in wave w_to
  present_next <- data %>%
    filter(age >= 5, age <= 20, wave == w_to) %>%
    distinct(pid) %>%
    mutate(present_next = 1)

  # Flag dropouts
  transition_df <- present_now %>%
    left_join(present_next, by = "pid") %>%
    mutate(
      dropped = if_else(is.na(present_next), 1L, 0L),
      gender  = factor(gender, levels = c("Male", "Female"))
    )

  n_present  <- nrow(transition_df)
  n_dropped  <- sum(transition_df$dropped)
  drop_rate  <- round(n_dropped / n_present * 100, 1)

  cat(sprintf("--- Wave %d -> Wave %d  (n = %d, dropout rate = %s%%) ---\n",
              w_from, w_to, n_present, drop_rate))

  fit <- tryCatch(
    glm(dropped ~ gender + age + race + log1p(hh_income) +
          drought + drought_moderate_count5 + drought_extreme_count5,
        data = transition_df, family = binomial(link = "logit")),
    error = function(e) { cat("  Model failed:", conditionMessage(e), "\n"); NULL }
  )

  if (!is.null(fit)) {
    coef_tbl <- coef(summary(fit))
    # Print only drought-related rows plus a separator
    drought_rows <- grep("drought|Intercept", rownames(coef_tbl))
    print(round(coef_tbl[drought_rows, ], 4))
    cat("\n")
  }
}

# ---- 7. Wave-by-wave attrition rates by subgroup ----
cat("\n--- Wave-by-wave attrition rates by race ---\n")
wave_by_race <- data_school %>%
  group_by(race, wave) %>%
  summarise(n_individuals = n_distinct(pid), .groups = "drop") %>%
  group_by(race) %>%
  mutate(pct_of_wave1 = n_individuals / n_individuals[wave == 1] * 100) %>%
  ungroup()
print(wave_by_race, n = 40)

cat("\n--- Wave-by-wave attrition rates by gender ---\n")
wave_by_gender <- data_school %>%
  mutate(gender = factor(gender, levels = c(1, 2), labels = c("Male", "Female"))) %>%
  group_by(gender, wave) %>%
  summarise(n_individuals = n_distinct(pid), .groups = "drop") %>%
  group_by(gender) %>%
  mutate(pct_of_wave1 = n_individuals / n_individuals[wave == 1] * 100) %>%
  ungroup()
print(wave_by_gender)

# ---- 8. Visualise: attrition over waves by race ----
ggplot(wave_by_race %>% filter(!is.na(race)),
       aes(x = wave, y = pct_of_wave1, color = race, group = race)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(limits = c(0, 105), labels = scales::label_number(suffix = "%")) +
  scale_x_continuous(breaks = 1:5, labels = paste0("W", 1:5)) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "NIDS Wave", y = "% Retained (relative to Wave 1)",
       color = "Race") +
  theme_clean

# ---- 9. Visualise: mean baseline income, stayers vs attritors ----
att_income_plot <- att_df_w1 %>%
  filter(!is.na(hh_income), !is.na(race)) %>%
  mutate(group = if_else(attrited_w5 == 1, "Attritor", "Stayer")) %>%
  group_by(race, group) %>%
  summarise(mean_income = mean(hh_income, na.rm = TRUE), .groups = "drop")

ggplot(att_income_plot, aes(x = race, y = mean_income, fill = group)) +
  geom_col(position = "dodge", width = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("Attritor" = "#E07B7B", "Stayer" = "steelblue")) +
  labs(x = "Race", y = "Mean Household Income (Rand)", fill = NULL) +
  theme_clean

# Step 3 - Run regressions!

# Average effects: contemporaneous drought + cumulative counts split by severity
m1 <- feols(enrolled ~ drought + drought_moderate_count5 + drought_extreme_count5 +
              school_phase + hh_income + gender + race |
              district + year,
            data = data_school, cluster = ~district)

# All interactions: cumulative counts (split by severity) interacted with demographics
m2 <- feols(enrolled ~ drought*drought_moderate_count5 + drought*drought_extreme_count5 +
              drought*school_phase +
              drought*hh_income +
              drought*gender +
              drought*race +
              drought_moderate_count5*school_phase +
              drought_moderate_count5*hh_income +
              drought_moderate_count5*gender +
              drought_moderate_count5*race +
              drought_extreme_count5*school_phase +
              drought_extreme_count5*hh_income +
              drought_extreme_count5*gender +
              drought_extreme_count5*race |
              district + year,
            data = data_school, cluster = ~district)

# View results
etable(m1, m2)

# ── CUMULATIVE DROUGHT SPLIT: M1 COEFFICIENT PLOT ────────────────────────────
# Key figure for results: effect per additional moderate vs extreme drought year
# in the past 5 years on P(Enrolled), from M1 (average effects, no interactions)

cumul_split_colors <- c("Moderate drought years" = "#FDB863",
                        "Extreme drought years"  = "#D7191C")

cumul_split_terms <- c("drought_moderate_count5", "drought_extreme_count5")
cumul_split_coefs <- tibble(
  term     = cumul_split_terms,
  label    = c("Moderate drought years", "Extreme drought years"),
  estimate = coef(m1)[cumul_split_terms],
  conf.low  = confint(m1)[cumul_split_terms, 1],
  conf.high = confint(m1)[cumul_split_terms, 2]
)

p_cumul_split <- ggplot(cumul_split_coefs,
                        aes(x = estimate, y = label, color = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.7) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(values = cumul_split_colors) +
  labs(x = "Effect on P(Enrolled) per additional drought year (past 5 years)",
       y = NULL,
       subtitle = "M1 (no interactions); district + year FE; SEs clustered by district") +
  theme_clean +
  theme(legend.position = "none")

p_cumul_split
ggsave("cumulative_drought_split_coefs.png", p_cumul_split,
       width = 6, height = 2.5, dpi = 300)

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

# 4 Plot results 
# ── COLOUR PALETTES ───────────────────────────────────────────────────────────

drought_colors <- c("No drought"       = "#74ADD1",
                    "Moderate drought" = "#FDB863",
                    "Extreme drought"  = "#D7191C")

count_colors <- setNames(
  scales::seq_gradient_pal("#C6DBEF", "#08306B")(seq(0, 1, length.out = 6)),
  as.character(0:5)
)

# ── AVERAGE MARGINAL EFFECTS (AME) ────────────────────────────────────────────
# avg_slopes() gives the AME of moving from the reference (no drought) to each
# drought level, and the AME of a 1-unit increase in drought_any_count5.
# These directly represent the regression coefficients in probability terms,
# averaged over the observed distribution of all other covariates.

# AME PLOT 1: AME of drought by race
# Uses m2, which includes drought*race interaction — this is what allows the
# drought effect to genuinely differ across race groups. Using m1 here would
# produce the same estimate for every race since drought is only a control there.
ame_drought_race <- avg_slopes(m2, variables = "drought",
                               by = c("contrast", "race")) %>%
  mutate(race = factor(race, levels = c("Black", "Coloured", "White", "Asian/Indian")))

ggplot(ame_drought_race, aes(x = estimate, y = contrast, color = race)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(0.6), size = 0.6) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2", name = "Race") +
  labs(x = "Avg. Marginal Effect on P(Enrolled)", y = NULL) +
  theme_clean +
  theme(legend.position = "right")

# AME PLOT 2: AME of drought by gender
# Uses m2, which includes drought*gender interaction.
ame_drought_gender <- avg_slopes(m2, variables = "drought",
                                 by = c("contrast", "gender")) %>%
  mutate(gender = factor(gender, levels = c("Female", "Male")))

ggplot(ame_drought_gender, aes(x = estimate, y = contrast, color = gender)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(0.6), size = 0.6) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2", name = "Gender") +
  labs(x = "Avg. Marginal Effect on P(Enrolled)", y = NULL) +
  theme_clean +
  theme(legend.position = "right")

# AME PLOT 3: AME of drought by school phase
# Uses m2, which includes drought*school_phase interaction.
ame_drought_phase <- avg_slopes(m2, variables = "drought",
                                by = c("contrast", "school_phase")) %>%
  mutate(school_phase = factor(school_phase,
                               levels = c("Pre-primary", "Primary", "Secondary")))

ggplot(ame_drought_phase, aes(x = estimate, y = contrast, color = school_phase)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(0.6), size = 0.6) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2", name = "School Phase") +
  labs(x = "Avg. Marginal Effect on P(Enrolled)", y = NULL) +
  theme_clean +
  theme(legend.position = "right")

# ── COUNTERFACTUAL PREDICTIONS ────────────────────────────────────────────────
# avg_predictions() sets drought to each level for every observation, computes
# predicted enrollment, then averages — giving the expected enrollment rate if
# the entire sample were exposed to each drought condition.
# vcov = FALSE required for feols: FE uncertainty cannot be propagated through
# predictions, so point estimates only (no CIs).

relabel_drought <- function(df) {
  df %>% mutate(drought = factor(drought,
                                 levels = c("no drought", "moderate drought", "extreme drought"),
                                 labels = c("No drought", "Moderate drought", "Extreme drought")))
}

# CF PLOT 1: Overall counterfactual predictions by drought level
cf_drought <- avg_predictions(m1, variables = "drought", vcov = FALSE) %>%
  relabel_drought()

ggplot(cf_drought, aes(x = drought, y = estimate, color = drought)) +
  geom_point(size = 4) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = drought_colors) +
  labs(x = "Drought Status", y = "Avg. Predicted P(Enrolled)") +
  theme_clean +
  theme(legend.position = "none")

# CF PLOT 2: Counterfactual predictions by drought level and race
cf_drought_race <- avg_predictions(m1, variables = "drought", vcov = FALSE,
                                   by = c("drought", "race")) %>%
  relabel_drought() %>%
  mutate(race = factor(race, levels = c("Black", "Coloured", "White", "Asian/Indian")))

ggplot(cf_drought_race, aes(x = race, y = estimate, color = drought)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = drought_colors, name = "Drought Status") +
  labs(x = "Race Group", y = "Avg. Predicted P(Enrolled)") +
  theme_clean +
  theme(legend.position = "bottom")

# CF PLOT 3: Counterfactual predictions by drought level and gender
cf_drought_gender <- avg_predictions(m1, variables = "drought", vcov = FALSE,
                                     by = c("drought", "gender")) %>%
  relabel_drought() %>%
  mutate(gender = factor(gender, levels = c("Female", "Male")))

ggplot(cf_drought_gender, aes(x = gender, y = estimate, color = drought)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = drought_colors, name = "Drought Status") +
  labs(x = "Gender", y = "Avg. Predicted P(Enrolled)") +
  theme_clean +
  theme(legend.position = "bottom")

# CF PLOT 4: Counterfactual predictions by drought level and school phase
cf_drought_phase <- avg_predictions(m1, variables = "drought", vcov = FALSE,
                                    by = c("drought", "school_phase")) %>%
  relabel_drought() %>%
  mutate(school_phase = factor(school_phase,
                               levels = c("Pre-primary", "Primary", "Secondary")))

ggplot(cf_drought_phase, aes(x = school_phase, y = estimate, color = drought)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = drought_colors, name = "Drought Status") +
  labs(x = "School Phase", y = "Avg. Predicted P(Enrolled)") +
  theme_clean +
  theme(legend.position = "bottom")

# CF PLOT 5: Counterfactual predictions across cumulative drought count (0–5),
# split by moderate and extreme severity.
# Each panel fixes one count variable at its observed values while varying the other.

cf_count_mod <- avg_predictions(m1, vcov = FALSE,
                                variables = list(drought_moderate_count5 = 0:5)) %>%
  mutate(drought_count = factor(drought_moderate_count5, levels = 0:5),
         severity = "Moderate drought years")

cf_count_ext <- avg_predictions(m1, vcov = FALSE,
                                variables = list(drought_extreme_count5 = 0:5)) %>%
  mutate(drought_count = factor(drought_extreme_count5, levels = 0:5),
         severity = "Extreme drought years")

cf_count_split <- bind_rows(cf_count_mod, cf_count_ext) %>%
  mutate(severity = factor(severity,
                           levels = c("Moderate drought years", "Extreme drought years")))

ggplot(cf_count_split, aes(x = drought_count, y = estimate, color = severity, group = severity)) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("Moderate drought years" = "#FDB863",
                                "Extreme drought years"  = "#D7191C"),
                     name = NULL) +
  labs(x = "Drought Years in Past 5", y = "Avg. Predicted P(Enrolled)") +
  theme_clean +
  theme(legend.position = "bottom")

# ── SCHOOL PHASE: COEFFICIENTS FROM SUBSET MODELS ────────────────────────────
# Each panel shows drought coefficients from a model fit only on that phase's
# students. Panels are NOT meant for cross-phase comparison — each is an
# independent estimate with its own sample, FEs, and covariate distribution.

get_drought_coefs <- function(model, phase) {
  cfs <- coef(model)
  cis <- confint(model)
  tibble(
    term         = names(cfs),
    estimate     = cfs,
    conf.low     = cis[, 1],
    conf.high    = cis[, 2],
    school_phase = phase
  ) %>%
    filter(str_detect(term, "^drought"),
           !term %in% c("drought_moderate_count5", "drought_extreme_count5")) %>%
    mutate(term = case_match(term,
                             "droughtmoderate drought"  ~ "Moderate drought",
                             "droughtextreme drought"   ~ "Extreme drought",
                             .default = term))
}

phase_coefs <- bind_rows(
  get_drought_coefs(models_preprimary$m1, "Pre-primary"),
  get_drought_coefs(models_primary$m1,    "Primary"),
  get_drought_coefs(models_secondary$m1,  "Secondary")
) %>%
  mutate(school_phase = factor(school_phase,
                               levels = c("Pre-primary", "Primary", "Secondary")))

ggplot(phase_coefs, aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), size = 0.6) +
  facet_wrap(~ school_phase, ncol = 3) +
  scale_x_continuous(labels = scales::percent, breaks = scales::breaks_width(0.05)) +
  labs(x = "Effect on P(Enrolled)", y = NULL,
       subtitle = "Phase-specific subset models — independent estimates, not cross-phase comparisons") +
  theme_clean

# ── BOOTSTRAP UNCERTAINTY FOR COUNTERFACTUAL PREDICTIONS ─────────────────────
# Cluster bootstrap resampling districts (the clustering unit in feols).
# Each replicated district gets a unique ID so feols treats it as a new unit.
# B = 200 for initial runs; increase to 500 for publication quality.
# Approximate run time: 10-30 mins depending on hardware.
# This section re-makes CF plots 1-4 with percentile-based 95% CIs.

vars_m1   <- c("enrolled", "drought", "drought_moderate_count5", "drought_extreme_count5",
               "school_phase", "hh_income", "gender", "race", "district", "year")
data_m1   <- data_school[complete.cases(data_school[, vars_m1]), ]
districts <- unique(data_m1$district)
B         <- 200
set.seed(42)

boot_overall <- boot_race <- boot_gender <- boot_phase <- vector("list", B)

for (b in seq_len(B)) {
  if (b %% 25 == 0) cat("Bootstrap iteration", b, "/", B, "\n")

  sampled   <- sample(districts, length(districts), replace = TRUE)
  boot_data <- map_dfr(seq_along(sampled), ~{
    data_m1 %>%
      filter(district == sampled[.x]) %>%
      mutate(district_b = paste0(district, "_", .x))
  })

  m_b <- tryCatch(
    feols(enrolled ~ drought + drought_moderate_count5 + drought_extreme_count5 +
            school_phase + hh_income + gender + race |
            district_b + year,
          data = boot_data, cluster = ~district_b,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )

  if (!is.null(m_b)) {
    boot_overall[[b]] <- avg_predictions(m_b, variables = "drought", vcov = FALSE) %>%
      select(drought, estimate)
    boot_race[[b]]    <- avg_predictions(m_b, variables = "drought", vcov = FALSE,
                                         by = c("drought", "race")) %>%
      select(drought, race, estimate)
    boot_gender[[b]]  <- avg_predictions(m_b, variables = "drought", vcov = FALSE,
                                         by = c("drought", "gender")) %>%
      select(drought, gender, estimate)
    boot_phase[[b]]   <- avg_predictions(m_b, variables = "drought", vcov = FALSE,
                                         by = c("drought", "school_phase")) %>%
      select(drought, school_phase, estimate)
  }
}

# Percentile CIs: 2.5th–97.5th across bootstrap estimates
pct_ci <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(conf.low  = quantile(estimate, 0.025, na.rm = TRUE),
              conf.high = quantile(estimate, 0.975, na.rm = TRUE),
              .groups = "drop")
}

ci_overall <- pct_ci(bind_rows(boot_overall), drought) %>% relabel_drought()
ci_race    <- pct_ci(bind_rows(boot_race),    drought, race) %>% relabel_drought()
ci_gender  <- pct_ci(bind_rows(boot_gender),  drought, gender) %>% relabel_drought()
ci_phase   <- pct_ci(bind_rows(boot_phase),   drought, school_phase) %>% relabel_drought()

# Join CIs to existing point estimates
cf_drought_boot       <- left_join(cf_drought,       ci_overall, by = "drought")
cf_drought_race_boot  <- left_join(cf_drought_race,  ci_race,    by = c("drought", "race"))
cf_drought_gender_boot <- left_join(cf_drought_gender, ci_gender, by = c("drought", "gender"))
cf_drought_phase_boot <- left_join(cf_drought_phase, ci_phase,   by = c("drought", "school_phase"))

boot_subtitle <- paste0("Bootstrap 95% CIs (B = ", B, ", cluster = district)")

# CF BOOT PLOT 1: Overall
ggplot(cf_drought_boot, aes(x = drought, y = estimate, color = drought)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = drought_colors) +
  labs(x = "Drought Status", y = "Avg. Predicted P(Enrolled)",
       subtitle = boot_subtitle) +
  theme_clean +
  theme(legend.position = "none")

# CF BOOT PLOT 2: By race
ggplot(cf_drought_race_boot, aes(x = race, y = estimate, color = drought)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(0.5), size = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = drought_colors, name = "Drought Status") +
  labs(x = "Race Group", y = "Avg. Predicted P(Enrolled)",
       subtitle = boot_subtitle) +
  theme_clean +
  theme(legend.position = "bottom")

# CF BOOT PLOT 3: By gender
ggplot(cf_drought_gender_boot, aes(x = gender, y = estimate, color = drought)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(0.5), size = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = drought_colors, name = "Drought Status") +
  labs(x = "Gender", y = "Avg. Predicted P(Enrolled)",
       subtitle = boot_subtitle) +
  theme_clean +
  theme(legend.position = "bottom")

# CF BOOT PLOT 4: By school phase
ggplot(cf_drought_phase_boot, aes(x = school_phase, y = estimate, color = drought)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(0.5), size = 0.7) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = drought_colors, name = "Drought Status") +
  labs(x = "School Phase", y = "Avg. Predicted P(Enrolled)",
       subtitle = boot_subtitle) +
  theme_clean +
  theme(legend.position = "bottom")

# ── CUMULATIVE DROUGHT: AME AND CF BY SUBGROUP ────────────────────────────────
# Uses m2 throughout: drought_any_count5 is interacted with race, gender, and
# school_phase in m2, so subgroup slopes are genuinely heterogeneous.
# CF plots use geom_line + geom_point to show the trajectory across count 0-5.

# ── AME: 1-unit increase in cumulative count (split by severity), by subgroup ─
# Both moderate and extreme counts are now separate variables in m2.
# Bind them together for a split-severity view within each subgroup.

bind_ame_counts <- function(by_var) {
  bind_rows(
    avg_slopes(m2, variables = "drought_moderate_count5", by = by_var) %>%
      mutate(severity = "Moderate drought years"),
    avg_slopes(m2, variables = "drought_extreme_count5",  by = by_var) %>%
      mutate(severity = "Extreme drought years")
  ) %>%
    mutate(severity = factor(severity,
                             levels = c("Moderate drought years", "Extreme drought years")))
}

ame_count_race   <- bind_ame_counts("race") %>%
  mutate(race = factor(race, levels = c("Black", "Coloured", "White", "Asian/Indian")))
ame_count_gender <- bind_ame_counts("gender") %>%
  mutate(gender = factor(gender, levels = c("Female", "Male")))
ame_count_phase  <- bind_ame_counts("school_phase") %>%
  mutate(school_phase = factor(school_phase,
                               levels = c("Pre-primary", "Primary", "Secondary")))

# AME COUNT PLOT 1: By race
ggplot(ame_count_race, aes(x = estimate, y = race, color = severity)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(0.5), size = 0.6) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(values = c("Moderate drought years" = "#FDB863",
                                "Extreme drought years"  = "#D7191C"),
                     name = "Severity") +
  labs(x = "AME on P(Enrolled) per additional drought year", y = NULL) +
  theme_clean +
  theme(legend.position = "right")

# AME COUNT PLOT 2: By gender
ggplot(ame_count_gender, aes(x = estimate, y = gender, color = severity)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(0.5), size = 0.6) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(values = c("Moderate drought years" = "#FDB863",
                                "Extreme drought years"  = "#D7191C"),
                     name = "Severity") +
  labs(x = "AME on P(Enrolled) per additional drought year", y = NULL) +
  theme_clean +
  theme(legend.position = "right")

# AME COUNT PLOT 3: By school phase
ggplot(ame_count_phase, aes(x = estimate, y = school_phase, color = severity)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(0.5), size = 0.6) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(values = c("Moderate drought years" = "#FDB863",
                                "Extreme drought years"  = "#D7191C"),
                     name = "Severity") +
  labs(x = "AME on P(Enrolled) per additional drought year", y = NULL) +
  theme_clean +
  theme(legend.position = "right")

# ── CF: Average predicted P(Enrolled) at each count level, by subgroup ────────
# Split by severity: vary one count variable at a time, facet by severity.

build_cf_count_by <- function(by_var, by_levels) {
  bind_rows(
    avg_predictions(m2, vcov = FALSE,
                    variables = list(drought_moderate_count5 = 0:5),
                    by = c("drought_moderate_count5", by_var)) %>%
      rename(drought_count_val = drought_moderate_count5) %>%
      mutate(severity = "Moderate drought years"),
    avg_predictions(m2, vcov = FALSE,
                    variables = list(drought_extreme_count5 = 0:5),
                    by = c("drought_extreme_count5", by_var)) %>%
      rename(drought_count_val = drought_extreme_count5) %>%
      mutate(severity = "Extreme drought years")
  ) %>%
    mutate(
      drought_count = factor(drought_count_val, levels = 0:5),
      severity = factor(severity,
                        levels = c("Moderate drought years", "Extreme drought years")),
      !!by_var := factor(.data[[by_var]], levels = by_levels)
    )
}

cf_count_race   <- build_cf_count_by("race",
                    c("Black", "Coloured", "White", "Asian/Indian"))
cf_count_gender <- build_cf_count_by("gender",
                    c("Female", "Male"))
cf_count_phase  <- build_cf_count_by("school_phase",
                    c("Pre-primary", "Primary", "Secondary"))

# CF COUNT PLOT 1: By race
ggplot(cf_count_race, aes(x = drought_count, y = estimate,
                          color = race, group = race)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ severity) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2", name = "Race") +
  labs(x = "Drought Years in Past 5", y = "Avg. Predicted P(Enrolled)") +
  theme_clean +
  theme(legend.position = "right")

# CF COUNT PLOT 2: By gender
ggplot(cf_count_gender, aes(x = drought_count, y = estimate,
                            color = gender, group = gender)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ severity) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2", name = "Gender") +
  labs(x = "Drought Years in Past 5", y = "Avg. Predicted P(Enrolled)") +
  theme_clean +
  theme(legend.position = "right")

# CF COUNT PLOT 3: By school phase
ggplot(cf_count_phase, aes(x = drought_count, y = estimate,
                           color = school_phase, group = school_phase)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ severity) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_brewer(palette = "Set2", name = "School Phase") +
  labs(x = "Drought Years in Past 5", y = "Avg. Predicted P(Enrolled)") +
  theme_clean +
  theme(legend.position = "right")

