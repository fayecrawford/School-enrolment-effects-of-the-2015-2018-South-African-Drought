library(tidyverse)

# ============================================================================
# NIDS ENROLLMENT PANEL CONSTRUCTION
# Reshapes NIDS waves 1-5 enrollment data from wide to long format
# Creates person-year panel for school enrollment analysis
# ============================================================================

# STEP 1: LOAD DATA --------------------------------------------------------
chh_w1 <- read.csv("Child_Household_W1.csv")
chh_w2 <- read.csv("Child_Household_W2.csv")
chh_w3 <- read.csv("Child_Household_W3.csv")
chh_w4 <- read.csv("Child_Household_W4.csv")
chh_w5 <- read.csv("Child_Household_W5.csv")

ahh_w1 <- read.csv("Adult_Household_W1.csv")
ahh_w2 <- read.csv("Adult_Household_W2.csv")
ahh_w3 <- read.csv("Adult_Household_W3.csv")
ahh_w4 <- read.csv("Adult_Household_W4.csv")
ahh_w5 <- read.csv("Adult_Household_W5.csv")

# STEP 2: HELPER FUNCTION TO RECODE ENROLLMENT ----------------------------
recode_enrollment <- function(x) {
  case_when(
    x == 1 ~ 1,
    x == 2 ~ 0,
    x %in% c(-9, -8, -5, -3, -2) ~ NA_real_,
    is.na(x) ~ NA_real_,
    TRUE ~ NA_real_
  )
}

# HELPER FUNCTION TO RECODE DATE OF BIRTH ---------------------------------
recode_dob <- function(x) {
  case_when(
    x %in% c(2222, 3333, 5555, 8888, 9999) ~ NA_real_,
    is.na(x) ~ NA_real_,
    TRUE ~ as.numeric(x)
  )
}

# HELPER FUNCTION TO CONVERT GRADES TO SCHOOL PHASES 
grade_to_phase <- function(grade) {
  case_when(
    is.na(grade) ~ NA_character_,
    grade < 0 ~ NA_character_,
    grade >= 23 ~ NA_character_,
    grade == 0 ~ "Pre-primary",
    grade >= 1 & grade <= 7 ~ "Primary",
    grade >= 8 & grade <= 12 ~ "Secondary",
    grade >= 13 & grade <= 22 ~ "Post-secondary",
    TRUE ~ NA_character_
  )
}

# HELPER FUNCTION TO RECODE PHASE
recode_phase <- function(phase_raw) {
  case_when(
    phase_raw == -3 ~ NA_character_,
    phase_raw == 1 ~ "Primary",
    phase_raw == 2 ~ "Pre-primary",
    phase_raw == 3 ~ "Pre-primary",
    phase_raw == 4 ~ "Pre-primary",
    phase_raw == 5 ~ "Pre-primary",
    phase_raw == 6 ~ "Other",
    phase_raw == 7 ~ NA_character_,
    TRUE ~ NA_character_
  )
}

# STEP 3: RESHAPE WAVE 1 (2008) --------------------------------------------
reshape_wave1 <- function() {
  w1_child <- chh_w1 %>%
    select(pid, 
           w1_c_ed07att, w1_c_ed08cur, w1_c_ed08curgrd, w1_c_ed08curlev,
           w1_c_dob_m, w1_c_dob_y, w1_c_gen, w1_c_popgrp,
           w1_hhid, w1_mdbdc2011, w1_prov2011, w1_geo2011,
           w1_hhincome) %>%
    pivot_longer(
      cols = c(w1_c_ed07att, w1_c_ed08cur),
      names_to = "var",
      values_to = "enrolled_raw"
    ) %>%
    mutate(
      year = case_when(
        str_detect(var, "07") ~ 2007,
        str_detect(var, "08") ~ 2008
      ),
      enrolled = recode_enrollment(enrolled_raw),
      phase_from_curlev = if_else(year == 2008, 
                                  grade_to_phase(w1_c_ed08curlev), 
                                  NA_character_),
      phase_from_curgrd = if_else(year == 2008, 
                                  recode_phase(w1_c_ed08curgrd), 
                                  NA_character_),
      school_phase = case_when(
        year == 2008 ~ coalesce(phase_from_curlev, phase_from_curgrd),
        year == 2007 ~ grade_to_phase(w1_c_ed08curlev - 1),
        TRUE ~ NA_character_
      ),
      wave = 1,
      questionnaire = "child",
      dob_year = w1_c_dob_y,
      dob_month = w1_c_dob_m,
      gender = w1_c_gen,
      race = w1_c_popgrp,
      hhid = w1_hhid,
      district = w1_mdbdc2011,
      province = w1_prov2011,
      geotype = w1_geo2011,
      hh_income = w1_hhincome
    ) %>%
    select(pid, year, enrolled, school_phase, wave, questionnaire, 
           dob_year, dob_month, gender, race, hhid, district, province, geotype,
           hh_income)
  
  w1_adult <- ahh_w1 %>%
    select(pid,
           w1_a_ed07att, w1_a_ed07lev, w1_a_ed08cur, w1_a_ed08curlev,
           w1_a_dob_m, w1_a_dob_y, w1_a_gen, w1_a_popgrp,
           w1_hhid, w1_mdbdc2011, w1_prov2011, w1_geo2011,
           w1_hhincome) %>%
    pivot_longer(
      cols = c(w1_a_ed07att, w1_a_ed08cur),
      names_to = "var",
      values_to = "enrolled_raw"
    ) %>%
    mutate(
      year = case_when(
        str_detect(var, "07") ~ 2007,
        str_detect(var, "08") ~ 2008
      ),
      enrolled = recode_enrollment(enrolled_raw),
      grade_raw = case_when(
        year == 2007 ~ w1_a_ed07lev,
        year == 2008 ~ w1_a_ed08curlev
      ),
      school_phase = grade_to_phase(grade_raw),
      wave = 1,
      questionnaire = "adult",
      dob_year = w1_a_dob_y,
      dob_month = w1_a_dob_m,
      gender = w1_a_gen,
      race = w1_a_popgrp,
      hhid = w1_hhid,
      district = w1_mdbdc2011,
      province = w1_prov2011,
      geotype = w1_geo2011,
      hh_income = w1_hhincome
    ) %>%
    select(pid, year, enrolled, school_phase, wave, questionnaire,
           dob_year, dob_month, gender, race, hhid, district, province, geotype,
           hh_income)
  
  bind_rows(w1_child, w1_adult)
}

# STEP 4: RESHAPE WAVE 2 (2010-2011) --------------------------------------
reshape_wave2 <- function() {
  w2_child <- chh_w2 %>%
    select(pid,
           w2_c_ed08att, w2_c_ed09att, w2_c_ed10cur,
           w2_c_ed09lev, w2_c_ed10curlev, w2_c_ed10curgrd,
           w2_c_dob_m, w2_c_dob_y, w2_c_gen, w2_c_popgrp,
           w2_hhid, w2_mdbdc2011, w2_prov2011, w2_geo2011,
           w2_hhincome) %>%
    pivot_longer(
      cols = c(w2_c_ed08att, w2_c_ed09att, w2_c_ed10cur),
      names_to = "var",
      values_to = "enrolled_raw"
    ) %>%
    mutate(
      year = case_when(
        str_detect(var, "08") ~ 2008,
        str_detect(var, "09") ~ 2009,
        str_detect(var, "10") ~ 2010
      ),
      enrolled = recode_enrollment(enrolled_raw),
      phase_from_curlev = if_else(year == 2010, 
                                  grade_to_phase(w2_c_ed10curlev), 
                                  NA_character_),
      phase_from_curgrd = if_else(year == 2010, 
                                  recode_phase(w2_c_ed10curgrd), 
                                  NA_character_),
      school_phase = case_when(
        year == 2010 ~ coalesce(phase_from_curlev, phase_from_curgrd),
        year == 2009 ~ grade_to_phase(w2_c_ed09lev - 1),
        year == 2008 ~ grade_to_phase(w2_c_ed09lev - 1),
        TRUE ~ NA_character_
      ),
      wave = 2,
      questionnaire = "child",
      dob_year = w2_c_dob_y,
      dob_month = w2_c_dob_m,
      gender = w2_c_gen,
      race = w2_c_popgrp,
      hhid = w2_hhid,
      district = w2_mdbdc2011,
      province = w2_prov2011,
      geotype = w2_geo2011,
      hh_income = w2_hhincome
    ) %>%
    select(pid, year, enrolled, school_phase, wave, questionnaire,
           dob_year, dob_month, gender, race, hhid, district, province, geotype,
           hh_income)
  
  w2_adult <- ahh_w2 %>%
    select(pid,
           w2_a_ed08att, w2_a_ed09att, w2_a_ed10cur,
           w2_a_ed09lev, w2_a_ed10curlev,
           w2_a_dob_m, w2_a_dob_y, w2_a_gen, w2_a_popgrp,
           w2_hhid, w2_mdbdc2011, w2_prov2011, w2_geo2011,
           w2_hhincome) %>%
    pivot_longer(
      cols = c(w2_a_ed08att, w2_a_ed09att, w2_a_ed10cur),
      names_to = "var",
      values_to = "enrolled_raw"
    ) %>%
    mutate(
      year = case_when(
        str_detect(var, "08") ~ 2008,
        str_detect(var, "09") ~ 2009,
        str_detect(var, "10") ~ 2010
      ),
      enrolled = recode_enrollment(enrolled_raw),
      grade_raw = case_when(
        year == 2008 ~ w2_a_ed09lev - 1,
        year == 2009 ~ w2_a_ed09lev,
        year == 2010 ~ w2_a_ed10curlev,
        TRUE ~ NA_real_
      ),
      school_phase = grade_to_phase(grade_raw),
      wave = 2,
      questionnaire = "adult",
      dob_year = w2_a_dob_y,
      dob_month = w2_a_dob_m,
      gender = w2_a_gen,
      race = w2_a_popgrp,
      hhid = w2_hhid,
      district = w2_mdbdc2011,
      province = w2_prov2011,
      geotype = w2_geo2011,
      hh_income = w2_hhincome
    ) %>%
    select(pid, year, enrolled, school_phase, wave, questionnaire,
           dob_year, dob_month, gender, race, hhid, district, province, geotype,
           hh_income)
  
  bind_rows(w2_child, w2_adult)
}

# STEP 5: RESHAPE WAVE 3 (2012) --------------------------------------------
reshape_wave3 <- function() {
  w3_child <- chh_w3 %>%
    select(pid,
           w3_c_ed10att, w3_c_ed11att, w3_c_ed12cur,
           w3_c_ed11lev, w3_c_ed12curlev, w3_c_ed12curgrd,
           w3_c_dob_m, w3_c_dob_y, w3_c_gen, w3_c_popgrp,
           w3_hhid, w3_mdbdc2011, w3_prov2011, w3_geo2011,
           w3_hhincome) %>%
    pivot_longer(
      cols = c(w3_c_ed10att, w3_c_ed11att, w3_c_ed12cur),
      names_to = "var",
      values_to = "enrolled_raw"
    ) %>%
    mutate(
      year = case_when(
        str_detect(var, "10") ~ 2010,
        str_detect(var, "11") ~ 2011,
        str_detect(var, "12") ~ 2012
      ),
      enrolled = recode_enrollment(enrolled_raw),
      phase_from_curlev = if_else(year == 2012, 
                                  grade_to_phase(w3_c_ed12curlev), 
                                  NA_character_),
      phase_from_curgrd = if_else(year == 2012, 
                                  recode_phase(w3_c_ed12curgrd), 
                                  NA_character_),
      school_phase = case_when(
        year == 2012 ~ coalesce(phase_from_curlev, phase_from_curgrd),
        year == 2011 ~ grade_to_phase(w3_c_ed11lev - 1),
        year == 2010 ~ grade_to_phase(w3_c_ed11lev - 1),
        TRUE ~ NA_character_
      ),
      wave = 3,
      questionnaire = "child",
      dob_year = w3_c_dob_y,
      dob_month = w3_c_dob_m,
      gender = w3_c_gen,
      race = w3_c_popgrp,
      hhid = w3_hhid,
      district = w3_mdbdc2011,
      province = w3_prov2011,
      geotype = w3_geo2011,
      hh_income = w3_hhincome
    ) %>%
    select(pid, year, enrolled, school_phase, wave, questionnaire,
           dob_year, dob_month, gender, race, hhid, district, province, geotype,
           hh_income)
  
  w3_adult <- ahh_w3 %>%
    select(pid,
           w3_a_ed10att, w3_a_ed11att, w3_a_ed12cur,
           w3_a_ed11lev, w3_a_ed12curlev,
           w3_a_dob_m, w3_a_dob_y, w3_a_gen, w3_a_popgrp,
           w3_hhid, w3_mdbdc2011, w3_prov2011, w3_geo2011,
           w3_hhincome) %>%
    pivot_longer(
      cols = c(w3_a_ed10att, w3_a_ed11att, w3_a_ed12cur),
      names_to = "var",
      values_to = "enrolled_raw"
    ) %>%
    mutate(
      year = case_when(
        str_detect(var, "10") ~ 2010,
        str_detect(var, "11") ~ 2011,
        str_detect(var, "12") ~ 2012
      ),
      enrolled = recode_enrollment(enrolled_raw),
      grade_raw = case_when(
        year == 2010 ~ w3_a_ed11lev - 1,
        year == 2011 ~ w3_a_ed11lev,
        year == 2012 ~ w3_a_ed12curlev,
        TRUE ~ NA_real_
      ),
      school_phase = grade_to_phase(grade_raw),
      wave = 3,
      questionnaire = "adult",
      dob_year = w3_a_dob_y,
      dob_month = w3_a_dob_m,
      gender = w3_a_gen,
      race = w3_a_popgrp,
      hhid = w3_hhid,
      district = w3_mdbdc2011,
      province = w3_prov2011,
      geotype = w3_geo2011,
      hh_income = w3_hhincome
    ) %>%
    select(pid, year, enrolled, school_phase, wave, questionnaire,
           dob_year, dob_month, gender, race, hhid, district, province, geotype,
           hh_income)
  
  bind_rows(w3_child, w3_adult)
}

# STEP 6: RESHAPE WAVE 4 (2014-2015) --------------------------------------
reshape_wave4 <- function() {
  w4_child <- chh_w4 %>%
    select(pid, w4_c_intrv_y,
           w4_c_ed13att, w4_c_ed14att, w4_c_ed15cur,
           w4_c_ed13lev, w4_c_ed14lev, w4_c_ed15curlev,
           w4_c_dob_m, w4_c_dob_y, w4_c_gen, w4_c_popgrp,
           w4_hhid, w4_mdbdc2011, w4_prov2011, w4_geo2011,
           w4_hhincome) %>%
    pivot_longer(
      cols = c(w4_c_ed13att, w4_c_ed14att, w4_c_ed15cur),
      names_to = "var",
      values_to = "enrolled_raw"
    ) %>%
    mutate(
      year_temp = case_when(
        str_detect(var, "13") ~ 2013,
        str_detect(var, "14") ~ 2014,
        str_detect(var, "15") ~ 2015
      ),
      year = if_else(str_detect(var, "cur"), w4_c_intrv_y, year_temp),
      enrolled = recode_enrollment(enrolled_raw),
      grade_raw = case_when(
        year == 2013 ~ w4_c_ed13lev,
        year == 2014 ~ w4_c_ed14lev,
        year == 2015 ~ w4_c_ed15curlev,
        TRUE ~ NA_real_
      ),
      school_phase = grade_to_phase(grade_raw),
      wave = 4,
      questionnaire = "child",
      dob_year = w4_c_dob_y,
      dob_month = w4_c_dob_m,
      gender = w4_c_gen,
      race = w4_c_popgrp,
      hhid = w4_hhid,
      district = w4_mdbdc2011,
      province = w4_prov2011,
      geotype = w4_geo2011,
      hh_income = w4_hhincome
    ) %>%
    select(pid, year, enrolled, school_phase, wave, questionnaire,
           dob_year, dob_month, gender, race, hhid, district, province, geotype,
           hh_income)
  
  w4_adult <- ahh_w4 %>%
    select(pid, w4_a_intrv_y,
           w4_a_ed13att, w4_a_ed14att, w4_a_ed15cur,
           w4_a_ed13lev, w4_a_ed14lev, w4_a_ed15curlev,
           w4_a_dob_m, w4_a_dob_y, w4_a_gen, w4_a_popgrp,
           w4_hhid, w4_mdbdc2011, w4_prov2011, w4_geo2011,
           w4_hhincome) %>%
    pivot_longer(
      cols = c(w4_a_ed13att, w4_a_ed14att, w4_a_ed15cur),
      names_to = "var",
      values_to = "enrolled_raw"
    ) %>%
    mutate(
      year_temp = case_when(
        str_detect(var, "13") ~ 2013,
        str_detect(var, "14") ~ 2014,
        str_detect(var, "15") ~ 2015
      ),
      year = if_else(str_detect(var, "cur"), w4_a_intrv_y, year_temp),
      enrolled = recode_enrollment(enrolled_raw),
      grade_raw = case_when(
        year == 2013 ~ w4_a_ed13lev,
        year == 2014 ~ w4_a_ed14lev,
        year == 2015 ~ w4_a_ed15curlev,
        TRUE ~ NA_real_
      ),
      school_phase = grade_to_phase(grade_raw),
      wave = 4,
      questionnaire = "adult",
      dob_year = w4_a_dob_y,
      dob_month = w4_a_dob_m,
      gender = w4_a_gen,
      race = w4_a_popgrp,
      hhid = w4_hhid,
      district = w4_mdbdc2011,
      province = w4_prov2011,
      geotype = w4_geo2011,
      hh_income = w4_hhincome
    ) %>%
    select(pid, year, enrolled, school_phase, wave, questionnaire,
           dob_year, dob_month, gender, race, hhid, district, province, geotype,
           hh_income)
  
  bind_rows(w4_child, w4_adult)
}

# STEP 7: RESHAPE WAVE 5 (2017) --------------------------------------------
reshape_wave5 <- function() {
  w5_child <- chh_w5 %>%
    select(pid,
           w5_c_ed08att, w5_c_ed09att, w5_c_ed10att, w5_c_ed11att,
           w5_c_ed12att, w5_c_ed13att, w5_c_ed14att, w5_c_ed15att,
           w5_c_ed16att, w5_c_ed17curatt,
           w5_c_ed08lev, w5_c_ed09lev, w5_c_ed10lev, w5_c_ed11lev,
           w5_c_ed12lev, w5_c_ed13lev, w5_c_ed14lev, w5_c_ed15lev,
           w5_c_ed16lev, w5_c_ed17curlev,
           w5_c_dob_m, w5_c_dob_y, w5_c_gen, w5_c_popgrp,
           w5_hhid, w5_mdbdc2011, w5_prov2011, w5_geo2011,
           w5_hhincome_extu) %>%
    pivot_longer(
      cols = starts_with("w5_c_ed") & (ends_with("att") | ends_with("curatt")),
      names_to = "var",
      values_to = "enrolled_raw"
    ) %>%
    mutate(
      year = case_when(
        str_detect(var, "08") ~ 2008,
        str_detect(var, "09") ~ 2009,
        str_detect(var, "10") ~ 2010,
        str_detect(var, "11") ~ 2011,
        str_detect(var, "12") ~ 2012,
        str_detect(var, "13") ~ 2013,
        str_detect(var, "14") ~ 2014,
        str_detect(var, "15") ~ 2015,
        str_detect(var, "16") ~ 2016,
        str_detect(var, "17") ~ 2017
      ),
      enrolled = recode_enrollment(enrolled_raw),
      grade_raw = case_when(
        year == 2008 ~ w5_c_ed08lev,
        year == 2009 ~ w5_c_ed09lev,
        year == 2010 ~ w5_c_ed10lev,
        year == 2011 ~ w5_c_ed11lev,
        year == 2012 ~ w5_c_ed12lev,
        year == 2013 ~ w5_c_ed13lev,
        year == 2014 ~ w5_c_ed14lev,
        year == 2015 ~ w5_c_ed15lev,
        year == 2016 ~ w5_c_ed16lev,
        year == 2017 ~ w5_c_ed17curlev,
        TRUE ~ NA_real_
      ),
      school_phase = grade_to_phase(grade_raw),
      wave = 5,
      questionnaire = "child",
      dob_year = w5_c_dob_y,
      dob_month = w5_c_dob_m,
      gender = w5_c_gen,
      race = w5_c_popgrp,
      hhid = w5_hhid,
      district = w5_mdbdc2011,
      province = w5_prov2011,
      geotype = w5_geo2011,
      hh_income = w5_hhincome_extu
    ) %>%
    select(pid, year, enrolled, school_phase, wave, questionnaire,
           dob_year, dob_month, gender, race, hhid, district, province, geotype,
           hh_income)
  
  w5_adult <- ahh_w5 %>%
    select(pid,
           w5_a_ed08att, w5_a_ed09att, w5_a_ed10att, w5_a_ed11att,
           w5_a_ed12att, w5_a_ed13att, w5_a_ed14att, w5_a_ed15att,
           w5_a_ed16att, w5_a_ed17cur,
           w5_a_ed08lev, w5_a_ed09lev, w5_a_ed10lev, w5_a_ed11lev,
           w5_a_ed12lev, w5_a_ed13lev, w5_a_ed14lev, w5_a_ed15lev,
           w5_a_ed16lev, w5_a_ed17curlev,
           w5_a_dob_m, w5_a_dob_y, w5_a_gen, w5_a_popgrp,
           w5_hhid, w5_mdbdc2011, w5_prov2011, w5_geo2011,
           w5_hhincome_extu) %>%
    pivot_longer(
      cols = starts_with("w5_a_ed") & (ends_with("att") | ends_with("cur")),
      names_to = "var",
      values_to = "enrolled_raw"
    ) %>%
    mutate(
      year = case_when(
        str_detect(var, "08") ~ 2008,
        str_detect(var, "09") ~ 2009,
        str_detect(var, "10") ~ 2010,
        str_detect(var, "11") ~ 2011,
        str_detect(var, "12") ~ 2012,
        str_detect(var, "13") ~ 2013,
        str_detect(var, "14") ~ 2014,
        str_detect(var, "15") ~ 2015,
        str_detect(var, "16") ~ 2016,
        str_detect(var, "17") ~ 2017
      ),
      enrolled = recode_enrollment(enrolled_raw),
      grade_raw = case_when(
        year == 2008 ~ w5_a_ed08lev,
        year == 2009 ~ w5_a_ed09lev,
        year == 2010 ~ w5_a_ed10lev,
        year == 2011 ~ w5_a_ed11lev,
        year == 2012 ~ w5_a_ed12lev,
        year == 2013 ~ w5_a_ed13lev,
        year == 2014 ~ w5_a_ed14lev,
        year == 2015 ~ w5_a_ed15lev,
        year == 2016 ~ w5_a_ed16lev,
        year == 2017 ~ w5_a_ed17curlev,
        TRUE ~ NA_real_
      ),
      school_phase = grade_to_phase(grade_raw),
      wave = 5,
      questionnaire = "adult",
      dob_year = w5_a_dob_y,
      dob_month = w5_a_dob_m,
      gender = w5_a_gen,
      race = w5_a_popgrp,
      hhid = w5_hhid,
      district = w5_mdbdc2011,
      province = w5_prov2011,
      geotype = w5_geo2011,
      hh_income = w5_hhincome_extu
    ) %>%
    select(pid, year, enrolled, school_phase, wave, questionnaire,
           dob_year, dob_month, gender, race, hhid, district, province, geotype,
           hh_income)
  
  bind_rows(w5_child, w5_adult)
}

# STEP 8: COMBINE ALL WAVES ------------------------------------------------
w1_long <- reshape_wave1()
w2_long <- reshape_wave2()
w3_long <- reshape_wave3()
w4_long <- reshape_wave4()
w5_long <- reshape_wave5()
all_waves <- bind_rows(w1_long, w2_long, w3_long, w4_long, w5_long)

# STEP 9: DEDUPLICATE PERSON-YEARS -----------------------------------------
all_waves <- all_waves %>%
  mutate(
    is_contemporaneous = case_when(
      wave == 1 & year == 2008 ~ 1,
      wave == 2 & year %in% c(2010, 2011) ~ 1,
      wave == 3 & year == 2012 ~ 1,
      wave == 4 & year %in% c(2014, 2015) ~ 1,
      wave == 5 & year == 2017 ~ 1,
      TRUE ~ 0
    ),
    has_phase = !is.na(school_phase)
  )

enrollment_panel <- all_waves %>%
  group_by(pid, year) %>%
  arrange(desc(is_contemporaneous), desc(has_phase), desc(wave)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-has_phase)

# STEP 10: ADD AGE -----------------------------------------------
enrollment_panel <- enrollment_panel %>%
  mutate(
    dob_year_clean = recode_dob(dob_year),
    dob_month_clean = recode_dob(dob_month),
    age = year - dob_year_clean
  ) %>%
  filter(!is.na(enrolled))

# STEP 11: CLEAN UP --------------------------------------------------------
enrollment_panel <- enrollment_panel %>%
  mutate(
    gender = case_when(
      gender == -8 ~ NA_character_,
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Male", "Female")),
    
    race = case_when(
      race %in% c(-8, -3) ~ NA_character_,
      race == 1 ~ "Black",
      race == 2 ~ "Coloured",
      race == 3 ~ "Asian/Indian",
      race == 4 ~ "White",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Black", "Coloured", "Asian/Indian", "White"))
  ) %>%
  select(pid, year, enrolled, school_phase, age, wave, questionnaire, 
         gender, race, hhid, district, province, geotype,
         dob_year, dob_month, is_contemporaneous, hh_income) %>%
  arrange(pid, year)

# STEP 12: SUMMARY STATISTICS ----------------------------------------------
cat("\n=== ENROLLMENT PANEL SUMMARY ===\n")
cat("Total person-years:", nrow(enrollment_panel), "\n")
cat("Unique individuals:", n_distinct(enrollment_panel$pid), "\n")
cat("Years covered:", min(enrollment_panel$year), "to", max(enrollment_panel$year), "\n\n")

cat("Observations by wave:\n")
print(enrollment_panel %>% count(wave))

cat("\nObservations by year:\n")
print(enrollment_panel %>% count(year))

cat("\nEnrollment rate by year:\n")
print(enrollment_panel %>% 
        group_by(year) %>% 
        summarize(enrollment_rate = mean(enrolled, na.rm = TRUE)))

cat("\nAge distribution:\n")
print(summary(enrollment_panel$age))

cat("\nHousehold income distribution:\n")
print(summary(enrollment_panel$hh_income))

# Save the panel
saveRDS(enrollment_panel, "enrollment_panel.rds")

enrollment_panel
