# ==============================================================================
# Script Name: data_processing.R
# Description: Handles data ingestion, cleaning, and the core diagnostic logic
#              to classify shots based on dynamic, club-specific medians.
# ==============================================================================

library(tidyverse)
library(janitor)
library(logger)

#' Load and Clean Golf Telemetry Data
#' Includes Error Handling and Validation
load_and_clean_data <- function(file_path) {
  
  # Validation: Check if file exists
  if (!file.exists(file_path)) {
    log_error("Input file not found: {file_path}")
    stop("Execution halted: Missing input file.")
  }
  
  log_info("Loading data from {file_path}")
  
  # Error Handling: Use tryCatch for safe reading
  df <- tryCatch({
    cols <- colnames(read_csv(file_path, n_max = 0, show_col_types = FALSE))
    read_csv(file_path, skip = 2, col_names = cols, show_col_types = FALSE) %>% clean_names()
  }, error = function(e) {
    log_error("Failed to read CSV file: {e$message}")
    stop("Execution halted: CSV parsing error.")
  })
  
  # Validation: Ensure required columns exist
  required_cols <- c("carry_distance", "club_type", "smash_factor", "launch_angle", "spin_rate")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    log_error("Missing required columns: {paste(missing_cols, collapse = ', ')}")
    stop("Execution halted: Invalid data schema.")
  }
  
  log_info("Data successfully loaded. Converting data types...")
  
  df <- df %>%
    mutate(
      across(any_of(c("carry_distance", "total_distance", "smash_factor", "launch_angle", 
                      "spin_rate", "carry_deviation_distance", "attack_angle", "club_speed", 
                      "face_to_path")), as.numeric)
    ) %>%
    filter(!is.na(carry_distance) & !is.na(club_type))
  
  if(!"total_distance" %in% names(df)) df <- df %>% mutate(total_distance = carry_distance)
  
  log_info("Data cleaning complete. Processed {nrow(df)} valid shots.")
  return(df)
}

#' Classify Golf Shots using dynamic configurations
classify_shots <- function(df, config_thresh) {
  log_info("Applying shot classification logic...")
  
  df %>%
    group_by(club_type) %>%
    mutate(
      median_carry  = median(carry_distance, na.rm = TRUE),
      median_smash  = median(smash_factor, na.rm = TRUE),
      median_launch = median(launch_angle, na.rm = TRUE),
      median_spin   = median(spin_rate, na.rm = TRUE),
      
      strike_type = case_when(
        launch_angle < (median_launch - config_thresh$thin_launch_drop) & 
          carry_distance < (median_carry * config_thresh$thin_carry_pct) ~ "1. Thin Shot",
        
        carry_distance < (median_carry * config_thresh$fat_carry_pct) & 
          spin_rate < (median_spin * config_thresh$fat_spin_pct)         ~ "2. Fat Shot",
        
        smash_factor < (median_smash * config_thresh$offcenter_smash_pct)~ "3. Off-center",
        
        abs(carry_deviation_distance) > (config_thresh$severe_offline_pct * median_carry) ~ "4. Severe Offline",
        
        TRUE ~ "5. Solid Strike"
      )
    ) %>%
    ungroup()
}