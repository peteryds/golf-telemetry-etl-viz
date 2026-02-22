# ==============================================================================
# Script Name: metrics_calculation.R
# Description: Filters the classified dataset for "Solid Strikes" only and 
#              calculates presentation-ready average metrics.
# ==============================================================================

library(tidyverse)
library(logger)

calculate_solid_averages <- function(df_classified, club_order) {
  log_info("Calculating average metrics for Solid Strikes...")
  
  df_classified %>%
    filter(strike_type == "5. Solid Strike") %>%
    group_by(club_type) %>%
    summarise(
      solid_shots      = n(),
      avg_carry        = mean(carry_distance, na.rm = TRUE),
      avg_total        = mean(total_distance, na.rm = TRUE),
      avg_smash        = mean(smash_factor, na.rm = TRUE),
      avg_spin         = mean(spin_rate, na.rm = TRUE),
      avg_attack_angle = mean(attack_angle, na.rm = TRUE),
      avg_club_speed   = mean(club_speed, na.rm = TRUE),
      avg_face_to_path = mean(face_to_path, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(club_type = factor(club_type, levels = club_order)) %>%
    arrange(club_type) %>%
    mutate(
      avg_carry        = round(avg_carry, 1),
      avg_total        = round(avg_total, 1),
      avg_smash        = round(avg_smash, 2),
      avg_spin         = round(avg_spin, 0),
      avg_club_speed   = round(avg_club_speed, 1),
      avg_attack_angle = sprintf("%+.1f°", avg_attack_angle),
      avg_face_to_path = sprintf("%+.1f°", avg_face_to_path)
    ) %>%
    rename(
      `Club Type`            = club_type,
      `Solid Shots`          = solid_shots,
      `Avg Carry (yds)`      = avg_carry,
      `Avg Total (yds)`      = avg_total,
      `Avg Smash`            = avg_smash,
      `Avg Spin (rpm)`       = avg_spin,
      `Avg Attack Angle`     = avg_attack_angle,
      `Avg Club Speed (mph)` = avg_club_speed,
      `Avg Face to Path`     = avg_face_to_path
    )
}