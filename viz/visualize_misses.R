# ==============================================================================
# Script Name: visualize_misses.R
# Description: Isolates "Severe Offline" shots to create a diverging bar chart,
#              revealing directional biases. Includes safety checks for empty data.
# ==============================================================================

library(tidyverse)
library(ggplot2)

plot_miss_direction <- function(df_classified, output_path = "Diverging_Miss_Direction.png") {
  
  # 1. Filter for severe offline shots
  df_miss_raw <- df_classified %>%
    filter(strike_type == "4. Severe Offline")
    
  # 🛡️ Safety Net: If there are no severe offline shots, print a message 
  # and skip plotting to prevent execution errors (e.g., when the player hits perfectly).
  if (nrow(df_miss_raw) == 0) {
    cat("No 'Severe Offline' shots found. Skipping miss direction plot.\n")
    return(invisible(NULL))
  }
  
  # 2. Calculate left/right deviations
  df_miss <- df_miss_raw %>%
    mutate(miss_direction = if_else(carry_deviation_distance > 0, "Right", "Left")) %>%
    group_by(club_type, miss_direction) %>%
    summarise(shot_count = n(), .groups = "drop") %>%
    pivot_wider(names_from = miss_direction, values_from = shot_count, values_fill = list(shot_count = 0))
  
  # Ensure both Left and Right columns exist (even if the player only misses to one side)
  if(!"Left" %in% names(df_miss)) df_miss$Left <- 0
  if(!"Right" %in% names(df_miss)) df_miss$Right <- 0
    
  df_miss <- df_miss %>%
    mutate(
      Total_Offline = Left + Right,
      Left_Pct = round((Left / Total_Offline) * 100, 1),
      Right_Pct = round((Right / Total_Offline) * 100, 1),
      Left_Plot = -Left
    ) %>%
    # pivot_longer transforms the 'Right' column into 'Count'
    pivot_longer(cols = c(Left_Plot, Right), names_to = "Direction", values_to = "Count") %>%
    mutate(
      # 🐞 Bug Fix: Use 'Count' instead of the melted 'Right' column
      Label = case_when(
        Direction == "Left_Plot" & Left > 0 ~ paste0(Left, " (", Left_Pct, "%)"),
        Direction == "Right" & Count > 0 ~ paste0(Count, " (", Right_Pct, "%)"),
        TRUE ~ ""
      ),
      Hjust_Value = case_when(
        Direction == "Left_Plot" & Count < -5 ~ 0.5,
        Direction == "Left_Plot" & Count >= -5 ~ 1.1,
        Direction == "Right" & Count > 5 ~ 0.5,
        Direction == "Right" & Count <= 5 ~ -0.1,
        TRUE ~ 0.5
      ),
      Text_Color = if_else(abs(Count) > 5, "white", "black")
    )
  
  club_levels <- c("Pitching Wedge", "9 Iron", "8 Iron", "7 Iron", "6 Iron", "3 Wood", "Driver")
  df_miss <- df_miss %>% mutate(club_type = factor(club_type, levels = club_levels))
  
  # 3. Generate the Diverging Bar Chart
  plot <- ggplot(df_miss, aes(x = Count, y = club_type, fill = Direction)) +
    geom_bar(stat = "identity", color = "black", width = 0.6) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +
    geom_text(aes(label = Label, hjust = Hjust_Value, color = Text_Color), size = 4, fontface = "bold", show.legend = FALSE) +
    scale_fill_manual(
      values = c("Left_Plot" = "#1E90FF", "Right" = "#FF4500"), 
      labels = c("Left_Plot" = "Left Miss (Pull / Hook)", "Right" = "Right Miss (Push / Slice)")
    ) +
    scale_color_identity() +
    labs(
      title = "Severe Offline Analysis: Left vs. Right Misses", 
      subtitle = "Counts & % of shots deviating > 20% of median carry distance",
      x = NULL, y = NULL, fill = NULL
    ) +
    scale_x_continuous(limits = c(-45, 45), breaks = NULL) + 
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, margin = margin(b = 10)),
      plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 20)),
      axis.text.y = element_text(face = "bold", size = 13, color = "black"),
      axis.text.x = element_blank(),
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.x = element_blank(),
      legend.position = "top", 
      legend.justification = "right"
    ) +
    annotate("text", x = 2, y = 7.5, label = "Target Line", fontface = "bold.italic", size = 4, hjust = 0)
  
  ggsave(output_path, plot = plot, width = 11, height = 7, dpi = 300)
  cat("Generated:", output_path, "\n")
}