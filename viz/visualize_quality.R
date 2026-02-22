# ==============================================================================
# Script Name: 01_visualize_quality.R
# Description: Generates a stacked bar chart displaying the breakdown of miss 
#              types vs. solid strikes, with a dedicated definitions sidebar.
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(grid)
library(gridtext)
library(patchwork) # Added for advanced layout management

#' Plot Shot Quality Breakdown
#' 
#' @param df_classified The classified data frame.
#' @param output_path File path to save the generated PNG.
plot_shot_quality <- function(df_classified, output_path = "output/Shot_Quality_Solid_Base.png") {
  
  # 1. Calculate percentages and construct labels
  strike_summary <- df_classified %>%
    group_by(club_type) %>%
    mutate(total_shots = n()) %>% 
    group_by(club_type, strike_type, total_shots) %>%
    summarise(count = n(), .groups = "drop") %>%
    mutate(percentage = (count / total_shots) * 100) %>%
    # Append the sample size (n) to the club label for context
    mutate(club_label = paste0(club_type, "\n(n=", total_shots, ")"))
  
  # Set strict factor levels to ensure correct rendering order
  club_levels <- c("Driver", "3 Wood", "6 Iron", "7 Iron", "8 Iron", "9 Iron", "Pitching Wedge")
strike_levels <- c(
    "1. Thin Shot", 
    "2. Fat Shot", 
    "3. Off-center", 
    "4. Severe Offline", 
    "5. Solid Strike"
  )
  
  strike_summary <- strike_summary %>%
    mutate(
      club_type = factor(club_type, levels = club_levels),
      strike_type = factor(strike_type, levels = strike_levels)
    ) %>%
    arrange(club_type) %>%
    mutate(club_label = factor(club_label, levels = unique(club_label)))
  
  # Define custom hex colors
  custom_colors <- c(
    "5. Solid Strike"   = "#32CD32", "1. Thin Shot"      = "#EAEAEA", 
    "2. Fat Shot"       = "#CCCCCC", "3. Off-center"     = "#999999", 
    "4. Severe Offline" = "#666666"
  )
  
  # 2. Build the main stacked bar chart
  p_main <- ggplot(strike_summary, aes(x = club_label, y = percentage, fill = strike_type)) +
    geom_bar(stat = "identity", color = "black", width = 0.75) +
    geom_text(
      aes(
        # Logic: Only display percentage text if the segment is >= 5% to prevent overflow
        label = ifelse(percentage >= 5, paste0(round(percentage, 1), "%"), ""),
        # Adaptive text color for readability against light/dark backgrounds
        color = ifelse(strike_type %in% c("1. Thin Shot", "2. Fat Shot"), "black", "white")
      ), 
      position = position_stack(vjust = 0.5), size = 3.5, fontface = "bold", show.legend = FALSE
    ) +
    scale_fill_manual(values = custom_colors) +
    scale_color_identity() +
    labs(
      title = "Shot Quality Breakdown by Club", 
      subtitle = "(Solid Strikes Plotted at the Base for Stability Evaluation)",
      x = "Club Type", y = "Percentage of Shots (%)", fill = "Strike Type"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      axis.text.x = element_text(face = "bold", size = 11, lineheight = 1.2),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom" # Move legend to the bottom to free up horizontal space
    )
  
  # 3. Create a dedicated text block for definitions
  # Using HTML/Markdown tags supported by gridtext for rich text formatting
  definitions_text <- "Missed Shots Definitions:\n\n<span style='color:#32CD32'>**• Solid Strike:**</span>\n  Good Contact & Direction\n\n**• Thin Shot:**\n  Launch < (Median - 5°) &\n  Carry < 95%\n\n**• Fat Shot:**\n  Carry < 70% &\n  Spin < 80%\n\n**• Off-center:**\n  Smash Factor < 80%\n\n**• Severe Offline:**\n  Deviation Dist > 20%\n  of Carry Dist."
  
  # Generate an empty ggplot canvas to hold the rich text grob
  p_text <- ggplot() +
    theme_void() + 
    annotation_custom(
      grob = richtext_grob(definitions_text, gp = gpar(fontsize = 11, lineheight = 1.4), hjust = 0, vjust = 1),
      xmin = -1, xmax = 1, ymin = 0, ymax = 1
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off")
  
  # 4. Compose the final layout using patchwork
  # The main plot takes up roughly 75% of the width, and the text sidebar takes 25%
  plot_final <- p_main + p_text + plot_layout(widths = c(4, 1.2))
  
  # 5. Export and save the composite image
  # bg = "white" prevents transparent backgrounds in some IDE environments
  # 5. Export and save the final combined plot
# Add explicit right margin to prevent text from being clipped
ggsave(
  output_path,
  # We add a 2cm margin to the right side of the final patchwork plot
  plot = plot_final + theme(plot.margin = margin(r = 2, unit = "cm")),
  width = 14, # Slightly increase total width to accommodate the new margin
  height = 7,
  dpi = 300,
  bg = "white"
)
  cat("Generated:", output_path, "\n")  
}