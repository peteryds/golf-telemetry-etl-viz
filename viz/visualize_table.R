# ==============================================================================
# Script Name: visualize_table.R
# Description: Calculates the average metrics for "Solid Strikes" and generates
#              a cleanly formatted graphical table saved as a PNG.
# ==============================================================================

library(tidyverse)
library(gridExtra)
library(grid)
library(gtable)

plot_solid_averages_table <- function(df_classified, output_path = "output/Solid_Strike_Table.png") {
  
  # 1. Filter for Solid Strikes and calculate averages
  df_solid <- df_classified %>%
    filter(strike_type == "5. Solid Strike") %>%
    group_by(club_type) %>%
    summarise(
      `Avg Carry (yds)` = round(mean(carry_distance, na.rm = TRUE), 1),
      `Avg Total (yds)` = round(mean(total_distance, na.rm = TRUE), 1),
      `Avg Smash Factor` = round(mean(smash_factor, na.rm = TRUE), 2),
      `Avg Spin (rpm)` = round(mean(spin_rate, na.rm = TRUE), 0),
      `Shot Count` = n(),
      .groups = "drop"
    )
  
  # Safety Net: If there are no solid strikes, skip table generation
  if(nrow(df_solid) == 0) {
    cat("No 'Solid Strike' shots found. Skipping table generation.\n")
    return(invisible(NULL))
  }
  
  # 2. Sort the clubs in logical order from shortest to longest
  # Note: Adjust these levels if your bag setup changes!
  club_levels <- c("Pitching Wedge", "9 Iron", "8 Iron", "7 Iron", "6 Iron", "3 Wood", "Driver")
  df_solid <- df_solid %>%
    mutate(club_type = factor(club_type, levels = club_levels)) %>%
    arrange(desc(club_type)) %>% # Longest club at the top
    rename(Club = club_type)
  
  # 3. Design the graphical table theme
  # We use the same "Solid Strike" green color (#32CD32) for the header to match the charts!
  ttheme_custom <- ttheme_default(
    core = list(
      bg_params = list(fill = c("#F9F9F9", "#FFFFFF"), col = NA),
      fg_params = list(fontsize = 11)
    ),
    colhead = list(
      bg_params = list(fill = "#32CD32", col = "black"),
      fg_params = list(col = "white", fontface = "bold", fontsize = 12)
    )
  )
  
  # Convert the data frame to a table grob (graphical object)
  table_grob <- tableGrob(df_solid, rows = NULL, theme = ttheme_custom)
  
  # 4. Add a clean title to the top of the table
  title_grob <- textGrob(
    "Solid Strike Baseline Averages", 
    gp = gpar(fontsize = 16, fontface = "bold")
  )
  
  padding <- unit(1, "line")
  final_grob <- gtable_add_rows(table_grob, heights = grobHeight(title_grob) + padding, pos = 0)
  final_grob <- gtable_add_grob(final_grob, title_grob, 1, 1, 1, ncol(final_grob))
  
  # 5. Save as a PNG file
  # Automatically calculate the width and height based on the table's content
  h <- convertHeight(grobHeight(final_grob), "in", valueOnly = TRUE) + 0.5
  w <- convertWidth(grobWidth(final_grob), "in", valueOnly = TRUE) + 0.5
  
ggsave(
    filename = output_path, 
    plot = final_grob, 
    width = max(w, 8.5), 
    height = max(h, 3.5),  
    dpi = 300, 
    bg = "white"
  )
  
  cat("Generated:", output_path, "\n")
}