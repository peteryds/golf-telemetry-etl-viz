# ==============================================================================
# Script Name: main.R
# Description: Orchestrator script with Logging, Config, and Error Handling.
# ==============================================================================

# Load system packages
library(logger)

# Configure logger to output to both console and a log file
log_appender(appender_tee("pipeline.log"))
log_info("🚀 Starting the Golf Telemetry ETL & Viz Pipeline...")

# Load configurations from config.yml
cfg <- config::get()
log_info("Configuration loaded successfully.")

# Source modular functions
tryCatch({
  source("analysis/R/data_processing.R")
  source("analysis/R/metrics_calculation.R")
  source("viz/visualize_quality.R")
  source("viz/visualize_misses.R")
  source("viz/visualize_table.R")
}, error = function(e) {
  log_error("Failed to source modules: {e$message}")
  stop("Execution halted.")
})

# ==============================================================================
# PIPELINE EXECUTION
# ==============================================================================

# Step 1: Ingest and Classify
df_raw <- load_and_clean_data(cfg$paths$input_file)

# Pass the thresholds from config to the function
df_classified <- classify_shots(df_raw, cfg$thresholds) 

# Step 2: Calculate Metrics for Solid Strikes
# Pass the club order from config
solid_metrics <- calculate_solid_averages(df_classified, cfg$clubs$order)

# Export metrics
write_csv(solid_metrics, cfg$paths$output_csv)
log_info("✅ Successfully exported metrics to: {cfg$paths$output_csv}")

# Step 3: Generate Visualizations
log_info("Generating visual reports...")
plot_shot_quality(df_classified, output_path = cfg$paths$plot_quality)
plot_miss_direction(df_classified, output_path = cfg$paths$plot_misses)
plot_solid_averages_table(df_classified, output_path = cfg$paths$plot_table)

log_info("🎉 Pipeline execution completed successfully!")