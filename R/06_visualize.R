# R/06_visualize.R
# Visualizes wage premia trends for selected MSAs from the pooled model output.

source("R/00_packages.R")

# --- 1. Define File Paths & Parameters ---
cat("--- 06_visualize.R: Starting visualization of pooled wage premia trends ---\n")

# Input file from R/05_deflate.R (using pooled model data)
input_file <- "data/output/msa_wage_premia_pooled_simplified_cpi.csv"

# Output directory and plot file names
output_dir_plots <- "data/output/"
dir.create(output_dir_plots, showWarnings = FALSE, recursive = TRUE)
plot_file_theta_normalized <- file.path(output_dir_plots, "msa_premia_trends_pooled_theta_normalized.png")

# Parameters
num_msas_to_plot <- 10 # Number of MSAs to plot (e.g., top N by recent population or a fixed list)

# --- 2. Load Data ---
cat("--- Loading processed data ---\n")
if (!file.exists(input_file)) {
  stop(paste("Input file not found:", input_file, " - Please run R/05_deflate.R for the pooled model."))
}
premia_data <- fread(input_file)

cat("First few rows of loaded premia_data (input to R/06_visualize.R):\n")
print(head(premia_data))
cat("Structure of loaded premia_data:\n")
str(premia_data)

# Ensure met2013 is character right after loading
premia_data[, met2013 := as.character(met2013)]
cat(sprintf("Loaded pooled premia data with CPI. Rows: %d, Columns: %d\n", nrow(premia_data), ncol(premia_data)))

if (!("theta_normalized" %in% names(premia_data))) {
  stop("Column 'theta_normalized' not found in the input data. Check R/05_deflate.R output.")
}
if (!("n_observations" %in% names(premia_data))) {
  warning("Column 'n_observations' (for weighting population proxy) not found. Will not use weights for MSA selection if so.")
  # Add a dummy if missing to prevent error, though it won't be used effectively
  if(!("n_observations" %in% names(premia_data))) premia_data[, n_observations := 1]
}


# --- 3. Select MSAs to Plot ---
cat("--- Selecting MSAs to plot ---\n")

# Option 1: Select MSAs with the largest average number of observations (as a proxy for population/robustness)
# We need a consistent measure across years. Let's use average n_observations over all years.
msa_avg_size <- premia_data[, .(avg_n_obs = mean(n_observations, na.rm = TRUE)), by = met2013]
setorder(msa_avg_size, -avg_n_obs)

cat("Top N MSAs by average n_observations (msa_avg_size table):\n")
print(head(msa_avg_size, num_msas_to_plot))

top_msas_codes <- head(msa_avg_size, num_msas_to_plot)$met2013

cat("Selected MET2013 codes for plotting:", paste(top_msas_codes, collapse=", "), "\n")

# Filter data for these MSAs
plot_data <- premia_data[met2013 %in% top_msas_codes]

if (nrow(plot_data) == 0) {
  stop("No data to plot after filtering for top MSAs. Check MSA selection logic and input data.")
}

# Ensure NAME is a factor for consistent ggplot grouping and legend
plot_data[, NAME := factor(met2013)] 

# For MSA names in plot legend (requires fertility data or other lookup)
# For now, we will use MET2013 codes. If fertility data path is stable, we can re-integrate name merging.
# fertility_data_file <- "/Users/tommasodesanto/Desktop/Projects/Fertility/Codes/R/msa_panel_output/msa_panel_complete.csv"
# if (file.exists(fertility_data_file)) {
#   msa_names_lookup <- unique(fread(fertility_data_file)[, .(met2013 = as.character(GEOID), NAME)])
#   plot_data <- merge(plot_data, msa_names_lookup, by = "met2013", all.x = TRUE)
#   plot_data[is.na(NAME), NAME := met2013] # Fallback to code if name not found
# } else {
  plot_data[, NAME := met2013] # Use met2013 codes if name lookup fails
# }

# --- 4. Create and Save Plot for theta_normalized ---
cat("--- Generating plot for theta_normalized ---\n")

trend_plot_theta_norm <- ggplot(plot_data, aes(x = year, y = theta_normalized, group = NAME, color = NAME)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(plot_data$year, na.rm=T), max(plot_data$year, na.rm=T), by = 2)) +
  labs(
    title = paste("Wage Premia Trends for Top", num_msas_to_plot, "Largest MSAs (Pooled Model)"),
    subtitle = "Premia (theta_normalized) are relative to median MSA in median year (2014)",
    x = "Year",
    y = "Log Wage Premium (theta_normalized)",
    color = "MSA (MET2013 Code)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(plot_file_theta_normalized, plot = trend_plot_theta_norm, width = 12, height = 8, dpi = 300)
cat(paste("Plot saved to:", plot_file_theta_normalized, "\n"))

# --- Optional: Plot demeaned theta_normalized (demeaned relative to annual mean of theta_normalized) ---
# This would show MSA deviations from the simple average of *these specific premia* each year.
# Given the normalization already applied, this might be less critical but can be added if desired.

cat("--- 06_visualize.R: Finished ---\n") 